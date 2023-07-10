;;; org-heatmap.el --- Show heatmap in calendar  -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/.elemacs
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (org "9.6") (emacsql "20230220"))
;; Keywords: Calendar, Org, Habits
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'calendar)
(require 'org-habit)
(require 'org-clock)
(eval-when-compile
  (if (require 'sqlite nil t)
	  (require 'emacsql-sqlite-builtin)
	(require 'emacsql-sqlite)))

(defface org-heatmap-habit-statics '((t (:foreground "#B0BEC5"))) "")

(defface org-heatmap-calendar-scale-1  '((((background light)) :foreground "black" :background "#c6e48b")
										 (((background dark))  :foreground "white" :background "#c6e48b"))
  "Face for fewer activities.")
(defface org-heatmap-calendar-scale-2  '((((background light)) :foreground "black" :background "#7bc96f")
										 (((background dark))  :foreground "white" :background "#7bc96f"))
  "Face for few activities.")
(defface org-heatmap-calendar-scale-3  '((((background light)) :foreground "black" :background "#239a3b")
										 (((background dark))  :foreground "white" :background "#239a3b"))
  "Face for medium activities.")
(defface org-heatmap-calendar-scale-4  '((((background light)) :foreground "black" :background "#196127")
										 (((background dark))  :foreground "white" :background "#196127"))
  "Face for many activities.")

(defface org-heatmap-overview-calendar-scale-1  '((((background light)) :foreground "#c6e48b")
												  (((background dark))  :foreground "#c6e48b"))
  "Face for fewer activities.")
(defface org-heatmap-overview-calendar-scale-2  '((((background light)) :foreground "#7bc96f")
												  (((background dark))  :foreground "#7bc96f"))
  "Face for few activities.")
(defface org-heatmap-overview-calendar-scale-3  '((((background light)) :foreground "#239a3b")
												  (((background dark))  :foreground "#239a3b"))
  "Face for medium activities.")
(defface org-heatmap-overview-calendar-scale-4  '((((background light)) :foreground "#196127")
												  (((background dark))  :foreground "#196127"))
  "Face for many activities.")

(defface org-heatmap-empty-rectangle '((t (:foreground "#B0BEC5"))) "")

(defgroup org-heatmap nil
  "Settings for `org-heatmap'."
  :group 'org)

(defcustom org-heatmap-database-connector (if (and (progn
													 (require 'emacsql-sqlite-builtin nil t)
													 (functionp 'emacsql-sqlite-builtin))
                                                   (functionp 'sqlite-open))
											  'sqlite-builtin
											'sqlite)
  "The database connector used by org-heatmap.

If you are using Emacs 29, then the recommended connector is
`sqlite-builtin', which uses the new builtin support for SQLite.
You need to install the `emacsql-sqlite-builtin' package to use
this connector.
If you are using an older Emacs release, please `sqlite'"
  :group 'org-heatmap
  :type '(choice 
          (const sqlite-builtin)
          (const sqlite)))

(defcustom org-heatmap-enable-habit-statics t
  "Whether to shoaw habit statics.

Add three data after the habit entry: (current streak, max streak
  and total done number).

*Note*: If you want to chage this variable, please set it before
loading org-heatmap or use `setopt'."
  :group 'org-heatmap
  :type 'boolean
  :set (lambda (sym val)
		 (set-default sym val)
		 (cond
		  (val
		   (advice-add 'org-habit-parse-todo :around #'org-heatmap-habit-parse-todo-advice)
		   (add-hook 'org-agenda-finalize-hook #'org-heatmap-habit-add-streak))
		  (t
		   (advice-remove 'org-habit-parse-todo #'org-heatmap-habit-parse-todo-advice)
		   (remove-hook 'org-agenda-finalize-hook #'org-heatmap-habit-add-streak)))))

(defcustom org-heatmap-rectangle "██"
  ""
  :group 'org-heatmap
  :type 'string)

(defcustom org-heatmap-threshold '((default . ((0 . default)
											   (1 . org-heatmap-calendar-scale-1)
											   (3 . org-heatmap-calendar-scale-2)
											   (5 . org-heatmap-calendar-scale-3)
											   (7 . org-heatmap-calendar-scale-4)))
								   (habit . ((0 . org-heatmap-calendar-scale-1)
											 (15 . org-heatmap-calendar-scale-2)
											 (30 . org-heatmap-calendar-scale-3)
											 (60 . org-heatmap-calendar-scale-4)))
								   (overview . ((0 . org-heatmap-overview-calendar-scale-1)
												(15 . org-heatmap-overview-calendar-scale-2)
												(30 . org-heatmap-overview-calendar-scale-3)
												(60 . org-heatmap-overview-calendar-scale-4))))
  "Choose a different face based on the threshold arrived."
  :group 'org-heatmap
  :type '(repeat (cons symbol (cons number symbol))))

(defcustom org-heatmap-get-threshold-function #'org-heatmap-get-threshold-defualt
  "Function used to get threshold."
  :group 'org-heatmap
  :type 'function)

(defcustom org-heatmap-db-location "~/.emacs.d/var/org/org-heatmap.db"
  "Default database location."
  :group 'org-heatmap
  :type 'directory)

(defvar org-heatmap-current-streak nil
  "Hash table used to store current streak.")

;;;; database
;;; the code in this section is mainly learned from org-roam.

(declare-function emacsql-sqlite "ext:emacsql-sqlite")
(declare-function emacsql-sqlite-builtin "ext:emacsql-sqlite-builtin")
(defun org-heatmap-db--conn-fn ()
  "Return the function for creating the database connection."
  (cl-case org-heatmap-database-connector
	(sqlite-builtin
	 (require 'emacsql-sqlite-builtin)
	 #'emacsql-sqlite-builtin)
	(sqlite
	 (require 'emacsql-sqlite)
	 #'emacsql-sqlite)))

(defvar org-heatmap--db nil
  "Current connected org-heatmap database.")

(defun org-heatmap-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for the database in
the current `org-heatmap-db-location'."
  (unless db
    (setq db org-heatmap--db))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)
	(setq org-heatmap--db nil)))

(defun org-heatmap-db--init (db table)
  "Initialize database DB with the correct schema and user version."
  (emacsql-with-transaction db
    (emacsql db [:create-table $s1 ([(date :primary-key)
									 (num :not-null)])]
			 table)))

(defun org-heatmap-db ()
  "Connect Org-heatmap database."
  (unless (and org-heatmap--db
			   (emacsql-live-p org-heatmap--db))
	(let ((init-db (not (file-exists-p org-heatmap-db-location))))
	  (make-directory (file-name-directory org-heatmap-db-location) t)
	  (let ((conn (funcall (org-heatmap-db--conn-fn) org-heatmap-db-location)))
		(emacsql conn [:pragma (= foreign_keys ON)])
		(when-let* ((process (emacsql-process conn))
                    ((processp process)))
          (set-process-query-on-exit-flag process nil))
		(when init-db
          (org-heatmap-db--init conn 'done-items))
		(setq org-heatmap--db conn))))
  org-heatmap--db)

(defun org-heatmap-db--query (sql &rest args)
  "Run SQL query on Org-heatmap database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (emacsql-with-transaction (org-heatmap-db)
	(apply #'emacsql org-heatmap--db sql args)))

(defun org-heatmap-db--init-done-items ()
  "Insert new record."
  (let* ((date (calendar-current-date)))
	(org-heatmap-db--query [:insert :into $s1
									:values $v2]
						   'done-items
						   (vector date 1))))

(defun org-heatmap-db--query-date (d table)
  (org-heatmap-db--query [:select [date num]
								  :from $s1
								  :where (= date $s2)]
						 table d))

(defun org-heatmap-db--update-done-items (date num)
  (org-heatmap-db--query [:update $s1
								  :set (= num $s2)
								  :where (= date $s3)]
						 'done-items
						 num
						 date))

(defun org-heatmap-db--table-exist-p (table)
  (emacsql-with-transaction (org-heatmap-db) 
	(emacsql org-heatmap--db [:select name :from sqlite_master
									  :where (and (= type 'table) (= name $s1))]
			 table)))

;;;###autoload
(defun org-heatmap-db--drop (table)
  "Delete TABLE."
  (interactive (list (completing-read "Select a table: "
									  (mapcar #'car
											  (org-heatmap-db--query
											   [:select name :from sqlite_master
														:where (= type 'table)])))))
  (when (y-or-n-p (format "Delete table: %s?" table))
	(org-heatmap-db--query [:drop-table :if-exists $s1] table)))

;;;; utilities
(defun org-heatmap-habit-update-p ()
  "Return non-nil if the entry at point is a habit and is
stored in the database, else return nil."
  (and (org-is-habit-p)
	   (org-heatmap-db--table-exist-p (org-heatmap--hd-name))))

(defun org-heatmap-habit-p ()
  "Return non-nil if the entry at point is a habit, else return nil."
  (or (org-is-habit-p)
	  (let* ((marker (or (org-get-at-bol 'org-marker)
						 (org-agenda-error)))
			 (buffer (marker-buffer marker))
			 (pos (marker-position marker)))
		(with-current-buffer buffer
		  (goto-char pos)
		  (org-is-habit-p)))))

(defun org-heatmap-update-counter ()
  (when (string= "DONE" (org-get-todo-state))
	(let ((td (calendar-current-date))
		  (hd-name (org-heatmap--hd-name)))
	  (cond
	   ((org-heatmap-habit-update-p)
		(let ((time (save-excursion
					  (save-restriction
						(org-narrow-to-subtree)
						(org-heatmap-clock-sum td)))))
		  (org-heatmap-db--query [:insert :into $s1
										  :values $v2]
								 hd-name
								 (vector td time))))
	   (t (if-let* ((result (cadar (org-heatmap-db--query-date td 'done-items))))
			  (org-heatmap-db--update-done-items td (1+ result))
			(org-heatmap-db--init-done-items)))))))

(defun org-heatmap-get-streak (table)
  "Query org-heatmap database and set `org-heatmap-current-streak'."
  (let ((streak (org-heatmap-db--query [:select [date num]
												:from $s1]
									   table))
		(table (make-hash-table :test #'equal))
		(type (if (eq table 'done-items)
				  'default
				'habit)))
	(dolist (item streak)
      (puthash (car item) (cadr item) table))
	(setq org-heatmap-current-streak (cons type table))))

(defun org-heatmap-get-threshold-defualt (n)
  "Default function to get threshold in `org-heatmap-threshold'."
  (cdr
   (cl-find-if (lambda (pair)
				 (>= n (car pair)))
			   (reverse (alist-get (car org-heatmap-current-streak)
								   org-heatmap-threshold)))))

(defun org-heatmap-generate (month year _indent)
  "Mark dates in the calendar window highlights."
  (when org-heatmap-current-streak
    (dotimes (i 31)
	  (let ((date (list month (1+ i) year))
            (count-scaled (gethash (list month (1+ i) year)
								   (cdr org-heatmap-current-streak))))
        (when count-scaled
		  (calendar-mark-visible-date
		   date
		   (funcall org-heatmap-get-threshold-function count-scaled)))))))

(defun org-heatmap-clear (&rest _args)
  "Call after quit calendar."
  (setq org-heatmap-current-streak nil))

(defun org-heatmap-time-format (date)
  "Format DATE into YYYY-MM-DD."
  (format "%04d-%02d-%02d"
		  (nth 2 date) 
		  (nth 0 date) 
		  (nth 1 date)))

(defun org-heatmap-clock-sum (date)
  "Sum the times spent on DATE for current habit."
  (let* ((cc (org-clock-special-range (org-heatmap-time-format date)))
		 (ts (car cc))
		 (te (nth 1 cc)))
	(org-heatmap-clock-sum-1 ts te)))

(defun org-heatmap-clock-sum-1 (tstart tend)
  (let* ((re (concat "^[ \t]*"
					 org-clock-string
					 "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
		 (tstart (float-time tstart))
		 (tend (float-time tend))
		 (sum 0))
	(save-excursion
	  (goto-char (point-max))
	  (while (re-search-backward re nil t)
		(let* ((ss (match-string 1))
			   (se (match-string 2))
			   (ts (org-time-string-to-seconds ss))
		       (te (org-time-string-to-seconds se))
			   (dt (- (if tend (min te tend) te)
					  (if tstart (max ts tstart) ts))))
		  (when (> dt 0) (cl-incf sum (floor dt 60))))))
	sum))

(defun org-heatmap--hd-name ()
  "Get current habit name."
  (cond
   ((eq major-mode 'org-agenda-mode)
	(org-with-point-at (org-get-at-bol 'org-hd-marker)
      (nth 4 (org-heading-components))))
   (t (nth 4 (org-heading-components)))))

(defun org-heatmap-habit-parse-todo ()
  "Parse the TODO surrounding point for its all habit-related data.

Return a list of all the past dates this todo was mark closed."
  (let ((org-habit-preceding-days 99999)
		(org-habit-following-days 99999))
	(reverse (nth 4 (org-habit-parse-todo)))))

(defun org-heatmap-habit--collect (&rest _args)
  "Save the current habit information into the database.

*Note*: It may take a long time to finish, depending on the size
 of your log of the habit."
  (let* ((marker (or (org-get-at-bol 'org-marker)
					 (org-agenda-error)))
		 (buffer (marker-buffer marker))
		 (pos (marker-position marker))
		 (hd-name (org-heatmap--hd-name))
		 closed-dates)
	(unless buffer
	  (user-error "Trying to switch to non-existent buffer"))
	(with-current-buffer buffer
	  (save-excursion
		(save-restriction
		  (goto-char pos)
		  (org-back-to-heading t)
		  (setq closed-dates (org-heatmap-habit-parse-todo))
		  (org-narrow-to-subtree)
		  (org-heatmap-db--init (or org-heatmap--db
									(funcall (org-heatmap-db--conn-fn) org-heatmap-db-location))
								hd-name)
		  (dolist (closed-date closed-dates) 
			(let ((date (calendar-gregorian-from-absolute closed-date)))
			  (org-heatmap-db--query [:insert :into $s1
											  :values $v2]
									 hd-name
									 (vector date (org-heatmap-clock-sum date))))
			(sleep-for 0.01)))))))

(defun org-heatmap-year-filter (days)
  "Get all the dates of this year."
  (let* ((date (calendar-current-date))
         (year (calendar-extract-year date))
         (year-first (list 1 1 year)))
	(cl-remove-if-not (lambda (record)
						(>= (calendar-absolute-from-gregorian (car record))
							(calendar-absolute-from-gregorian year-first)))
					  days)))

(defun org-heatmap-add-color (table)
  "Choose the appropriate color based on the activity of each day."
  (let ((days (org-heatmap-year-filter
			   (org-heatmap-db--query [:select [date num]
											   :from $s1]
									  table))))
	(dolist (day days)
	  (let* ((day-num (calendar-day-number (car day)))
			 (face (cdr
					(cl-find-if (lambda (pair)
								  (>= (cadr day) (car pair)))
								(reverse (alist-get 'overview org-heatmap-threshold)))))
			 (beg (+ (point) (* (/ day-num 30) 90) (* 3 (1- (% day-num 30)))))
			 (end (+ beg (length org-heatmap-rectangle))))
		(put-text-property beg end 'face face (current-buffer))
		(make-button beg end
					 'help-echo (format "%d minutes are spent on %s" (cadr day )
										(org-heatmap-time-format (car day)))
					 'face face
					 'action
					 (lambda (_)
					   (let ((org-agenda-sticky nil))
						 (org-agenda-list nil (calendar-absolute-from-gregorian
											   (car day))
										  nil))))))))
;;;; habit statics
(defun org-heatmap-habit-parse-todo-advice (orig &rest args)
  (let ((org-habit-preceding-days 99999)
		(org-habit-following-days 99999))
	(apply orig args)))


(defun org-heatmap-habit-streaks (habit)
  (let ((closed-days (nth 4 habit))
	    (counter 1)
	    (sum (length (nth 4 habit)))
	    (streaks 1)
	    (current-streaks 0)
	    (today (time-to-days (current-time)))
	    (max-streaks 1))
    (while (< counter (length closed-days))
	  (if (= (time-convert (time-subtract (nth  counter closed-days)
										  (nth (1- counter) closed-days))
						   'integer) 1)
		  (progn (setq streaks (1+ streaks)))
	    (if (> streaks max-streaks)
	        (progn (setq max-streaks streaks)
				   (setq streaks 1))))
	  (setq counter (1+ counter)))
    (setq counter (1- counter))
    (if (= (time-convert (time-subtract today (nth counter closed-days))
                         'integer) 1)
	    (progn (setq current-streaks (1+ current-streaks))
			   (while (= (time-convert (time-subtract
                                        (nth  counter closed-days)
                                        (nth (1- counter) closed-days))
									   'integer) 1)
		         (setq current-streaks (1+ current-streaks))
		         (setq counter (1- counter)))
			   ))
    (if (> streaks max-streaks)
	    (setq max-streaks streaks))
    (propertize (concat " ("
                        (number-to-string current-streaks)
                        "/"
                        (number-to-string max-streaks)
                        "/"
                        (number-to-string sum) ")")
                'face 'org-heatmap-habit-statics)))

(defun org-heatmap-habit-clean-ov ()
  "Clear all habit streak overlays in org agenda buffer."
  (mapc (lambda (ov)
		  (when (overlay-get ov 'after-string)
			(delete-overlay ov)))
		(overlays-in (point-min) (point-max))))

(defun org-heatmap-habit-add-streak ()
  "Insert consistency graph for any habitual tasks."
  (org-heatmap-habit-clean-ov)
  (let ((buffer-invisibility-spec '(org-link))
		(inhibit-read-only t))
    (save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
	    (when-let ((habit (get-text-property (point) 'org-habit-p))
				   (pos (search-forward (org-heatmap--hd-name)))
				   (ov (make-overlay (1- pos) pos))
				   (streak (org-heatmap-habit-streaks habit)))
		  (when (org-get-at-bol 'tags)
			(delete-char (length streak)))
		  (overlay-put ov 'after-string streak))
	    (forward-line)))))

;;;; interactive functions

;;;###autoload
(defun org-heatmap-habit-draw-overview ()
  "Draw an overview heatmap for the habit at point."
  (interactive)
  (if-let ((pos (get-text-property (point) 'org-heatmap-has-overview-p)))
	  (progn
		(goto-char (point-min))
		(org-agenda-redo)
		(goto-char pos))
	(if (org-heatmap-habit-p)
		(let ((inhibit-read-only t)
			  (hd-name (org-heatmap--hd-name)))
		  (unless (or line-spacing
					  (alist-get 'line-spacing default-frame-alist))
			(setq-local line-spacing 5))
		  (unless (org-heatmap-db--table-exist-p hd-name)
			(org-heatmap-habit--collect))
		  (save-excursion
			(end-of-line)
			(insert "\n\n")
			(save-excursion
			  (cl-loop repeat 13 do
					   (insert
						(propertize (concat (mapconcat 'identity
													   (make-list 30 org-heatmap-rectangle)
													   " ")
											"\n")
									'face 'org-heatmap-empty-rectangle))))
			(org-heatmap-add-color hd-name))
		  (put-text-property (point-min)
							 (point-max)
							 'org-heatmap-has-overview-p
							 (point)))
	  (user-error "Not on a habit!"))))

;;;###autoload
(defun org-heatmap-habit-calendar ()
  "Display a three-month Gregorian calendar for the habit at point.

Add highlights indicating the times spent on the habit on the
current calendar date."
  (interactive (unless (org-heatmap-habit-p)
				 (user-error "Not on a habit!")))
  (let ((hd-name (org-heatmap--hd-name)))
	(unless (org-heatmap-db--table-exist-p hd-name)
	  (org-heatmap-habit--collect))
	(org-heatmap-get-streak hd-name)
	(calendar)))

;;;###autoload
(defun org-heatmap-calendar ()
  "Display a three-month Gregorian calendar.

Add highlights indicating the activities on the current calendar date."
  (interactive)
  (org-heatmap-get-streak 'done-items)
  (calendar))

;;;###autoload
(defun org-heatmap-calendar-query ()
  "Get information about the activities on the current calendar date.

When used with `org-heatmap-calendar', it shows how many items are
done on the current calendar date.

When used with `org-heatmap-habit-calendar', it shows how many
times is spent on the habit on the current calendar date."
  (interactive (unless (eq major-mode 'calendar-mode)
				 (user-error "Must be used in calendar mode!")))
  (when-let* ((date (calendar-cursor-to-date t))
			  (ht (cdr-safe org-heatmap-current-streak))
			  (tasks (gethash date ht)))
	(message "%d %s in %s"
			 (if (numberp tasks) tasks 0)
			 (if (eq (car org-heatmap-current-streak) 'default)
				 "items are done"
			   "minutes are spent")
			 (org-heatmap-time-format date))))

;;;###autoload
(defun org-heatmap-adjust ()
  "Change the number of done items on the current calendar date.

*Note* that this function is not applied to `org-heatmap-habit-calendar'
for now."
  (interactive (unless (and (eq major-mode 'calendar-mode)
							(eq (car-safe org-heatmap-current-streak) 'default))
				 (user-error "Must be used with default streak in calendar mode!")))
  (if-let* ((date (calendar-cursor-to-date t))
			(tasks (gethash date (cdr org-heatmap-current-streak)))
			(num (read-number
				  (format "Input a num(current: %d): " tasks))))
	  (progn
		(org-heatmap-db--update-done-items date num)
		(puthash date num (cdr org-heatmap-current-streak))
		(mapc #'delete-overlay (overlays-in (1- (point)) (1+ (point))))
		(calendar-mark-visible-date
		 (calendar-cursor-to-date t)
		 (funcall org-heatmap-get-threshold-function
				  (gethash date (cdr org-heatmap-current-streak)))))
	(error "%s hasn't any record!" (org-heatmap-time-format date))))

;;;###autoload
(define-minor-mode org-heatmap-mode
  "Show heatmap in calendar."
  :global t
  :group 'org-heatmap
  (cond
   (org-heatmap-mode
	(advice-add #'calendar-exit :after #'org-heatmap-clear)
	(advice-add #'calendar-generate-month :after #'org-heatmap-generate)
	(add-hook 'kill-emacs-hook #'org-heatmap-db--close)
	(add-hook 'org-after-todo-state-change-hook #'org-heatmap-update-counter)
	(define-key calendar-mode-map (kbd "j") #'org-heatmap-adjust)
	(define-key calendar-mode-map (kbd "f") #'org-heatmap-calendar-query)
	(when org-heatmap-enable-habit-statics
	  (advice-add 'org-habit-parse-todo :around #'org-heatmap-habit-parse-todo-advice)
	  (add-hook 'org-agenda-finalize-hook #'org-heatmap-habit-add-streak))
	(with-eval-after-load 'org-agenda
	  (org-defkey org-agenda-mode-map "h" #'org-heatmap-habit-draw-overview)))
   (t
	(org-heatmap-db--close)
	(advice-remove #'calendar-exit #'org-heatmap-clear)
	(advice-remove #'calendar-generate-month #'org-heatmap-generate)
	(advice-remove #'org-habit-parse-todo #'org-heatmap-habit-parse-todo-advice)
	(remove-hook 'org-after-todo-state-change-hook #'org-heatmap-update-counter)
	(remove-hook 'kill-emacs-hook #'org-heatmap-db--close)
    (remove-hook 'org-agenda-finalize-hook #'org-heatmap-habit-add-streak)
	(when (eq major-mode 'org-agenda-mode)
	  (org-heatmap-habit-clean-ov)
	  (org-agenda-redo 'all))
	(define-key calendar-mode-map (kbd "j") nil)
	(define-key calendar-mode-map (kbd "f") nil)
	(org-defkey org-agenda-mode-map "h" #'org-agenda-holidays))))

(provide 'org-heatmap)
;;; org-heatmap.el ends here.
