;;; -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defun orgp/org-agenda-switch-to-delete-other-window ()
  (interactive)
  (org-agenda-switch-to t))

(defun orgp/org-agenda-cmp-todos-up (a b)
  "Ranks todos/scheduled items without timestamp first, then
with timestamp, than all other items without timestamps."
  (let* ((time-a (get-text-property 1 'time-of-day a))
         (time-b (get-text-property 1 'time-of-day b))
         (is-todo-a (get-text-property 1 'todo-state a))
         (is-todo-b (get-text-property 1 'todo-state b))
         (is-done-a (string= is-todo-a "DONE"))
         (is-done-b (string= is-todo-b "DONE")))
    (let ((result (cond
                   ((and time-a time-b) nil)            ;; both timestamps -> equal
                   ((and is-todo-a time-b) 1)           ;; a todo, b timestamp -> up
                   ((and time-a is-todo-b) -1)
                   ((and is-done-a is-done-b) nil)      ;; both TODOs done -> equal
                   ((and is-done-a is-todo-b) 1)        ;; one TODOs done -> down
                   ((and is-todo-a is-done-b) -1)
                   ((and is-todo-a is-todo-b)           ;; both TODOs -> compare
                    (org-cmp-todo-state a b))
                   ((and is-todo-a (not is-todo-b)) 1)  ;; a todo, b not -> up
                   ((and (not is-todo-a) is-todo-b) -1)
                   ((and time-a (not time-b)) 1)        ;; a timestamp, b not -> down
                   ((and (not time-a) time-b) -1)
                   (t nil))))
      result)))

(defun orgp/org-agenda-color-by-tag (tag face)
  "Apply FACE for agenda entries with TAG.

Only calendar events with a time-of-day are recolored."
  (interactive)
  (goto-char (point-min))
  (while (< (point) (point-max))
    (let ((tags (get-text-property (point) 'tags)))
      (if (and tags (member tag tags)
               (or (memq (face-at-point t) '(org-agenda-calendar-event org-agenda-calendar-sexp))
                   (and (get-text-property (point) 'time-of-day)
                        (member (face-at-point t) '(org-scheduled default)))
                   (equal (get-text-property (point) 'type) "block")))
          (progn
            (add-text-properties (point) (+ 1 (point)) `(face ,face))
            (forward-char))
        ;; exploit that entries always start at beginning of line to quickly skip all other lines
        (forward-line)))))

(defun orgp/org-agenda-pagebreak-before-new-date ()
  "Add a pagebreak before each new date in the agenda view."
  (save-excursion
    (let ((first-date-seen))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (member (face-at-point t)
                      '(org-agenda-date org-agenda-date-today org-agenda-date-weekend))
          (if first-date-seen
              (insert "\n")
            (setq first-date-seen t)))
        (forward-line)))))

(defun orgp/org-agenda-capture (&optional without-time)
  "Like `org-agenda-capture' but reverses use of prefix. I.e.,
time of day is captured by default and disabled with a prefix
argument."
  (interactive "P")
  (org-agenda-capture (if without-time nil 1)))

(defun orgp/current-time-stamp (&optional inactive without-time time zone)
  "Format the current time (or the specified time) as an org timestamp."
  (let* ((format (if without-time
                     (car org-time-stamp-formats)
                   (cdr org-time-stamp-formats))))
    (setq format (string-trim format "<\\|\\[" ">\\|\\]"))
    (if inactive
        (setq format (concat "[" format "]"))
      (setq format (concat "<" format ">")))
    (format-time-string format time zone)))

(defun orgp/org-agenda-capture-time-stamp (&optional inactive)
  "Like \"%T\" in `org-capture-templates', but inserts time of
  day only if not midnight. This can be used in capture templates
  via \"%(orpg/org-agenda-capture-timestamp)\" to default to
  full-day events when no timestamp had been specified. This
  method should only be used in capture templates."
  (let* ((time (decode-time org-overriding-default-time))
         (without-time (and (= 0 (nth 0 time)) ;; seconds
                            (= 0 (nth 1 time)) ;; minutes
                            (= 0 (nth 2 time))))) ;; hours
    (orgp/current-time-stamp inactive without-time org-overriding-default-time)))

(defun orgp/org-agenda-modify-mouse-face ()
  "Modifies the mouse face such that it does not highlight in the agenda view.

Needs to be run as last (or at least late) hook."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (get-text-property (point) 'mouse-face)
          (progn
            (remove-text-properties (point) (+ 1 (point)) '(mouse-face t))
            (add-text-properties (point) (+ 1 (point)) (list 'mouse-face (get-text-property (point) 'face)))
            (forward-char))
        (forward-line)))))

;; link handling
(defun orgp/org-remove-link ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description (if (match-end 3)
                               (org-match-string-no-properties 3)
                             (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

(defun orgp/org-babel-tangle-block ()
  "Tangle the current block.

  When no tangle file is set for the current block, ask for file
  name with sensible defaults."
  (interactive)
  (let ((block-info (org-babel-get-src-block-info)))
    (if block-info
        (let ((file-name (cdr (assoc :tangle (nth 2 block-info)))))
          (when (string-equal file-name "no")
            (let* ((language (nth 0 block-info))
                   (name (nth 4 block-info))
                   (default-file-name
                    (concat
                     (or name
                         (file-name-sans-extension
                          (file-name-nondirectory (buffer-file-name)))
                         "export")
                     "."
                     (or
                      (cdr (assoc language org-babel-tangle-lang-exts))
                      language
                      "block"))))
              (setq file-name
                    (read-file-name "Export to" nil nil nil default-file-name))))
          (org-babel-tangle '(4) file-name)
          (message "Tangled current block to file %s" file-name))
      (user-error "Not in a block"))))

(defun orgp/org-babel-tangle-subtree ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (org-babel-tangle)))))

(defun orgp/org-babel-yank-block ()
  (interactive)
  "Tangle the current block.

  When no tangle file is set for the current block, ask for file
  name with sensible defaults."
  (interactive)
  (let ((block-info (org-babel-get-src-block-info)))
    (unless block-info
      (user-error "Not in a block"))
    (kill-new (nth 1 block-info))
    (message "Copied %s block to kill ring" (nth 0 block-info))))

(defun orgp/helm-org-rifle-agenda-files-with-archives ()
  (interactive)
  (helm-org-rifle-files (org-agenda-files nil t)))

;; https://emacs.stackexchange.com/questions/35865/org-agenda-remove-time-grid-lines-that-are-in-an-appointment
(defun orgp/org-time-to-minutes (time)
  "Convert an HHMM time to minutes"
  (+ (* (/ time 100) 60) (% time 100)))

;; https://emacs.stackexchange.com/questions/35865/org-agenda-remove-time-grid-lines-that-are-in-an-appointment
(defun orpg/org-time-from-minutes (minutes)
  "Convert a number of minutes to an HHMM time"
  (+ (* (/ minutes 60) 100) (% minutes 60)))

;; Based on: https://emacs.stackexchange.com/questions/35865/org-agenda-remove-time-grid-lines-that-are-in-an-appointment
(defun orgp//org-agenda-grid-tweakify (orig-fun list ndays todayp)
  "Remove time grid lines in org agenda when there is an overlapping appointment"
  (if (member 'remove-match (car org-agenda-time-grid))
      (cl-flet ((extract-window
                  (line)
                  (let ((start (get-text-property 1 'time-of-day line))
                        (dur (get-text-property 1 'duration line)))
                    (cond
                     ((and start dur)
                      (cons start
                            (orpg/org-time-from-minutes
                             (truncate
                              (+ dur (orgp/org-time-to-minutes start))))))
                     (start start)
                     (t nil)))))
        (let* ((windows (delq nil (mapcar #'extract-window list)))
               (org-agenda-time-grid
                (list
                 (car org-agenda-time-grid)
                 (remove-if
                  (lambda (time)
                    (find-if (lambda (w)
                               (if (numberp w)
                                   (equal w time)
                                 (and (>= time (car w))
                                      (< time (cdr w)))))
                             windows))
                  (cadr org-agenda-time-grid) )
                 (caddr org-agenda-time-grid)
                 (cadddr org-agenda-time-grid)
                 )))
          (apply orig-fun (list list ndays todayp))))
    (apply orig-fun (list list ndays todayp))))

;; stderr is always redirected (may want option)
;; TODO some header arguments (esp. vars and sessions) are ignored right now
;; TODO may be nice to say whether to use script or execute one by one (session)
;; TODO should really be an ob-bat.el file
(defun org-babel-execute:bat (body params)
  "Execute a block of bat commands with Babel."
  (let ((in-file (org-babel-temp-file "ob-bat-" ".bat")))
    (with-temp-file in-file
      (insert body))
    (org-babel-eval
     (concat "cmd.exe"
             " /C " (org-babel-process-file-name in-file)
             " 2>&1" ;; redirect stderr ouput
             )
     "")))

(defun orgp/org-open-at-point-or-evil-ret (&rest args)
  "Call `org-open-at-point' when point on a link handled by that
function, else call `evil-ret'."
  (interactive "P")
  (condition-case nil
      (call-interactively 'org-open-at-point)
    (error (call-interactively 'evil-ret))))

(defun orgp/org-subtree-htmlize-to-temp-file (&optional open)
  "Exports current subtree or region to a temporary HTML file and
adds its full name to the kill ring. The file can then, for
example, be easily attached to an email. With prefix argument,
also open the created file (using `org-open-file')."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (save-restriction
        (let* ((temp-directory (make-temp-file "org-export-" t))
               (filename (file-name-nondirectory (org-export-output-file-name ".html" t)))
               (fullname (concat temp-directory "/" filename))
               (org-export-coding-system org-html-coding-system)
               (what
                (if (org-region-active-p)
                    "region"
                  (org-narrow-to-subtree)
                  (beginning-of-buffer)
                  (string-trim (thing-at-point 'line) "[ \\t\\n\\r\\*]+"))))
          (org-export-to-file 'html fullname nil t)
          (kill-new fullname)
          (message "Exported %s to %s" what fullname)
          (when open
            (org-open-file fullname))
          fullname)))))

(setq orgp//calendar-toggle-diary-window nil)
(defun orgp/calendar-toggle-diary ()
  "When entering a date in org, show/hide diary for currently selected calendar entry."
  (interactive)
  (condition-case nil
      (let ((inhibit-message t))
        (org-eval-in-calendar
         '(progn
            (setq orgp//calendar-toggle-diary-window (selected-window))
            (diary-view-entries))))
    (error
     (let ((win (selected-window)))
       (select-window orgp//calendar-toggle-diary-window)
       (quit-window)
       (select-window win)))))

(defun orgp/org-agenda-date-earlier-days (arg)
  "Change the time of this item, in units of days."
  (interactive "p")
  (org-agenda-date-earlier arg 'day))

(defun orgp/org-agenda-date-later-days (arg)
  "Change the time of this item, in units of days."
  (interactive "p")
  (org-agenda-date-later arg 'day))

(defun orgp//org-table-yank (format)
  "Yank the table at point using the specified FORMAT."
  (let ((tempfile (make-temp-file "org-table-yank-")))
    (org-table-export tempfile format)
    (with-temp-buffer
      (insert-file-contents tempfile)
      (message "Yanked %d lines in %s format"
               (count-lines (point-min) (point-max))
               (replace-regexp-in-string "orgtbl-to-" "" format))
      (kill-new (buffer-string)))))
(defun orgp/org-table-yank ()
  "Yank the table at point in ORG format."
  (interactive)
  (orgp//org-table-yank "orgtbl-to-orgtbl"))
(defun orgp/org-table-yank-tsv ()
  "Yank the table at point in TSV format."
  (interactive)
  (orgp//org-table-yank "orgtbl-to-tsv"))
(defun orgp/org-table-yank-csv ()
  "Yank the table at point in CSV format."
  (interactive)
  (orgp//org-table-yank "orgtbl-to-csv"))

(defun orgp/org-table-paste (&optional separator)
  "Paste table in kill ring interpreting it in the specified FORMAT.

If format is `nil', try to determine it. See `org-table-import.'"
  (interactive "P")
  (unless (bolp) (insert "\n"))
  (let ((beg (point)))
    (yank)
    (org-table-convert-region beg (point) separator)))
(defun orgp/org-table-paste-csv ()
  "Paste table in kill ring interpreting it as CSV."
  (interactive)
  (orgp/org-table-paste '(4)))
(defun orgp/org-table-paste-tsv ()
  "Paste table in kill ring interpreting it as TSV."
  (interactive)
  (orgp/org-table-paste '(16)))

(defun orgp/org-copy-subtree-without-ids (&optional n cut force-store-markers nosubtrees)
  (interactive "p")
  (let ((inhibit-message t)
        message-log-max)
    (org-copy-subtree n cut force-store-markers nosubtrees))
  (with-temp-buffer
    (let ((inhibit-message t)
          message-log-max)
      (org-mode)
      (insert (pop kill-ring))
      (org-delete-property-globally "ID")
      (kill-ring-save (point-min) (point-max)))
    (message "Copied: Subtree(s) without IDs with %d characters" (buffer-size))))
