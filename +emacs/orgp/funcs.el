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
         (is-todo-b (get-text-property 1 'todo-state b)))
    (let ((result (cond
                   ((and time-a time-b) nil)            ;; both timestamps -> equal
                   ((and is-todo-a time-b) 1)           ;; a todo, b timestamp -> up
                   ((and time-a is-todo-b) -1)
                   ((and is-todo-a is-todo-b)            ;; both todo -> compare states
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

(defun orgp/org-agenda-modify-mouse-face ()
  "Modifies the mouse face such that it does not highlight in the agenda view.

Needs to be run as last (or at least late) hook."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (get-text-property (point) 'mouse-face)
          (progn
            (remove-text-properties (point) (+ 1 (point)) '(mouse-face t))
            (add-text-properties (point) (+ 1 (point)) (list 'mouse-face (face-at-point t)))
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
