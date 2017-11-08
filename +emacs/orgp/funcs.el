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
                   ((and is-todo-a is-todo-b) nil)      ;; both todo -> equal
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
      (when (and tags (member tag tags)
                 (or (eq (face-at-point t) 'org-agenda-calendar-event)
                     (eq (face-at-point t) 'org-agenda-calendar-sexp)
                     (and (get-text-property (point) 'time-of-day)
                          (member (face-at-point t) '(org-scheduled default)))))
        (add-face-text-property (point) (+ 1 (point)) face)))
    (forward-char)))

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
      (when (get-text-property (point) 'mouse-face)
        (remove-text-properties (point) (+ 1 (point)) '(mouse-face t))
        (add-text-properties (point) (+ 1 (point)) (list 'mouse-face (face-at-point t))))
      (forward-char))))

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

(defun orgp/org-export-src-block ()
  "Save the content of the current src block to a file"
  (interactive)
  (let ((block-info (org-babel-get-src-block-info)))
    (if block-info
        (let* ((language (nth 0 block-info))
               (body (nth 1 block-info))
               (name (nth 4 block-info))
               (default-file-name (concat
                                   (or name
                                       (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                                       "export")
                                   "."
                                   (or
                                    (cdr (assoc language org-babel-tangle-lang-exts))
                                    language
                                    "block")))
               (file-name (read-file-name "Export to" nil nil nil default-file-name)))
          (with-temp-file file-name
            (insert body "\n"))
          (message "Block exported to %s" file-name))
      (user-error "Not in a block"))))

(defun orgp/helm-org-rifle-agenda-files-with-archives ()
  (interactive)
  (helm-org-rifle-files (org-agenda-files nil t)))
