;;; -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

;; functions for creating/switching common environments
(defmacro latexp/LaTeX-environment (environment)
  "Define function latexp/LaTeX-ENVIRONMENT.

  The function creates or switches to (when its argument is
  non-nil) to ENVIRONMENT.
  "
  (let ((fun (intern (concat "latexp/LaTeX-" environment))))
    `(defun ,fun (arg)
       ,(concat "Create or switch to (when ARG non-nil) '" environment "' environment.")
       (interactive "*P")
       (if arg
           (LaTeX-modify-environment ,environment)
         (LaTeX-environment-menu ,environment)
         (save-mark-and-excursion
           (forward-line)
           (LaTeX-mark-environment)
           (call-interactively 'indent-region))))))

(latexp/LaTeX-environment "align")
(latexp/LaTeX-environment "align*")
(latexp/LaTeX-environment "array")
(latexp/LaTeX-environment "center")
(latexp/LaTeX-environment "enumerate")
(latexp/LaTeX-environment "equation")
(latexp/LaTeX-environment "equation*")
(latexp/LaTeX-environment "figure")
(latexp/LaTeX-environment "frame")
(latexp/LaTeX-environment "itemize")
(latexp/LaTeX-environment "tabular")
(latexp/LaTeX-environment "table")

;; functions to create/toggle latex math
(defun latexp/empty-or-whitespace-region-p (beg end)
  "Whether the region between BEG and END is empty or contains only whitespace."
  (string-match-p
   "^\\s-*$"
   (buffer-substring-no-properties beg end)))

(defun latexp/LaTeX-toggle-math ()
  "Create or toggle LaTeX math ($'s or \\[ and \\]).

  If region is active, surrounds it by \\[ and \\]. Also inserts
  appropriate newlines.

  If point is in LaTeX math mode and surrounded by $'s, surrounds
  it by \\[ and \\] instead. Also inserts appropriate newlines.

  If point is in LaTeX math mode and surrounded by \\[ and
  \\], surrounds it by $'s instead.

  If point is in LaTeX math mode and neither surrounded by $'s or
  \\[ and \\], does nothing and reports an error.

  If point is not in LaTeX math mode and on an empty line,
  inserts \\[ and \\] and puts the point in between. Also inserts
  appropriate newlines.

  If point is not in LaTeX math mode and not on an empty
  line, inserts a new line below the current line and proceeds as
  above.
  "
  (interactive)
  (cond
   ((use-region-p) ;; surround region with \[ \]
    (error "LaTeX-math on region not yet implemented"))
   ((texmathp)
    (cond
     ((string-equal (car texmathp-why) "$") ;; change $$ to \[\]
      (save-excursion
        (let* ((beg (cdr texmathp-why))
               (end (search-forward-regexp "^\\$\\|[^\\]\\$")))
          ;; update end marker
          (goto-char end)
          (backward-char 1)
          (unless (latexp/empty-or-whitespace-region-p (line-beginning-position) (point))
            (insert "\n"))
          (delete-char 1)
          (insert "\\]") ;; closing $
          (unless (latexp/empty-or-whitespace-region-p (point) (line-end-position))
            (insert "\n"))
          (setq end (+ 1 (line-end-position)))

          ;; update start marker
          (goto-char beg)
          (unless (latexp/empty-or-whitespace-region-p (line-beginning-position) beg)
            (insert "\n"))
          (delete-char 1) ;; opening $
          (insert "\\[")
          (setq beq (line-beginning-position))
          (unless (latexp/empty-or-whitespace-region-p (point) (line-end-position))
            (insert "\n"))
          (indent-region beg end))))
     ((string-equal (car texmathp-why) "\\[") ;; change \[\] to $$
      (save-excursion
        (let* ((beg (cdr texmathp-why))
               (end (search-forward-regexp "^\\\\\\]\\|[^\\]\\\\\\]")))
          (goto-char end)
          (delete-char -2)
          (insert "$")
          (goto-char beg)
          (delete-char 2)
          (insert "$")
          )))
     (t ;; nothing to do
      (error "Point in math mode but surrounded by %s" (car texmathp-why)))))
   (t ;; insert new \[ \] pair
    (progn
      (unless (latexp/empty-or-whitespace-region-p
               (line-beginning-position) (line-end-position))
        (end-of-line)
        (insert "\n"))
      (insert "\\[")
      (LaTeX-indent-line)
      (insert "\n\n\\]")
      (LaTeX-indent-line)
      (forward-line -1)
      (LaTeX-indent-line)))))

(defun latexp/LaTeX-remove-math-and-mark ()
  (interactive)
  (if (texmathp)
      (error "NYI")
    (user-error "Point not in math mode")))

(defun latexp/TeX-disable-prompt-for-command (cmd)
  "Disable prompt for viewer program for CMD in TeX.

  For example, when CMD is \"View\", disables prompt for viewer
  program.
  "
  (setq TeX-command-list
        (mapcar
         (lambda (entry)
           (when (string-equal (nth 0 entry) cmd)
             (setf (nth 3 entry) nil))
           entry)
         TeX-command-list)))

(defun latexp/find-master ()
  "Open the TeX-master file."
  (interactive)
  (find-file (TeX-master-file t)))

(defun latexp//font-latex-match-simple-command (limit)
  (let ((result (font-latex-match-simple-command limit)))
    (when result
      (if (assoc (match-string 0) tex--prettify-symbols-alist)
          (progn
            (goto-char (match-end 0))
            (latexp//font-latex-match-simple-command limit))
        result))))

(defvar-local latexp//font-latex-match-math-end 0)
(defun latexp/font-latex-match-math-and-simple-command (limit)
  (when (or (= (point) 1) (< limit latexp//font-latex-match-dollar-math-end))
    (setq latexp//font-latex-match-math-end 0))
  (let ((result
         (if (< (point) latexp//font-latex-match-math-end)
             (latexp//font-latex-match-simple-command limit)
           (when (font-latex-match-math-env limit)
             (goto-char (match-beginning 1))
             (setq latexp//font-latex-match-math-end (match-end 1))
             (latexp//font-latex-match-simple-command limit)))))
    (when result
      (if (< (match-beginning 0) latexp//font-latex-match-math-end)
          result
        (goto-char latexp//font-latex-match-math-end)
        (latexp/font-latex-match-math-and-simple-command limit)))))

(defvar-local latexp//font-latex-match-mathII-end 0)
(defun latexp/font-latex-match-mathII-and-simple-command (limit)
  (when (or (= (point) 1) (< limit latexp//font-latex-match-dollar-math-end))
    (setq latexp//font-latex-match-mathII-end 0))
  (let ((result
         (if (< (point) latexp//font-latex-match-mathII-end)
             (latexp//font-latex-match-simple-command limit)
           (when (font-latex-match-math-envII limit)
             (goto-char (match-beginning 1))
             (setq latexp//font-latex-match-mathII-end (match-end 1))
             (latexp//font-latex-match-simple-command limit)))))
    (when result
      (if (< (match-beginning 0) latexp//font-latex-match-mathII-end)
          result
        (goto-char latexp//font-latex-match-mathII-end)
        (latexp/font-latex-match-mathII-and-simple-command limit)))))

(defvar-local latexp//font-latex-match-dollar-math-end 0)
(defun latexp/font-latex-match-dollar-math-and-simple-command (limit)
  (when (or (= (point) 1) (< limit latexp//font-latex-match-dollar-math-end))
    (setq latexp//font-latex-match-dollar-math-end 0))
  (let ((result
         (if (< (point) latexp//font-latex-match-dollar-math-end)
             (latexp//font-latex-match-simple-command limit)
           (when (font-latex-match-dollar-math limit)
             (goto-char (match-beginning 0))
             (setq latexp//font-latex-match-dollar-math-end (match-end 0))
             (latexp//font-latex-match-simple-command limit)))))
    (when result
      (if (< (match-beginning 0) latexp//font-latex-match-dollar-math-end)
          result
        (goto-char latexp//font-latex-match-dollar-math-end)
        (latexp/font-latex-match-dollar-math-and-simple-command limit)))))
