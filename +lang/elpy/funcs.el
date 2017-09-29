;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

;; functions for flashing a region; only flashes when package eval-sexp-fu is
;; loaded and its minor mode enabled
(defun elpy/flash-region (begin end)
  (when (and (bound-and-true-p eval-sexp-fu-flash-mode)
             (not (eq begin end)))
    (multiple-value-bind (bounds hi unhi eflash) (eval-sexp-fu-flash (cons begin end))
      (eval-sexp-fu-flash-doit (lambda () t) hi unhi))))

;; functions for sending blocks, groups, and entire buffer to the Python shell
(defun elpy/current-line-empty-p ()
  (eq (string-match-p "^\\s-*$" (thing-at-point 'line)) 0))

(defun elpy/current-line-comment-p ()
  (string-match-p "^\\s-*#" (thing-at-point 'line)))

(defun elpy/current-line-else-p ()
  (string-match-p "^\\s-*el\\(?:se:\\|if[^\w]\\)" (thing-at-point 'line)))

(defun elpy/current-line-indented-p ()
  (eq (string-match-p "^\\s-+[^\\s-]+" (thing-at-point 'line)) 0))

(defun elpy/skip-empty-and-comment-lines (&optional backwards)
  (if backwards
      (while (and (or (elpy/current-line-empty-p) (elpy/current-line-comment-p))
                  (not (eq (point) (point-min))))
        (forward-line -1))
    (while (and (or (elpy/current-line-empty-p) (elpy/current-line-comment-p))
                (not (eq (point) (point-max))))
      (forward-line))))

(defun elpy/python-shell-ensure-running ()
  (unless (python-shell-get-process)
    (call-interactively 'run-python)
    (sit-for 3))
  (python-shell-get-process))

(defmacro elpy/with-maybe-echo (body)
  `(elpy/with-maybe-echo-output
    (elpy/with-maybe-echo-input
     ,body)))

(defmacro elpy/with-maybe-echo-input (body)
  `(if elpy/shell-display-buffer-after-send
       (prog1 (progn ,body)
         (elpy-shell-display-buffer))
     (cl-flet ((elpy-shell-display-buffer () ()))
       (progn ,body))))

;; modified version of python-shell-output-filter
(defun elpy/shell-output-filter (string)
  "Filter used in `elpy/capture-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`python-shell-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (setq
   string (ansi-color-filter-apply string)
   python-shell-output-filter-buffer
   (concat python-shell-output-filter-buffer string))
  (when (python-shell-comint-end-of-output-p
         python-shell-output-filter-buffer)
    ;; Output ends when `python-shell-output-filter-buffer' contains
    ;; the prompt attached at the end of it.
    (setq python-shell-output-filter-in-progress nil
          python-shell-output-filter-buffer
          (substring python-shell-output-filter-buffer
                     0 (match-beginning 0))))
  string)

;; modified version of python-shell-send-string-no-output (in that it now also
;; sends output)
(defmacro elpy/with-maybe-echo-output (body)
  `(let* ((process (elpy/python-shell-ensure-running))
          (shell-visible (or elpy/shell-display-buffer-after-send
                             (get-buffer-window (process-buffer process)))))
     (if (cond
          ((null elpy/shell-echo-output) t)
          ((eq elpy/shell-echo-output 'when-shell-not-visible) shell-visible))
         (progn ,body)
       (let ((comint-preoutput-filter-functions
              '(elpy/shell-output-filter))
             (python-shell-output-filter-in-progress t)
             (inhibit-quit t))
         (or
          (with-local-quit
            (progn ,body)
            (sit-for eval-sexp-fu-flash-duration)
            (while python-shell-output-filter-in-progress
              ;; `elpy/shell-output-filter' takes care of setting
              ;; `python-shell-output-filter-in-progress' to NIL after it
              ;; detects end of output.
              (accept-process-output process))
            (prog1
                (progn
                  ;; this is delayed so that the flash overlay stays visible
                  (run-at-time "1 millisec" nil
                               (lambda (s)
                                 (let (message-log-max) ;; no need to log in messages
                                   (message "%s" s)))
                               (string-trim python-shell-output-filter-buffer))
                  python-shell-output-filter-buffer)
              (setq python-shell-output-filter-buffer nil)))
          (with-current-buffer (process-buffer process)
            (comint-interrupt-subjob)))))))

(defun elpy/nav--beginning-of-current-statement ()
  """Moves the point to the beginning of the current or next Python statement.

     If the current line starts with a statement, behaves exactly
     like python-nav-beginning of statement. If the point is on
     an empty or comment line, skips forward to the first line
     holding a statement. If the line is an else/elif clause,
     goes backward to the beginning of the corresponding if
     clause.
  """
  (elpy/skip-empty-and-comment-lines)
  (python-nav-beginning-of-statement)
  (let ((p))
    (while (and (not (eq p (point)))
                (elpy/current-line-else-p))
      (elpy-nav-backward-block)
      (setq p (point)))))

;; internal; point needs to be exactly at the beginning of the statement
(defun elpy/nav--end-of-current-statement ()
  """Moves the point to the end of the current Python statement.

     Assumes that the point is at the beginning of a
     statement (e.g., after calling
     elpy/nav--beginning-of-current-statement). Correctly handles
     if/elif/else statements.
  """
  (let ((indent (current-column))
        (continue t)
        (p))
    (while (and (not (eq p (point)))
                continue)
      ;; check if there is a another block at the same indentation level
      (setq p (point))
      (elpy-nav-forward-block)

      ;; if not, go to the end of the block and done
      (if (eq p (point))
          (progn
            (python-nav-end-of-block)
            (setq continue nil))
        ;; otherwise check if its an else/elif clause
        (unless (elpy/current-line-else-p)
          (forward-line -1)
          (elpy/skip-empty-and-comment-lines t)
          (setq continue nil)))))
  (end-of-line))

(defun elpy/shell-send-current-statement-and-step ()
  """Send current or next statement to Python shell and step.

   If the current line starts with a statement, send this
   statement. If the point is on an empty or comment line, send
   the next statement below point. Correctly handles if/else/elif
   statements.
  """
  (interactive)
  (elpy/python-shell-ensure-running)
  (let ((beg (progn (elpy/nav--beginning-of-current-statement)
                    (save-excursion
                      (beginning-of-line)
                      (point))))
        (end (progn (elpy/nav--end-of-current-statement) (point))))
    (unless (eq beg end)
      (elpy/flash-region beg end)
      (elpy-shell-get-or-create-process)
      (let ((buffer (current-buffer))
            (s))
        ;; remove spurious indentation
        (with-temp-buffer
          (insert (with-current-buffer buffer (buffer-substring beg end)))
          (goto-char (point-min))
          (while (elpy/current-line-indented-p)
            (python-indent-shift-left (point-min) (point-max)))
          (setq s (buffer-string)))
        ;; and send
        (elpy/with-maybe-echo
         (python-shell-send-string s)))))
  (python-nav-forward-statement))

(defun elpy/shell-send-current-region-or-buffer-and-step ()
  (interactive)
  (if (use-region-p)
      (elpy/flash-region (region-beginning) (region-end))
    (elpy/flash-region (window-start) (window-end)))
  (elpy/with-maybe-echo
   (elpy-shell-send-region-or-buffer))
  (if (use-region-p)
      (goto-char (region-end))
    (goto-char (point-max))))

(defun elpy/shell-send-current-buffer-and-step ()
  "Send entire buffer to Python shell"
  (interactive)
  (elpy/python-shell-ensure-running)
  (elpy/flash-region (window-start) (window-end))
  (elpy/with-maybe-echo
   (python-shell-send-buffer))
  (goto-char (point-max)))

(defun elpy/nav--beginning-of-current-defun ()
  """Move the point to the beginning of the current or next function or top-level statement.

  If the point is within a function or top-level statement, move
  to its beginning. Otherwise, move to the beginning of the next
  top-level statement.
  """
  (interactive)
  (elpy/nav--beginning-of-current-statement)
  (let ((p))
    (while (and (not (eq p (point)))
                (elpy/current-line-indented-p))
      (forward-line -1)
      (elpy/skip-empty-and-comment-lines t)
      (elpy/nav--beginning-of-current-statement))))

(defun elpy/shell-send-current-defun-and-step ()
  """Send the current function or top-level statement to the Python shell and step.

   If the point is within a function or top-level statement, send
   this one. Otherwise, send the next one below point.
  """
  (interactive)
  (elpy/python-shell-ensure-running)
  (let* ((beg (progn (elpy/nav--beginning-of-current-defun) (point)))
         (end (progn (elpy/nav--end-of-current-statement) (point))))
    (elpy/flash-region beg end)
    (if (string-match-p "\\`[^\n]*\\'" (buffer-substring beg end))
        ;; single line
        (elpy/shell-send-current-statement-and-step)
      ;; multiple lines
      (elpy/with-maybe-echo
       (python-shell-send-region beg end))
      (setq mark-active nil)
      (python-nav-forward-statement))))

(defun elpy/shell-send-current-group-and-step ()
  """Send the current or next group of top-level statements to the Python shell and step.

   A sequence of top-level statements is a group if they are not
   separated by empty lines. (Empty lines within each top-level
   statement are ignored though.)

   If the point is within a top-level statement, send the group
   around this statement. Otherwise, go to the top-level
   statement below point and send the group around this
   statement.
   """
  (interactive)
  (elpy/python-shell-ensure-running)
  (let* ((beg (progn
                ;; go to start of group
                (elpy/nav--beginning-of-current-defun)
                (while (not (or (elpy/current-line-empty-p)
                                (eq (point) (point-min))))
                  (unless (elpy/current-line-comment-p)
                    (elpy/nav--beginning-of-current-defun))
                  (forward-line -1)
                  (beginning-of-line))
                (when (elpy/current-line-empty-p)
                  (forward-line 1)
                  (beginning-of-line))
                (point)))
         (end (progn
                ;; go forward to end of group
                (elpy/nav--end-of-current-statement)
                (let ((p))
                  (while (not (eq p (point)))
                    (setq p (point))
                    (forward-line)
                    (if (elpy/current-line-empty-p)
                        (goto-char p)
                      (elpy/nav--end-of-current-statement))))
                (point))))
    (if (> end beg)
        (progn
          (elpy/flash-region beg end)
          ;; send the region and jump to next statement
          (if (string-match-p "\\`[^\n]*\\'" (buffer-substring beg end))
              ;; single line
              (elpy/shell-send-current-statement-and-step)
            ;; multiple lines
            (elpy/with-maybe-echo
             (python-shell-send-region beg end))
            (python-nav-forward-statement)))
      (goto-char beg))
    (setq mark-active nil)))

;; functions to inject output text into the Python shell
(defun elpy/insert-and-font-lock (string face &optional no-font-lock)
  (let ((from-point (point)))
    (insert string)
    (if (not no-font-lock)
        (add-text-properties from-point (point)
                             (list 'front-sticky t 'font-lock-face face)))))

(defun elpy/append-to-shell-output (string &optional no-font-lock prepend-cont-prompt)
  "Appends the given string to the output of the Python shell
and (unless no-font-lock is set) formats it as input. Prepends a
continuation promt if specified."
  (let ((buffer (current-buffer)))
    (set-buffer (process-buffer (python-shell-get-process)))
    (let ((initial-point (point))
          (mark-point (process-mark (python-shell-get-process))))
      (goto-char mark-point)
      (if prepend-cont-prompt
          (let* ((column (+ (- (point) (progn (forward-line -1) (end-of-line) (point))) 1))
                 (prompt (concat (make-string (max 0 (- column 7)) ? ) "...: "))
                 (lines (split-string string "\n")))
            (goto-char mark-point)
            (elpy/insert-and-font-lock (car lines) 'comint-highlight-input no-font-lock)
            (if (cdr lines)
                ;; no additional newline at end for multiline
                (dolist (line (cdr lines))
                  (insert "\n")
                  (elpy/insert-and-font-lock prompt 'comint-highlight-prompt no-font-lock)
                  (elpy/insert-and-font-lock line 'comint-highlight-input no-font-lock))
              ;; but put one for single line
              (insert "\n")))
        (elpy/insert-and-font-lock string 'comint-highlight-input no-font-lock))
      (set-marker (process-mark (python-shell-get-process)) (point))
      (goto-char initial-point))
    (set-buffer buffer)))

;; functions to extract first and last lines from a string
(defun elpy/string-head-lines (string n)
  (let* ((any "\\(?:.\\|\n\\)")
         (line "\\(?:\\(?:.*\n\\)\\|\\(?:.+\\'\\)\\)")
         (lines (concat line "\\{" (number-to-string n) "\\}"))
         (regexp (concat "\\`" "\\(" lines "\\)")))
    (if (string-match regexp string)
        (match-string 1 string)
      string)))

(defun elpy/string-tail-lines (string n)
  (let* ((any "\\(?:.\\|\n\\)")
         (line "\\(?:\\(?:.*\n\\)\\|\\(?:.+\\'\\)\\)")
         (lines (concat line "\\{" (number-to-string n) "\\}"))
         (regexp (concat "\\(" lines "\\)" "\\'")))
    (if (string-match regexp string)
        (match-string 1 string)
      string)))

;; Functions to enable and disable echoing of the python shell. This is done
;; using an advice modifying the behaviour of python-shell-send-string so that
;; it echoes the string being sent; drops intermediate lines when there are
;; many; can show continuation prompt
;; (require 'subr-x)
(defun elpy/python-shell-send-string-echo-advice (string &optional process msg)
  (interactive)
  (let* ((append-string ; strip setup code from Python shell
          (if (string-match "import codecs, os.*__pyfile = codecs.open.*$" string)
              (replace-match "" nil nil string)
            string))
         (append-string ; here too
          (if (string-match "^# -\\*- coding: utf-8 -\\*-\n*$" append-string)
              (replace-match "" nil nil append-string)
            append-string))
         (append-string ; strip newlines from beginning and white space from end
          (string-trim-right
           (if (string-match "\\`\n+" append-string)
               (replace-match "" nil nil append-string)
             append-string)))
         (head (elpy/string-head-lines append-string elpy/shell-echo-input-lines-head))
         (tail (elpy/string-tail-lines append-string elpy/shell-echo-input-lines-tail))
         (append-string (if (> (length append-string) (+ (length head) (length tail)))
                            (concat head "...\n" tail)
                          append-string)))
    ;; append the modified string to the shell output; prepend a newline for
    ;; multi-line strings
    (if elpy/shell-echo-input-cont-prompt
        (elpy/append-to-shell-output append-string nil t)
      (elpy/append-to-shell-output
       (concat (if (string-match "\n" append-string) "\n" "")
               append-string
               "\n")))))

(defun elpy/shell-enable-echo ()
  (when elpy/shell-echo-input
    (advice-add 'python-shell-send-string
                :before 'elpy/python-shell-send-string-echo-advice)))

(defun elpy/shell-disable-echo ()
  (advice-remove 'python-shell-send-string
                 'elpy/python-shell-send-string-echo-advice))

;; now add the advice to our functions
(advice-add 'elpy/shell-send-current-statement-and-step :before 'elpy/shell-enable-echo)
(advice-add 'elpy/shell-send-current-statement-and-step :after 'elpy/shell-disable-echo)
(advice-add 'elpy/shell-send-current-buffer-and-step :before 'elpy/shell-enable-echo)
(advice-add 'elpy/shell-send-current-buffer-and-step :after 'elpy/shell-disable-echo)
(advice-add 'elpy/shell-send-current-region-or-buffer-and-step :before 'elpy/shell-enable-echo)
(advice-add 'elpy/shell-send-current-region-or-buffer-and-step :after 'elpy/shell-disable-echo)
(advice-add 'elpy/shell-send-current-group-and-step :before 'elpy/shell-enable-echo)
(advice-add 'elpy/shell-send-current-group-and-step :after 'elpy/shell-disable-echo)
(advice-add 'elpy/shell-send-current-defun-and-step :before 'elpy/shell-enable-echo)
(advice-add 'elpy/shell-send-current-defun-and-step :after 'elpy/shell-disable-echo)

;; overwrites python-shell-send-file modified such that: if the file ends with
;; an expression, it's evaluated separately so that the result is recognized by
;; the python shell.  This let's use see the output of the last expression in a
;; multiline statement
(with-eval-after-load 'python
  (defun python-shell-send-file (file-name &optional process temp-file-name
                                           delete msg)
    "Send FILE-NAME to inferior Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.  If TEMP-FILE-NAME and DELETE are non-nil, then
TEMP-FILE-NAME is deleted after evaluation is performed.  When
optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
    (interactive
     (list
      (read-file-name "File to send: ")   ; file-name
      nil                                 ; process
      nil                                 ; temp-file-name
      nil                                 ; delete
      t))                                 ; msg
    (let* ((process (or process (python-shell-get-process-or-error msg)))
           (encoding (with-temp-buffer
                       (insert-file-contents
                        (or temp-file-name file-name))
                       (python-info-encoding)))
           (file-name (expand-file-name
                       (or (file-remote-p file-name 'localname)
                           file-name)))
           (temp-file-name (when temp-file-name
                             (expand-file-name
                              (or (file-remote-p temp-file-name 'localname)
                                  temp-file-name)))))
      (python-shell-send-string
       (format
        (concat
         "import codecs, os, ast;"
         "__pyfile = codecs.open('''%s''', encoding='''%s''');"
         "__code = __pyfile.read().encode('''%s''');"
         "__pyfile.close();"
         (when (and delete temp-file-name)
           (format "os.remove('''%s''');" temp-file-name))
         "__block = ast.parse(__code, '''%s''', mode='exec');"
         "__last = __block.body[-1];" ;; the last statement
         "__isexpr = isinstance(__last,ast.Expr);" ;; is it an expression?
         "__block.body.pop() if __isexpr else None;" ;; if so, remove it
         "exec(compile(__block, '''%s''', mode='exec'));" ;; execute everything else
         "eval(compile(ast.Expression(__last.value), '''%s''', mode='eval')) if __isexpr else None" ;; if it was an expression, it has been removed; now evaluate it
         )
        (or temp-file-name file-name) encoding encoding file-name file-name file-name)
       process))))

(defun elpy/send-with-step-go (fun &optional step go)
  (interactive)
  (let ((orig (point)))
    (call-interactively fun)
    (when (not step)
      (goto-char orig)))
  (when go
    (elpy-shell-switch-to-shell)
    (evil-insert-state)))

(defun elpy/shell-send-current-statement ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-statement-and-step nil nil))

(defun elpy/shell-send-current-defun ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-defun-and-step nil nil))

(defun elpy/shell-send-current-group ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-group-and-step nil nil))

(defun elpy/shell-send-current-region-or-buffer ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-region-or-buffer-and-step nil nil))

(defun elpy/shell-send-current-buffer ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-buffer-and-step nil nil))

(defun elpy/shell-send-current-statement-and-go ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-statement-and-step nil t))

(defun elpy/shell-send-current-defun-and-go ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-defun-and-step nil t))

(defun elpy/shell-send-current-group-and-go ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-group-and-step nil t))

(defun elpy/shell-send-current-region-or-buffer-and-go ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-region-or-buffer-and-step nil t))

(defun elpy/shell-send-current-buffer-and-go ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-buffer-and-step nil t))

(defun elpy/shell-send-current-statement-and-step-and-go ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-statement-and-step t t))

(defun elpy/shell-send-current-defun-and-step-and-go ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-defun-and-step t t))

(defun elpy/shell-send-current-group-and-step-and-go ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-group-and-step t t))

(defun elpy/shell-send-current-region-or-buffer-and-step-and-go ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-region-or-buffer-and-step t t))

(defun elpy/shell-send-current-buffer-and-step-and-go ()
  (interactive)
  (elpy/send-with-step-go 'elpy/shell-send-current-buffer-and-step t t))

(defun elpy-shell-switch-to-shell-in-current-window ()
  (interactive)
  (setq elpy--shell-last-py-buffer (buffer-name))
  (switch-to-buffer (process-buffer (elpy-shell-get-or-create-process))))

(defun elpy-shell-switch-to-buffer-in-current-window ()
  (interactive)
  (switch-to-buffer elpy--shell-last-py-buffer))

(defun elpy/insert-codecell-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "# <codecell>\n")))

(defun elpy/insert-markdowncell-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "# <markdowncell>\n")))
