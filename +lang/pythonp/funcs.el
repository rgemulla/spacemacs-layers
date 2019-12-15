;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:


(defun pythonp//no-python-mode-hook-advice (orig-fun &rest args)
  (let ((python-mode-hook nil))
    (apply orig-fun args)))


;; -- glue code for elpy-shell -------------------------------------------------

(defun elpy-project-root ()
  (lsp-workspace-root))

(defun elpy-nav-forward-block ()
  "Move to the next line indented like point.

This will skip over lines and statements with different
indentation levels."
  (interactive "^")
  (let ((indent (current-column))
        (start (point))
        (cur nil))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-forward-statement)
    (while (and (< indent (current-indentation))
                (not (eobp)))
      (when (equal (point) cur)
        (error "Statement does not finish"))
      (setq cur (point))
      (python-nav-forward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun elpy-nav-backward-block ()
  "Move to the previous line indented like point.

This will skip over lines and statements with different
indentation levels."
  (interactive "^")
  (let ((indent (current-column))
        (start (point))
        (cur nil))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-backward-statement)
    (while (and (< indent (current-indentation))
                (not (bobp)))
      (when (equal (point) cur)
        (error "Statement does not start"))
      (setq cur (point))
      (python-nav-backward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))


;; -- send symbol under point to shell -----------------------------------------

(defun pythonp/elpy-shell-send-symbol-and-step ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol))
        (bounds (bounds-of-thing-at-point 'symbol)))
    (elpy-shell--flash-and-message-region (car bounds) (cdr bounds))
    (elpy-shell--with-maybe-echo
     (python-shell-send-string symbol))
    (goto-char (cdr bounds))
    (re-search-forward "[^ 	\n]")
    (goto-char (- (point) 1))))

;; -- search project for definitions -------------------------------------------

(defun pythonp//ivy--regex-fuzzy-restricted (str forbidden-chars)
  "Like `ivy--regex-fuzzy' but does not allow FORBIDDEN-CHARS
between characters of search string."
  (if (string-match "\\`\\(\\^?\\)\\(.*?\\)\\(\\$?\\)\\'" str)
      (prog1
          (concat (match-string 1 str)
                  (let ((lst (string-to-list (match-string 2 str))))
                    (apply #'concat
                           (cl-mapcar
                            #'concat
                            (cons "" (cdr (mapcar
                                           (lambda (c)
                                             (format "[^%c%s\n]*" c forbidden-chars))
                                           lst)))
                            (mapcar (lambda (x) (format "\\(%s\\)" (regexp-quote (char-to-string x))))
                                    lst))))
                  (match-string 3 str))
        (setq ivy--subexps (length (match-string 2 str))))
    str))


(defun pythonp//make-counsel-search-in-context-function
    (tool pre-regex post-regex forbidden-chars extensions)
  "Like `spacemacs//make-counsel-search-function' but allows to
transparently add a pre regex, a post regex, a set of
forbidden-to-skip chars, and/or a set of filename extensions to
the search."
  (let ((base-cmd
                 (cdr (assoc-string tool spacemacs--counsel-commands))))
    (lambda (string &optional _pred &rest _unused)
      "Grep in the current directory for STRING."
      ;; `ivy-more-chars' returns non-nil when more chars are needed,
      ;; minimal chars count is configurable via `ivy-more-chars-alist'
      (or (ivy-more-chars)
          (let* ((default-directory (ivy-state-directory ivy-last))
                 (args (if (string-match-p " -- " string)
                           (let ((split (split-string string " -- ")))
                             (prog1 (pop split)
                               (setq string (mapconcat #'identity split " -- "))))
                         ""))
                 (args (concat
                        args
                        (when extensions
                          (cond
                           ((string= tool "rg")
                            (concat " "
                                    (mapconcat (lambda (ext)
                                                 (concat "-g \"*." ext "\""))
                                               extensions " ")))
                           ((string= tool "ag")
                            (format " -G \"%s\""
                                    (mapconcat (lambda (ext)
                                                 (concat "\\." ext "$"))
                                               extensions "|")))
                           ((string= tool "grep")
                            (format " --include \"%s\""
                                    (mapconcat (lambda (ext)
                                                 (concat "\\." ext "$"))
                                               extensions "|")))
                           (t (error
                               (format "support for filename pattern not yet implemented for tool %s" tool)))))))
                 (regex (counsel--elisp-to-pcre
                         (concat pre-regex
                                 "\\("
                                 (setq ivy--old-re ;; used for highlight
                                 (pythonp//ivy--regex-fuzzy-restricted string forbidden-chars))
                                 "\\)"
                                 post-regex))))
            (setq spacemacs--counsel-search-cmd (format base-cmd args regex))
            (spacemacs//counsel-async-command spacemacs--counsel-search-cmd)
            nil)))))

(defun pythonp//counsel-search-in-context (&optional tools use-initial-input initial-directory pre-regex post-regex forbidden-chars extensions prompt)
  "Like `spacemacs/counsel-search' but allows to transparently
add a pre regex, a post regex, a set of forbidden-to-skip chars,
and/or a set of filename extensions to the search."
  (interactive)

  (require 'counsel)
  (letf* ((initial-input (if use-initial-input
                             (if (region-active-p)
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end))
                               (thing-at-point 'symbol t))
                           ""))
          (tool (catch 'tool
                  (dolist (tool tools)
                    (when (and (assoc-string tool spacemacs--counsel-commands)
                               (executable-find tool))
                      (throw 'tool tool)))
                  (throw 'tool "grep")))
          (default-directory
            (or initial-directory (read-directory-name "Start from directory: ")))
          (prompt (or prompt "%s from[%s]:")))
    (ivy-read
     (format prompt
             tool
             (if (< (length default-directory)
                    spacemacs--counsel-search-max-path-length)
                 default-directory
               (concat
                "..." (substring default-directory
                                 (- (length default-directory)
                                    spacemacs--counsel-search-max-path-length)
                                 (length default-directory)))))
     (pythonp//make-counsel-search-in-context-function tool pre-regex post-regex forbidden-chars extensions)
     :initial-input (when initial-input (rxt-quote-pcre initial-input))
     :dynamic-collection t
     :history 'counsel-git-grep-history
     :action #'counsel-git-grep-action
     :caller 'spacemacs/counsel-search
     :keymap spacemacs--counsel-map
     :unwind (lambda ()
               (counsel-delete-process)
               (swiper--cleanup)))))

(defun pythonp/counsel-projectile-python-class ()
  "Search for a Python class definition in the current project."
  (interactive)
  (let ((ivy-more-chars-alist '((t . 0))))
    (pythonp//counsel-search-in-context
     dotspacemacs-search-tools nil (projectile-project-root)
     "^[ \t]*class[ \t]+[^\n(:]*" ""
     "(: \t"
     '("py" "ipy")
     "(%s in [%s]) Class: ")))

(defun pythonp/counsel-projectile-python-function ()
  "Search for a Python function definition in the current project."
  (interactive)
  (let ((ivy-more-chars-alist '((t . 0))))
    (pythonp//counsel-search-in-context
     dotspacemacs-search-tools nil (projectile-project-root)
     "^[ \t]*def[ \t]+[^\n(:]*" ""
     "(: \t"
     '("py" "ipy")
     " (%s in [%s]) Function: ")))
