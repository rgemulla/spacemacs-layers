;;; -*- lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defun notmuchp/tree-filter (query)
  "Filter or LIMIT the current tree results based on an additional query string.

Runs a new tree matching only messages that match both the
current search results AND the additional query string provided."
  (interactive (list (notmuch-read-query "Filter tree: ")))
  (let ((grouped-query (notmuch-group-disjunctive-query-string query))
        (grouped-original-query (notmuch-group-disjunctive-query-string
                                  (notmuch-tree-get-query))))
    (notmuch-tree (if (string= grouped-original-query "*")
                      grouped-query
                    (concat grouped-original-query " and " grouped-query)))))

(defun notmuchp/tree-filter-by-tag (tag)
  "Filter the current tree results based on a single tag.

Runs a new tree matching only messages that match both the
current search results AND that are tagged with the given tag."
  (interactive
    (list (notmuch-select-tag-with-completion "Filter by tag: " (notmuch-tree-get-query))))
  (notmuch-tree (concat (notmuch-tree-get-query) " and tag:" tag) ))

(defun notmuchp//compose-count ()
  "Returns number of emails currently in composition."
  (length
   (seq-filter
    (lambda (buffer)
      (with-current-buffer buffer
        (and (eq major-mode 'notmuch-message-mode)
             (not (string-prefix-p "*sent" (buffer-name))))))
    (buffer-list))))

(defun notmuchp//kill-emacs-query-function-compose ()
  "When killing emacs, ask if there mails currently being composed."
  (if (and notmuchp-kill-emacs-query-compose (> (notmuchp//compose-count) 0))
      (yes-or-no-p "There are mails being composed. Really exit?")
    t))

(defun notmuchp//confirm-empty-subject ()
  "Confirm send when e-mail cotains an empty subject"
  (when notmuchp-confirm-empty-subject
    (or (message-field-value "Subject")
        (yes-or-no-p "Subject field empty. Really send?")
        (keyboard-quit))))

(defun notmuchp/jump-search-reuse ()
  "Like `notmuch-jump-search', but reuse existing query buffers."
  (interactive)

  ;; Build the action map
  (let (action-map buffer-name-map)
    (dolist (saved-search notmuch-saved-searches)
      (let* ((saved-search (notmuch-hello-saved-search-to-plist saved-search))
            (key (plist-get saved-search :key)))
        (when key
          (let ((name (plist-get saved-search :name))
                (query (plist-get saved-search :query))
                (oldest-first
                (case (plist-get saved-search :sort-order)
                  (newest-first nil)
                  (oldest-first t)
                  (otherwise (default-value 'notmuch-search-oldest-first)))))
            (push (list key name
                        (if (eq (plist-get saved-search :search-type) 'tree)
                            `(lambda ()
                                (if (get-buffer (concat "*notmuch-tree-" ,query "*"))
                                    (progn
                                      (pop-to-buffer (concat "*notmuch-tree-" ,query "*"))
                                      (notmuch-refresh-this-buffer))
                                  (notmuch-tree ',query)))
                          `(lambda ()
                              (if (get-buffer (notmuch-search-buffer-title ',query))
                                  (progn
                                    (pop-to-buffer (notmuch-search-buffer-title ',query))
                                    (notmuch-refresh-this-buffer))
                                (notmuch-search ',query ',oldest-first)))))
                  action-map)))))
    (setq action-map (nreverse action-map))

    (if action-map
        (notmuch-jump action-map "Search: ")
      (error "To use notmuch-jump, please customize shortcut keys in notmuch-saved-searches."))))

;; stop current serach
(defun notmuchp/terminate-search-process ()
  "Terminates the current notmuch search process but keeps the results buffer."
  (interactive)
  (when (memq major-mode '(notmuch-search-mode notmuch-tree-mode))
    (let ((process (get-buffer-process (current-buffer))))
      ;; (delete-process process)
      (signal-process process 'SIGTERM))))

;; notmuch-message-mode-bindings (default via: C-c)
(defun notmuchp/quick-attach-files ()
  "Attach the file(s) at the top of the kill ring (must be a
    filename / list of filenames separated by newline) to the
    current mail without asking any questions. With prefix
    arg, attach list of space-separated files."
  (interactive)
  (let ((files (split-string (car kill-ring) "\n")))
    ;; check if all files exit
    (dolist (file files)
      (unless (file-exists-p file)
        (user-error "No such file: %s" file)))
    (dolist (file files)
      (save-excursion
        (let* ((type (or (mm-default-file-encoding file) "application/octet-stream"))
                (disposition (mml-content-disposition type file)))
          (end-of-buffer)
          (mml-attach-file file type nil disposition)
          (message "Attached file %s (%s, %s)" file type disposition))))
    (when (> (length files) 1)
      (message "Attached %d files" (length files)))))

;; counsel function for notmuch address history
(defvar notmuchp//counsel-address-history nil)
(defun notmuchp/counsel-address (&optional initial-input)
  "Insert an email address from `notmuch-address-completions' using Ivy.

The default action appends the e-mail adress when on a header
lines (e.g., to/cc/bcc) or simply inserts it otherwise. "
  (interactive)
  (ivy-read "Address Search: "
            notmuch-address-completions
            :initial-input initial-input
            :history notmuchp//counsel-address-history
            :action '(1 ("a" notmuchp//counsel-address-action "Append or insert"))
            :caller 'notmuchp/counsel-address))

(defun notmuchp//counsel-address-action (address)
  "When on a address header line, append the address. Else insert at point."
  (let ((line (thing-at-point 'line)))
    (if (string-match-p notmuch-address-completion-headers-regexp line)
        (save-excursion
          (end-of-line)
          (unless (string-match-p ":[\s-]*$\\|,[\s-]*$" line)
            (insert ","))
          (unless (string-match-p "[\s-]+$" line)
            (insert " "))
          (insert address))
      (insert address))))

(defun notmuchp/counsel-addresses ()
  "Like `notmuchp/counsel-address' but repeatedly inserts e-mail
addresses until aborted."
  (interactive)
  (while t ;; cancel with C-g
    (notmuchp/counsel-address)))

;; determine names of recipients of mail (useful for yassnippets)
(defun notmuchp/get-recipient-names (&optional last last-sep default-sep)
  "Return formatted string of the names of the recipients of an email.

If LAST is non-nil, return last names (else given names).
DEFAULT-SEP is the separator between recipient names (defaults to
\", \"). LAST-SEP is a separator before the last recipient
name (defaults to \", \"). For
example, `(notmuchp/get-recipient-names nil \" and \" \", \")'
returns a string such as \"Anna, Bob and Charlie\".

Recipients are taken from the TO header. If only the email
address but not the name of a recipient is in the TO header, try
to find the name in the email body via heuristics (e.g., the
citation in a reply often contains the name).
"
  (interactive)
  (let ((result "") (sep "") name words (notdone t))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^To:[\s-]*")
      (while (and notdone
                  (re-search-forward "'\\([^']+\\)'\\|\"\\([^\"]+\\)\"\\|\\([^\",<>]+\\)" (line-end-position) t))
        (setq name (string-trim
                    (buffer-substring-no-properties (match-beginning 0) (match-end 0))
                    "[ \t\n\r\"']+" "[\t\n\r\"']+"))

        ;; if "name" is actually an e-mail address, search for name in rest of
        ;; email
        (save-excursion
          (save-match-data
            (when (string-match-p "@" name)
              (when (re-search-forward (concat "^\\(.*\\)<"
                                               (regexp-quote name)
                                               "> writes:")
                                       nil t)
                (setq name (match-string 1))))))

        ;; handle names given opposite order via comma
        (unless (string-match-p "@" name)
          (setq name (string-join (reverse (split-string name "," t "[ \t\n\r]+")) " "))
          (setq words (split-string name "[ \t\n\r]+" t))

          ;; capitalize first and last word of name
          (setf (nth 0 words) (capitalize (nth 0 words)))
          (setf (nth (- (length words) 1) words) (capitalize (nth (- (length words) 1) words))))

        ;; see if there are more recipients
        (setq notdone (re-search-forward ",[\s-]*" (line-end-position) t))

        ;; now update result
        (setq sep (cond
                   ((eq result "") "")
                   (notdone (if default-sep default-sep ", "))
                   (t (if last-sep last-sep ", "))))
        (setq result
              (concat result sep
                      (cond
                       ((string-match-p "@" name) name)
                       (last (car (last words)))
                       (t (car words)))))))
    result))
