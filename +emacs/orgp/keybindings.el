;;; -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

;; add all org-babel keybindings in major mode keys; if a function has multiple
;; key bindings, only add the simplest one
(spacemacs/declare-prefix-for-mode 'org-mode "mb" "babel")
(let ((bindings))
  (dolist (km org-babel-key-bindings)
    (when (listp km)
      (let* ((key (key-description (car km)))
             (command (cdr km))
             (existing-key (alist-get command bindings)))
        (if (null existing-key)
            (add-to-list 'bindings `(,command . ,key))
          (when (< (length key) (length existing-key))
            (setf (alist-get command bindings) key)))))) ;; short key wins
  (dolist (km bindings)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode (concat "b " (cdr km)) (car km))))

;; and add some more
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "bT" 'orgp/org-babel-tangle-block)
