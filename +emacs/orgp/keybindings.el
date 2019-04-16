;;; -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

;; add all org-babel keybindings in major mode keys; if a function has multiple
;; key bindings, only add the simplest one
(with-eval-after-load 'ob-keys
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
      (spacemacs/set-leader-keys-for-major-mode 'org-mode (concat "b " (cdr km)) (car km)))))

;; and change some
(spacemacs/set-leader-keys-for-major-mode 'org-mode "bb" #'org-babel-execute-src-block)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "bB" #'org-babel-execute-buffer)

;; and add some more
(spacemacs/set-leader-keys-for-major-mode 'org-mode "bt" nil) ;; undefine tangle
(spacemacs/declare-prefix-for-mode 'org-mode "mbt" "tangle") ;; and make prefix
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "btt" #'org-babel-tangle
  "bts" #'orgp/org-babel-tangle-subtree
  "btb" #'orgp/org-babel-tangle-block)

;; make RET more useful
;; more useful return key in insert mode (continue list/tables; or cancel when left empty)
(evil-define-key 'insert org-mode-map (kbd "RET") 'evil-org-return)
;; use VIM ret motion unless we are on a link
(evil-define-key 'normal org-mode-map (kbd "RET") 'orgp/org-open-at-point-or-evil-ret)
