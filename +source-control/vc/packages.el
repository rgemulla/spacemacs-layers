;; packages.el --- vc layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;; Integrates Emacs' vc into Spacemacs.
;;
;;; Code:

(defconst vc-packages
	'(vc))

;; use evilification for vc-dir and add gv prefix for all its commands
(defun vc/init-vc ()
  (use-package vc
    :config
    (with-eval-after-load 'vc-dir
      (evil-set-initial-state 'vc-dir-mode 'evilified)
      (define-key vc-dir-mode-map "c" 'vc-next-action)
      (spacemacs/declare-prefix "gv" "vc")
      (dolist (km vc-prefix-map)
        (when (listp km)
          (let ((key (car km))
                (command (cdr km)))
            (spacemacs/set-leader-keys (concat "gv" (char-to-string key)) command)))))))
