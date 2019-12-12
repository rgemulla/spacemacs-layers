;;; -*- lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defconst notmuchp-packages
  '(
    el-patch
    notmuch
    ))

(defun notmuchp/pre-init-notmuch ()
  (spacemacs|use-package-add-hook notmuch
    :post-init
    :post-config
    ;; add filtering of emails in tree model
    (define-key notmuch-tree-mode-map "L" #'notmuchp/tree-filter)
    (define-key notmuch-tree-mode-map "t" #'notmuchp/tree-filter-by-tag)

    ;; detect "Re[<number>]:" styles of reply subject as well; also detect AW;
    ;; this overwrites the corresponding notmuch function
    (require 'el-patch)
    (el-patch-defun notmuch-show-strip-re (string)
      (replace-regexp-in-string
       (el-patch-swap
         "^\\([Rr]e: *\\)+"  ;; OLD
         "^\\(\\(Re\\|RE\\|Aw\\|AW\\)\\(\\[[0-9]+\\]\\)?: *\\)+")  ;; new
       "" string))

    ;; alert when killing with mails in composition
    (add-hook 'kill-emacs-query-functions #'notmuchp//kill-emacs-query-function-compose)

    ;; alert before sending an email with empty subject
    (add-hook 'message-send-hook #'notmuchp//confirm-empty-subject)

    ;; add refresh bindings in buffers where undef
    (evil-declare-key 'evilified notmuch-show-mode-map "gr" 'notmuch-refresh-this-buffer)
    (evil-declare-key 'evilified notmuch-show-mode-map "gR" 'notmuch-refresh-all-buffers)
    (evil-declare-key 'evilified notmuch-tree-mode-map "gr" 'notmuch-refresh-this-buffer)
    (evil-declare-key 'evilified notmuch-tree-mode-map "gR" 'notmuch-refresh-all-buffers)

    ;; add bindings to terminate a LONG search
    (evil-declare-key 'evilified notmuch-search-mode-map "X" #'notmuchp/terminate-search-process)
    (evil-declare-key 'evilified notmuch-tree-mode-map "X" #'notmuchp/terminate-search-process)

    ;; improved bindings for mail composition
    (spacemacs/set-leader-keys-for-major-mode 'notmuch-message-mode
      "," #'notmuch-mua-send-and-exit
      "k" #'message-kill-buffer
      "d" #'message-dont-send
      "a" #'mml-attach-file
      "A" #'notmuchp/quick-attach-files
      "s" #'notmuch-draft-save
      "p" #'notmuch-draft-postpone
      "c" #'notmuchp/counsel-address
      "C" #'notmuchp/counsel-addresses)

    ;; set notmuch window purpose
    (with-eval-after-load 'window-purpose
      (add-to-list 'purpose-user-regexp-purposes '("\\*notmuch.*" . mail))
      (add-to-list 'purpose-user-regexp-purposes '("\\*unsent mail.*" . mail))
      (add-to-list 'purpose-user-mode-purposes '(notmuch-hello-mode . mail))
      (add-to-list 'purpose-user-mode-purposes '(notmuch-show-mode . mail))
      (add-to-list 'purpose-user-mode-purposes '(notmuch-search-mode . mail))
      (add-to-list 'purpose-user-mode-purposes '(notmuch-tree-mode . mail))
      (add-to-list 'purpose-user-mode-purposes '(notmuch-message-mode . mail))
      (purpose-compile-user-configuration)
      (purpose-x-popwin-update-conf))

    ;; ignore for projectile
    (with-eval-after-load 'projectile
      (add-to-list 'projectile-globally-ignored-modes "notmuch-.*-mode"))

    ;; enable yas (seems like a spacemacs problem)
    (with-eval-after-load 'yasnippet
      (add-hook 'notmuch-message-mode-hook
                (lambda ()
                  (spacemacs/load-yasnippet)
                  (yas-minor-mode 1))))

    (when (configuration-layer/layer-used-p 'auto-completion)
      ;; enable address completion with company
      (spacemacs|add-company-backends ;; tried second
        :backends company-ispell
        :modes notmuch-message-mode)
      (spacemacs|add-company-backends ;; tried first
        :backends notmuch-company
        :modes notmuch-message-mode))))

(defun notmuchp/init-el-patch ()
  (use-package el-patch))
