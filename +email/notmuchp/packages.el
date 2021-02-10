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
    org
    ))

(defun notmuchp/pre-init-org ()
  ;; nothing to do
  )

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
    (spacemacs|spacebind
     :major
     (notmuch-message-mode
      ("," notmuch-mua-send-and-exit "send&exit")
      ("k" message-kill-buffer "kill")
      ("d" message-dont-send "don't send")
      ("a" mml-attach-file "attach file")
      ("A" notmuchp/quick-attach-files "attach files")
      ("s" notmuch-draft-save "save draft")
      ("p" notmuch-draft-postpone "postpone draft")
      ("i" "insert"
       ("i" notmuchp/counsel-address "insert address")
       ("I" notmuchp/counsel-addresses "insert addresses")
       ("t" notmuchp/counsel-to-address "add TO recipient")
       ("c" notmuchp/counsel-cc-address "add CC recipient")
       ("b" notmuchp/counsel-bcc-address "add BCC recipient"))))

    ;; view parts externally
    (spacemacs/set-leader-keys-for-major-mode
      'notmuch-show-mode "pp" #'notmuchp/view-part-externally)

    ;; navigate to attachments
    (evil-declare-key 'evilified notmuch-show-mode-map
      "ga" 'notmuchp/goto-next-attachment
      "gJ" 'notmuchp/goto-next-attachment
      "gK" 'notmuchp/goto-prev-attachment)

    ;; Enable org links to attachments.
    (org-link-set-parameters
     "notmuch-attachment"
     :follow #'notmuchp/open-org-attachment-link
     :store #'notmuchp/store-org-attachment-link)
    ;; Move notmuch-attachment parameters to end of list so that they take
    ;; precedence
    (let ((data (assoc "notmuch-attachment" org-link-parameters)))
      (setq org-link-parameters (remove data org-link-parameters))
      (add-to-list 'org-link-parameters data t))

    ;; quickly yank all attachments
    (spacemacs/set-leader-keys-for-major-mode
      'notmuch-show-mode "A" #'notmuchp/store-all-attachment-links)

    ;; set notmuch window purpose
    (with-eval-after-load 'window-purpose
      (add-to-list 'purpose-user-regexp-purposes '("\\*notmuch.*" . mail))
      (add-to-list 'purpose-user-regexp-purposes '("\\*unsent mail.*" . mail))
      (add-to-list 'purpose-user-mode-purposes '(notmuch-hello-mode . mail))
      (add-to-list 'purpose-user-mode-purposes '(notmuch-show-mode . mail))
      (add-to-list 'purpose-user-mode-purposes '(notmuch-search-mode . mail))
      (add-to-list 'purpose-user-mode-purposes '(notmuch-tree-mode . mail))
      (add-to-list 'purpose-user-mode-purposes '(notmuch-message-mode . mail))
      (purpose-compile-user-configuration))

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
        :modes notmuch-message-mode))

    ;; add access to unthreaded mode on oO
    (evil-define-key 'evilified notmuch-hello-mode-map "o" #'notmuch-unthreaded)
    (evil-define-key 'evilified notmuch-tree-mode-map "o" #'notmuch-unthreaded)
    (evil-define-key 'evilified notmuch-search-mode-map "o" #'notmuch-unthreaded)
    ;; disabled to not overwrite "o" for folding
    ;; (evil-define-key 'evilified notmuch-show-mode-map "o" #'notmuch-unthreaded)

    (evil-define-key 'evilified notmuch-tree-mode-map "O" #'notmuch-unthreaded-from-tree-current-query)
    (evil-define-key 'evilified notmuch-search-mode-map "O" #'notmuch-unthreaded-from-search-current-query)
    ;; disabled to not overwrite "O" for folding
    ;; (evil-define-key 'evilified notmuch-show-mode-map "O" #'notmuch-unthreaded-from-show-current-query)
    ))

(defun notmuchp/init-el-patch ()
  (use-package el-patch))
