;;; -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defconst orgp-packages
  '(
    org
    calfw
    calfw-org
    org-agenda-property
    ;; TODO this does not work anymore since helm-org-rifle has been added to org
    ;; helm
    ;; helm-org-rifle ;; (helm layer does not need to be used)
    org-sticky-header
    ))

(defun orgp/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-init
    ;; quick jump to refile/capture locations
    (spacemacs/declare-prefix "aof" "files")
    (spacemacs/set-leader-keys "aofr" 'org-refile-goto-last-stored)
    (spacemacs/set-leader-keys "aofc" 'org-capture-goto-last-stored)

    :post-config
    (spacemacs/declare-prefix-for-mode 'org-mode "mf" "files")
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "fr" 'org-refile-goto-last-stored)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "fc" 'org-capture-goto-last-stored)
    (spacemacs/declare-prefix-for-mode 'org-agenda-mode "mf" "files")
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode "fr" 'org-refile-goto-last-stored)
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode "fc" 'org-capture-goto-last-stored)

    ;; quicker way to reveal things around point
    (defun org-reveal-with-siblings ()
      (interactive)
      (org-reveal '(4)))
    (spacemacs/declare-prefix-for-mode 'org-mode "mv" "visibility")
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "vc" 'org-show-children
      "ve" 'org-show-entry
      "vo" 'org-overview
      "vr" 'org-reveal
      "vs" 'org-show-subtree
      "vR" 'org-reveal-with-siblings)

    ;; add C-<return> binding to agenda to select entry and close other window
    (with-eval-after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "C-<return>") 'orgp/org-agenda-switch-to-delete-other-window)
      (define-key org-agenda-mode-map (kbd "C-<enter>") 'orgp/org-agenda-switch-to-delete-other-window))

    ;; handle callto links
    (org-link-set-parameters
     "callto"
     :follow
     (lambda (number)
       (if (eq window-system 'w32)
           (call-process
            shell-file-name nil nil nil shell-command-switch
            (concat
             "start callto://"
             (replace-regexp-in-string
              "^\\+" "00"
              (replace-regexp-in-string "\\s-" "-" number))))
         (message "I do not know how to open callto links."))))

    ;; make the breadcrumb information shown by org-eldoc when on an org-headline
    ;; also show timestamp information
    (with-eval-after-load 'org-eldoc
      ;; override
      (defun org-eldoc-get-breadcrumb ()
        "Return breadcrumb if on a headline or nil. Add timestamp information if present."
        (let ((case-fold-search t) cur path time (time-prefix ""))
          (save-excursion
            (beginning-of-line)
            (save-match-data
              (when (looking-at org-complex-heading-regexp)
                (setq cur (match-string 4))
                (setq path
                      (org-format-outline-path
                       (append (org-get-outline-path) (list cur))
                       (frame-width) "" org-eldoc-breadcrumb-separator))
                (cond
                 ((setq time (org-entry-get (point) "TIMESTAMP"))
                  )
                 ((setq time (org-entry-get (point) "DEADLINE"))
                  (setq time-prefix
                        (concat (propertize "DEADLINE:" 'face 'org-special-keyword) " ")))
                 ((setq time (org-entry-get (point) "SCHEDULED"))
                  (setq time-prefix
                        (concat (propertize "SCHEDULED: " 'face 'org-special-keyword) " ")))
                 ((setq time (org-entry-get (point) "TIMESTAMP_IA"))
                  ))
                (if time
                    (concat path
                            (make-string (- (frame-width) 3
                                            (length path) (length time-prefix) (length time))
                                         ? )
                            time-prefix
                            (propertize time 'face 'org-date))
                  path)
                ))))))

    (when orgp/org-agenda-time-grid-hide-when-appointment
      (advice-add 'org-agenda-add-time-grid-maybe :around #'orgp//org-agenda-grid-tweakify))

    (with-eval-after-load 'ob-shell
      (defadvice org-babel-sh-evaluate (around set-shell activate)
        "Add header argument :file-coding that sets the buffer-file-coding-system."
        (let ((file-coding-param (cdr (assoc :file-coding params))))
          (if file-coding-param
              (let ((file-coding (intern file-coding-param))
                    (default-file-coding (default-value 'buffer-file-coding-system)))
                (setq-default buffer-file-coding-system file-coding)
                ad-do-it
                (setq-default buffer-file-coding-system default-file-coding))
            ad-do-it))))
  ))

(defun orgp/init-calfw ()
  (use-package calfw
    :after org
    :config
    (setq cfw:org-overwrite-default-keybinding nil)
    (setq cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap)
    (setq cfw:org-capture-template '("f"))

    ;; use Unicode characters
    (setq cfw:fchar-junction ?╋
          cfw:fchar-vertical-line ?┃
          cfw:fchar-horizontal-line ?━
          cfw:fchar-left-junction ?┣
          cfw:fchar-right-junction ?┫
          cfw:fchar-top-junction ?┯
          cfw:fchar-top-left-corner ?┏
          cfw:fchar-top-right-corner ?┓)

    (evil-set-initial-state 'cfw:calendar-mode 'emacs)))

(defun orgp/init-calfw-org ()
  (use-package calfw-org
    :after calfw
    :commands cfw:open-org-calendar
    ))

;; shows locations in agenda
(defun orgp/init-org-agenda-property ()
  (use-package org-agenda-property
    :after org
    :config
    (setq org-agenda-property-list nil) ;; disable by default
    (setq org-agenda-property-position 'where-it-fits)
    (setq org-agenda-property-column 0)))

;; layer and explicitly excludes it when helm layer not loaded
;; rifle through org files even when helm not active
;; (defun orgp/pre-init-helm-org-rifle ()
;;   (spacemacs|use-package-add-hook helm-org-rifle
;;     :post-init
;;     ;; show full headline path when rifling
;;     (setq helm-org-rifle-show-path t)

;;     ;; additional key bindings
;;     (spacemacs/set-leader-keys
;;       "aofa" 'helm-org-rifle-agenda-files
;;       "aofA" 'orgp/helm-org-rifle-agenda-files-with-archives)

;;     (spacemacs/set-leader-keys-for-major-mode 'org-mode
;;       "jr" 'helm-org-rifle-current-buffer)
;;     ))

;; (unless (configuration-layer/layer-used-p 'helm)
;;   ;; needed to define some variables so that the advices of Spacemacs for helm
;;   ;; functions don't make trouble
;;   (load (concat (configuration-layer/get-layer-path 'helm) "config.el"))

;;   ;; avoid helm to overwrite ivy's minibuffer history bindings
;;   (setq helm-minibuffer-history-key nil)

;;   ;; the Spacemacs keybindings (e.g., C-j and C-k to move down/up in helm) come
;;   ;; from with-eval-after-load's from spacemacs-completion. So no need to do
;;   ;; anything here.
;;   (defun orgp/init-helm ()
;;     (use-package helm
;;       :defer t)
;;     (orgp/pre-init-helm-org-rifle)
;;     (org/init-helm-org-rifle)
;;     (defun spacemacs-layouts/post-init-helm ()) ;; don't do that
;;     )
;;   )

(defun orgp/init-org-sticky-header ()
  (use-package org-sticky-header
    :commands (org-sticky-header-mode)
    :init
    (add-hook 'org-mode-hook #'org-sticky-header-mode)
    :config
    (setq org-sticky-header-full-path 'full
          org-sticky-header-prefix ""
          org-sticky-header-outline-path-separator " / "
          )

    (defun orgp/org-sticky-header-no-properties (orig-fun &rest arg)
      (let ((s (apply orig-fun arg)))
        (when s
          (substring-no-properties s))))
    (advice-add 'org-sticky-header--fetch-stickyline :around #'orgp/org-sticky-header-no-properties)))
