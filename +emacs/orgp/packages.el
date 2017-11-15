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
    helm
    helm-org-rifle ;; (helm layer does not need to be used)
    ))

(defun orgp/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    ;; quick jump to refile/capture locations
    (spacemacs/declare-prefix "aof" "files")
    (spacemacs/set-leader-keys "aofr" 'org-refile-goto-last-stored)
    (spacemacs/set-leader-keys "aofc" 'org-capture-goto-last-stored)
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
      "vR" 'org-reveal-with-siblings
      )

    ;; add C-<return> binding to agenda to select entry and close other window
    (define-key org-agenda-mode-map (kbd "C-<return>") 'orgp/org-agenda-switch-to-delete-other-window)
    (define-key org-agenda-mode-map (kbd "C-<enter>") 'orgp/org-agenda-switch-to-delete-other-window)

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
                            (make-string (- (frame-width)
                                            (length path) (length time-prefix) (length time))
                                         ? )
                            time-prefix
                            (propertize time 'face 'org-date))
                  path)
                ))))))

    ;; exporting src blocks
    ;; use SPC u ,bt instead and set :tangle file
    ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "eb" 'orgp/org-export-src-block)
  ))

(defun orgp/init-calfw ()
  (use-package calfw
    :after org
    :defer t
    :config
    (setq cfw:org-overwrite-default-keybinding t)
    (setq cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap)

    ;; use Unicode characters
    (setq cfw:fchar-junction ?╋
          cfw:fchar-vertical-line ?┃
          cfw:fchar-horizontal-line ?━
          cfw:fchar-left-junction ?┣
          cfw:fchar-right-junction ?┫
          cfw:fchar-top-junction ?┯
          cfw:fchar-top-left-corner ?┏
          cfw:fchar-top-right-corner ?┓)))

(defun orgp/init-calfw-org ()
  (use-package calfw-org
    :after calfw
    :defer t))

;; shows locations in agenda
(defun orgp/init-org-agenda-property ()
  (use-package org-agenda-property
    :after org
    :config
    (setq org-agenda-property-list nil) ;; disable by default
    (setq org-agenda-property-position 'where-it-fits)
    (setq org-agenda-property-column 0)))

;; rifle through org files
(defun orgp/init-helm-org-rifle ()
  (use-package helm-org-rifle
    :defer t
    :after helm
    :init
    ;; show full headline path when rifling
    (setq helm-org-rifle-show-path t)

    ;; key bindings
    (spacemacs/set-leader-keys
      "aofa" 'helm-org-rifle-agenda-files
      "aofA" 'orgp/helm-org-rifle-agenda-files-with-archives)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "js" 'helm-org-rifle-current-buffer)
    ))

(unless (configuration-layer/layer-used-p 'helm)
  ;; needed to define some variables so that the advices of Spacemacs for helm
  ;; functions don't make trouble
  (load (concat (configuration-layer/get-layer-path 'helm) "config.el"))

  ;; and explicitly initializing this gives us the Spacemacs keybindings (e.g.,
  ;; C-J and C-K to move down/up in helm)
  (defun orgp/init-helm ()
    (use-package helm
      :defer t)))
