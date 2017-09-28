;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defconst elpy-packages
  '(python
    elpy
    jedi))

(defun elpy/init-elpy () ) ;; done below

(defun elpy/init-jedi () ) ;; done below

(defun elpy/init-python ()
  (use-package python
    :defer t
    :ensure elpy
    :ensure jedi ; for autocompletion
    :mode (("\\.py\\'" . python-mode)
           ("\\.ipy\\'" . python-mode))
    :init
    (setq python-indent-offset 4)
    :config
    ;; enable elpy
    (elpy-enable)

    ;; set lighter
    (diminish 'elpy-mode " Ⓔ")

    ;; configure auto-completion
    (setq elpy-rpc-backend "jedi")
    (add-hook  'elpy-mode-hook
               '(lambda ()
                  ;; after 2 seconds or C-tab
                  (setq company-minimum-prefix-length 2)
                  (setq company-idle-delay 2)
                  (define-key elpy-mode-map (kbd "C-<tab>") 'company-complete)))

    ;; make jupyter console the default
    (if (string-equal system-type "windows-nt")
        (setq python-shell-interpreter "jupyter"
              python-shell-interpreter-args "console --simple-prompt")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "--simple-prompt"))
    (setq elpy-rpc-python-command "python")

    ;; definitions and key bindings for sending code fragments to the Python
    ;; shell (and echo them there)
    (add-hook 'elpy-mode-hook
              '(lambda ()
                 (define-key elpy-mode-map (kbd "C-c C-l")
                   'elpy/shell-send-current-statement-and-step)
                 (define-key elpy-mode-map (kbd "C-<return>") 'elpy/shell-send-current-defun-and-step)
                 (define-key elpy-mode-map (kbd "C-c C-c")
                   'elpy/shell-send-current-group-and-step)
                 (define-key elpy-mode-map (kbd "C-c C-b") 'elpy/shell-send-current-buffer-and-step)))

    ;; general major-mode key bindings
    (spacemacs/declare-prefix-for-mode 'python-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'python-mode "me" "send to REPL")
    (spacemacs/declare-prefix-for-mode 'python-mode "ms" "send to REPL and step")
    (spacemacs/declare-prefix-for-mode 'python-mode "mV" "pyvenv")
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "i" 'elpy-shell-switch-to-shell
      "I" 'elpy-shell-switch-to-shell-in-current-window
      "h" 'elpy-doc
      "go" 'elpy-occur-definitions
      "gg" 'elpy-goto-definition
      "gG" 'elpy-goto-definition-other-window
      "ee" 'elpy/shell-send-current-statement
      "eE" 'elpy/shell-send-current-statement-and-go
      "ef" 'elpy/shell-send-current-defun
      "eF" 'elpy/shell-send-current-defun-and-go
      "ed" 'elpy/shell-send-current-group
      "eD" 'elpy/shell-send-current-group-and-go
      "er" 'elpy/shell-send-current-region-or-buffer
      "eR" 'elpy/shell-send-current-region-or-buffer-and-go
      "eb" 'elpy/shell-send-current-buffer
      "eB" 'elpy/shell-send-current-buffer-and-go
      "se" 'elpy/shell-send-current-statement-and-step
      "sE" 'elpy/shell-send-current-statement-and-step-and-go
      "sf" 'elpy/shell-send-current-defun-and-step
      "sF" 'elpy/shell-send-current-defun-and-step-and-go
      "sd" 'elpy/shell-send-current-group-and-step
      "sD" 'elpy/shell-send-current-group-and-step-and-go
      "sr" 'elpy/shell-send-current-region-or-buffer-and-step
      "sR" 'elpy/shell-send-current-region-or-buffer-and-step-and-go
      "sb" 'elpy/shell-send-current-buffer-and-step
      "sB" 'elpy/shell-send-current-buffer-and-step-and-go
      "Va" 'pyvenv-activate
      "Vd" 'pyvenv-deactivate
      "Vw" 'pyvenv-workon)

    (spacemacs/set-leader-keys-for-major-mode 'inferior-python-mode
      "i" 'elpy-shell-switch-to-buffer
      "I" 'elpy-shell-switch-to-buffer-in-current-window)

    ;; disable smartscan-mode to make M-p and M-n select previous/next statement in python shell
    (with-eval-after-load 'smartscan
      (add-hook 'inferior-python-mode-hook
                '(lambda () (smartscan-mode -1))))
    ))
