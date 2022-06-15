;;; -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defconst latexp-packages
  '(auctex))

(defun latexp/pre-init-auctex ()
  (spacemacs|use-package-add-hook tex
    :post-config
    ;; additional key bindings
    (define-key spacemacs-latex-mode-map "c" nil) ;; close env
    (define-key spacemacs-latex-mode-map "e" nil) ;; env
    (spacemacs/declare-prefix-for-mode 'latex-mode "me" "environment")
    (spacemacs/set-leader-keys-for-major-mode 'latex-mode
      "e]" #'LaTeX-close-environment
      "ea" #'latexp/LaTeX-align*
      "eA" #'latexp/LaTeX-align
      "ec" #'latexp/LaTeX-center
      "ee" #'LaTeX-environment
      "ef" #'latexp/LaTeX-frame
      "eF" #'latexp/LaTeX-figure
      "ei" #'latexp/LaTeX-itemize
      "eI" #'latexp/LaTeX-enumerate
      "em" #'latexp/LaTeX-toggle-math
      "eq" #'latexp/LaTeX-equation*
      "eQ" #'latexp/LaTeX-equation
      "er" #'latexp/LaTeX-array
      "et" #'latexp/LaTeX-tabular
      "eT" #'latexp/LaTeX-table
      "ge" #'TeX-next-error
      "gE" #'TeX-previous-error
      "gm" #'latexp/find-master
      )

    ;; set error function
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq next-error-function #'TeX-next-error)))

    ;; disable fill in some environments
    (with-eval-after-load 'latex
      (add-to-list 'LaTeX-indent-environment-list '("figure"))
      (add-to-list 'LaTeX-indent-environment-list '("tikzpicture")))

    ;; basic vim-style navigation in reftex toc buffers (rebinds k to x, and x
    ;; to X)
    (with-eval-after-load 'reftex-toc
      ;; so we only map the most important keys (e.g., want to keep space)
      (define-key reftex-toc-mode-map "j" 'reftex-toc-next)
      (define-key reftex-toc-mode-map (kbd "C-j") 'reftex-toc-next-heading)
      (define-key reftex-toc-mode-map "k" 'reftex-toc-previous)
      (define-key reftex-toc-mode-map (kbd "C-k") 'reftex-toc-previous-heading)
      (define-key reftex-toc-mode-map (kbd "C-d") 'evil-scroll-down)
      (define-key reftex-toc-mode-map (kbd "C-u") 'evil-scroll-up)
      (define-key reftex-toc-mode-map "x" 'reftex-toc-quit-and-kill)
      (define-key reftex-toc-mode-map "X" 'reftex-toc-external))

    ;; and in reftex-select-label buffers
    (with-eval-after-load 'reftex-sel
      (define-key reftex-select-label-mode-map "j" 'reftex-select-next)
      (define-key reftex-select-label-mode-map (kbd "C-j") 'reftex-select-next-heading)
      (define-key reftex-select-label-mode-map "k" 'reftex-select-previous)
      (define-key reftex-select-label-mode-map (kbd "C-k") 'reftex-select-previous-heading)
      (define-key reftex-select-label-mode-map (kbd "C-d") 'evil-scroll-down)
      (define-key reftex-select-label-mode-map (kbd "C-u") 'evil-scroll-up))
    ))
