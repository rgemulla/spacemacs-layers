;;; -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defconst latexp-packages
  '(
    auctex
    ))

(defun latexp/pre-init-auctex ()
  (spacemacs|use-package-add-hook tex
    :post-config
    ;; additional key bindings
    (define-key spacemacs-latex-mode-map "c" nil) ;; close env
    (define-key spacemacs-latex-mode-map "e" nil) ;; env
    (spacemacs/declare-prefix-for-mode 'latex-mode "me" "environment")
    (spacemacs/set-leader-keys-for-major-mode 'latex-mode
      "e]" 'LaTeX-close-environment
      "ea" 'latexp/LaTeX-align*
      "eA" 'latexp/LaTeX-align
      "ec" 'latexp/LaTeX-center
      "ee" 'LaTeX-environment
      "ef" 'latexp/LaTeX-frame
      "eF" 'latexp/LaTeX-figure
      "ei" 'latexp/LaTeX-itemize
      "eI" 'latexp/LaTeX-enumerate
      "em" 'latexp/LaTeX-toggle-math
      "eq" 'latexp/LaTeX-equation*
      "eQ" 'latexp/LaTeX-equation
      "er" 'latexp/LaTeX-array
      "et" 'latexp/LaTeX-tabular
      "eT" 'latexp/LaTeX-table
      "ge" 'TeX-next-error
      "gE" 'TeX-previous-error)

    ;; disable fill in some environments
    (with-eval-after-load 'latex
      (add-to-list 'LaTeX-indent-environment-list '("figure"))
      (add-to-list 'LaTeX-indent-environment-list '("tikzpicture")))
    ))
