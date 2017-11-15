;;; -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(with-eval-after-load 'which-key
  (spacemacs|add-toggle org-indent-mode
    :mode org-indent-mode
    :documentation "Toggle org-indent-mode."
    :evil-leader-for-mode (org-mode . "TI")))
