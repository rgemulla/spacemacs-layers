;;; -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Author:  <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defvar orgp/org-agenda-time-grid-hide-when-appointment t
  "Hide time grid lines in org-agenda when there is an overlapping appointment")

(with-eval-after-load 'which-key
  (spacemacs|add-toggle org-indent-mode
    :mode org-indent-mode
    :documentation "Toggle org-indent-mode."
    :evil-leader-for-mode (org-mode . "TI")))
