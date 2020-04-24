;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defvar umlauts-remap-when-not-mapped t
  "When non-nil, remap öäÖÄ to []{} whenever they are unbound in
  the current location.")

(defvar umlauts-remap-initially nil
  "When non-nil, initially enable `global-umlauts-minor-mode'")

(spacemacs|add-toggle umlauts-minor-mode
  :mode umlauts-minor-mode
  :documentation "Locally toggle remapping of öäÖÄü to []{}\\."
  :evil-leader "t]" "tä")

(spacemacs|add-toggle global-umlauts-minor-mode
  :mode global-umlauts-minor-mode
  :documentation "Globally toggle remapping of öäÖÄü to []{}\\."
  :evil-leader "t}" "tÄ")
