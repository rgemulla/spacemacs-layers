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
  "When non-nil, initially enable `umlauts-minor-mode'")

(spacemacs|add-toggle umlauts-minor-mode
  :mode umlauts-minor-mode
  :documentation "Toggle remapping of öäÖÄü to []{}\\."
  :evil-leader "t]")
