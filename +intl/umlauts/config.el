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

(defvar umlauts-remap-everywhere-initially nil
  "When non-nil, initially enable remapping of öäÖÄ to []{}
  everywhere.")

(spacemacs|add-toggle umlauts-minor-mode
  :mode umlauts-minor-mode
  :documentation "Toggle remapping of öäÖÄ to []{} everywhere."
  :evil-leader "t]")
