;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(when umlauts-remap-when-not-mapped
  (umlauts//remap-on local-function-key-map t))

(when umlauts-remap-initially
  (spacemacs/toggle-umlauts-minor-mode-on))
