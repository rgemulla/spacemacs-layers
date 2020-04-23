;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(spacemacs|unless-dumping-and-eval-after-loaded-dump umlauts
  (when umlauts-remap-when-not-mapped
    (umlauts//remap-on local-function-key-map t))

  (when umlauts-remap-initially
    (spacemacs/toggle-global-umlauts-minor-mode-on)))
