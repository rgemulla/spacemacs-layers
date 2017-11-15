;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defconst frames-packages
  '(frame-cmds))

(defun frames/init-frame-cmds ()
  (use-package frame-cmds
    :init
    (require 'frame-cmds)))
