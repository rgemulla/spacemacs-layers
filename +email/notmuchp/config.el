;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defvar notmuchp-kill-emacs-query-compose t
  "When non-nil, ask to confirm killing Emacs when emails are
  currently being composed.")

(defvar notmuchp-confirm-empty-subject t
  "When non-nil, ask to confirm sending a message when the
  subject field is empty.")
