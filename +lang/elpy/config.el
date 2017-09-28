;;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defcustom elpy/shell-display-buffer-after-send nil
  "Whether to show the Python shell after sending something to it."
  :type 'boolean
  :group 'elpy)

(defcustom elpy/shell-echo-output 'when-shell-not-visible
  "Whether to echo the Python shell output in the echo area after input has been sent to the shell.

  Possible choices are nil (=never), when-shell-not-visible, or
  t (=always)."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "When shell not visible" when-shell-not-visible)
                 (const :tag "Always" t))
  :group 'elpy)

(defcustom elpy/shell-echo-input t
  "Whether to echo input sent to the Python shell in the shell buffer."
  :type 'boolean
  :group 'elpy)

(defcustom elpy/shell-echo-input-cont-prompt t
  "Whether to show a continuation prompt when echoing to the Python shell."
  :type 'boolean
  :group 'elpy)

(defcustom elpy/shell-echo-input-lines-head 10
  "Maximum number of lines to show before truncating echoed input in the Python shell."
  :type 'integer
  :group 'elpy)

(defcustom elpy/shell-echo-input-lines-tail 10
  "Maximum number of lines to show after truncating echoed input in the Python shell."
  :type 'integer
  :group 'elpy)

(with-eval-after-load 'which-key
  (spacemacs/declare-prefix "tP" "elpy")

  (spacemacs|add-toggle elpy/shell-display-buffer-after-send
    :documentation "Toggles whether to show the python shell after sending something to it"
    :status elpy/shell-display-buffer-after-send
    :on (setq elpy/shell-display-buffer-after-send t)
    :off (setq elpy/shell-display-buffer-after-send nil)
    :evil-leader "tPd")

  (spacemacs|add-toggle elpy/shell-echo-input
    :documentation "Toggles whether to echo input sent to the Python shell in the shell buffer"
    :status elpy/shell-echo-input
    :on (setq elpy/shell-echo-input t)
    :off (setq elpy/shell-echo-input nil)
    :evil-leader "tPi")

  (spacemacs|add-toggle elpy/shell-echo-output
    :documentation "Toggles whether to echo the Python shell output in the echo area"
    :status elpy/shell-echo-output
    :on (setq elpy/shell-echo-output 'when-shell-not-visible)
    :off (setq elpy/shell-echo-output nil)
    :evil-leader "tPo"))
