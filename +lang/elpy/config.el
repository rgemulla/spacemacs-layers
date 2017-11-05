;;; -*- lexical-binding: t -*-
;;
;; Author: Rainer Gemulla <rgemulla@gmx.de>
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(with-eval-after-load 'which-key
  (spacemacs/declare-prefix-for-mode 'python-mode "t" "toggles")

  (spacemacs|add-toggle elpy/shell-display-buffer-after-send
    :documentation "Toggles whether to show the python shell after sending something to it"
    :status elpy-shell-display-buffer-after-send
    :on (setq elpy-shell-display-buffer-after-send t)
    :off (setq elpy-shell-display-buffer-after-send nil)
    :evil-leader-for-mode (python-mode . "td"))

  (spacemacs|add-toggle elpy/shell-echo-input
    :documentation "Toggles whether to echo input sent to the Python shell in the shell buffer"
    :status elpy-shell-echo-input
    :on (setq elpy-shell-echo-input t)
    :off (setq elpy-shell-echo-input nil)
    :evil-leader-for-mode (python-mode . "ti"))

  (spacemacs|add-toggle elpy/shell-echo-output
    :documentation "Toggles whether to echo the Python shell output in the echo area"
    :status elpy-shell-echo-output
    :on (setq elpy-shell-echo-output 'when-shell-not-visible)
    :off (setq elpy-shell-echo-output nil)
    :evil-leader-for-mode (python-mode . "to"))
  )
