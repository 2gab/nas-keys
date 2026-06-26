;;; nas-map.el --- Default keymaps -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: 2gab <info@2gabriel.com>
;; Maintainer: 2gab <info@2gabriel.com>
;; Source: https://github.com/2gab/nas-keys
;; Keywords: editing, modal, keys
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;
;; This file is not part of GNU Emacs

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;
;;; Commentary:
;;
;; Default key bindings for Nas.
;;

;;; Code:

(require 'nas-state)

;;; Dispatch commands

(nas-define-dispatch nas-delete-dispatch " d"
  (?d  (delete-char 1))
  (?j  (delete-char -1))
  (?\; (delete-char 1))
  (?k  (kill-line))
  (?l  (kill-line 0))
  (?w  (kill-word 1))
  (?a  (when (yes-or-no-p "Erase entire buffer? ")
         (erase-buffer))))

;;; Prefix maps

(nas-prefix go
  "g" #'beginning-of-buffer
  "l" #'goto-line
  "c" #'goto-char
  "f" #'find-function)

(nas-prefix leader
  "j"   #'backward-word
  ";"   #'forward-word
  "k"   #'forward-paragraph
  "l"   #'backward-paragraph
  "f"   #'isearch-forward
  "r"   #'query-replace
  "h"   #'help-command
  "SPC" #'nas-enter-sequential)

;;; Insert state

(nas-bind insert
  "M-SPC" #'nas-enter-visual)

;;; Visual state

;; Suppress self-insert: letters must not type text in visual state.
(suppress-keymap nas-visual-map t)

(nas-bind visual
  ;; Mode switching
  "RET"   #'nas-enter-insert
  "M-SPC" #'nas-enter-insert
  ;; Navigation — character level  (j k l ; = arrow keys)
  "j"     #'backward-char
  "k"     #'next-line
  "l"     #'previous-line
  ";"     #'forward-char
  ;; Navigation — extremes  (Shift = maximum power)
  "J"     #'beginning-of-line
  ":"     #'end-of-line
  "L"     #'beginning-of-buffer
  "K"     #'end-of-buffer
  ;; Prefix maps
  "g"     nas-go-map
  "SPC"   nas-leader-map
  ;; Delete
  "d"     #'nas-delete-dispatch
  ;; Select / region
  "s"     #'set-mark-command
  "S"     #'mark-whole-buffer
  ;; Clipboard
  "y"     #'yank
  "c"     #'kill-ring-save
  "x"     #'kill-region
  ;; Undo
  "u"     #'undo
  ;; Open / buffer
  "o"     #'find-file
  "b"     #'switch-to-buffer)

;;; Sequential state

(defun nas-sequential-capture-key ()
  "Append the current key to `nas--seq-keys' and refresh the mode-line."
  (interactive)
  (setq nas--seq-keys (vconcat nas--seq-keys (this-command-keys-vector)))
  (force-mode-line-update))

(defun nas-sequential-backspace ()
  "Remove the last key from `nas--seq-keys'."
  (interactive)
  (when (> (length nas--seq-keys) 0)
    (setq nas--seq-keys (seq-subseq nas--seq-keys 0 (1- (length nas--seq-keys))))
    (force-mode-line-update)))

(defun nas-sequential-execute ()
  "Execute the accumulated key sequence via the visual keymap, then return to visual."
  (interactive)
  (let ((keys nas--seq-keys))
    (setq nas--seq-keys [])
    (nas-enter-visual)
    (unless (= (length keys) 0)
      ;; Replay the sequence as if typed in visual state.
      ;; Prefix maps (g, SPC) and dispatch commands (d) resolve correctly
      ;; because execute-kbd-macro feeds keys one at a time to the lookup.
      (let ((overriding-local-map nas-visual-map))
        (execute-kbd-macro keys)))))

(defun nas-sequential-cancel ()
  "Discard the accumulated sequence and return to visual."
  (interactive)
  (setq nas--seq-keys [])
  (nas-enter-visual))

;; [t] = catch-all: any key not explicitly bound is appended to the buffer.
(define-key nas-sequential-map [t]          #'nas-sequential-capture-key)
(define-key nas-sequential-map (kbd "RET")  #'nas-sequential-execute)
(define-key nas-sequential-map (kbd "ESC")  #'nas-sequential-cancel)
(define-key nas-sequential-map (kbd "DEL")  #'nas-sequential-backspace)

;;; Window management

(nas-bind window
  "M-j" #'windmove-left
  "M-k" #'windmove-down
  "M-l" #'windmove-up
  "M-;" #'windmove-right
  "M-n" #'split-window-below
  "M-v" #'split-window-right
  "M-d" #'delete-window
  "M-a" #'delete-other-windows)

;;; Emulation keymap alist
;;
;; Priority order: sequential > visual > insert > window (always on).
;; `nas-mode' is the minor-mode variable; window map is active whenever Nas is.

(setq nas-mode-map-alist
      `((nas--sequential-active . ,nas-sequential-map)
        (nas--visual-active     . ,nas-visual-map)
        (nas--insert-active     . ,nas-insert-map)
        (nas-mode               . ,nas-window-map)))

(provide 'nas-map)
;;; nas-map.el ends here
