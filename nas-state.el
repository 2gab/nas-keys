;;; nas-state.el --- State machine -*- lexical-binding: t; -*-

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
;; State machine for Nas.
;; Handles state transitions and shared state utilities.
;;

;;; Code:

;;; State flags

(defvar nas-state 'visual
  "Current Nas state.  One of the symbols: insert, visual, sequential.")

(defvar nas--insert-active nil
  "Non-nil when insert state is active.  Used as key in `nas-mode-map-alist'.")

(defvar nas--visual-active nil
  "Non-nil when visual state is active.  Used as key in `nas-mode-map-alist'.")

(defvar nas--sequential-active nil
  "Non-nil when sequential state is active.  Used as key in `nas-mode-map-alist'.")

;;; Sequential buffer

(defvar nas--seq-keys []
  "Vector of keys accumulated while in sequential state.
Reset on entry; consumed by `nas-sequential-execute'.")

;;; Keymaps

(defvar nas-insert-map (make-sparse-keymap)
  "Keymap active in insert state.
Only overrides mode-switching keys; all other input falls through to Emacs.")

(defvar nas-visual-map (make-sparse-keymap)
  "Keymap active in visual state.
Self-insert is suppressed; every meaningful key must be explicitly bound.")

(defvar nas-sequential-map (make-sparse-keymap)
  "Keymap active in sequential state.")

(defvar nas-window-map (make-sparse-keymap)
  "Window-management keymap.  Active in all states when `nas-mode' is on.")

;;; Emulation keymap alist
;;
;; Populated by nas-map.el after all keymaps are configured.
;; Registered in `emulation-mode-map-alists' by `nas--enable'.
;; Priority is determined by list order: sequential > visual > insert > window.

(defvar nas-mode-map-alist nil
  "Alist of (FLAG . KEYMAP) for Nas states.
Each FLAG is a variable; its keymap is active while the variable is non-nil.")

;;; State transitions

(defun nas-set-state (state)
  "Transition Nas to STATE (a symbol: insert, visual or sequential).
Toggles the per-state flag variables so `nas-mode-map-alist' reflects the
new state immediately — no deactivation thunk required."
  (setq nas--insert-active     (eq state 'insert)
        nas--visual-active     (eq state 'visual)
        nas--sequential-active (eq state 'sequential)
        nas-state              state)
  (nas--update-cursor state)
  (force-mode-line-update))

(defun nas-enter-insert ()
  "Enter insert state."
  (interactive)
  (nas-set-state 'insert))

(defun nas-enter-visual ()
  "Enter visual state."
  (interactive)
  (nas-set-state 'visual))

(defun nas-enter-sequential ()
  "Enter sequential state, clearing any previously accumulated keys."
  (interactive)
  (setq nas--seq-keys [])
  (nas-set-state 'sequential))

;;; Cursor

(defun nas--update-cursor (state)
  "Update cursor shape to reflect STATE."
  (setq cursor-type
        (pcase state
          ('insert     'bar)
          ('visual     'box)
          ('sequential 'hollow)
          (_           'box))))

;;; Mode line

(defun nas--lighter ()
  "Return mode-line string indicating the active Nas state.
In sequential state, shows the accumulated key sequence."
  (pcase nas-state
    ('insert     " NAS[I]")
    ('visual     " NAS[V]")
    ('sequential (if (> (length nas--seq-keys) 0)
                     (format " NAS[S: %s]" (key-description nas--seq-keys))
                   " NAS[S]"))
    (_           " NAS[?]")))

;;; Macros

(defmacro nas-define-dispatch (name prompt &rest bindings)
  "Define NAME as an interactive command that reads one key and dispatches.
PROMPT is shown in the echo area while waiting for input.
BINDINGS are `pcase' clauses.  Unless BINDINGS already supplies a `_'
clause, an unmatched key is re-queued unread by default."
  (declare (indent 2))
  `(defun ,name ()
     (interactive)
     (let ((key (read-key (propertize ,prompt 'face 'minibuffer-prompt))))
       (pcase key
         ,@bindings
         ,@(unless (assq '_ bindings)
             '((_ (push key unread-command-events))))))))

(defmacro nas-bind (state &rest bindings)
  "Bind BINDINGS in the Nas keymap for STATE.
STATE is a symbol: insert, visual, sequential or window.
BINDINGS alternate between KEY (string, passed through `kbd') and COMMAND."
  (declare (indent 1))
  (let ((map (intern (format "nas-%s-map" state)))
        forms)
    (while bindings
      (let ((key (pop bindings))
            (cmd (pop bindings)))
        (push `(define-key ,map (kbd ,key) ,cmd) forms)))
    `(progn ,@(nreverse forms))))

(defmacro nas-prefix (name &rest bindings)
  "Define nas-NAME-map as a prefix keymap and populate it with BINDINGS.
BINDINGS alternate between KEY (string, passed through `kbd') and COMMAND.
Equivalent to declaring a `defvar' sparse keymap and calling `define-key'
for each pair — kept in one place for consistency with `nas-bind'."
  (declare (indent 1))
  (let ((map-sym (intern (format "nas-%s-map" name)))
        forms)
    (while bindings
      (let ((key (pop bindings))
            (cmd (pop bindings)))
        (push `(define-key ,map-sym (kbd ,key) ,cmd) forms)))
    `(progn
       (defvar ,map-sym (make-sparse-keymap)
         ,(format "Prefix keymap for the `%s' key in Nas visual state." name))
       ,@(nreverse forms))))

(provide 'nas-state)
;;; nas-state.el ends here
