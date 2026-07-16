;;; nas-layout.el --- Keyboard layout translation -*- lexical-binding: t; -*-

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
;; Nas bindings (`nas-bind', `nas-prefix', `nas-define-dispatch') are all
;; written positionally against `qwerty': "j" always means "the letter
;; where a US keyboard has j".  On a different physical layout the same
;; position can produce a different character (e.g. ABNT2 has `ç' where
;; qwerty has `;'), so every key goes through `nas--kbd' / `nas--dispatch-char'
;; before being bound, translating it to whatever `nas-keyboard-layout'
;; produces in that same slot.
;;

;;; Code:

(require 'nas-state)

;;; Layout diagrams

(defvar nas-layout-diagrams (make-hash-table :test 'equal)
  "Layout name (string) -> text diagram of the rows Nas binds against.
Diagrams must list tokens in the same order as `qwerty' so they can be
compared positionally.  Add more layouts with `puthash'.")

(puthash "qwerty" "
q w e r t y u i o p
a s d f g h j k l ; '
z x c v b n m , . /

Q W E R T Y U I O P
A S D F G H J K L : \"
Z X C V B N M < > ?
" nas-layout-diagrams)

(puthash "qwerty-abnt" "
q w e r t y u i o p
a s d f g h j k l ç ~
z x c v b n m , . ;

Q W E R T Y U I O P
A S D F G H J K L Ç ^
Z X C V B N M < > :
" nas-layout-diagrams)

;;; Current layout

(defcustom nas-keyboard-layout "qwerty"
  "Physical keyboard layout Nas keybindings should target.
Bindings are written positionally against `qwerty' and translated to
this layout's key in the same slot; see `nas-layout-diagrams' for the
supported values.  Call `nas-set-layout' instead of setting this
directly, so the keymaps get rebuilt against the new layout."
  :type '(choice (const "qwerty") (const "qwerty-abnt") string)
  :group 'nas)

(defun nas--layout-names ()
  "Return the list of layout names known to `nas-layout-diagrams'."
  (let (names)
    (maphash (lambda (name _diagram) (push name names)) nas-layout-diagrams)
    names))

(defun nas--layout-conv-table (layout)
  "Return a hashtable translating a `qwerty' key to its LAYOUT equivalent.
Keys that are the same in both layouts are omitted."
  (let ((from (split-string (gethash "qwerty" nas-layout-diagrams) "[ \n]+" t))
        (to   (gethash layout nas-layout-diagrams)))
    (unless to
      (error "Nas: unknown keyboard layout %S" layout))
    (setq to (split-string to "[ \n]+" t))
    (unless (= (length from) (length to))
      (error "Nas: layout %S doesn't line up with `qwerty'" layout))
    (let ((table (make-hash-table :test 'equal)))
      (seq-mapn (lambda (a b) (unless (string= a b) (puthash a b table)))
                from to)
      table)))

(defvar nas--layout-table (nas--layout-conv-table nas-keyboard-layout)
  "Cached qwerty -> `nas-keyboard-layout' key-translation table.
Rebuilt by `nas-set-layout'.")

;;; Translation

(defun nas--translate-key (key)
  "Translate a single qwerty KEY token to its `nas-keyboard-layout' equivalent."
  (or (gethash key nas--layout-table) key))

(defun nas--kbd (keystr)
  "Like `kbd', but translate KEYSTR through `nas--layout-table' first.
Modifier prefixes (`M-', `C-', `S-', ...) are preserved; only the final
component of each token — the actual key — gets translated."
  (kbd (mapconcat
        (lambda (chunk)
          (if (string-match "\\`\\(\\(?:[ACHMSs]-\\)*\\)\\(.+\\)\\'" chunk)
              (concat (match-string 1 chunk)
                      (nas--translate-key (match-string 2 chunk)))
            chunk))
        (split-string keystr " " t)
        " ")))

(defun nas--dispatch-char (char)
  "Translate CHAR (as read by `read-key') like `nas--kbd' does for strings.
Used by `nas-define-dispatch', which matches raw characters instead of
going through a keymap."
  (let ((translated (nas--translate-key (char-to-string char))))
    (if (= (length translated) 1) (aref translated 0) char)))

;;; Binding registry

(defvar nas--binding-registry nil
  "List of (MAP-SYM KEY CMD) triples registered by `nas-bind'/`nas-prefix'.
Replayed by `nas-set-layout' to rebind everything against a new layout.")

(defun nas--register-key (map-sym key cmd)
  "Bind KEY to CMD in the keymap named MAP-SYM, translated for the current
`nas-keyboard-layout', and remember the binding for `nas-set-layout'."
  (push (list map-sym key cmd) nas--binding-registry)
  (define-key (symbol-value map-sym) (nas--kbd key) cmd))

;;; Switching layout

(defun nas-set-layout (layout)
  "Switch Nas to LAYOUT and rebind every key registered so far.
LAYOUT must be a key of `nas-layout-diagrams' (see `nas--layout-names')."
  (interactive
   (list (completing-read "Nas keyboard layout: "
                           (sort (nas--layout-names) #'string<)
                           nil t nil nil nas-keyboard-layout)))
  (setq nas-keyboard-layout layout
        nas--layout-table   (nas--layout-conv-table layout))
  (dolist (map-sym (delete-dups (mapcar #'car nas--binding-registry)))
    (setcdr (symbol-value map-sym) nil))
  (suppress-keymap nas-visual-map t)
  (dolist (binding (reverse nas--binding-registry))
    (define-key (symbol-value (nth 0 binding))
      (nas--kbd (nth 1 binding))
      (nth 2 binding)))
  (message "Nas: keyboard layout set to %s" layout))

(provide 'nas-layout)
;;; nas-layout.el ends here
