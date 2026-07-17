;;; nas-tests.el --- Tests for Nas -*- lexical-binding: t; -*-

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
;; Run with:
;;   emacs -Q --batch -L . -l nas-tests.el -f ert-run-tests-batch-and-exit
;;

;;; Code:

(require 'ert)
(require 'nas)

;;; Test helper

(defun nas-test--buffer-with-point ()
  "Return the current buffer's text with `|' marking point."
  (concat (buffer-substring (point-min) (point)) "|"
          (buffer-substring (point) (point-max))))

(defun nas-test--run (initial state keys)
  "Seed a buffer from INITIAL (`|' marks point), enter STATE, run KEYS.
Returns the resulting buffer text, again with `|' marking point.
A real window is required for `execute-kbd-macro' to behave correctly
under `emacs --batch' (see `switch-to-buffer-other-window' below)."
  (let ((buffer (generate-new-buffer " *nas-test*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer buffer
            (switch-to-buffer-other-window buffer)
            (nas-mode 1)
            (insert (replace-regexp-in-string "|" "" initial))
            (goto-char (1+ (string-match "|" initial)))
            (nas-set-state state)
            (execute-kbd-macro (kbd keys))
            (nas-test--buffer-with-point)))
      (nas-mode -1)
      (kill-buffer buffer))))

(defmacro nas-should-buffer (initial state keys expected)
  "Assert that running KEYS from INITIAL in STATE produces EXPECTED.
See `nas-test--run' for the `|' point-marker convention."
  `(should (string= (nas-test--run ,initial ,state ,keys) ,expected)))

;;; State machine

(ert-deftest nas-test-enter-insert ()
  (with-temp-buffer
    (nas-enter-insert)
    (should (eq nas-state 'insert))
    (should nas--insert-active)
    (should-not nas--visual-active)
    (should-not nas--sequential-active)
    (should (eq cursor-type 'bar))))

(ert-deftest nas-test-enter-visual ()
  (with-temp-buffer
    (nas-enter-visual)
    (should (eq nas-state 'visual))
    (should nas--visual-active)
    (should (eq cursor-type 'box))))

(ert-deftest nas-test-enter-sequential-clears-seq-keys ()
  (with-temp-buffer
    (setq nas--seq-keys [106 107])
    (nas-enter-sequential)
    (should (eq nas-state 'sequential))
    (should nas--sequential-active)
    (should (equal nas--seq-keys []))
    (should (eq cursor-type 'hollow))))

(ert-deftest nas-test-lighter ()
  (with-temp-buffer
    (nas-enter-insert)
    (should (equal (nas--lighter) " NAS[I]"))
    (nas-enter-visual)
    (should (equal (nas--lighter) " NAS[V]"))
    (nas-enter-sequential)
    (should (equal (nas--lighter) " NAS[S]"))
    (setq nas--seq-keys (kbd "j k"))
    (should (equal (nas--lighter) " NAS[S: j k]"))))

;;; Visual motion

(ert-deftest nas-test-visual-char-motion ()
  (nas-should-buffer "hello |world" 'visual "j j j" "hel|lo world"))

(ert-deftest nas-test-visual-power-motion ()
  (nas-should-buffer "hello\nwo|rld" 'visual "J" "hello\n|world"))

;;; Delete dispatch

(ert-deftest nas-test-delete-dispatch-forward ()
  (nas-should-buffer "|hello" 'visual "d d" "|ello"))

(ert-deftest nas-test-delete-dispatch-backward ()
  (nas-should-buffer "hel|lo" 'visual "d j" "he|lo"))

(ert-deftest nas-test-delete-dispatch-word ()
  (nas-should-buffer "|hello world" 'visual "d w" "| world"))

(ert-deftest nas-test-delete-dispatch-fallback-deletes-and-reprocesses ()
  ;; `n' is not one of nas-delete-dispatch's bindings: it should delete
  ;; the char under point, then get reprocessed (as an unbound visual
  ;; key, `n' is a no-op) rather than being silently swallowed.
  (nas-should-buffer "|hello" 'visual "d n" "|ello"))

;;; Sequential mode

(ert-deftest nas-test-sequential-capture-and-execute ()
  (nas-should-buffer "hello |world" 'visual "SPC SPC j j RET" "hell|o world"))

(ert-deftest nas-test-sequential-cancel-discards-input ()
  (with-temp-buffer
    (insert "hello world")
    (goto-char (point-max))
    (nas-mode 1)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer-other-window (current-buffer))
          (nas-enter-visual)
          (execute-kbd-macro (kbd "SPC SPC j j ESC"))
          (should (eq nas-state 'visual))
          (should (= (point) (point-max))))
      (nas-mode -1))))

(ert-deftest nas-test-sequential-backspace ()
  (with-temp-buffer
    (nas-enter-sequential)
    (setq nas--seq-keys (kbd "j k"))
    (nas-sequential-backspace)
    (should (equal nas--seq-keys (kbd "j")))))

;;; Keyboard layout

(ert-deftest nas-test-translate-key-noop-on-qwerty ()
  (should (equal (nas--translate-key ";") ";"))
  (should (equal (nas--translate-key "j") "j")))

(ert-deftest nas-test-kbd-preserves-modifiers ()
  (should (equal (nas--kbd "j") (kbd "j")))
  (should (equal (nas--kbd "M-j") (kbd "M-j"))))

(ert-deftest nas-test-set-layout-translates-and-restores ()
  (unwind-protect
      (progn
        (nas-set-layout "qwerty-abnt")
        (should (equal (nas--translate-key ";") "ç"))
        (should (equal (nas--kbd "M-;") (kbd "M-ç")))
        (should (null (lookup-key nas-visual-map (kbd ";"))))
        (should (eq (lookup-key nas-visual-map (kbd "ç")) 'forward-char))
        ;; suppress-keymap's self-insert remap must survive the rebuild
        (should (eq (lookup-key nas-visual-map [remap self-insert-command])
                    'undefined)))
    (nas-set-layout "qwerty")))

(provide 'nas-tests)
;;; nas-tests.el ends here
