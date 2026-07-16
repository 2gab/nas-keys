;;; nas.el --- A modal editing system for Emacs -*- lexical-binding: t; -*-

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
;; Nas is not a Vim clone.
;; Keys behave like ideograms: each has a meaning, and combining
;; them creates deeper meaning.
;;
;; States:
;;   Insert     — text insertion       (cursor: bar)
;;   Visual     — navigation/editing   (cursor: box)
;;   Sequential — command sequences    (cursor: hollow)
;;
;; Quick start:
;;   (require 'nas)
;;   (nas-mode 1)
;;
;; To activate on startup:
;;   (add-hook 'after-init-hook #'nas-mode)
;;
;; Bindings are declared positionally against a US qwerty keyboard.  On a
;; different physical layout (e.g. Brazilian ABNT2), call:
;;   (nas-set-layout "qwerty-abnt")
;; See `nas-layout-diagrams' for the supported layouts.
;;

;;; Code:

(require 'nas-state)
(require 'nas-layout)
(require 'nas-map)

;;; Customization group

(defgroup nas nil
  "Nas modal editing."
  :group 'editing
  :prefix "nas-")

;;; Minor mode

;;;###autoload
(define-minor-mode nas-mode
  "Nas modal editing.
In visual state, keys act as navigation and editing commands.
In insert state, keys insert text as usual."
  :global t
  :group 'nas
  :lighter (:eval (nas--lighter))
  (if nas-mode
      (nas--enable)
    (nas--disable)))

;;; Enable / Disable

(defun nas--enable ()
  "Activate Nas: register `nas-mode-map-alist', set up hooks, enter visual state."
  (add-to-list 'emulation-mode-map-alists 'nas-mode-map-alist)
  (add-hook 'minibuffer-setup-hook #'nas-enter-insert)
  (add-hook 'minibuffer-exit-hook  #'nas-enter-visual)
  (nas-enter-visual))

(defun nas--disable ()
  "Deactivate Nas: unregister keymaps, remove hooks, restore cursor."
  (setq emulation-mode-map-alists
        (delq 'nas-mode-map-alist emulation-mode-map-alists))
  (remove-hook 'minibuffer-setup-hook #'nas-enter-insert)
  (remove-hook 'minibuffer-exit-hook  #'nas-enter-visual)
  (setq nas--insert-active     nil
        nas--visual-active     nil
        nas--sequential-active nil
        cursor-type            t))

(provide 'nas)
;;; nas.el ends here
