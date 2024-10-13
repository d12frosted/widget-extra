;;; widget-extra.el --- Extra widgets that extend built-in library  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/widget-extra
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: extensions

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Extra widgets that extend built-in library.
;;

;;; Code:

(require 'widget)
(require 'wid-edit)

;;; * Title

(defface widget-title '((t
                         :height 1.6
                         :weight extra-bold))
  "Face used for title."
  :group 'widgets
  :group 'faces)

(define-widget 'title 'item
  "A title widget."
  :format "%v\n"
  :value-create (lambda (widget)
                  (insert (propertize (widget-get widget :value) 'face 'widget-title))))

;;; * Headings

(defface widget-heading-1 '((t
                             :height 1.4
                             :weight bold))
  "Face used for level 1 heading."
  :group 'widgets
  :group 'faces)

(define-widget 'heading-1 'item
  "A level 1 heading widget."
  :format "%v\n"
  :value-create (lambda (widget)
                  (insert (propertize (widget-get widget :value) 'face 'widget-heading-1))))

(defface widget-heading-2 '((t
                             :height 1.2
                             :weight semi-bold))
  "Face used for level 2 heading."
  :group 'widgets
  :group 'faces)

(define-widget 'heading-2 'item
  "A level 2 heading widget."
  :format "%v\n"
  :value-create (lambda (widget)
                  (insert (propertize (widget-get widget :value) 'face 'widget-heading-2))))

;;; * Generic field

(define-widget 'field 'default
  "A generic field widget."
  :prompt "Value: "
  :unbound nil
  :format "%[%v%]"
  :match (lambda (_widget _value) t)
  :match-error-message (lambda (widget _value)
                         (format "Value does not match %S type" (car widget)))
  :void "__"
  :child-format "%t"
  :action
  (lambda (widget &optional event)
    ;; we are not using `widget-prompt-value' because it modifies prompt
    (let* ((prompt (widget-get widget :prompt))
           (value (widget-get widget :value))
           (unbound (widget-get widget :unbound))
           (value (widget-apply widget :prompt-value prompt value unbound)))
      (unless (widget-apply widget :match value)
        (error (widget-apply widget :match-error-message value)))
      (widget-value-set widget value)
      (widget-setup)
      (widget-apply widget :notify widget event)
      (run-hook-with-args 'widget-edit-functions widget)))
  :mouse-down-action 'widget-choice-mouse-down-action

  :format-value (lambda (_widget value) value)
  :value-create
  (lambda (widget &rest _)
    (let* ((value (widget-get widget :value))
           (tag (if value
                    (widget-apply widget :format-value value)
                  (widget-get widget :void)))
           (current `(item :tag ,tag :value ,value :format ,(widget-get widget :child-format))))
      (widget-put widget :children (list (widget-create-child-value widget current value)))))
  :value-get (lambda (widget &rest _) (widget-get widget :value))
  :prompt-value (lambda (_widget prompt _value _unbound) (read-string prompt)))

;;; * Integer field

(define-widget 'int-field 'field
  "An integer field."
  :format-value (lambda (_widget value) (number-to-string value))
  :match (lambda (_widget value) (integerp value))
  :match-error-message (lambda (_widget _value)
                         "Value must be an integer")
  :prompt-value (lambda (_widget prompt _value _unbound) (read-number prompt)))

(define-widget 'bounded-int-field 'int-field
  "A bounded integer field."
  :min-value most-negative-fixnum
  :max-value most-positive-fixnum
  :match (lambda (widget value)
           (and (integerp value)
                (>= value (widget-get widget :min-value))
                (<= value (widget-get widget :max-value))))
  :match-error-message (lambda (widget _value)
                         (format "Value must be an integer in [%d, %d] range"
                                 (widget-get widget :min-value)
                                 (widget-get widget :max-value))))

;;; * Numeric field

(define-widget 'numeric-field 'field
  "A numeric field."
  :format-value (lambda (_widget value) (number-to-string value))
  :match (lambda (_widget value) (numberp value))
  :match-error-message (lambda (_widget _value)
                         "Value must be a number")
  :prompt-value (lambda (_widget prompt _value _unbound) (read-number prompt)))

(define-widget 'bounded-numeric-field 'numeric-field
  "A bounded numeric field."
  :min-value most-negative-fixnum
  :max-value most-positive-fixnum
  :match (lambda (widget value)
           (and (>= value (widget-get widget :min-value))
                (<= value (widget-get widget :max-value))))
  :match-error-message (lambda (widget _value)
                         (format "Value must be a number in [%s, %s] range"
                                 (number-to-string (widget-get widget :min-value))
                                 (number-to-string (widget-get widget :max-value)))))

;;; * Horizontal choice

(define-widget 'horizontal-choice 'default
  "Select one of multiple options in a horizontal layout.

This widget creates a buttons for each value in :values and uses
horizontal layout to place them.

The current value (as well as initial) is stored in :value property of
the widget."
  :convert-widget 'widget-types-convert-widget
  :copy 'widget-types-copy
  :offset 4
  :format "%v\n"
  :entry-format "%v"
  :value-create 'widget-horizontal-choice-value-create
  :value-get 'widget-horizontal-choice-value-get
  :value-set 'widget-horizontal-choice-value-set
  :error "You must push one of the buttons")

(defun widget-horizontal-choice-value-create (widget)
  "Expand %v by inserting all values for horizontal choice WIDGET."
  (seq-map-indexed
   (lambda (item index)
     (widget-horizontal-choice-add-item widget item index))
   (widget-get widget :values)))

(defun widget-horizontal-choice-add-item (widget item index)
  "Add an ITEM with INDEX to horizontal choice WIDGET."
  (and (widget--should-indent-p)
       (widget-get widget :indent)
       (insert-char ?\s (widget-get widget :indent)))
  (widget-specify-insert
   (let* ((buttons (widget-get widget :buttons))
	  (from (point))
          (choice (widget-get widget :choice))
	  (chosen (if choice
                      (= choice index)
                    (equal item (widget-get widget :value))))
	  button)
     (insert (widget-get widget :entry-format))
     (goto-char from)
     ;; Parse % escapes in format.
     (while (re-search-forward "%\\([v%]\\)" nil t)
       (let ((escape (char-after (match-beginning 1))))
	 (delete-char -2)
	 (cond ((eq escape ?%)
		(insert ?%))
	       ((eq escape ?v)
                (setq button (widget-create-child
                              widget
                              (list
                               'push-button
                               :format (if (> index 0) " %[%v%]" "%[%v%]")
                               :notify (lambda (&rest _)
                                         (widget-horizontal-choice-value-set widget item))
                               :index index
                               :value item)))
		(when chosen
		  (widget-apply button :deactivate)))
	       (t
		(error "Unknown escape `%c'" escape)))))
     (when chosen
       (widget-put widget :choice index)
       (widget-put widget :value item))
     (when button
       (widget-put widget :buttons (nconc buttons (list button))))
     button)))

(defun widget-horizontal-choice-value-get (widget)
  "Get selected value of a horizontal choice WIDGET."
  (if-let ((index (widget-get widget :choice)))
      (nth index (widget-get widget :values))))

(defun widget-horizontal-choice-value-set (widget value)
  "Set selected VALUE in horizontal choice WIDGET."
  (widget-put widget :value nil)
  (widget-put widget :choice nil)
  (dolist (button (widget-get widget :buttons))
    (if (string= (widget-get button :value) value)
        (progn
          (widget-put widget :choice (widget-get button :index))
          (widget-put widget :value (widget-get button :value))
          (widget-apply button :deactivate))
        (widget-apply button :activate)))
  (widget-apply widget :notify))

;;; * Buffer setup

(defmacro widget-buffer-setup (buffer-or-name &rest body)
  "Setup a clean buffer for widgets usage.

Switches to BUFFER-OR-NAME and evaluate BODY.

This macro takes care of all the initialisation, cleanup and setup."
  (declare (debug (form body)) (indent 1))
  `(progn
     (switch-to-buffer ,buffer-or-name)
     (kill-all-local-variables)
     (let ((inhibit-read-only t))
      (erase-buffer))
     (remove-overlays)

     ,@body

     (use-local-map widget-keymap)
     (widget-setup)
     (goto-char (point-min))))

(provide 'widget-extra)
;;; widget-extra.el ends here
