;;; widget-extra.el --- Extra widgets that extend built-in library  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/widget-extra
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (s "1.13.0"))
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
(require 'dash)
(require 's)

;;; * Generic label

(define-widget 'label 'item
  "A generic label."
  :face 'default
  :tag-face 'default
  :offset 1
  :padding ?\s
  ;; note - only value is truncated as tags are generally static hence there is no need to truncate them
  :truncate nil
  :format "%T%v"
  :format-handler
  (lambda (widget escape)
    ;; we support custom tag prefix (optional + offsets)
    (cond ((eq escape ?T)
           (when-let ((tag (widget-get widget :tag)))
             (let ((offset (widget-get widget :offset)))
               (insert (propertize tag 'face (widget-get widget :tag-face))
                       (make-string offset (widget-get widget :padding))))))))
  :format-value (lambda (_widget value) value)
  :value-create
  (lambda (widget)
    (let* ((s (widget-apply widget :format-value (widget-get widget :value)))
           (truncate (widget-get widget :truncate)))
      (insert
       (propertize
        (if truncate (s-truncate truncate s) s)
        'face (widget-get widget :face))))))

;;; * Numeric labels

(define-widget 'numeric-label 'label
  "A numeric label."
  :format-value (lambda (_widget value) (number-to-string value)))

;;; * Title

(defface widget-title '((t
                         :height 1.6
                         :weight extra-bold))
  "Face used for title."
  :group 'widgets
  :group 'faces)

(define-widget 'title 'label
  "A title widget."
  :format "%v\n"
  :face 'widget-title)

;;; * Headings

(defface widget-heading-1 '((t
                             :height 1.4
                             :weight bold))
  "Face used for level 1 heading."
  :group 'widgets
  :group 'faces)

(define-widget 'heading-1 'label
  "A level 1 heading widget."
  :format "%v\n"
  :face 'widget-heading-1)

(defface widget-heading-2 '((t
                             :height 1.2
                             :weight semi-bold))
  "Face used for level 2 heading."
  :group 'widgets
  :group 'faces)

(define-widget 'heading-2 'label
  "A level 2 heading widget."
  :format "%v\n"
  :face 'widget-heading-2)

;;; * Generic field

(define-widget 'field 'default
  "A generic field widget."
  :prompt "Value: "
  :tag-face 'default
  :unbound nil
  :offset 1
  :padding ?\s
  ;; note - only value is truncated as tags are generally static hence there is no need to truncate them
  :truncate nil
  :format "%T%[%v%]"
  :format-handler
  (lambda (widget escape)
    ;; we support custom tag prefix (optional + offsets)
    (cond ((eq escape ?T)
           (when-let ((tag (widget-get widget :tag))
                      (offset (widget-get widget :offset)))
             (insert (propertize tag 'face (widget-get widget :tag-face))
                     (make-string offset (widget-get widget :padding)))))))
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
      (widget-default-action widget event)
      (run-hook-with-args 'widget-edit-functions widget)))
  :mouse-down-action #'widget-choice-mouse-down-action
  :format-value (lambda (_widget value) value)
  :value-create
  (lambda (widget &rest _)
    (let* ((value (widget-get widget :value))
           (truncate (widget-get widget :truncate))
           (tag (if value
                    (widget-apply widget :format-value value)
                  (widget-get widget :void)))
           (tag (if truncate (s-truncate truncate tag) tag))
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

;;; * Fields group

(define-widget 'fields-group 'default
  "Group multiple fields into one widget.

It automatically aligns values of the child fields."
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :extra-offset 1
  :value-create #'widget-fields-group-value-create)

(defun widget-fields-group-value-create (widget)
  "Expand %v by inserting all children of the WIDGET."
  (let* ((args (widget-get widget :args))
         (max-tag-length (seq-max
                          (seq-map
                           (lambda (x) (length (or (widget-get x :tag) "")))
                           args))))
    (dolist (arg args)
      (widget-fields-group-add-item widget arg max-tag-length))))

(defun widget-fields-group-add-item (widget item max-tag-length)
  "Add ITEM to WIDGET.

Use MAX-TAG-LENGTH to calculate offset."
  (let* ((tag (widget-get item :tag))
         (tag-length (if tag (length tag) 0))
         (offset (+ (widget-get widget :extra-offset)
                    (- max-tag-length tag-length)))
         (format (or (widget-get item :format)
                     "%T%[%v%]"))
         (format (if (s-ends-with-p "\n" format) format (concat format "\n"))))
    (widget-put item :format format)
    (widget-put item :offset offset)
    (widget-create-child widget item)))

;;; * Horizontal choice

(define-widget 'horizontal-choice 'default
  "Select one of multiple options in a horizontal layout.

This widget creates a buttons for each value in :values and uses
horizontal layout to place them.

The current value (as well as initial) is stored in :value property of
the widget."
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :gap 1
  :format "%v\n"
  :entry-format "%v"
  :value-create #'widget-horizontal-choice-value-create
  :value-get #'widget-horizontal-choice-value-get
  :value-set #'widget-horizontal-choice-value-set
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
          (gap (make-string (widget-get widget :gap) ?\s))
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
                               :format (if (> index 0) (concat gap "%[%v%]") "%[%v%]")
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

;;; * Table widget

(define-widget 'table 'default
  "A table widget.

Arguments are of types:

- hline - an empty line/separator controlled by :hline-* properties; by
  itself has not properties.

- row - actual content of the table; controlled by :row-* properties.
  Arguments are other widgets (multi-line widgets are not really
  supported). The only extra property it supports is :padding-type to
  override table :padding-type."
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :row-start "| "
  :row-conj " | "
  :row-end " |"
  :padding ?\s
  ;; either a symbol (right or left) or alist of column index to padding type
  :padding-type 'right
  ;; either nil (unbounded or alist of column index to max width); works only with widgets that support :truncate
  ;; property
  :truncate nil
  :hline-start "|-"
  :hline-content ?-
  :hline-conj "-+-"
  :hline-end "-|"
  :value-create #'widget-table-value-create
  :notify (lambda (widget child &optional _event)
            (let* ((child-new (widget-copy child))
                   (row-index (widget-get child-new :row-index))
                   (col-index (widget-get child-new :col-index))
                   (child-from (marker-position (widget-get child :from)))
                   (delta (when child-from (- (point) child-from))))
              (widget-put
               widget :args
               (--update-at
                row-index
                (progn
                  (widget-put
                   it :args
                   (-replace-at col-index child-new (widget-get it :args)))
                  it)
                (widget-get widget :args)))
              (widget-default-value-set widget (widget-get widget :value))
              ;; properly move point after full recreation of table widget
              (when-let ((child (--find (and (= row-index (widget-get it :row-index))
                                             (= col-index (widget-get it :col-index)))
                                        (widget-get widget :children))))
                (when delta
                  (goto-char (+ (widget-get child :from) delta)))))))

(defun widget-table-value-create (widget)
  "Expand %v by inserting all children of the WIDGET."
  (let* ((args (widget-get widget :args))
         (truncate (widget-get widget :truncate))
         (cols (->> args
                    (--map (widget-get it :args))
                    (-map #'length)
                    (-max)))
         (widths (->> (-iota cols)
                      ;; transpose operation
                      (-map
                       (lambda (i)
                         (-map (-partial #'nth i) (--map (widget-get it :args) args))))
                      ;; calculate length of each column
                      (--map-indexed
                       (let ((max-width (alist-get it-index truncate)))
                         (--map
                          (if-let ((length (when it
                                             (with-temp-buffer
                                               (widget-create it)
                                               (- (point) 1)))))
                              (if max-width (min max-width length) length)
                            0)
                          it))
                       )))
         (max-widths (-map #'-max widths))
         (children))
    (-each-indexed args
      (lambda (row-index row)
        (pcase (car row)
          ;; regular row
          (`row
           (widget-insert (widget-get widget :row-start))
           (-each-indexed (widget-get row :args)
             (lambda (col-index col)
               (unless (= 0 col-index)
                 (widget-insert (widget-get widget :row-conj)))
               (widget-put col :row-index row-index)
               (widget-put col :col-index col-index)
               (let* ((w (nth row-index (nth col-index widths)))
                      (mw (nth col-index max-widths))
                      (pad-type (or (widget-get row :padding-type)
                                    (widget-get widget :padding-type)))
                      (pad-type (when (and pad-type (listp pad-type))
                                  (alist-get col-index pad-type)))
                      (pad-type (or pad-type 'right))
                      (pad (- mw w))
                      (truncate (widget-get widget :truncate))
                      (col (widget-copy col)))
                 (when (and (> pad 0) (eq pad-type 'left))
                   (widget-insert (make-string pad (widget-get widget :padding))))
                 (widget-put col :truncate (alist-get col-index truncate))
                 (setq children (cons (widget-create-child widget col) children))
                 (when (and (> pad 0) (eq pad-type 'right))
                   (widget-insert (make-string pad (widget-get widget :padding)))))))
           (let ((cols-in-row (length (widget-get row :args))))
             (--each (-iota (- cols cols-in-row))
               (widget-insert
                (widget-get widget :row-conj)
                (make-string (nth (+ it cols-in-row) max-widths) ?\s))))
           (widget-insert (widget-get widget :row-end) "\n"))

          ;; horizontal line
          (`hline
           (widget-insert (widget-get widget :hline-start))
           (--each (-iota cols)
             (unless (= it 0)
               (widget-insert (widget-get widget :hline-conj)))
             (widget-insert
              (make-string
               (nth it max-widths)
               (widget-get widget :hline-content))))
           (widget-insert (widget-get widget :hline-end) "\n"))

          ;; unsupported types
          (type (user-error "Unsupported type %S" type)))
        (widget-put widget :children (reverse children))))))

(defun widget-table-insert-decoration ())

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
