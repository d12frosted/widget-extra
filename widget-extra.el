;;; widget-extra.el --- Extra widgets that extend built-in library  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Boris Buliga

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
;; This package provides additional widgets that extend Emacs' built-in
;; widget library (wid-edit.el). These widgets are designed to make it
;; easier to build interactive buffer-based UIs in Emacs.
;;
;; Widgets provided:
;;
;; Labels (read-only display):
;;   - `label'         - generic label with face, tag, truncation support
;;   - `numeric-label' - label that formats numeric values
;;   - `title'         - large title text (uses `widget-title' face)
;;   - `heading-1'     - level 1 heading (uses `widget-heading-1' face)
;;   - `heading-2'     - level 2 heading (uses `widget-heading-2' face)
;;
;; Fields (editable via click/RET):
;;   - `field'                 - generic string field
;;   - `int-field'             - integer field
;;   - `bounded-int-field'     - integer with min/max bounds
;;   - `numeric-field'         - any number field
;;   - `bounded-numeric-field' - number with min/max bounds
;;
;; Buttons:
;;   - `toggle-button' - toggles between on/off states
;;   - `action-button' - triggers action without holding state
;;   - `link-button'   - styled as link, opens URL/note/function
;;
;; Layout widgets:
;;   - `fields-group'      - groups fields with automatic tag alignment
;;   - `horizontal-choice' - horizontal tabs/button group
;;   - `table'             - table with rows, columns, and separators
;;
;; Utilities:
;;   - `widget-buffer-setup' - macro for initializing widget buffers
;;
;; Basic usage:
;;
;;   (widget-buffer-setup "*my-ui*"
;;     (widget-create 'title "My Application")
;;     (widget-create 'heading-1 "Settings")
;;     (widget-create 'field :tag "Name:" :value "default"))
;;

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'dash)
(require 's)

;;; * Generic label

;;;###autoload
(define-widget 'label 'item
  "A generic label widget for displaying read-only text.

Properties:
  :face       - face for the value (symbol or function taking widget and value)
  :tag-face   - face for the tag
  :tag        - optional prefix label
  :offset     - spacing between tag and value (default 1)
  :padding    - padding character (default space)
  :truncate   - max length for value (nil for no truncation)
  :format     - format string (default \"%T%v\")

The %T escape in format inserts the tag with offset.

Example:
  (widget-create \\='label :tag \"Name:\" :value \"Boris\")
  (widget-create \\='label :truncate 10 :value \"A very long string\")"
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
           (truncate (widget-get widget :truncate))
           (face (widget-get widget :face)))
      ;; Only call face as function if it's not a known face symbol
      ;; (some face names like 'error are also function names)
      (when (and (functionp face) (not (facep face)))
        (setq face (widget-apply widget :face (widget-get widget :value))))
      (insert (propertize (if truncate (s-truncate truncate s) s) 'face face)))))

;;; * Numeric labels

;;;###autoload
(define-widget 'numeric-label 'label
  "A label widget that displays numeric values.

Inherits all properties from `label'.
Automatically converts numeric values to strings for display.

Example:
  (widget-create \\='numeric-label :tag \"Count:\" :value 42)"
  :format-value (lambda (_widget value) (number-to-string value)))

;;; * Title

(defface widget-title '((t
                         :height 1.6
                         :weight extra-bold))
  "Face used for title."
  :group 'widgets
  :group 'faces)

;;;###autoload
(define-widget 'title 'label
  "A large title widget using `widget-title' face.

Displays text with extra-bold weight and 1.6x height.
Automatically adds a newline after the value.

Example:
  (widget-create \\='title \"My Application\")"
  :format "%v\n"
  :face 'widget-title)

;;; * Headings

(defface widget-heading-1 '((t
                             :height 1.4
                             :weight bold))
  "Face used for level 1 heading."
  :group 'widgets
  :group 'faces)

;;;###autoload
(define-widget 'heading-1 'label
  "A level 1 heading widget using `widget-heading-1' face.

Displays text with bold weight and 1.4x height.
Automatically adds a newline after the value.

Example:
  (widget-create \\='heading-1 \"Section Title\")"
  :format "%v\n"
  :face 'widget-heading-1)

(defface widget-heading-2 '((t
                             :height 1.2
                             :weight semi-bold))
  "Face used for level 2 heading."
  :group 'widgets
  :group 'faces)

;;;###autoload
(define-widget 'heading-2 'label
  "A level 2 heading widget using `widget-heading-2' face.

Displays text with semi-bold weight and 1.2x height.
Automatically adds a newline after the value.

Example:
  (widget-create \\='heading-2 \"Subsection Title\")"
  :format "%v\n"
  :face 'widget-heading-2)

;;; * Generic field

;;;###autoload
(define-widget 'field 'default
  "A generic editable field widget.

Click or press RET on the field to edit its value via the minibuffer.

Properties:
  :value      - current field value
  :prompt     - minibuffer prompt (default \"Value: \")
  :tag        - optional prefix label
  :tag-face   - face for the tag
  :offset     - spacing between tag and value (default 1)
  :padding    - padding character (default space)
  :truncate   - max display length (nil for no truncation)
  :void       - placeholder when value is nil (default \"__\")
  :notify     - function called when value changes

Example:
  (widget-create \\='field :tag \"Name:\" :value \"Boris\"
                 :notify (lambda (widget &rest _)
                           (message \"New value: %s\" (widget-value widget))))"
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

;;;###autoload
(define-widget 'int-field 'field
  "An editable integer field.

Inherits all properties from `field'.
Only accepts integer values; signals error otherwise.

Example:
  (widget-create \\='int-field :tag \"Age:\" :value 30)"
  :format-value (lambda (_widget value) (number-to-string value))
  :match (lambda (_widget value) (integerp value))
  :match-error-message (lambda (_widget _value)
                         "Value must be an integer")
  :prompt-value (lambda (_widget prompt _value _unbound) (read-number prompt)))

;;;###autoload
(define-widget 'bounded-int-field 'int-field
  "An editable integer field with min/max bounds.

Inherits all properties from `int-field'.

Additional properties:
  :min-value - minimum allowed value (default `most-negative-fixnum')
  :max-value - maximum allowed value (default `most-positive-fixnum')

Example:
  (widget-create \\='bounded-int-field
                 :tag \"Quantity:\"
                 :value 5
                 :min-value 0
                 :max-value 100)"
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

;;;###autoload
(define-widget 'numeric-field 'field
  "An editable numeric field (integers or floats).

Inherits all properties from `field'.
Accepts any numeric value (integers or floating-point).

Example:
  (widget-create \\='numeric-field :tag \"Price:\" :value 19.99)"
  :format-value (lambda (_widget value) (number-to-string value))
  :match (lambda (_widget value) (numberp value))
  :match-error-message (lambda (_widget _value)
                         "Value must be a number")
  :prompt-value (lambda (_widget prompt _value _unbound) (read-number prompt)))

;;;###autoload
(define-widget 'bounded-numeric-field 'numeric-field
  "An editable numeric field with min/max bounds.

Inherits all properties from `numeric-field'.

Additional properties:
  :min-value - minimum allowed value (default `most-negative-fixnum')
  :max-value - maximum allowed value (default `most-positive-fixnum')

Example:
  (widget-create \\='bounded-numeric-field
                 :tag \"Rating:\"
                 :value 3.5
                 :min-value 0.0
                 :max-value 5.0)"
  :min-value most-negative-fixnum
  :max-value most-positive-fixnum
  :match (lambda (widget value)
           (and (numberp value)
                (>= value (widget-get widget :min-value))
                (<= value (widget-get widget :max-value))))
  :match-error-message (lambda (widget _value)
                         (format "Value must be a number in [%s, %s] range"
                                 (number-to-string (widget-get widget :min-value))
                                 (number-to-string (widget-get widget :max-value)))))

;;; * Toggle button

;;;###autoload
(define-widget 'toggle-button 'item
  "A button that toggles between two states.

Displays :on label when value is non-nil, :off otherwise.
Clicking toggles the value and triggers :notify.

Properties:
  :on       - label when value is non-nil (default \"[X]\")
  :off      - label when value is nil (default \"[ ]\")
  :on-face  - face for on state (default nil)
  :off-face - face for off state (default nil)
  :value    - current boolean state
  :notify   - function called when toggled

Example:
  (widget-create \\='toggle-button
                 :on \"[X]\" :off \"[ ]\"
                 :value t
                 :notify (lambda (w &rest _)
                           (message \"Now: %s\" (widget-value w))))"
  :format "%[%v%]"
  :on "[X]"
  :off "[ ]"
  :on-face nil
  :off-face nil
  :value nil
  :value-create
  (lambda (widget)
    (let* ((value (widget-value widget))
           (label (if value
                      (widget-get widget :on)
                    (widget-get widget :off)))
           (face (if value
                     (widget-get widget :on-face)
                   (widget-get widget :off-face))))
      (insert (if face (propertize label 'face face) label))))
  :action
  (lambda (widget &optional event)
    (let ((new-value (not (widget-value widget))))
      (widget-value-set widget new-value)
      (widget-setup)
      (widget-apply widget :notify widget event))))

;;; * Action button

;;;###autoload
(define-widget 'action-button 'push-button
  "A button that triggers an action without holding state.

Use for actions like delete, add, refresh where no value is stored.
The :action-data property can pass context to :notify.

Properties:
  :action-data - arbitrary data accessible in :notify via widget-get
  :notify      - function called when clicked, receives (widget event)
  :truncate    - max display length (nil for no truncation)

Example:
  (widget-create \\='action-button
                 :value \"Delete\"
                 :action-data \\='(item-id . 42)
                 :notify (lambda (w &rest _)
                           (let ((data (widget-get w :action-data)))
                             (message \"Delete: %S\" data))))"
  :format "%[%v%]"
  :action-data nil
  :truncate nil
  :value-create
  (lambda (widget)
    (let* ((value (widget-get widget :value))
           (truncate (widget-get widget :truncate))
           (tag (if truncate (s-truncate truncate value) value)))
      (insert tag)))
  :action
  (lambda (widget &optional event)
    (widget-apply widget :notify widget event)))

;;; * Link button

;;;###autoload
(define-widget 'link-button 'push-button
  "A button styled as a link that opens a target.

Target can be:
  - URL string (http/https): opens with `browse-url'
  - vulpea-note: opens with `vulpea-visit' (if available)
  - function: calls it with no arguments

Properties:
  :target   - what to open when clicked
  :value    - display text for the link
  :truncate - max display length (nil for no truncation)

Example:
  (widget-create \\='link-button
                 :value \"GitHub\"
                 :target \"https://github.com\")

  (widget-create \\='link-button
                 :value \"Open note\"
                 :target some-vulpea-note)"
  :format "%[%v%]"
  :button-face 'link
  :target nil
  :truncate nil
  :value-create
  (lambda (widget)
    (let* ((value (widget-get widget :value))
           (truncate (widget-get widget :truncate))
           (tag (if truncate (s-truncate truncate value) value)))
      (insert tag)))
  :action
  (lambda (widget &optional _event)
    (let ((target (widget-get widget :target)))
      (cond
       ((and (fboundp 'vulpea-note-p)
             (funcall 'vulpea-note-p target))
        (funcall 'vulpea-visit target))
       ((and (stringp target)
             (string-match-p "^https?://" target))
        (browse-url target))
       ((functionp target)
        (funcall target))))))

;;; * Fields group

;;;###autoload
(define-widget 'fields-group 'default
  "Group multiple fields with automatic tag alignment.

Child widgets are arranged vertically with their tags aligned.
This creates a clean form-like appearance.

Properties:
  :extra-offset - additional spacing after aligned tags (default 1)

Example:
  (widget-create
   \\='fields-group
   (list \\='field :tag \"Name:\" :value \"Boris\")
   (list \\='int-field :tag \"Age:\" :value 30)
   (list \\='field :tag \"Email:\" :value \"boris@example.com\"))

Result:
  Name:  Boris
  Age:   30
  Email: boris@example.com"
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

;;;###autoload
(define-widget 'horizontal-choice 'default
  "A horizontal tab/button group for selecting one option.

Displays clickable buttons arranged horizontally. The selected
button is deactivated (not clickable) while others remain active.

Properties:
  :values - list of available choices
  :value  - currently selected value
  :gap    - spacing between buttons (default 1)
  :notify - function called when selection changes

Example:
  (widget-create \\='horizontal-choice
                 :value \"plan\"
                 :values \\='(\"plan\" \"scores\" \"settings\")
                 :notify (lambda (widget &rest _)
                           (message \"Selected: %s\" (widget-value widget))))"
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
    (if (equal (widget-get button :value) value)
        (progn
          (widget-put widget :choice (widget-get button :index))
          (widget-put widget :value (widget-get button :value))
          (widget-apply button :deactivate))
        (widget-apply button :activate)))
  (widget-apply widget :notify))

;;; * Table widget

(defun widget-table--update-spec-value (spec new-value)
  "Return a copy of widget SPEC with :value set to NEW-VALUE.
For menu-choice widgets, also updates :tag to match the new value
so the display reflects the selection."
  ;; Use copy-tree for deep copy to avoid corrupting original spec
  (let ((copy (copy-tree spec)))
    ;; (cdr copy) is the plist of properties after the widget type
    (if (plist-member (cdr copy) :value)
        (plist-put (cdr copy) :value new-value)
      ;; Insert :value after the widget type
      (setcdr copy (cons :value (cons new-value (cdr copy)))))
    ;; For menu-choice, update :tag to match value for proper display
    (when (eq (car copy) 'menu-choice)
      (plist-put (cdr copy) :tag new-value))
    copy))

;;;###autoload
(define-widget 'table 'default
  "A table widget with rows, columns, and separators.

Rows can contain any widget (preferably single-line).
Columns are automatically aligned based on content width.

Row types:
  (hline)     - horizontal separator line
  (row ...)   - data row containing child widgets

Properties:
  :row-start      - string before first column (default \"\")
  :row-conj       - string between columns (default \" | \")
  :row-end        - string after last column (default \"\")
  :hline-start    - separator line start (default \"\")
  :hline-conj     - separator between columns (default \"-+-\")
  :hline-end      - separator line end (default \"\")
  :hline-content  - character for separator (default ?-)
  :padding        - padding character (default space)
  :padding-type   - \\='right or \\='left, or alist of column->type
  :truncate       - alist of column index to max width

Example:
  (widget-create
   \\='table
   \\='(row (label :value \"Name\") (label :value \"Age\"))
   \\='(hline)
   \\='(row (label :value \"Boris\") (numeric-label :value 30))
   \\='(row (label :value \"Alice\") (numeric-label :value 25)))

Result:
  Name  | Age
  ------+----
  Boris |  30
  Alice |  25"
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :row-start ""
  :row-conj " | "
  :row-end ""
  :padding ?\s
  ;; either a symbol (right or left) or alist of column index to padding type
  :padding-type 'right
  ;; either nil (unbounded or alist of column index to max width); works only with widgets that support :truncate
  ;; property
  :truncate nil
  :hline-start ""
  :hline-content ?-
  :hline-conj "-+-"
  :hline-end ""
  :value-create #'widget-table-value-create
  :notify (lambda (widget child &optional _event)
            (let* ((row-index (widget-get child :row-index))
                   (col-index (widget-get child :col-index))
                   (child-from (marker-position (widget-get child :from)))
                   (delta (when child-from (- (point) child-from)))
                   (new-value (widget-value child))
                   ;; Get the original spec from args and update its value
                   (row (nth row-index (widget-get widget :args)))
                   (original-spec (nth col-index (widget-get row :args)))
                   (updated-spec (widget-table--update-spec-value original-spec new-value)))
              (widget-put
               widget :args
               (--update-at
                row-index
                (progn
                  (widget-put
                   it :args
                   (-replace-at col-index updated-spec (widget-get it :args)))
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
                          it)))))
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
               (let* ((w (nth row-index (nth col-index widths)))
                      (mw (nth col-index max-widths))
                      (pad-type (or (widget-get row :padding-type)
                                    (widget-get widget :padding-type)))
                      (pad-type (when (and pad-type (listp pad-type))
                                  (alist-get col-index pad-type)))
                      (pad-type (or pad-type 'right))
                      (pad (- mw w))
                      (truncate (widget-get widget :truncate))
                      ;; Convert widget spec properly to handle compound widgets
                      ;; like menu-choice that have nested :args needing conversion
                      (col (apply #'widget-convert (car col) (cdr col))))
                 (widget-put col :row-index row-index)
                 (widget-put col :col-index col-index)
                 (widget-put col :parent widget)
                 (widget-put col :truncate (alist-get col-index truncate))
                 (when (and (> pad 0) (eq pad-type 'left))
                   (widget-insert (make-string pad (widget-get widget :padding))))
                 (widget-apply col :create)
                 (setq children (cons col children))
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

;;; * Buffer setup

;;;###autoload
(defmacro widget-buffer-setup (buffer-or-name &rest body)
  "Setup a clean buffer for widgets usage.

This macro handles all widget buffer initialization:
1. Switches to BUFFER-OR-NAME (creating if needed)
2. Clears all content and overlays
3. Evaluates BODY (where you create widgets)
4. Sets up widget keymap and finalizes widgets
5. Moves point to beginning of buffer

Example:
  (widget-buffer-setup \"*my-app*\"
    (widget-create \\='title \"My Application\")
    (widget-create \\='heading-1 \"Settings\")
    (widget-create
     \\='fields-group
     (list \\='field :tag \"Name:\" :value \"default\")
     (list \\='int-field :tag \"Count:\" :value 0)))"
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
