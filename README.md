# widget-extra

A collection of extra widgets that extend Emacs' built-in widget library, making it easier to build interactive buffer-based UIs.

## Installation

This package is not yet available on MELPA. Install it manually using your preferred package manager.

### Using elpaca with use-package

```elisp
(use-package widget-extra
  :ensure (widget-extra :host github :repo "d12frosted/widget-extra" :wait t)
  :demand t)
```

### Using straight.el

```elisp
(straight-use-package
 '(widget-extra :type git :host github :repo "d12frosted/widget-extra"))
```

### Manual installation

Clone the repository and add it to your load path:

```elisp
(add-to-list 'load-path "/path/to/widget-extra")
(require 'widget-extra)
```

## Requirements

- Emacs 29.1+
- [dash](https://github.com/magnars/dash.el) 2.19.1+
- [s](https://github.com/magnars/s.el) 1.13.0+

## Quick Start

```elisp
(require 'widget-extra)

(widget-buffer-setup "*my-app*"
  (widget-create 'title "My Application")
  (widget-create 'heading-1 "User Settings")
  (widget-create
   'fields-group
   (list 'field :tag "Name:" :value "Boris")
   (list 'int-field :tag "Age:" :value 30)
   (list 'field :tag "Email:" :value "boris@example.com")))
```

## Widgets

### Labels (read-only display)

#### `label`

A generic label widget with support for faces, tags, and truncation.

```elisp
;; Simple label
(widget-create 'label :value "Hello, World!")

;; Label with tag
(widget-create 'label :tag "Status:" :value "Active")

;; Label with truncation
(widget-create 'label :truncate 20 :value "A very long string that will be truncated")

;; Label with dynamic face based on value
(widget-create 'label
               :value 100
               :face (lambda (_widget value)
                       (if (> value 50) 'success 'warning)))
```

#### `numeric-label`

A label that automatically formats numeric values.

```elisp
(widget-create 'numeric-label :tag "Count:" :value 42)
```

#### `title`

A large title widget using `widget-title` face (1.6x height, extra-bold).

```elisp
(widget-create 'title "My Application")
```

#### `heading-1` and `heading-2`

Heading widgets for section titles.

```elisp
(widget-create 'heading-1 "Main Section")    ; 1.4x height, bold
(widget-create 'heading-2 "Subsection")      ; 1.2x height, semi-bold
```

### Fields (editable via click/RET)

#### `field`

A generic editable field. Click or press RET to edit the value.

```elisp
(widget-create 'field
               :tag "Name:"
               :value "Boris"
               :notify (lambda (widget &rest _)
                         (message "New value: %s" (widget-value widget))))
```

#### `int-field`

An integer-only field.

```elisp
(widget-create 'int-field :tag "Age:" :value 30)
```

#### `bounded-int-field`

An integer field with min/max bounds.

```elisp
(widget-create 'bounded-int-field
               :tag "Quantity:"
               :value 5
               :min-value 0
               :max-value 100)
```

#### `numeric-field`

A field accepting any number (integer or float).

```elisp
(widget-create 'numeric-field :tag "Price:" :value 19.99)
```

#### `bounded-numeric-field`

A numeric field with min/max bounds.

```elisp
(widget-create 'bounded-numeric-field
               :tag "Rating:"
               :value 3.5
               :min-value 0.0
               :max-value 5.0)
```

### Layout Widgets

#### `fields-group`

Groups multiple fields with automatic tag alignment.

```elisp
(widget-create
 'fields-group
 (list 'field :tag "Name:" :value "Boris")
 (list 'int-field :tag "Age:" :value 30)
 (list 'field :tag "Email:" :value "boris@example.com"))

;; Produces aligned output:
;; Name:  Boris
;; Age:   30
;; Email: boris@example.com
```

#### `horizontal-choice`

A horizontal tab/button group for selecting one option.

```elisp
(widget-create 'horizontal-choice
               :value "plan"
               :values '("plan" "scores" "settings")
               :notify (lambda (widget &rest _)
                         (message "Selected: %s" (widget-value widget))))
```

#### `table`

A table widget with rows, columns, and separators.

```elisp
(widget-create
 'table
 '(hline)
 '(row (label :value "Name") (label :value "Age") (label :value "VIP"))
 '(hline)
 '(row (label :value "Boris") (numeric-label :value 30) (label :value "yes"))
 '(row (label :value "Alice") (numeric-label :value 25) (label :value "no"))
 '(hline))

;; Produces:
;; |-------+-----+-----|
;; | Name  | Age | VIP |
;; |-------+-----+-----|
;; | Boris |  30 | yes |
;; | Alice |  25 | no  |
;; |-------+-----+-----|
```

Table with editable fields:

```elisp
(widget-create
 'table
 :truncate '((0 . 20))  ; truncate first column to 20 chars
 '(hline)
 '(row (label :value "Item") (label :value "Qty"))
 '(hline)
 '(row (field :value "Widget") (int-field :value 10))
 '(row (field :value "Gadget") (int-field :value 5))
 '(hline))
```

### Utilities

#### `widget-buffer-setup`

A macro that handles all widget buffer initialization.

```elisp
(widget-buffer-setup "*my-app*"
  ;; Create your widgets here
  (widget-create 'title "My Application")
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (message "Clicked!"))
                 "Click Me"))
```

## Creating Custom Widgets

You can create custom widgets by inheriting from existing ones:

```elisp
;; A label for monetary values
(define-widget 'money-label 'label
  "A label for displaying monetary values."
  :format-value (lambda (_widget value)
                  (format "$%.2f" value)))

;; Usage
(widget-create 'money-label :tag "Total:" :value 99.99)
```

## Development

```sh
# Install dependencies
make prepare

# Compile
make compile

# Lint
make lint

# Test
make test
```

## License

GPL-3.0-or-later. See [LICENSE](https://www.gnu.org/licenses/gpl-3.0.html) for details.
