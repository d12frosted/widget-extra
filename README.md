A few extensions to the Emacs widgets library.

## Widgets

- `label` - a simple label widget (item widget with extra functionality).
- `numeric-label` - a simple numeric label widget.
- `title` - a title; uses `widget-title` face.
- `heading-1` - a level 1 heading; uses `widget-heading-1` face.
- `heading-2` - a level 2 heading; uses `widget-heading-2` face.
- `field` - a generic field (string by default) that can be edited by clicking on it. See other fields to learn what can be done with this generic field.
- `int-field` - an integer field.
- `bounded-int-field` - a bounded integer field. Min/max values are controlled by `:min-value` and `:max-value` properties.
- `numeric-field` - a generic numeric field.
- `bounded-numeric-field` - a generic bounded numeric field. Min/max values are controlled by `:min-value` and `:max-value` properties.
- `fields-group` - a widget to group and automatically align/offset fields.
- `horizontal-choice` - a simple horizontal choice / tabs-like widget.
- `table` - a simple table widget that supports custom padding, truncation (works only with `label`- and `field`-derived widgets) and automatic resize on content change.

See definition of each widget to learn more about them.

## Functions

- `widget-buffer-setup` - a macro that setups a buffer for widgets usage. Basically it switches to provided `buffer-or-name`, cleans it up, evaluates your `body`, setups widgets and jumps to the min point. It simply helps to avoid boilerplate (this is the code I write every time).

## Running locally

```sh
# install deps
make prepare

# compile
make compile

# lint
make lint

# test
make test
```
