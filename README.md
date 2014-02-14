# multicolumn - Support for multiple side-by-side windows

*Author:* Anders Lindgren<br>
*Version:* 0.0.0<br>

Commands helpful when using multiple side-by-side windows.

## Overview

Using multiple side-by-side windows is a great way to utilize the
large high-resolution displays that exists today. This package
provides the "missing features" of Emacs to create a side-by-side
layout, to navigate efficiently, and to manage the windows.

This package is especially useful in conjunction with Follow mode, a
package provided with Emacs that create the illusion that several
windows showing the same buffer form a very tall virtual window.

## Example

In the screenshots below, a text is being edited which is narrower
but taller than the Emacs frame.

![Image of Emacs with one window](doc/demo1.png)

With `multicolumn-delete-other-windows-and-split-with-follow-mode`
four equally spaced side-by-side windows are created and Follow mode
is enabled, which makes it possible to see the entire text at once.

![Image of Emacs with four side-by-side windows](doc/demo2.png)

## Usage

Place the source file in a directory in the load path. Add the
following lines to an appropriate init file:

       (require 'multicolumn)

## Creating window layout

* <kbd>C-x 4 3</kbd> (`multicolumn-delete-other-windows-and-split`) creates
a number of side-by-side windows. The number of windows it creates
depends on the width of the frame and on `multicolumn-min-width`.
With a numerical argument, create this many windows.
* `multicolumn-delete-other-windows-and-split-with-follow-mode`
does the same and enables Follow mode, so that one buffer takes
over where the previous buffer ends. This gives you an
unprecedented overview of the buffer.
* <kbd>C-x 4 u</kbd> (`multicolumn-pop-window-configuration`) restored the
previous windows layout.

## Navigation

* <kbd>C-M-<</kbd> (`multicolumn-select-first-window`) and `C-M->`
(`multicolumn-select-last-window`) select the leftmost and rightmost
window, respectively.
* <kbd>C-x 4 p</kbd> (`multicolumn-select-previous-window`) selects the
previous windows. This package does not provide a function to
select the next. However, it binds <kbd>C-x 4 n</kbd> to `other-window`.

## Window content management

* <kbd>C-x 4 t</kbd> (`multicolumn-transpose-windows`) swaps the content of
the current and the next window.
* <kbd>C-x 4 <</kbd> (`multicolumn-extend-left`) and `C-x 4 >`
(`multicolumn-extend-right`) displays the current buffer in the
previous and next windows, respectively.
* `multicolumn-collect-windows` ensures that all windows displaying
the same buffer becomes adjacent.

## Trackpad support

* <kbd>C-wheel-left</kbd> and <kbd>C-wheel-right</kbd> selects the next and previous
window using `multicolumn-trackpad-select-previous-window` and
`multicolumn-trackpad-select-next-window`, respectively.
* <kbd>M-wheel-left</kbd> and <kbd>M-wheel-right</kbd> selects the first and last
window using `multicolumn-trackpad-select-first-window` and
`multicolumn-trackpad-select-last-window`, respectively.

## OS X Note

The latest official release for OS X (as of this writing), 24.3,
does not support horizontal mouse event. However, it will be
included in the next release.


---
Converted from `multicolumn.el` by [*el2markup*](https://github.com/Lindydancer/el2markdown).
