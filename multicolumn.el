;;; multicolumn.el --- Creating and managing multiple side-by-side windows.

;; Copyright (C) 2000-2014 Anders Lindgren.

;; Author: Anders Lindgren
;; Created: 2000-??-??
;; Version: 0.1.1
;; URL: https://github.com/Lindydancer/multicolumn

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Commands helpful when using multiple side-by-side windows.

;; Overview:
;;
;; Using multiple side-by-side windows is a great way to utilize the
;; large high-resolution displays that exists today. This package
;; provides the "missing features" of Emacs to create a side-by-side
;; layout, to navigate efficiently, and to manage the windows.
;;
;; This package is especially useful in conjunction with Follow mode,
;; a package provided with Emacs that create the illusion that several
;; windows showing the same buffer form a very tall virtual window.
;; For example, I use six side-by-side windows spread out across two
;; monitors, which lets me see 888 consecutive lines of code.
;; Concretely, this allows me to see all of the code in this package
;; at once.

;; Example:
;;
;; In the screenshots below, a text is being edited which is narrower
;; but taller than the Emacs frame.
;;
;; ![Image of Emacs with one window](doc/demo1.png)
;;
;; With `multicolumn-delete-other-windows-and-split-with-follow-mode'
;; four equally spaced side-by-side windows are created and Follow mode
;; is enabled, which makes it possible to see the entire text at once.
;;
;; ![Image of Emacs with four side-by-side windows](doc/demo2.png)

;; Usage:
;;
;; This package provides a number of functions for creating and
;; managing multiple side-by-side windows. It also provides
;; Multicolumn global mode that binds a number of keys to the
;; functions.

;; Installation:
;;
;; Place this package in a directory in the load-path. To activate it,
;; use *customize* or place the following lines in a suitable init
;; file:
;;
;;    (require 'multicolumn)
;;    (multicolumn-global-mode 1)

;; Creating side-by-side windows:
;;
;; * `C-x 3' (`multicolumn-delete-other-windows-and-split') creates
;; a number of side-by-side windows. The number of windows it creates
;; depends on the width of the frame and on `multicolumn-min-width'.
;; With a numerical argument, create this many windows.
;;
;; * `multicolumn-delete-other-windows-and-split-with-follow-mode'
;; does the same and enables Follow mode, so that one buffer takes
;; over where the previous buffer ends. This gives you an
;; unprecedented overview of the buffer.
;;
;; * `C-x 4 u' (`multicolumn-pop-window-configuration') restored the
;; previous windows layout.

;; Resizing the frame:
;;
;; * `multicolumn-resize-frame' resizes and repositions the frame
;; to accommodate side-by-side windows of a specific width. You can
;; use this as an alternative to using a full-screen mode.
;;
;; * `multicolumn-resize-and-split-frame' resizes and positions
;; the frame and creates a number of side-by-side windows. This
;; function can be called from to suitable init file to ensure that a
;; number of side-by-side windows are created when Emacs is started.

;; Navigation:
;;
;; * `C-M-<' (`multicolumn-select-first-window') and `C-M->'
;; (`multicolumn-select-last-window') select the leftmost and rightmost
;; window, respectively.
;;
;; * `C-x 4 p' (`multicolumn-select-previous-window') selects the
;; previous windows. This package does not provide a function to
;; select the next. However, it binds `C-x 4 n' to `other-window'.
;;
;; * `C-x 4 DIGIT' (`multicolumn-select-window-number') go to window
;;   number DIGIT, where 1 is the leftmost.
;;
;; * `C-x 4 :' (`multicolumn-select-minibuffer') go to the minibuffer.

;; Window content management:
;;
;; * `C-x 4 t' (`multicolumn-transpose-windows') swaps the content of
;; the current and the next window.
;;
;; * `C-x 4 <' (`multicolumn-extend-left') and `C-x 4 >'
;; (`multicolumn-extend-right') displays the current buffer in the
;; previous and next windows, respectively.
;;
;; * `multicolumn-collect-windows' ensures that all windows displaying
;; the same buffer becomes adjacent.

;; Trackpad support:
;;
;; * `C-wheel-left' and `C-wheel-right' selects the next and previous
;; window using `multicolumn-trackpad-select-previous-window' and
;; `multicolumn-trackpad-select-next-window', respectively.
;;
;; * `M-wheel-left' and `M-wheel-right' selects the first and last
;; window using `multicolumn-trackpad-select-first-window' and
;; `multicolumn-trackpad-select-last-window', respectively.

;; Configuration:
;;
;; Unfortunately, it's hard from within Emacs to find out information
;; about the environment outside Emacs, for example a window manager
;; may reserve parts of the screen. This package tries to contains
;; information for as many systems as possible, however, you may need
;; configure this package to match your system.
;;
;; See variable is the source code for how to configure this package.

;; MS-Windows Notes:
;;
;; Width of multiple monitor display:
;;
;; The functions `display-pixel-width' and `display-pixel-height'
;; functions only return the dimensions of the primary monitor, in
;; some Emacs versions. To make this package use the full display, you
;; can do something like:
;;
;;     (defun my-display-pixel-width ()
;;       ;; The full width of the display
;;       3200)
;;
;;     (setq multicolumn-display-pixel-width-function
;;           'my-display-pixel-width)

;; OS X Notes:
;;
;; Some features are only available in newer Emacs versions.
;; Horizontal mouse events, for example, require Emacs 24.4.
;;
;; In newer Emacs version, you can set `ns-auto-hide-menu-bar' to t to
;; utilize more of the display.
;;
;; In OS X 10.9, each monitor is a separate space. If you want to
;; stretch an Emacs frame across multiple monitors, you can change
;; this in "System Preferences -> Mission Control -> Displays have
;; separate Spaces".

;;; Code:

(eval-when-compile
  (require 'cl))


(defvar multicolumn-min-width 72
  "*The minimal width a window will have after a split, in characters.")


(defvar multicolumn-windows-configuration-stack '()
  "Stack of window configurations.")


(defvar multicolumn-ignore-undefined-wheel-events t
  "*When non-nil, undefined wheel events are bound to `ignore'.

This is done in the global key map when this package is loaded.
For this variable to take effect, it must be set prior to this.")


(defvar multicolumn-resize-frame-default-width 80
  "*Default window width in characters for `multicolumn-resize-frame'.")


(defvar multicolumn-resize-frame-full-lines-only t
  "*When non-nil, `multicolumn-resize-frame' use full lines.

When this is nil, the frame will fill up the entire height of the
display (if supported). However, the last line might be clipped.")


(defvar multicolumn-resize-frame-place-title-above-screen t
  "*When non-nil the window title is placed outside the screen.

Currently, this is only done for OS X, when the menu bar is auto
hidden. (See `ns-auto-hide-menu-bar'.)")


(defvar multicolumn-display-pixel-width-function
  'display-pixel-width
  "A function that is called to retrieve the width of the display.")


(defvar multicolumn-display-pixel-height-function
  'display-pixel-height
  "A function that is called to retrieve the height of the display.")


(defvar multicolumn-extra-height-function
  'multicolumn-extra-height-default-function
  "A function that is called to find the height of non-text parts a frame.")


(defvar multicolumn-frame-top-function
  'multicolumn-frame-top-default-function
  "A function that is called to find the offset from the top of the display.")


(defvar multicolumn-frame-full-border-width-function
  'multicolumn-frame-full-border-width-default-function
  "A function that is called to find the offset from the top of the display.")


;; -------------------------------------------------------------------
;; Utilities.
;;


(defalias 'multicolumn-user-error
  (if (fboundp 'user-error)
      'user-error
    'error))


;; -------------------------------------------------------------------
;; Resize and position frame.
;;

;; It's somewhat of black magic to find out exactly how large an Emacs
;; frame is allowed to be on a specific system. The functions below
;; are written to include as much information as possible. However, it
;; is not unlikely that you would need to override one of the
;; `...-function' variables and write your own function.

;; Note: `set-frame-size' takes the "text" area, i.e. without the one
;; set of fringes, scroll bars, and two frame borders. However, on
;; some systems the tool-bar is included in the text are, in some it's
;; not.
;;
;; Same as `frame-pixel-width':
;;
;; (let ((res 0))
;;   (dolist (w (window-list))
;;     (setq res (+ (window-width w t)
;;                  (frame-scroll-bar-width)
;;                  (frame-fringe-width)
;;                  res)))
;;   (+ res (* 2 (frame-border-width))))

;; The following facts appears to be impossible to find out:
;;
;; * The display area available to use (e.g. if the system has a menu).
;;
;; * The height of the menu bar, in pixels.
;;
;; * The height of the tool bar, in pixels.
;;
;; * If the tool bar is included in the text height of a frame.

(defconst multicolumn-ns-title-height 24)
(defconst multicolumn-ns-menu-height 22)

(defconst multicolumn-w32-title-height 24)
(defconst multicolumn-w32-menu-height 24)

(defun multicolumn-extra-height-default-function ()
  "Number of vertical pixels wasted in frame for the current window system."
  (+
   ;; Window title.
   (cond ((eq window-system 'w32)
          multicolumn-w32-title-height)
         ((memq window-system '(mac ns))
          multicolumn-ns-title-height)
         ((eq window-system 'x)
          ;; Window title and menu bar.
          ;;
          ;; Note: This is just an estimate, your mileage may wary.
          22)
         (t
          0))
   ;; Menu bar.
   (cond ((not menu-bar-mode)
          0)
         ((eq window-system 'w32)
          multicolumn-w32-title-height)
         ((memq window-system '(mac ns))
          ;; For OS X, the menu bar is not part of the frame.
          0)
         ((eq window-system 'x)
          23)
         (t
          (* (frame-parameter (selected-frame) 'menu-bar-lines)
             (frame-char-height))))
   ;; Tool bar.
   (cond ((not (and (boundp 'tool-bar-mode)
                    tool-bar-mode))
          0)
         ((eq window-system 'x)
          ;; For X11, the tool bar is included in the "text" area.
          0)
         (t
          ;; Note: This might not be true for all window systems.
          32))))


(defun multicolumn-frame-top-default-function ()
  "The offset from the top of the display the frame could be placed."
  (cond ((memq window-system '(mac ns))
         (if (and (boundp 'ns-auto-hide-menu-bar)
                  ns-auto-hide-menu-bar)
             (if multicolumn-resize-frame-place-title-above-screen
                 -24
               0)
           22))
        ((eq window-system 'w32)
         0)
        (t
         ;; Just a number picked out of a hat.
         22)))


(defun multicolumn-frame-parameter (frame parameter)
  "Return FRAME's value for parameter PARAMETER, or 0 if nil or nonexisting."
  (let ((value (frame-parameter frame parameter)))
    (if (null value)
        0
      value)))


(defun multicolumn-window-extra-width ()
  "The width in pixels of the fringes and scroll bar.

Prior to `window-resize-pixelwise' was introduced (i.e. up to and
including Emacs 24.3), the fringes and scroll bars were padded to
a multiple of the width of a frame character."
  (let ((extra-width (+ (multicolumn-frame-parameter nil 'scroll-bar-width)
                        (multicolumn-frame-parameter nil 'left-fringe)
                        (multicolumn-frame-parameter nil 'right-fringe))))
    (if (boundp 'window-resize-pixelwise)
        extra-width
      ;; Round up to nearest multiple of the frame char width.
      (+ extra-width
         (- (frame-char-width)
            (% extra-width (frame-char-width)))))))


;;
;; Frame parameters (as seen in the wild):
;;
;;                         w32  ns    x
;;
;; internal-border-width    0    2    1
;; border-width             2    0    0
;;

(defun multicolumn-frame-full-border-width-default-function ()
  "The width of the frame borders, in pixels."
  (cond ((eq window-system 'w32)
         ;; The frame border can vary between windows versions, and
         ;; depending on which mode is used.
         ;;
         ;; 4 pixels represent a classic border. Please override
         ;; `multicolumn-frame-full-border-width-function' if this
         ;; doesn't match your system.
         ;;
         ;; TODO: Investigate if there is away to find the actual
         ;; border width.
         4)
        (t
         (+ (multicolumn-frame-parameter nil 'border-width)
            (multicolumn-frame-parameter nil 'internal-border-width)))))

(defun multicolumn-window-pixel-width (width)
  "The width of a window, with WIDTH characters, in pixels."
  (+ (* width
        (frame-char-width))
     (multicolumn-window-extra-width)))


(defun multicolumn-resize-frame--optimal-number-of-windows (width-in-chars)
  "Return number of side-by-side windows the display can accommodate.

WIDTH-IN-CHARS is the width of each window, in characters."
  (max 1
       (/ (- (funcall multicolumn-display-pixel-width-function)
             (* 2 (funcall multicolumn-frame-full-border-width-function)))
          (multicolumn-window-pixel-width width-in-chars))))

(defun multicolumn-resize-frame--read-interactive-arguments ()
  "Read interactive arguments for `multicolumn-resize-frame' etc."
  (if current-prefix-arg
      (let ((width (read-number
                    "Width: "
                    multicolumn-resize-frame-default-width)))
        (list width
              (read-number
               "Columns: "
               (multicolumn-resize-frame--optimal-number-of-windows width))))
    '(nil nil)))



;;;###autoload
(defun multicolumn-resize-frame (&optional
                                 width-in-chars
                                 number-of-windows)
  "Resize and position frame to accommodate multiple side-by-side windows.

With \\[universal-argument], prompt for window width and number
of windows.

Return intended number of windows, or nil in case there is no
window system."
  (interactive (multicolumn-resize-frame--read-interactive-arguments))
  (if (not window-system)
      ;; By returning nil, it's possible to chain this function with
      ;; `multicolumn-delete-other-windows-and-split' even when a
      ;; window system isn't used.
      nil
    (unless width-in-chars
      (setq width-in-chars multicolumn-resize-frame-default-width))
    (unless number-of-windows
      (setq number-of-windows
            (multicolumn-resize-frame--optimal-number-of-windows
             width-in-chars)))
    (let* ((top (funcall multicolumn-frame-top-function))
           ;; `set-frame-size' expects the width of the "text area",
           ;; i.e. without one set of fringes and a scroll bar.
           (width (- (* number-of-windows
                        (multicolumn-window-pixel-width width-in-chars))
                     (multicolumn-window-extra-width)))
           (height
            (- (funcall
                multicolumn-display-pixel-height-function)
               top)))
      (setq height
            (- height
               (funcall multicolumn-extra-height-function)))
      (when multicolumn-resize-frame-full-lines-only
        ;; Ensure only full lines.
        (setq height (- height (% height (frame-char-height)))))
      (if (boundp 'frame-resize-pixelwise)
          (let ((frame-resize-pixelwise t))
	    (with-no-warnings
	      (set-frame-size (selected-frame)
			      width
			      height
			      'pixelwise)))
        (set-frame-size (selected-frame)
                        (/ width (frame-char-width))
                        (/ height (frame-char-height))))
      ;; Note, `set-frame-position' can't be used as it is documented
      ;; to handle negative values in a special way. (Even if it
      ;; actually don't, at least for OS X.)
      (set-frame-parameter
       (selected-frame)
       'left
       ;; Center horizontally, to ensure that no window will be split
       ;; between two monitors. (Assuming that an even number of
       ;; columns and symmetrical monitors are used.)
       (/ (- (funcall multicolumn-display-pixel-width-function)
             (+ width
                (multicolumn-window-extra-width)
                (* 2 (funcall multicolumn-frame-full-border-width-function))))
          2))
      (set-frame-parameter (selected-frame)
                           'top
                           (list '+ top))
      ;; Return value.
      number-of-windows)))


;;;###autoload
(defun multicolumn-resize-and-split-frame (&optional
                                           width-in-chars
                                           number-of-windows)
  "Resize, position, and split frame with multiple side-by-side windows.

With \\[universal-argument], prompt for window width and number
of windows."
  (interactive (multicolumn-resize-frame--read-interactive-arguments))
  (multicolumn-delete-other-windows-and-split
   (multicolumn-resize-frame width-in-chars number-of-windows)))


;; -------------------------------------------------------------------
;; Create layout with multiple side-by-side windows
;;

;;;###autoload
(defun multicolumn-split (&optional number-of-windows)
  "Split selected window horizontally into side-by-side windows.

Split into NUMBER-OF-WINDOWS windows. Should it be nil, create as
many windows as possible as long as they will not become narrower
than `multicolumn-min-width'."
  (interactive "P")
  (if number-of-windows
      (setq number-of-windows (prefix-numeric-value number-of-windows)))
  (let ((extra-width (multicolumn-window-extra-width))
        (original-window (selected-window)))
    (if (boundp 'window-resize-pixelwise)
        ;; Split pixelwise.
        (progn
          (unless number-of-windows
            (setq number-of-windows
                  (/ (+ (with-no-warnings
                          (window-width nil 'pixelwise))
                        extra-width)
                     (+ (* multicolumn-min-width
                           (frame-char-width))
                        extra-width))))
          (let ((width (- (with-no-warnings
                            (window-width nil 'pixelsize))
                          (* (- number-of-windows 1)
                             extra-width))))
            (while (> number-of-windows 1)
              (let ((window-resize-pixelwise t))
		(with-no-warnings
		  (split-window
		   nil
		   (+ (/ width number-of-windows) extra-width)
		   'right
		   'pixelwise)))
              (setq width (- width (with-no-warnings
                                     (window-width nil 'pixelwise))))
              (other-window 1)
              (setq number-of-windows (- number-of-windows 1)))))
      ;; Split characterwise.
      ;;
      ;; Up to Emacs 24.3, splitting windows could only be done
      ;; characterwise. Also, the sum of the width of the fringes and
      ;; the scroll bars were a multiple of the frame character width.
      (setq extra-width (/ extra-width (frame-char-width)))
      ;; After a frame resize, `window-width' doesn't return the
      ;; correct value without this. (Seen on Emacs 22 under Windows.)
      (sit-for 0.1)
      (unless number-of-windows
        (setq number-of-windows (/ (+ (window-width)
                                      extra-width)
                                   (+ multicolumn-min-width
                                      extra-width))))
      (let ((width (- (window-width) (* (- number-of-windows 1) extra-width))))
        (while (> number-of-windows 1)
          (split-window-horizontally
           (+ (/ width number-of-windows) extra-width))
          (setq width (- width (window-width)))
          (other-window 1)
          (setq number-of-windows (- number-of-windows 1)))))
    (select-window original-window)))


;;;###autoload
(defun multicolumn-delete-other-windows-and-split
    (&optional number-of-windows)
  "Fill frame with buffer of selected window in ARG side-by-side windows.

Should NUMBER-OF-WINDOWS be nil as many windows as possible are
created as long as they are will not become narrower than
`multicolumn-min-width'.

The previous window layout can be restored using
`multicolumn-pop-window-configuration'."
  (interactive "P")
  (push (current-window-configuration) multicolumn-windows-configuration-stack)
  (delete-other-windows)
  (multicolumn-split number-of-windows))


;;;###autoload
(defun multicolumn-delete-other-windows-and-split-with-follow-mode
    (&optional number-of-windows)
  "Fill frame with selected window in ARG windows with `follow-mode' enabled.

Should NUMBER-OF-WINDOWS be nil as many windows as possible are
created as long as they are will not become narrower than
`multicolumn-min-width'.

The previous window layout can be restored using
`multicolumn-pop-window-configuration'."
  (interactive "P")
  (multicolumn-delete-other-windows-and-split number-of-windows)
  (follow-mode 1))


;;;###autoload
(defun multicolumn-pop-window-configuration ()
  "Go back to the previous window configuration."
  (interactive)
  (if (null multicolumn-windows-configuration-stack)
      (error "Multicolumn window configuration stack is empty."))
  (let ((config (pop multicolumn-windows-configuration-stack)))
    (set-window-configuration config)))


;; -------------------------------------------------------------------
;; Collect all windows displaying the same buffer
;;

;;;###autoload
(defun multicolumn-collect-windows ()
  "Make sure windows displaying the same buffer are adjacent."
  (interactive)
  (let ((all-windows '()))
    (let ((window (frame-first-window)))
      (while
          (progn
            (push window all-windows)
            (setq window (next-window window))
            (not (eq window (frame-first-window))))))
    (setq all-windows (nreverse all-windows))
    (while all-windows
      (let ((window (pop all-windows)))
        ;; Skip existing streak of windows.
        (while (and all-windows
                    (eq (window-buffer window)
                        (window-buffer (car all-windows))))
          (setq window (pop all-windows)))
        ;; Move other windows up the list.
        (dolist (rest all-windows)
          (if (eq (window-buffer rest)
                  (window-buffer window))
              (progn
                (setq window (next-window window))
                (pop all-windows)
                (multicolumn-swap-windows-content rest window))))))))


;;;###autoload
(defun multicolumn-transpose-windows ()
  "Swap the buffers of the current and the next window."
  (interactive)
  (multicolumn-swap-windows-content (selected-window) (next-window))
  (select-window (next-window)))


;;;###autoload
(defun multicolumn-swap-windows-content (win1 win2)
  "Swap buffers of WIN1 and WIN2."
  (if (not (eq win1 win2))
      (let ((buf1 (window-buffer win1))
            (buf2 (window-buffer win2)))
        (set-window-buffer win1 buf2)
        (set-window-buffer win2 buf1))))




;; -------------------------------------------------------------------
;; Navigation and manipulation side-by-side windows.
;;

;;;###autoload
(defun multicolumn-extend-right ()
  "Display the current buffer in the next window to the right."
  (interactive)
  (multicolumn-extend-direction 'next-window))


;;;###autoload
(defun multicolumn-extend-left ()
  "Display the current buffer in the next window to the left."
  (interactive)
  (multicolumn-extend-direction 'previous-window))


(defun multicolumn-extend-direction (pick-window-function)
  "Display the buffer in the selected window in another window."
  (let ((original-window (selected-window))
        (buffer (current-buffer))
        (window nil)
        (cont t))
    (while cont
      (setq window (funcall pick-window-function window))
      (cond ((eq window original-window)
             (setq window nil)
             (setq cont nil))
            ((not (eq (window-buffer window) buffer))
             (setq cont nil))))
    (if window
        (set-window-buffer window buffer))))


;;;###autoload
(defun multicolumn-select-first-window ()
  "Select the leftmost window in the frame."
  (interactive)
  (select-window (frame-first-window)))


;;;###autoload
(defun multicolumn-select-last-window ()
  "Select the rightmost window in the frame."
  (interactive)
  (select-window (previous-window (frame-first-window))))


;;;###autoload
(defun multicolumn-select-previous-window ()
  "Select previous window."
  (interactive)
  (other-window -1))


;;;###autoload
(defun multicolumn-select-window-number (number)
  "Select window NUMBER, where 1 is the leftmost.

When called interactively, this is assumed to be bound to a key
seqeunce ending in a digit."
  (interactive (list (- last-command-event ?0)))
  (if (< number 1)
    (multicolumn-user-error "Illegal window number"))
  (let ((count 1)
        (win (frame-first-window)))
    (while (< count number)
      (setq win (next-window win))
      (setq count (+ count 1)))
    (select-window win)))


;;;###autoload
(defun multicolumn-select-minibuffer ()
  "Select the minibuffer, if visible."
  (interactive)
  (let ((win (active-minibuffer-window)))
    (if win
        (select-window win)
      (multicolumn-user-error "Minibuffer is not active"))))


;; -------------------------------------------------------------------
;; Trackpad (horizontal mouse wheel) support.
;;

;;
;; When a swipe is performed, the following occurs:
;;
;; * A number of wheel-XXX events are emitted. They keep on coming
;;   even after the trackpad has been released in a kind of slowdown
;;   effect. Each event is numbered with a "click-count", starting
;;   from 1 and increasing.
;;
;; * If the qualifier key is released (or pressed), the remaining
;;   events will be associated with the new qualifier. The effect is
;;   that they may cause another function to be called! By default,
;;   there is no binding for wheel-xxx for most of the combinations of
;;   qualifiers, causing Emacs to "bing" a lot.
;;
;; * Sometimes, two wheel-XXX events may be intertwined, e.g. wheel-up
;;   and wheel-right. In that case, the "click-count" will restart
;;   from 1 each time.
;;
;; * Events can be marked as "click" (i.e. single), "double", and
;;   "triple" (presumably, to be analogous with mouse click events).
;;   Unlike other qualifiers, you don't bind, say, `triple-wheel-up'
;;   to a function, instead you bind `wheel-up' and check if `triple'
;;   is present using `event-modifiers'.
;;

(defvar multicolumn-trackpad-timer nil)

(defvar multicolumn-trackpad-quarantine-active nil
  "Non-nil when not OK to handle horizontal mouse events.")

(defvar multicolumn-trackpad-last-horizontal-wheel-event nil
  "Either `wheel-left', `wheel-right', or nil.

Set the `wheel-left' or `wheel-right' when a
`multicolumn-trackpad-' command is executed. Cleared to nil when
non-wheel event occurs.")


(defun multicolumn-trackpad-pre-command-hook ()
  "Determine if horizontal trackpad commands should be issued multiple times.

When doing a sweep on a trackpad, horizontal and vertical mouse
events are triggered. To avoid that each horizontal event
re-issues a command, this puts the horizontal events in
quarantine when mixed with vertical mouse events."
  (when multicolumn-trackpad-last-horizontal-wheel-event
    (let ((type (event-basic-type last-input-event)))
      (if (memq type '(wheel-left wheel-right wheel-up wheel-down))
          (when multicolumn-trackpad-quarantine-active
            (multicolumn-trackpad-quarantine-start-timer))
        ;; A non-wheel event occurred.
        (setq multicolumn-trackpad-last-horizontal-wheel-event nil)))))


(add-hook 'pre-command-hook 'multicolumn-trackpad-pre-command-hook)

(defun multicolumn-trackpad-quarantine-end ()
  "Enable trackpad, normally called by timer after a time-out time."
  ;; (message ">>>>> Quarantine end")
  (setq multicolumn-trackpad-quarantine-active nil)
  (setq multicolumn-trackpad-last-horizontal-wheel-event nil))


(defun multicolumn-trackpad-quarantine-start-timer ()
  "Start trackpad quarantine timer.

Ensure that one sweep does not trigger multiple command, when
horizontal and vertical trackpad events are mixed."
  (if multicolumn-trackpad-timer
      (cancel-timer multicolumn-trackpad-timer))
  (setq multicolumn-trackpad-quarantine-active t)
  ;; (message "<<<<< Quarantine start")
  (setq multicolumn-trackpad-timer
        (run-with-timer 0.3 nil 'multicolumn-trackpad-quarantine-end)))


(defmacro multicolumn-trackpad-do (&rest body)
  `(prog1 (if (and (eq (event-click-count last-input-event) 1)
                   (or (not multicolumn-trackpad-last-horizontal-wheel-event)
                       (not multicolumn-trackpad-quarantine-active)))
              (progn
                ;; (message "------> do")
                (progn ,@body))
            nil)
     (setq multicolumn-trackpad-last-horizontal-wheel-event
           (event-basic-type last-input-event))
     (multicolumn-trackpad-quarantine-start-timer)))


(defun multicolumn-trackpad-select-next-window ()
  "Select the next window using a trackpad."
  (interactive)
  (multicolumn-trackpad-do
   (other-window 1)))


(defun multicolumn-trackpad-select-previous-window ()
  "Select the previous window using a trackpad."
  (interactive)
  (multicolumn-trackpad-do
   (other-window -1)))


(defun multicolumn-trackpad-select-first-window ()
  "Select the first window in the frame using a trackpad."
  (interactive)
  (multicolumn-trackpad-do
   (multicolumn-select-first-window)))


(defun multicolumn-trackpad-select-last-window ()
  "Select the last window in the frame using a trackpad."
  (interactive)
  (multicolumn-trackpad-do
   (multicolumn-select-last-window)))


;; ------------------------------------------------------------
;; The minor mode.
;;

;; Note on key bindings:
;;
;; Even though this package is implemented as a minor mode, it should
;; be seen as a prototype of what Emacs might look like if it provided
;; similar functions. Hence, the provided key bindings does not follow
;; the normal minor mode form.

(defvar multicolumn-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "C-M-<") 'multicolumn-select-first-window)
    (define-key map (kbd "C-M->") 'multicolumn-select-last-window)

    ;; Replace `split-window-right'. The idea behind replacing this
    ;; command is that `C-x 3' is etched into the fingers of many
    ;; people to create a multicolumn layout, and that once you have a
    ;; multicolumn layout, you don't want to split the window again.
    (define-key map (kbd "C-x 3")  'multicolumn-delete-other-windows-and-split)
    ;; The `C-x 4' prefix is used for window-related functions.
    (define-key map (kbd "C-x 4 >") 'multicolumn-extend-right)
    (define-key map (kbd "C-x 4 <") 'multicolumn-extend-left)

    (define-key map (kbd "C-x 4 t") 'multicolumn-transpose-windows)

    (define-key map (kbd "C-x 4 p") 'multicolumn-select-previous-window)
    (define-key map (kbd "C-x 4 n") 'other-window)

    (define-key map (kbd "C-x 4 u") 'multicolumn-pop-window-configuration)

    (define-key map (kbd "C-x 4 :") 'multicolumn-select-minibuffer)

    (let ((count 1))
      (while (< count 10)
	;; Note: In newer Emacs versins, `kbd' and `read-kbd-macro'
	;; are the same. In older versions, however, `kbd' doesn't
	;; evaluate its argument.
        (define-key map (read-kbd-macro (format "C-x 4 %d" count))
          'multicolumn-select-window-number)
        (setq count (+ count 1))))

    (define-key map (kbd "<C-wheel-left>")
      'multicolumn-trackpad-select-previous-window)
    (define-key map (kbd "<C-wheel-right>")
      'multicolumn-trackpad-select-next-window)

    (define-key map (kbd "<M-wheel-left>")
      'multicolumn-trackpad-select-first-window)
    (define-key map (kbd "<M-wheel-right>")
      'multicolumn-trackpad-select-last-window)

    ;; And don't forget to "return" map.
    map))


;; Note that mouse wheel events are generated by the operating
;; system as a slow-down effect. However, releasing the qualifier
;; keys will make the new event arrive without it. If there is no
;; binding for the wheel, an error will be issued. Hence, it's
;; better to map all wheel events to `ignore'.
;;
;; This is done in the global map in order not to shadow other minor
;; modes.
(when multicolumn-ignore-undefined-wheel-events
  (dolist (direction '(wheel-left wheel-right wheel-up wheel-down))
    (dolist (control '(nil control))
      (dolist (meta '(nil meta))
        (dolist (super '(nil super))
          (dolist (hyper '(nil hyper))
            (let ((key (list direction)))
              (if meta
                  (push meta key))
              (if control
                  (push control key))
              (if super
                  (push super key))
              (if hyper
                  (push hyper key))
              (setq key (vector key))
              (unless (lookup-key global-map key)
                (global-set-key key 'ignore)))))))))


;;;###autoload
(define-minor-mode multicolumn-global-mode
  "Global minor mode for creating and managing side-by-side windows."
  :global t
  :keymap multicolumn-map)


;; ------------------------------------------------------------
;; The end
;;

(provide 'multicolumn)

;;; multicolumn.el ends here
