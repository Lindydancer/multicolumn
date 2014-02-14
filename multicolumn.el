;;; multicolumn.el -- Support for multiple side-by-side windows.

;; Copyright (C) 2000-2014 Anders Lindgren.

;; Author: Anders Lindgren
;; Created: 2000-??-??
;; Version: 0.0.0

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
;; This package is especially useful in conjunction with Follow mode, a
;; package provided with Emacs that create the illusion that several
;; windows showing the same buffer form a very tall virtual window.

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
;; Place the source file in a directory in the load path. Add the
;; following lines to an appropriate init file:
;;
;;    (require 'multicolumn)

;; Creating window layout:
;;
;; * `C-x 4 3' (`multicolumn-delete-other-windows-and-split') creates
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

;; Navigation:
;;
;; * `C-M-<' (`multicolumn-select-first-window') and `C-M->'
;; (`multicolumn-select-last-window') select the leftmost and rightmost
;; window, respectively.
;;
;; * `C-x 4 p' (`multicolumn-select-previous-window') selects the
;; previous windows. This package does not provide a function to
;; select the next. However, it binds `C-x 4 n' to `other-window'.

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

;; OS X Note:
;;
;; The latest official release for OS X (as of this writing), 24.3,
;; does not support horizontal mouse event. However, it will be
;; included in the next release.

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar multicolumn-min-width 72
  "*The minimul width of windows, in characters.")

(defvar multicolumn-windows-configuration-stack '()
  "Stack of window configurations.")

(defvar multicolumn-ignore-undefined-wheel-events t
  "When non-nil, undefined wheel events are bound to `ignore'.

This is done in the global key map when this package is loaded.
For this variable to take effect, it must be set prior to this.")


;; -------------------------------------------------------------------
;; Create multi column layout.
;;

(defun multicolumn-window-width (&optional win)
  (if (fboundp 'window-text-width)
      (window-text-width win)
    (window-width win)))

;; Note: This implementaion calculates the width by comparing the
;; width of one window with the width of two split windows. There
;; might be other ways to calculate this using `window-fringe', frame
;; properties like `scroll-bar-width' etc. However, such an
;; implementation would be error-prone as it would have to replicate
;; exacly how Emacs layout the frame pixel-by-pixel, something that
;; could change over time and might also vary between window systems.
(defun multicolumn-extra-columns-width ()
  "Width, in characters, of extra stuff in each window."
  (save-window-excursion
    (delete-other-windows)
    (let ((orig-width (multicolumn-window-width)))
      (split-window-horizontally)
      (- orig-width (+ (multicolumn-window-width)
                       (multicolumn-window-width (next-window)))))))


(defun multicolumn-split (&optional arg)
  "Split selected window horzontally into ARG side-by-side windows.

Should ARG be nil as many windows as possible are created as long
as they are will not become narrower than
`multicolumn-min-width'."
  (interactive "P")
  (let ((extra-width (multicolumn-extra-columns-width)))
    (unless arg
      (setq arg (/ (+ (multicolumn-window-width) extra-width)
		   (+ multicolumn-min-width extra-width))))
    (let ((width (- (multicolumn-window-width) (* (- arg 1) extra-width))))
      (while (> arg 1)
	(split-window-horizontally
	 (+ (/ width arg) extra-width))
	(setq width (- width (multicolumn-window-width)))
	(other-window 1)
	(setq arg (- arg 1))))))


(defun multicolumn-delete-other-windows-and-split
    (&optional arg)
  "Fill frame with buffer of selected window in ARG side-by-side windows.

Should ARG be nil as many windows as possible are created as long
as they are will not become narrower than
`multicolumn-min-width'.

The previous window layout can be restored using
`multicolumn-pop-window-configuration'."
  (interactive "P")
  (push (current-window-configuration) multicolumn-windows-configuration-stack)
  (delete-other-windows)
  (multicolumn-split arg))


(defun multicolumn-delete-other-windows-and-split-with-follow-mode
  (&optional arg)
  "Fill frame with selected window in ARG windows with `follow-mode' enabled.

Should ARG be nil as many windows as possible are created as long
as they are will not become narrower than
`multicolumn-min-width'.

The previous window layout can be restored using
`multicolumn-pop-window-configuration'."
  (interactive "P")
  (multicolumn-delete-other-windows-and-split arg)
  (follow-mode 1))


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


(defun multicolumn-transpose-windows ()
  "Swap the buffers of the current and the next window."
  (interactive)
  (multicolumn-swap-windows-content (selected-window) (next-window)))


(defun multicolumn-swap-windows-content (win1 win2)
  "Swap buffers of WIN1 and WIN2."
  (if (not (eq win1 win2))
      (let ((buf1 (window-buffer win1))
            (buf2 (window-buffer win2)))
        (set-window-buffer win1 buf2)
        (set-window-buffer win2 buf1))))




;; -------------------------------------------------------------------
;; Navigation and manipulation of windows in a multi column layout.
;;

(defun multicolumn-extend-right ()
  "Display the current buffer in the next window to the right."
  (interactive)
  (multicolumn-extend-direction 'next-window))


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


(defun multicolumn-select-first-window ()
  "Select the leftmost window in the frame."
  (interactive)
  (select-window (frame-first-window)))


(defun multicolumn-select-last-window ()
  "Select the rightmost window in the frame."
  (interactive)
  (select-window (previous-window (frame-first-window))))


(defun multicolumn-select-previous-window ()
  "Select previous window."
  (interactive)
  (other-window -1))


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
;;   "tripple" (presumably, to be analogous with mouse click events).
;;   Unlike other qualifiers, you don't bind, say, `tripple-wheel-up'
;;   to a function, instead you bind `wheel-up' and check if `tripple'
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
        ;; A non-wheel event occured.
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
;; Even thoug this package is implemented as a minor mode, it should
;; be seen as a prototype of what Emacs might look like if it provded
;; similar functions. Hence, the provided key bindings does not follow
;; the normal minor mode form.

(defvar multicolumn-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "C-M-<") 'multicolumn-select-first-window)
    (define-key map (kbd "C-M->") 'multicolumn-select-last-window)

    ;; Replace `split-window-right'. The idea behind replacing this
    ;; command is that `C-x 3' is etched into the fingers of many
    ;; people to create a multi column layout, and that once you have
    ;; a multi column layout, you don't want to split the window
    ;; again.
    (define-key map (kbd "C-x 3")  'multicolumn-delete-other-windows-and-split)
    ;; The `C-x 4' prefix is used for window-related functions.
    (define-key map (kbd "C-x 4 >") 'multicolumn-extend-right)
    (define-key map (kbd "C-x 4 <") 'multicolumn-extend-left)

    (define-key map (kbd "C-x 4 t") 'multicolumn-transpose-windows)

    (define-key map (kbd "C-x 4 p") 'multicolumn-select-previous-window)
    (define-key map (kbd "C-x 4 n") 'other-window)

    (define-key map (kbd "C-x 4 u") 'multicolumn-pop-window-configuration)

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
;; system as a slow-down effect. However, releaseing the qualifier
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


(define-minor-mode multicolumn-mode
  "Glocbal minor mode binds multi column functions to suitable keys."
  :global t
  :init-value t
  :keymap multicolumn-map)


;; ------------------------------------------------------------
;; The end
;;

(provide 'multicolumn)

;;; multicolumn.el ends here
