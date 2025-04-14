;;; imenu-list.el --- Show imenu entries in a separate buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021 Bar Magal & Contributors

;; Author: Bar Magal (2015)
;; Version: 0.9
;; Homepage: https://github.com/bmag/imenu-list
;; Package-Requires: ((emacs "24.3"))

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Shows a list of imenu entries for the current buffer, in another
;; buffer with the name "*Ilist*".
;;
;; Activation and deactivation:
;; M-x global-imenu-list-mode
;;
;; Key shortcuts from "*Ilist*" buffer:
;; <enter>: Go to current definition
;; <space>: display current definition
;; <tab>: expand/collapse subtree
;;
;; Change "*Ilist*" buffer's position and size:
;; `imenu-list-position', `imenu-list-size'.
;;
;; Should invoking `global-imenu-list-mode' also select the "*Ilist*"
;; window?
;; `imenu-list-focus-after-activation'

;;; Code:

(require 'imenu)
(require 'cl-lib)
(require 'wid-edit)
(require 'tree-widget)

(defconst imenu-list-buffer-name "*Ilist*"
  "Name of the buffer that is used to display imenu entries.")

(defvar imenu-list--imenu-entries nil
  "Currently used imenu entires.
This is a copy of the imenu entries of the buffer we want to
display in the imenu-list buffer.")

(defvar imenu-list--line-entries nil
  "List of imenu entries displayed in the imenu-list buffer.
The first item in this list corresponds to the first line in the
imenu-list buffer, the second item matches the second line, and so on.")

(defvar imenu-list--displayed-buffer nil
  "The buffer that the saved imenu entries relate to.")

(defvar imenu-list--last-location nil
  "Location from which last `imenu-list-update' was done.
Used to avoid updating if the point didn't move.")

;;; fancy display

(defgroup imenu-list nil
  "Variables for `imenu-list' package."
  :group 'imenu)

(defcustom imenu-list-persist-when-imenu-index-unavailable t
  "Whether or not to keep the old index if the new index is missing.
This option controls whether imenu-list will persist the entries
of the last current buffer during an attempt to update it from a
buffer that has no Imenu index.  Some users find this behavior
convenient for jumping back and forth between different buffers
when paired with window-purpose's x-code-1 configuration.

If you kill buffers often, set this to nil so x-code-1 will clear
the entries when focusing on a buffer that does not have an Imenu
index."
  :group 'imenu-list
  :type 'boolean)

(defcustom imenu-list-mode-line-format
  '("%e" mode-line-front-space
    " Imenu "
    (:eval (buffer-name imenu-list--displayed-buffer)) " "
    mode-line-end-spaces)
  "Local mode-line format for the imenu-list buffer.
This is the local value of `mode-line-format' to use in the imenu-list
buffer.  See `mode-line-format' for allowed values."
  :group 'imenu-list
  :type 'sexp)

(defcustom imenu-list-focus-after-activation nil
  "Whether or not to select imenu-list window after activation.
Non-nil to select the imenu-list window automatically when
`global-imenu-list-mode' is activated."
  :group 'imenu-list
  :type 'boolean)

(defcustom imenu-list-update-current-entry t
  "Whether or not `imenu-list-update' shows the current entry.
If non-nil, imenu-list shows the current entry on the menu
automatically during update."
  :group 'imenu-list
  :type 'boolean)

(defcustom imenu-list-custom-position-translator nil
  "Custom translator of imenu positions to buffer positions.
Imenu can be customized on a per-buffer basis not to use regular buffer
positions as the positions that are stored in the imenu index.  In such
cases, imenu-list needs to know how to translate imenu positions back to
buffer positions.  `imenu-list-custom-position-translator' should be a
function that returns a position-translator function suitable for the
current buffer, or nil.  See `imenu-list-position-translator' for details."
  :group 'imenu-list
  :type 'function)

(defface imenu-list-entry-face-0
  '((t :inherit font-lock-type-face))
  "Face for outermost imenu-list entries (depth 0)."
  :group 'imenu-list)

(defface imenu-list-entry-clickable-tag-face-0
  '((t :inherit imenu-list-entry-face-0
       :underline t))
  "Face for subalist entries with depth 0."
  :group 'imenu-list)

(defface imenu-list-entry-face-1
  '((t :inherit font-lock-function-name-face))
  "Face for imenu-list entries with depth 1."
  :group 'imenu-list)

(defface imenu-list-entry-clickable-tag-face-1
  '((t :inherit imenu-list-entry-face-1
       :underline t))
  "Face for subalist entries with depth 1."
  :group 'imenu-list)

(defface imenu-list-entry-face-2
  '((t :inherit font-lock-variable-name-face))
  "Face for imenu-list entries with depth 2."
  :group 'imenu-list)

(defface imenu-list-entry-clickable-tag-face-2
  '((t :inherit imenu-list-entry-face-2
       :underline t))
  "Face for subalist entries with depth 2."
  :group 'imenu-list)

(defface imenu-list-entry-face-3
  '((t :inherit font-lock-string-face))
  "Face for imenu-list entries with depth 3."
  :group 'imenu-list)

(defface imenu-list-entry-clickable-tag-face-3
  '((t :inherit imenu-list-entry-face-3
       :underline t))
  "Face for subalist entries with depth 0."
  :group 'imenu-list)

(defun imenu-list--get-face (depth clickablep)
  "Get face for entry.
DEPTH is the depth of the entry in the list."
  (cl-case depth
    (0 (if clickablep 'imenu-list-entry-clickable-tag-face-0 'imenu-list-entry-face-0))
    (1 (if clickablep 'imenu-list-entry-clickable-tag-face-1 'imenu-list-entry-face-1))
    (2 (if clickablep 'imenu-list-entry-clickable-tag-face-2 'imenu-list-entry-face-2))
    (3 (if clickablep 'imenu-list-entry-clickable-tag-face-3 'imenu-list-entry-face-3))
    (t (if clickablep 'imenu-list-entry-clickable-tag-face-3 'imenu-list-entry-face-2))))

;;; collect entries

(defun imenu-list-rescan-imenu ()
  "Force imenu to rescan the current buffer."
  (setq imenu--index-alist nil)
  (imenu--make-index-alist))

(defun imenu-list-collect-entries ()
  "Collect all `imenu' entries of the current buffer."
  (imenu-list-rescan-imenu)
  (setq imenu-list--imenu-entries imenu--index-alist)
  (setq imenu-list--displayed-buffer (current-buffer)))

;;; print entries

(defun imenu-list--event-ilist-buffer (event)
  (let ((window (posn-window (event-end event)))
        (ilist-buffer (get-buffer imenu-list-buffer-name)))
    (when (and (windowp window)
               (eql (window-buffer window) ilist-buffer))
      ilist-buffer)))

(defun imenu-list--action-goto-entry (event item)
  "Goto the entry that was clicked.
EVENT is the click event, ITEM is the item clocked on."
  (when-let ((buffer (imenu-list--event-ilist-buffer event)))
    (with-current-buffer buffer
      (imenu-list-goto-entry item))))

(defun imenu-list--imenu-to-line-entry (entry)
  (let ((kids (when (imenu--subalist-p entry)
                (mapcan #'imenu-list--imenu-to-line-entry (cdr entry))))
        (pos (imenu-list--item-pos entry)))
    (if pos
        (cons (cons (car entry) pos) kids)
      kids)))

(defun imenu-list-insert-entries ()
  (setq imenu-list--line-entries
        (mapcan #'imenu-list--imenu-to-line-entry imenu-list--imenu-entries))
  (let ((inhibit-read-only t)
        (n-top-level-widgets 0)
        first-widget)
    (erase-buffer)
    (cl-labels ((widgetize (item &optional (indent 0))
                  (cl-flet ((subalist-tag ()
                              (with-temp-buffer
                                (let* ((name (car item))
                                       (pos  (imenu-list--item-pos item))
                                       (face (imenu-list--get-face indent pos)))
                                  (insert-text-button name
                                                      'face face
                                                      'follow-link "\C-m"
                                                      'action (lambda (event)
                                                                (when-let ((buf (imenu-list--event-ilist-buffer event)))
                                                                  (with-current-buffer buf
                                                                    (while (plist-get (text-properties-at (point)) 'button)
                                                                      (backward-char))
                                                                    (backward-char)
                                                                    (let ((tree (widget-get (widget-at) :parent)))
                                                                      (unless (widget-get tree :open)
                                                                        (widget-apply-action tree)))))
                                                                (when pos
                                                                  (imenu-list-goto-entry item))))
                                  (buffer-substring (point-min) (point-max))))))
                    (apply #'widget-convert
                           (if (imenu--subalist-p item)
                               `(tree-widget :tag ,(subalist-tag)
                                             :args ,(mapcar (lambda (item)
                                                              (widgetize item (1+ indent)))
                                                            (cdr item)))
                             `(link :tag ,(car item)
                                    :button-face ,(imenu-list--get-face indent nil)
                                    :format "%[%t%]\n"
                                    :button-prefix ""
                                    :button-suffix ""
                                    :action ,(lambda (_ __)
                                               (imenu-list-goto-entry item))
                                    :follow-link "\C-m"
                                    ))))))
      (dolist (item imenu-list--imenu-entries)
        (let ((widget (widget-create (widgetize item))))
          (unless first-widget
            (setq first-widget widget))
          (cl-incf n-top-level-widgets))))
    (when (and (= 1 n-top-level-widgets)
               (eq (widget-type first-widget) 'tree-widget))
      ;; pre-expand the sole tree widget
      (widget-apply-action first-widget)))
  (goto-char (point-min)))

;;; goto entries

(defcustom imenu-list-after-jump-hook '(recenter)
  "Hook to run after jumping to an entry from the imenu-list buffer."
  :group 'imenu-list
  :type 'hook)

(defun imenu-list--find-entry ()
  "Find in `imenu-list--line-entries' the entry in the current line."
  (nth (1- (line-number-at-pos)) imenu-list--line-entries))

(defun imenu-list--item-pos (item)
  (or
   (when-let ((br (plist-get (text-properties-at 0 (car item)) 'breadcrumb-region)))
     (car br))
   (cond
    ((imenu--subalist-p item) nil)
    ((consp (cdr item))       (cadr item))
    (t                        (cdr item)))))

(defun imenu-list-goto-entry (item)
  "Switch to the original buffer and display the entry under point."
  (interactive)
  (pop-to-buffer imenu-list--displayed-buffer)
  (goto-char (imenu-list--item-pos item))
  (run-hooks 'imenu-list-after-jump-hook)
  (imenu-list--show-current-entry))

;; hide false-positive byte-compile warning. We only use these functions if
;; eglot is loaded.
(declare-function eglot--lsp-position-to-point "eglot")
(declare-function eglot-managed-p "eglot")

(defun imenu-list--translate-eglot-position (pos)
  "Get real position of position object POS created by eglot."
  ;; when Eglot is in charge of Imenu, then the index is created by `eglot-imenu', with a fallback to
  ;; `imenu-default-create-index-function' when `eglot-imenu' returns nil. If POS is an array, it means
  ;; it was created by `eglot-imenu' and we need to extract its position. Otherwise, it was created by
  ;; `imenu-default-create-index-function' and we should return it as-is.
  (if (arrayp pos)
      (eglot--lsp-position-to-point (plist-get (plist-get (aref pos 0) :range) :start) t)
    pos))

(defun imenu-list-position-translator ()
  "Get the correct position translator function for the current buffer.
A position translator is a function that takes a position as described in
`imenu--index-alist' and returns a number or marker that points to the
real position in the buffer that the position parameter points to.
This is necessary because positions in `imenu--index-alist' do not have to
be numbers or markers, although usually they are.  For example,
`semantic-create-imenu-index' uses overlays as position paramters.
If `imenu-list-custom-position-translator' is non-nil, then
`imenu-list-position-translator' asks it for a translator function.
If `imenu-list-custom-position-translator' is called and returns nil, then
continue with the regular logic to find a translator function."
  (cond
   ((and imenu-list-custom-position-translator
         (funcall imenu-list-custom-position-translator)))
   ((or (eq imenu-create-index-function 'semantic-create-imenu-index)
        (and (eq imenu-create-index-function
                 'spacemacs/python-imenu-create-index-python-or-semantic)
             (bound-and-true-p semantic-mode)))
    ;; semantic uses overlays, return overlay's start as position
    #'overlay-start)
   ((and (fboundp #'eglot-managed-p) (eglot-managed-p))
    #'imenu-list--translate-eglot-position)
   ;; default - return position as is
   (t #'identity)))

(defun imenu-list--current-entry ()
  "Find entry in `imenu-list--line-entries' matching current position."
  (let ((point-pos (point-marker))
        (offset (point-min-marker))
        (get-pos-fn (imenu-list-position-translator))
        match-entry)
    (dolist (entry imenu-list--line-entries match-entry)
      (unless (imenu--subalist-p entry)
        (let* ((entry-pos-raw (cdr entry))
               ;; sometimes imenu doesn't use numbers/markers as positions, so we
               ;; need to translate them back to "real" positions
               ;; (see https://github.com/bmag/imenu-list/issues/20)
               (entry-pos (funcall get-pos-fn entry-pos-raw)))
          (when (<= offset entry-pos point-pos)
            (setq offset entry-pos)
            (setq match-entry entry)))))))

(defun imenu-list--show-current-entry ()
  "Move the imenu-list buffer's point to the current position's entry."
  (when (get-buffer-window (imenu-list-get-buffer-create))
    (let ((line-number (cl-position (imenu-list--current-entry)
                                    imenu-list--line-entries
                                    :test 'equal)))
      (with-selected-window (get-buffer-window (imenu-list-get-buffer-create))
        (goto-char (point-min))
        (forward-line line-number)
        (hl-line-mode 1)))))

;;; window display settings

(defcustom imenu-list-size 0.3
  "Size (height or width) for the imenu-list buffer.
Either a positive integer (number of rows/columns) or a percentage."
  :group 'imenu-list
  :type 'number)

(defcustom imenu-list-position 'right
  "Position of the imenu-list buffer.
Either 'right, 'left, 'above, 'below or 'none.  'none means leave
`display-buffer-alist` alone and let user deal with window management."
  :group 'imenu-list
  :type '(choice (const above)
                 (const below)
                 (const left)
                 (const right)
                 (const none)))

(defcustom imenu-list-auto-resize nil
  "If non-nil, auto-resize window after updating the imenu-list buffer.
Resizing the width works only for Emacs 24.4 and newer.  Resizing the
height doesn't suffer that limitation."
  :group 'imenu-list
  :type 'boolean)

(defcustom imenu-list-update-hook nil
  "Hook to run after updating the imenu-list buffer."
  :group 'imenu-list
  :type 'hook)

(defun imenu-list-split-size ()
  "Convert `imenu-list-size' to proper argument for `split-window'."
  (let ((frame-size (if (member imenu-list-position '(left right))
                        (frame-width)
                      (frame-height))))
    (cond ((integerp imenu-list-size) (- imenu-list-size))
          (t (- (round (* frame-size imenu-list-size)))))))

(defun imenu-list-display-buffer (buffer alist)
  "Display the imenu-list buffer at the side.
This function should be used with `display-buffer-alist'.
See `display-buffer-alist' for a description of BUFFER and ALIST."
  (or (get-buffer-window buffer)
      (let ((window (ignore-errors (split-window (frame-root-window) (imenu-list-split-size) imenu-list-position))))
        (when window
          ;; since Emacs 27.0.50, `window--display-buffer' doesn't take a
          ;; `dedicated' argument, so instead call `set-window-dedicated-p'
          ;; directly (works both on new and old Emacs versions)
          (window--display-buffer buffer window 'window alist)
          (set-window-dedicated-p window t)
          window))))

(defun imenu-list-install-display-buffer ()
  "Install imenu-list display settings to `display-buffer-alist'."
  (cl-pushnew `(,(concat "^" (regexp-quote imenu-list-buffer-name) "$")
                imenu-list-display-buffer)
              display-buffer-alist
              :test #'equal))

(defun imenu-list-purpose-display-condition (_purpose buffer _alist)
  "Display condition for use with window-purpose.
Return t if BUFFER is the imenu-list buffer.

This function should be used in `purpose-special-action-sequences'.
See `purpose-special-action-sequences' for a description of _PURPOSE,
BUFFER and _ALIST."
  (string-equal (buffer-name buffer) imenu-list-buffer-name))

;; hide false-positive byte-compile warning
(defvar purpose-special-action-sequences)

(defun imenu-list-install-purpose-display ()
  "Install imenu-list display settings for window-purpose.
Install entry for imenu-list in `purpose-special-action-sequences'."
  (cl-pushnew '(imenu-list-purpose-display-condition imenu-list-display-buffer)
              purpose-special-action-sequences
              :test #'equal))

(eval-after-load 'window-purpose
  '(imenu-list-install-purpose-display))


;;; imenu-list buffer management

(defvar imenu-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "g") #'imenu-list-refresh)
    (define-key map (kbd "q") #'imenu-list-quit-window)
    map))

(define-derived-mode imenu-list-mode special-mode "Ilist"
  "Major mode for showing the `imenu' entries of a buffer (an Ilist).
\\{imenu-list-mode-map}"
  (setq-local mode-line-format imenu-list-mode-line-format)
  (read-only-mode 1))

(defun imenu-list-get-buffer-create ()
  (or (get-buffer imenu-list-buffer-name)
      (let ((buffer (get-buffer-create imenu-list-buffer-name)))
        (with-current-buffer buffer
          (imenu-list-mode))
        buffer)))

(defun imenu-list-resize-window ()
  "Resize imenu-list window according to its content."
  (when imenu-list--line-entries
    (let ((fit-window-to-buffer-horizontally t))
      (mapc #'fit-window-to-buffer
            (get-buffer-window-list (imenu-list-get-buffer-create))))))

(defun imenu-list-update (&optional force-update)
  "Update the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it.
If FORCE-UPDATE is non-nil, the imenu-list buffer is updated even if the
imenu entries did not change since the last update."
  (catch 'index-failure
    (let ((old-entries imenu-list--imenu-entries)
          (location (point-marker)))
      ;; don't update if `point' didn't move
      (unless (and (null force-update)
                   imenu-list--last-location
                   (marker-buffer imenu-list--last-location)
                   (= location imenu-list--last-location))
        (setq imenu-list--last-location location)
        (condition-case err
            (imenu-list-collect-entries)
          (imenu-unavailable (if imenu-list-persist-when-imenu-index-unavailable
                                 (throw 'index-failure nil)
                               (imenu-list-clear))))
        (when (or force-update
                  ;; check if Ilist buffer is alive, in case it was killed
                  ;; since last update
                  (null (get-buffer imenu-list-buffer-name))
                  (not (equal old-entries imenu-list--imenu-entries)))
          (with-current-buffer (imenu-list-get-buffer-create)
            (imenu-list-insert-entries)))
        (when imenu-list-update-current-entry
          (imenu-list--show-current-entry))
        (when imenu-list-auto-resize
          (imenu-list-resize-window))
        (run-hooks 'imenu-list-update-hook)
        nil))))

(defun imenu-list-clear ()
  "Clear the imenu-list buffer."
  (let ((imenu-buffer (get-buffer imenu-list-buffer-name)))
    (when imenu-buffer
      (setq imenu-list--imenu-entries nil
            imenu-list--line-entries nil)
      (with-current-buffer imenu-buffer
        (let ((inhibit-read-only t))
          (erase-buffer))))))

(defun imenu-list-refresh ()
  "Refresh imenu-list buffer."
  (interactive)
  (with-current-buffer imenu-list--displayed-buffer
    (imenu-list-update t)))

(defun imenu-list-show ()
  "Show the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (pop-to-buffer imenu-list-buffer-name))

(defun imenu-list-show-noselect ()
  "Show the imenu-list buffer, but don't select it.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (display-buffer imenu-list-buffer-name))

;;;###autoload
(defun imenu-list-noselect ()
  "Update and show the imenu-list buffer, but don't select it.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (imenu-list-update)
  (imenu-list-show-noselect))

;;;###autoload
(defun imenu-list ()
  "Update and show the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (imenu-list-update)
  (imenu-list-show))

;; hide false-positive byte-compile warning
(defvar global-imenu-list-mode)

(defun imenu-list-quit-window ()
  "Disable `global-imenu-list-mode' and hide the imenu-list buffer.
If `global-imenu-list-mode' is already disabled, just call `quit-window'."
  (interactive)
  ;; the reason not to call `(global-imenu-list-mode -1)' regardless of current
  ;; state, is that it quits all of imenu-list windows instead of just the
  ;; current one.
  (if global-imenu-list-mode
      ;; disabling `global-imenu-list-mode' also quits the window
      (global-imenu-list-mode -1)
    (quit-window)))

;;; define minor mode

(defvar imenu-list--timer nil)

(defcustom imenu-list-idle-update-delay idle-update-delay
  "Idle time delay before automatically updating the imenu-list buffer."
  :group 'imenu-list
  :type 'number
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (prog1 (set-default sym val)
           (when imenu-list--timer (imenu-list-start-timer)))))

(defun imenu-list-start-timer ()
  "Start timer to auto-update imenu-list index and window."
  (imenu-list-stop-timer)
  (setq imenu-list--timer
        (run-with-idle-timer imenu-list-idle-update-delay t
                             #'imenu-list-update)))

(defun imenu-list-stop-timer ()
  "Stop timer to auto-update imenu-list index and window."
  (when imenu-list--timer
    (cancel-timer imenu-list--timer)
    (setq imenu-list--timer nil)))

(defcustom imenu-list-auto-update t
  "Whether imenu-list should automatically update its index.
If non-nil, imenu-list automatically updates the entries of its
index every `imenu-list-idle-update-delay' seconds.  When
updating this value from Lisp code, you should call
`imenu-list-start-timer' or `imenu-list-stop-timer' explicitly
afterwards."
  :group 'imenu-list
  :type 'boolean
  :set (lambda (sym val)
         (prog1 (set-default sym val)
           (if (and val (bound-and-true-p imenu-list-mode))
               (imenu-list-start-timer)
             (imenu-list-stop-timer)))))

;;;###autoload
(define-minor-mode global-imenu-list-mode
  nil :global t :group 'imenu-list
  (if global-imenu-list-mode
      (progn
        (imenu-list-get-buffer-create)
        (when imenu-list-auto-update
          (imenu-list-start-timer))
        (let ((orig-buffer (current-buffer)))
          (if imenu-list-focus-after-activation
              (imenu-list-show)
            (imenu-list-show-noselect))
          (with-current-buffer orig-buffer
            (imenu-list-update t))))
    (imenu-list-stop-timer)
    (ignore-errors (quit-windows-on imenu-list-buffer-name))
    ;; make sure *Ilist* is buried even if it wasn't shown in any window
    (when (get-buffer imenu-list-buffer-name)
      (bury-buffer (get-buffer imenu-list-buffer-name)))))

;;;###autoload
(defun imenu-list-smart-toggle ()
  "Enable or disable `global-imenu-list-mode' according to buffer's visibility.
If the imenu-list buffer is displayed in any window, disable
`global-imenu-list-mode', otherwise enable it.
Note that all the windows in every frame searched, even invisible ones, not
only those in the selected frame."
  (interactive)
  (if (get-buffer-window imenu-list-buffer-name t)
      (global-imenu-list-mode -1)
    (global-imenu-list-mode 1)))

(provide 'imenu-list)

;;; imenu-list.el ends here
