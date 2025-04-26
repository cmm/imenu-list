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

(defvar-local imenu-list--buffer nil
  "The imenu-list buffer serving this one")

(defvar-local imenu-list--client-buffer nil
  "The buffer that this imenu-list buffer serves.")

(defvar imenu-list--last-location nil
  "Location from which last `imenu-list--update' was done.
Used to avoid updating if the point didn't move.")

;;; fancy display

(defgroup imenu-list nil
  "Variables for `imenu-list' package."
  :group 'imenu)

(defcustom imenu-list-mode-line-format
  '("%e" mode-line-front-space
    " Imenu "
    (:eval (buffer-name imenu-list--client-buffer)) " "
    mode-line-end-spaces)
  "Local mode-line format for the imenu-list buffer.
This is the local value of `mode-line-format' to use in the imenu-list
buffer.  See `mode-line-format' for allowed values."
  :group 'imenu-list
  :type 'sexp)

(defcustom imenu-list-custom-position-translator nil
  "Custom translator of imenu positions to buffer positions.
Imenu can be customized on a per-buffer basis not to use regular buffer
positions as the positions that are stored in the imenu index.  In such
cases, imenu-list needs to know how to translate imenu positions back to
buffer positions.  `imenu-list-custom-position-translator' should be a
function that returns a position-translator function suitable for the
current buffer, or nil.  See `imenu-list--position-translator' for details."
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

;;; collect entries

(defmacro with-imenu-list--buffer (&rest body)
  (let ((buf (gensym)))
    `(when-let ((,buf (imenu-list--get-buffer)))
       (with-current-buffer ,buf
         ,@body))))

(defun imenu-list--get-buffer (&optional create)
  (if (eq major-mode 'imenu-list-mode)
      (current-buffer)
    (let ((buffer (when (buffer-live-p imenu-list--buffer)
                    imenu-list--buffer)))
      (setq imenu-list--buffer
            (or buffer
                (when create
                  (let ((client (current-buffer))
                        (buffer (get-buffer-create (format " *Imenu: %s*" (buffer-name)))))
                    (with-current-buffer buffer
                      (imenu-list-mode)
                      (setq imenu-list--client-buffer client))
                    buffer)))))))

(defvar-local imenu-list--buffer-changed-since-last-update nil)

(defun imenu-list--collect-entries (force-update)
  "Collect all `imenu' entries of the current buffer."
  (when (or force-update
            (null imenu--index-alist)
            (and imenu-list--buffer-changed-since-last-update
                 (<= (buffer-size) imenu-auto-rescan-maxout)))
    (imenu--make-index-alist)
    (setq imenu-list--buffer-changed-since-last-update nil)))

;;; print entries

(defun imenu-list--event-ilist-buffer (event)
  (when-let* ((window (posn-window (event-end event)))
              (buffer (window-buffer window)))
    (when (eq (with-current-buffer buffer major-mode) 'imenu-list-mode)
      buffer)))

(defun imenu-list--action-goto-entry (event item)
  "Goto the entry that was clicked.
EVENT is the click event, ITEM is the item clocked on."
  (with-imenu-list--buffer
   (imenu-list--goto-entry item)))

(defvar-local imenu-list--pos-entries nil)

(defun imenu-list--insert-entries (index-alist)
  (with-imenu-list--buffer
   (let ((inhibit-read-only t)
         (idx 0)
         pos-entries
         last-icon)
     (erase-buffer)
     (cl-labels ((sorted (items)
                   (sort items :key (lambda (item)
                                      (let* ((raw-pos (imenu-list--item-pos item)))
                                        (cond
                                         ((markerp raw-pos) (or (marker-position raw-pos) -1))
                                         ((null raw-pos) 0)
                                         (t raw-pos))))))
                 (get-face (path clickablep)
                   (let ((depth (length path)))
                     (cl-ecase (% depth 4)
                       (0 (if clickablep 'imenu-list-entry-clickable-tag-face-0 'imenu-list-entry-face-0))
                       (1 (if clickablep 'imenu-list-entry-clickable-tag-face-1 'imenu-list-entry-face-1))
                       (2 (if clickablep 'imenu-list-entry-clickable-tag-face-2 'imenu-list-entry-face-2))
                       (3 (if clickablep 'imenu-list-entry-clickable-tag-face-3 'imenu-list-entry-face-3)))))
                 (bump-idx (&optional pos path name)
                   (let ((backlink (list (cl-incf idx))))
                     (when pos
                       (push (cons (cl-list* name backlink path) pos) pos-entries))
                     backlink))
                 (tracked-widget (backlink &rest args)
                   (let* ((spec (apply #'widget-convert args)))
                     (prog1 spec
                       (when backlink
                         (let ((create (widget-get spec :create)))
                           (widget-put spec :create (lambda (spec)
                                                      (let ((pos (point)))
                                                        (funcall create spec)
                                                        (let ((widget (widget-at pos)))
                                                          (while (not (member (widget-type widget) '(link tree-widget)))
                                                            (setq widget (widget-get widget :parent)))
                                                          ;; exclude buttons from tabbing
                                                          (dolist (button (widget-get widget :buttons))
                                                            (widget-put button :tab-order -1))
                                                          (setf (cdr backlink) widget))))))))))
                 (link (backlink tag face action)
                   (tracked-widget backlink 'link
                                   :tag tag
                                   :button-face face
                                   :format "%[%t%]\n"
                                   :button-prefix ""
                                   :button-suffix ""
                                   :action action
                                   :follow-link "\C-m"))
                 (widgetize (item path)
                   (cl-labels ((subalist-node (path)
                                 (let ((name (car item))
                                       (pos (imenu-list--item-pos item)))
                                   (link (when pos (bump-idx pos path name))
                                         name
                                         (get-face (cdr path) pos)
                                         (lambda (widget event)
                                           (widget-parent-action widget event)
                                           (when pos
                                             (imenu-list--goto-entry item)))))))
                     (if (imenu--subalist-p item)
                         (let* ((backlink (bump-idx))
                                (path (cons backlink path)))
                           (tracked-widget backlink 'tree-widget
                                           :node (subalist-node path)
                                           :args (mapcar (lambda (item)
                                                           (widgetize item path))
                                                         (sorted (cdr item)))))
                       (link (bump-idx (imenu-list--item-pos item) path (car item))
                             (car item)
                             (get-face path nil)
                             (lambda (_ __)
                               (imenu-list--goto-entry item)))))))
       (let* ((entries (sorted index-alist))
              path
              (root-tree (when (cdr entries)
                           (let ((backlink (list idx)))
                             (setq path (list backlink))
                             (tracked-widget backlink 'tree-widget
                                             :idx  idx
                                             :node (link (bump-idx)
                                                         "*root*"
                                                         'imenu-list-entry-face-0
                                                         (lambda (widget event)
                                                           (widget-parent-action widget event)))
                                             :args (mapcar (lambda (entry)
                                                             (widgetize entry path))
                                                           entries))))))
         (widget-create (or root-tree
                            (widgetize (car entries) path)))
         (widget-setup)))
     (setq imenu-list--pos-entries (sort (vconcat pos-entries) :key #'cdr :in-place t)))
   (goto-char (point-min))))

;;; goto entries

(defcustom imenu-list-after-jump-hook '(recenter)
  "Hook to run after jumping to an entry from the imenu-list buffer."
  :group 'imenu-list
  :type 'hook)

(cl-defun imenu-list--lower-bound (vec x &key (key #'identity) (lessp #'<))
  "A pretty normal \"lower-bound\" function actually.

Given a sorted vector VEC, find the greatest value that is <= X and
return its index, or NIL if not found."
  (unless (cl-plusp (length vec))
    (cl-return-from imenu-list--lower-bound nil))

  (let (from to lower upper found)
    (cl-labels ((key-at (idx)
                  (funcall key (elt vec idx)))
                (refresh (new-from new-to &optional new-lower new-upper)
                  (setq from  new-from
                        lower (or new-lower (key-at from))
                        to    new-to
                        upper (or new-upper (key-at to))
                        found from)))
      (refresh 0 (1- (length vec)))

      (when (funcall lessp x lower)
        (cl-return-from imenu-list--lower-bound nil))
      (when (funcall lessp upper x)
        (cl-return-from imenu-list--lower-bound to))

      (while (> (- to from) 1)
        (let* ((half-way (/ (+ from to) 2))
               (middle   (key-at half-way)))
          (if (funcall lessp x middle)
              (refresh from half-way lower middle)
            (refresh half-way to middle upper)))))
    found))

(defun imenu-list--find-pos-path ()
  (let ((get-pos (imenu-list--position-translator))
        (pos (point-marker)))
    (with-imenu-list--buffer
     (when-let ((idx (imenu-list--lower-bound imenu-list--pos-entries
                                              pos
                                              :key (lambda (elt) (funcall get-pos (cdr elt))))))
       (car (elt imenu-list--pos-entries idx))))))

(defun imenu-list--item-pos (item)
  (or
   (when-let ((br (get-char-property 0 'breadcrumb-region (car item))))
     (car br))
   (cond
    ((imenu--subalist-p item) nil)
    ((consp (cdr item))       (cadr item))
    (t                        (cdr item)))))

(defun imenu-list--goto-entry (item)
  "Switch to the original buffer and display the entry under point."
  (interactive)
  (when-let ((pos (imenu-list--item-pos item)))
    (pop-to-buffer imenu-list--client-buffer)
    (goto-char pos)
    (run-hooks 'imenu-list-after-jump-hook)
    (imenu-list--hl-current-entry)))

(defun imenu-list--position-translator ()
  "Get the correct position translator function for the current buffer.
A position translator is a function that takes a position as described in
`imenu--index-alist' and returns a number or marker that points to the
real position in the buffer that the position parameter points to.
This is necessary because positions in `imenu--index-alist' do not have to
be numbers or markers, although usually they are.  For example,
`semantic-create-imenu-index' uses overlays as position paramters.
If `imenu-list-custom-position-translator' is non-nil, then
`imenu-list--position-translator' asks it for a translator function.
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
   ;; default - return position as is
   (t #'identity)))

(cl-defun imenu-list--hl-current-entry ()
  (when-let ((path (imenu-list--find-pos-path)))
    (cl-labels ((rec (path)
                  (when-let ((backlink (car path)))
                    (cond
                     ((stringp backlink)
                      (cl-assert (string= (buffer-substring (point) (+ (point) (length backlink))) backlink))
                      (hl-line-mode 1))
                     (t
                      (when-let ((widget (cdr backlink)))
                        (cl-ecase (widget-type widget)
                          (link
                           (goto-char (widget-get widget :from)))
                          (tree-widget
                           (unless (widget-get widget :open)
                             (widget-apply-action widget)
                             (cl-return-from imenu-list--hl-current-entry))))
                        (rec (cdr path))))))))
      (with-selected-window (get-buffer-window (imenu-list--get-buffer))
        (rec (reverse path))))))

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

(defcustom imenu-list-update-hook nil
  "Hook to run after updating the imenu-list buffer."
  :group 'imenu-list
  :type 'hook)

(defun imenu-list--split-size ()
  "Convert `imenu-list-size' to proper argument for `split-window'."
  (let ((frame-size (if (member imenu-list-position '(left right))
                        (frame-width)
                      (frame-height))))
    (cond ((integerp imenu-list-size) (- imenu-list-size))
          (t (- (round (* frame-size imenu-list-size)))))))

(defun imenu-list--get-window ()
  (cl-find-if (lambda (window)
                (eq (with-current-buffer (window-buffer window)) major-mode) 'imenu-list-mode)
              (window-list)))

(defun imenu-list--display-buffer (buffer alist)
  "Display the imenu-list buffer at the side.
This function should be used with `display-buffer-alist'.
See `display-buffer-alist' for a description of BUFFER and ALIST."
  (or (imenu-list-window)
      (let ((window (ignore-errors (split-window (frame-root-window) (imenu-list--split-size) imenu-list-position))))
        (when window
          (window--display-buffer buffer window 'window alist)
          (set-window-dedicated-p window t)
          window))))

(defun imenu-list-install-display-buffer ()
  "Install imenu-list display settings to `display-buffer-alist'."
  (cl-pushnew '((derived-mode . imenu-list-mode)
                imenu-list--display-buffer)
              display-buffer-alist
              :test #'equal))

;;; imenu-list buffer management

(defvar imenu-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "g") #'imenu-list-refresh)
    (define-key map (kbd "q") #'imenu-list--quit-window)
    map))

(define-derived-mode imenu-list-mode special-mode "Ilist"
  "Major mode for showing the `imenu' entries of a buffer (an Ilist).
\\{imenu-list-mode-map}"
  (setq-local mode-line-format imenu-list-mode-line-format)
  (read-only-mode 1))

(defun imenu-list--imenu-available-p ()
  ;; this is lifted from imenu implementation, which is gross
  (or (and imenu-prev-index-position-function
	       imenu-extract-index-name-function)
	  imenu-generic-expression
	  (not (eq imenu-create-index-function
		   'imenu-default-create-index-function))))

(defun imenu-list--after-change (&rest _)
  (if global-imenu-list-mode
      (setq imenu-list--buffer-changed-since-last-update t)
    (delete 'imenu-list--after-change 'after-change-functions)))

(cl-defun imenu-list--update (&optional force-update)
  "Update the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it.
If FORCE-UPDATE is non-nil, the imenu-list buffer is updated even if the
imenu entries did not change since the last update."
  (unless global-imenu-list-mode
    (imenu-list--stop-timer)
    (cl-return-from imenu-list--update nil))

  ;; do nothing in imenu-list buffers themselves or if no window
  ;; displays imenu-list, and also if imenu is not available
  (unless (and (not (eq major-mode 'imenu-list-mode))
               (imenu-list--imenu-available-p)
               (imenu-list--get-window))
    (cl-return-from imenu-list--update nil))

  (add-to-list 'after-change-functions 'imenu-list--after-change)
  (setq-local imenu-auto-rescan nil)

  (imenu-list-show-noselect)

  (let ((old-entries imenu--index-alist)
        (location (point-marker)))
    ;; don't update if `point' didn't move
    (unless (and (marker-buffer location)
                 (null force-update)
                 imenu-list--last-location
                 (marker-buffer imenu-list--last-location)
                 (= location imenu-list--last-location))
      (imenu-list--collect-entries force-update)
      (setq imenu-list--last-location location)
      (when (or force-update
                (not (equal old-entries imenu--index-alist)))
        (imenu-list--insert-entries imenu--index-alist))
      (imenu-list--hl-current-entry)
      (run-hooks 'imenu-list-update-hook)
      nil)))

(defun imenu-list-refresh ()
  "Refresh imenu-list buffer."
  (interactive)
  (imenu-list--update t))

(defun imenu-list-show-noselect ()
  "Show the imenu-list buffer, but don't select it.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (display-buffer (imenu-list--get-buffer t)))

(defun imenu-list--quit-window ()
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
           (when imenu-list--timer
             (imenu-list--start-timer)))))

(defun imenu-list--start-timer ()
  "Start timer to auto-update imenu-list index and window."
  (imenu-list--stop-timer)
  (setq imenu-list--timer
        (run-with-idle-timer imenu-list-idle-update-delay t
                             'imenu-list--update)))

(defun imenu-list--stop-timer ()
  "Stop timer to auto-update imenu-list index and window."
  (when imenu-list--timer
    (cancel-timer imenu-list--timer)
    (setq imenu-list--timer nil)))

(defcustom imenu-list-auto-update t
  "Whether imenu-list should automatically update its index.
If non-nil, imenu-list automatically updates the entries of its index
every `imenu-list-idle-update-delay' seconds, if the buffer contents
changed.  When updating this value from Lisp code, you should call
`imenu-list--start-timer' or `imenu-list--stop-timer' explicitly
afterwards.  `imenu-auto-rescan-maxout' is observed."
  :group 'imenu-list
  :type 'boolean
  :set (lambda (sym val)
         (prog1 (set-default sym val)
           (if (and val (bound-and-true-p imenu-list-mode))
               (imenu-list--start-timer)
             (imenu-list--stop-timer)))))

;;;###autoload
(define-minor-mode global-imenu-list-mode
  nil :global t :group 'imenu-list
  (if global-imenu-list-mode
      (progn
        (when imenu-list-auto-update
          (imenu-list--start-timer))
        (imenu-list-show-noselect)
        (imenu-list--update t))
    (imenu-list--stop-timer)
    (dolist (buffer (buffer-list))
      (when (eq (with-current-buffer buffer major-mode) 'imenu-list-mode)
        (ignore-errors (quit-windows-on buffer))))))

(provide 'imenu-list)

;;; imenu-list.el ends here
