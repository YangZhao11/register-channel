;;; register-channel.el --- Jump around fast using registers

;; Copyright (C) 2014  Yang Zhao

;; Author: Yang Zhao <YangZhao11users.noreply.github.com>
;; Keywords: convenience

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;

;;; Code:

(require 'register)

(defcustom register-channel-backup-register ?`
  "The backup register used to save the current point / window
  configuration etc. when you do register-channel switching."
  :type '(character))

(setq register-channel-last-save-type nil)
(defun register-channel-save-backup (type register-val)
  (unless (and (eq last-command 'jump-or-insert-self-register)
               (eq register-channel-last-save-type type))
    (set-register register-channel-backup-register register-val)
    (setq register-channel-last-save-type type)))

(defun register-channel-last-command-char ()
  "Returns the character corresponding to last command, stripping
  any modifiers. E.g. if last command is M-1, should return 1."
  ;; C.f. digit-argument
  (let ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character))))
    (logand char ?\177)))

(defun register-channel-jump-or-insert (&optional arg)
  "Utilize the register pressed with the last key. If the command
is invoked with M-1, then the last key is `1'.

Will utilize register content intelligently, e.g. jump to
what's in register 1 if it is a position or window / frame
configuration, otherwise insert the contents.

The replaced content is saved in register
`register-channel-backup-register' (defaults to ``'), so that you
can jump back easily."
  (interactive "P")
  ;; Refer to code in register.el.
  (let* ((register (register-channel-last-command-char))
         (val (get-register register)))
    (cond ((not val)
           (user-error "No content in register %c" register))
          ((or (markerp val)
               (and (consp val)
                    (or (eq (car val) 'file)
                        (eq (car val) 'file-query))))
           (let ((save-point (point-marker)))
             (jump-to-register register arg)
             (register-channel-save-backup 'point save-point)))
          ((and (consp val)
                (frame-configuration-p (car val)))
           (let ((save-frame-configuration (current-frame-configuration))
                 (save-point (point-marker)))
             (jump-to-register register arg)
             (register-channel-save-backup
              'frame
              (list save-frame-configuration save-point))))
          ((and (consp val)
                (window-configuration-p (car val)))
           (let ((save-window-configuration (current-window-configuration))
                 (save-point (point-marker)))
             (jump-to-register register arg)
             (register-channel-save-backup
              'window
              (list save-window-configuration save-point))))
          ('t
           (insert-register register arg)))))

(defcustom register-channel-marker-advance t
"If true, by default register markers will advance when you
insert text at it."
:type '(boolean))

(defun register-channel-save-point (&optional arg)
  "Save point to register defined by last key press. E.g. if this
function is bound to ESC M-1, the point is saved in register 1."
  (interactive "P")
  (let ((digit-char (register-channel-last-command-char)))
    (point-to-register digit-char)
    (if register-channel-marker-advance
        (setq arg (not arg)))
    (set-marker-insertion-type (get-register digit-char) arg)
    (message "Point stored in register %c [%s]"
             digit-char
             (if arg "advance" "stay"))))

(defcustom register-channel-move-by-default nil
  "If true, register-channel-move-text deletes original text."
  :type '(boolean))

(defun register-channel-move-text (start end &optional delete-flag)
  "Copy region to register location."
  (interactive "r\nP")
  (if register-channel-move-by-default
      (setq delete-flag (not delete-flag)))
  (let* ((register (register-channel-last-command-char))
         (m (get-register register)))
    (if (not (markerp m))
        (user-error "Register %c is not a marker" register)
      (let ((string (filter-buffer-substring start end delete-flag)))
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (goto-char (marker-position m))
            (if (marker-insertion-type m)
                (insert-before-markers string)
              (insert string)
              (message "normal insert")))))
      (if (and (not delete-flag)
               (called-interactively-p 'interactive))
          (indicate-copied-region)))))

(defun register-channel-dwim (&optional arg)
  "Either save point to register, or move text if there is an
  active region and register contains marker."
  (interactive "P")
  (let* ((register (register-channel-last-command-char))
         (m (get-register register)))
    (if (and (use-region-p) (markerp m))
        (call-interactively 'register-channel-move-text)
      (call-interactively 'register-channel-jump-or-insert))))

(defun register-channel-save-window-configuration (&optional arg)
  "Save window configuration to register defined by last key press."
  (interactive "P")
  (let ((digit-char (register-channel-last-command-char)))
    (window-configuration-to-register digit-char arg)
    (message "Window configuration saved in register %c" digit-char)))

(defun register-channel-save-frame-configuration (&optional arg)
  "Save frame configuration to register defined by last key press."
  (interactive "P")
  (let ((digit-char (register-channel-last-command-char)))
    (frame-configuration-to-register digit-char arg)
    (message "Frame configuration saved in register %c" digit-char)))

(defun register-channel-default-keymap ()
  (let ((map (make-sparse-keymap)))
  (define-key map (kbd "M-g 1") 'register-channel-save-point)
  (define-key map (kbd "M-g 2") 'register-channel-save-point)
  (define-key map (kbd "M-g 3") 'register-channel-save-point)
  (define-key map (kbd "M-g 4") 'register-channel-save-point)
  (define-key map (kbd "M-g 5") 'register-channel-save-point)
  (define-key map (kbd "M-g 6") 'register-channel-save-window-configuration)
  (define-key map (kbd "M-g 7") 'register-channel-save-window-configuration)
  (define-key map (kbd "M-g 8") 'register-channel-save-window-configuration)
  (define-key map (kbd "M-`") 'register-channel-dwim)
  (define-key map (kbd "M-1") 'register-channel-dwim)
  (define-key map (kbd "M-2") 'register-channel-dwim)
  (define-key map (kbd "M-3") 'register-channel-dwim)
  (define-key map (kbd "M-4") 'register-channel-dwim)
  (define-key map (kbd "M-5") 'register-channel-dwim)
  (define-key map (kbd "M-6") 'register-channel-dwim)
  (define-key map (kbd "M-7") 'register-channel-dwim)
  (define-key map (kbd "M-8") 'register-channel-dwim)
  map))

(defvar register-channel-mode-map (register-channel-default-keymap)
  "Key map for register-channel minor mode")

;;;###autoload
(define-minor-mode register-channel-mode
  "Toggle register-channel mode"
  :keymap register-channel-mode-map
  :global t)

(provide 'register-channel)
;;; register-channel.el ends here
