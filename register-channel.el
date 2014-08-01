;;; register-channel.el --- Jump around fast using registers

;; Copyright 2011 Google Inc. All Rights Reserved.

;; Author: Yang Zhao <zhyang@google.com>
;; Maintainer: Yang Zhao <zhyang@google.com>
;; Created: Mar 29 2011
;; Version: 0.1

;; To use, put this in your init file:
;; (require 'register-channel)
;; (register-channel-mode 1)
;;
;; Then you can use ESC ESC 1 to put point position into register
;; 1. This works for register 1 to 5; 6 and 7 by default holds texts,
;; 8 and 9 holds window configurations. To utilize these registers,
;; use M-1 etc. The old position / window configuration are
;; automatically stored into register 0, so you can easily go back.

(require 'register)

(defvar register-channel-backup-register ?`
  "The backup register used to save the current point / window
  configuration etc. when you do register-channel switching.")

(setq register-channel-last-save-type nil)
(defun register-channel-save-backup (type register-val)
  (unless (and (eq last-command 'jump-or-insert-self-register)
               (eq register-channel-last-save-type type))
    (set-register register-channel-backup-register register-val)
    (setq register-channel-last-save-type type)))

(defun register-channel-last-command-char ()
  "Returns the character corresponding to last command, stripping
  any modifiers. E.g. if last command is M-1, should return 1."
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
  ;; Refer to code in digit-argument and register.el.
  (let* ((register (register-channel-last-command-char))
         (val (get-register register)))
    (cond ((not val)
           (message "No content in register %c" register))
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

(defun register-channel-save-point (&optional arg)
  "Save point to register defined by last key press. E.g. if this
function is bound to ESC M-1, the point is saved in register 1."
  (interactive "P")
  (let ((digit-char (register-channel-last-command-char)))
    (point-to-register digit-char)
    (set-marker-insertion-type (get-register digit-char) arg)
    (message "Point stored in register %c" digit-char)))

(defun register-channel-save-text (&optional delete-flag)
  "Copy region to register defined by last key press."
  (interactive "P")
  (let ((digit-char (register-channel-last-command-char)))
    (copy-to-register digit-char
                      (region-beginning) (region-end) delete-flag)
    (let* ((killed-text (get-register digit-char))
           (message-len (min (length killed-text) 40)))
      (if (= (point) (region-beginning))
          (message "Saved text until \"%s\" to register %c"
                   (substring killed-text (- message-len)) digit-char)
        (message "Saved text from \"%s\" to register %c"
                 (substring killed-text 0 message-len) digit-char)))))

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
  (define-key map (kbd "M-g M-1") 'register-channel-save-point)
  (define-key map (kbd "M-g M-2") 'register-channel-save-point)
  (define-key map (kbd "M-g M-3") 'register-channel-save-point)
  (define-key map (kbd "M-g M-4") 'register-channel-save-point)
  (define-key map (kbd "M-g M-5") 'register-channel-save-point)
  (define-key map (kbd "M-g M-6") 'register-channel-save-window-configuration)
  (define-key map (kbd "M-g M-7") 'register-channel-save-window-configuration)
  (define-key map (kbd "M-g M-8") 'register-channel-save-window-configuration)
  (define-key map (kbd "M-`") 'register-channel-jump-or-insert)
  (define-key map (kbd "M-1") 'register-channel-jump-or-insert)
  (define-key map (kbd "M-2") 'register-channel-jump-or-insert)
  (define-key map (kbd "M-3") 'register-channel-jump-or-insert)
  (define-key map (kbd "M-4") 'register-channel-jump-or-insert)
  (define-key map (kbd "M-5") 'register-channel-jump-or-insert)
  (define-key map (kbd "M-6") 'register-channel-jump-or-insert)
  (define-key map (kbd "M-7") 'register-channel-jump-or-insert)
  (define-key map (kbd "M-8") 'register-channel-jump-or-insert)
  map))

(defvar register-channel-mode-map (register-channel-default-keymap)
  "Key map for register-channel minor mode")

;;;###autoload
(define-minor-mode register-channel-mode
  "Toggle register-channel mode"
  :keymap register-channel-mode-map
  :global t)

(provide 'register-channel)
