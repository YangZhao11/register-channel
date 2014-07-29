;;; register-channel.el --- Jump around fast using registers
;;
;; To use, put this in your init file:
;; (require 'register-channel)
;; (register-channel-default-keybindings)
;;
;; Then you can use M-g M-1 to put point position into register
;; 1. This works for register 1 to 5; 6 to 8 by default hold window
;; configurations. To utilize these registers, use M-1 etc. The old
;; position / window configuration are automatically stored into
;; register ~, so you can easily go back with M-~.

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

(defun jump-or-insert-self-register (&optional arg)
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
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (register (logand char ?\177))
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

(defun point-to-self-register (&optional arg)
  "Save point to register defined by last key press. E.g. if this
function is bound to ESC M-1, the point is saved in register 1."
  (interactive "P")
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit-char (logand char ?\177)))
    (point-to-register digit-char)
    (set-marker-insertion-type (get-register digit-char) arg)
    (message "Point stored in register %c" digit-char)))

(defun copy-to-self-register (&optional delete-flag)
  "Copy region to register defined by last key press."
  (interactive "P")
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit-char (logand char ?\177)))
    (copy-to-register digit-char
                      (region-beginning) (region-end) delete-flag)
    (let* ((killed-text (get-register digit-char))
           (message-len (min (length killed-text) 40)))
      (if (= (point) (region-beginning))
          (message "Saved text until \"%s\" to register %c"
                   (substring killed-text (- message-len)) digit-char)
        (message "Saved text from \"%s\" to register %c"
                 (substring killed-text 0 message-len) digit-char)))))

(defun window-configuration-to-self-register (&optional arg)
  "Save window configuration to register defined by last key press."
  (interactive "P")
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit-char (logand char ?\177)))
    (window-configuration-to-register digit-char arg)
    (message "Window configuration saved in register %c" digit-char)))

(defun frame-configuration-to-self-register (&optional arg)
  "Save frame configuration to register defined by last key press."
  (interactive "P")
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit-char (logand char ?\177)))
    (frame-configuration-to-register digit-char arg)
    (message "Frame configuration saved in register %c" digit-char)))

;;;###autoload
(defun register-channel-default-keybindings ()
  "Default key bindings for register-channel: ESC M-1 to 5 to
save point, ESC M-6 to 8 to save window configuration. M-1 to 8
to use the saved register."
  (interactive)
  (global-set-key (kbd "M-g M-1") 'point-to-self-register)
  (global-set-key (kbd "M-g M-2") 'point-to-self-register)
  (global-set-key (kbd "M-g M-3") 'point-to-self-register)
  (global-set-key (kbd "M-g M-4") 'point-to-self-register)
  (global-set-key (kbd "M-g M-5") 'point-to-self-register)
  (global-set-key (kbd "M-g M-6") 'window-configuration-to-self-register)
  (global-set-key (kbd "M-g M-7") 'window-configuration-to-self-register)
  (global-set-key (kbd "M-g M-8") 'window-configuration-to-self-register)
  (global-set-key (kbd "M-`") 'jump-or-insert-self-register)
  (global-set-key (kbd "M-1") 'jump-or-insert-self-register)
  (global-set-key (kbd "M-2") 'jump-or-insert-self-register)
  (global-set-key (kbd "M-3") 'jump-or-insert-self-register)
  (global-set-key (kbd "M-4") 'jump-or-insert-self-register)
  (global-set-key (kbd "M-5") 'jump-or-insert-self-register)
  (global-set-key (kbd "M-6") 'jump-or-insert-self-register)
  (global-set-key (kbd "M-7") 'jump-or-insert-self-register)
  (global-set-key (kbd "M-8") 'jump-or-insert-self-register))

(provide 'register-channel)
