(require 'general)

(defvar preload-file "No preload selected")
(setq preload-file "No preload selected")
(defvar target-file "No target selected")
(setq target-file "No target selected")
(defvar longleaf-directory nil)
(setq longleaf-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun set-preload ()
  (interactive)
  (setq preload-file
        (expand-file-name
         (read-file-name "Preload file: ")))
  )

(defun set-target ()
  (interactive)
  (setq target-file
        (expand-file-name
         (read-file-name "Preload file: ")))
  )

(defun longleaf-backtest ()
  (interactive)
  (setq preload-file (read-file-name "Preload file: "))
  (setq target-file (read-file-name "Target file: "))
  (let* ((run-buffer-name "*longleaf-vterm*") ;; Custom buffer name
         (run-buffer (get-buffer run-buffer-name)))
    ;; If the buffer doesn't exist, create and initialize vterm
    (unless run-buffer
      (setq run-buffer (get-buffer-create run-buffer-name))
      (with-current-buffer run-buffer
        (vterm-mode)))

    ;; Switch to the vterm buffer
    (switch-to-buffer run-buffer)

    ;; Send the command to the vterm
    (let ((command (concat longleaf-directory "main.exe backtest --preload "
                           (shell-quote-argument preload-file)
                           " --target "
                           (shell-quote-argument target-file))))
      (vterm-send-string command)
      (vterm-send-return)
      (message "Running command in *longleaf-vterm*: %s" command))))

(defun longleaf-run-last ()
  (interactive)
  (let* ((run-buffer-name "*longleaf-vterm*") ;; Custom buffer name
         (run-buffer (get-buffer run-buffer-name)))
    ;; If the buffer doesn't exist, create and initialize vterm
    (unless run-buffer
      (setq run-buffer (get-buffer-create run-buffer-name))
      (with-current-buffer run-buffer
        (vterm-mode)))

    ;; Switch to the vterm buffer
    (switch-to-buffer run-buffer)

    ;; Send the command to the vterm
    (let ((command (concat longleaf-directory "main.exe backtest --preload "
                           (shell-quote-argument preload-file)
                           " --target "
                           (shell-quote-argument target-file))))
      (vterm-send-string command)
      (vterm-send-return)
      (message "Running command in *longleaf-vterm*: %s" command))))


(defun longleaf-shutdown ()
  "Open or switch to *shutdown-vterm*, run shutdown command."
  (interactive)
  (let* ((shutdown-buffer-name "*shutdown-vterm*") ;; Custom buffer name
         (shutdown-buffer (get-buffer shutdown-buffer-name)))
    ;; If buffer doesn't exist, create and start vterm
    (unless shutdown-buffer
      (setq shutdown-buffer (get-buffer-create shutdown-buffer-name))
      (with-current-buffer shutdown-buffer
        (vterm-mode)))

    ;; Switch to the vterm buffer
    (switch-to-buffer shutdown-buffer)

    ;; Send shutdown command
    (vterm-send-string (concat longleaf-directory "shutdown.exe"))
    (vterm-send-return)

    (message "Sent shutdown command to *shutdown-vterm*")
    (kill-buffer shutdown-buffer)
    ))

(defun my-insert-interactive-option (text callback)
  "Insert TEXT with a clickable or selectable CALLBACK function."
  (let ((start (point)))
    (insert text)
    (add-text-properties
     start (point)
     `(mouse-face highlight
       face underline
       help-echo "Click or press RET to select this option"
       keymap ,(let ((map (make-sparse-keymap)))
                 (define-key map [mouse-1] callback) ;; Clickable
                 (define-key map (kbd "RET") callback) ;; Enter key
                 map)))))

(defun longleaf-status-buffer ()
  "Open a custom status buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Longleaf Status*")))
    (with-current-buffer buffer
      (longleaf-mode) ;; Activate your custom mode
      (read-only-mode -1)
      (erase-buffer)
      ;; Add content to your buffer
      (insert (propertize "Longleaf Status Buffer\n\n" 'face 'bold))
      (insert (concat "Project directory: " longleaf-directory "\n\n"))
      (insert (concat "Preload: " preload-file "\n\n"))
      (insert (concat "Target: " target-file "\n\n"))
      (insert "Commands:\n")
      (my-insert-interactive-option "1. Repeat run" #'longleaf-run-last)
      ;; (insert "1. Option 1\n")
      ;; (insert "2. Option 2\n")
      ;; Add interactive elements or buttons as needed
      (read-only-mode 1))
    (switch-to-buffer buffer)))

(define-derived-mode longleaf-mode special-mode "longleaf"
  "A custom major mode for displaying a status buffer."
  ;; Keybindings for actions
  (define-key longleaf-mode-map (kbd "q") #'kill-current-buffer))

;; Bind your mode to a key sequence
;; (map! :leader
;;       :desc "Open My Status Buffer"
;;       "l l" #'longleaf-status-buffer)

(map! :leader
      :desc "Longleaf" "l" nil ; Parent menu under "l"
      (:prefix ("l" . "longleaf") ; Create a submenu
       :desc "Longleaf status" "l" #'longleaf-status-buffer
       :desc "Set preload" "p" #'set-preload
       :desc "Set target" "t" #'set-target
       :desc "Run on last inputs" "r" #'longleaf-run-last
       :desc "Shutdown" "s" #'longleaf-shutdown
       (:prefix ("o" . "Options") ; Nested submenu under "l s"
        :desc "Select new arguments" "n" #'longleaf-backtest
        :desc "Subtask 2" "2" #'my-subtask-2)
       (:prefix ("z" . "Another submenu")
        :desc "do something" "a" #'do-something
        )
       ))
