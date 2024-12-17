(require 'general)

(defvar preload-file nil)
(defvar target-file nil)

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
    (let ((command (concat "cd .. && ./main.exe backtest --preload "
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
    (let ((command (concat "cd .. && ./main.exe backtest --preload "
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
    (vterm-send-string "cd .. && ./shutdown.exe")
    (vterm-send-return)

    (message "Sent shutdown command to *shutdown-vterm*")
    ))


(map! :leader
      :desc "Longleaf" "l" nil ; Parent menu under "l"
      (:prefix ("l" . "longleaf") ; Create a submenu
       :desc "Run on last inputs" "l" #'longleaf-run-last
       :desc "Shutdown" "s" #'longleaf-shutdown
       (:prefix ("o" . "Options") ; Nested submenu under "l s"
        :desc "Select new arguments" "n" #'longleaf-backtest
        :desc "Subtask 2" "2" #'my-subtask-2)
       (:prefix ("z" . "Another submenu")
        :desc "do something" "a" #'do-something
        )
       ))
