(require 'general)

(defun longleaf-backtest ()
  "Display a custom menu for selecting a file and running a command in vterm."
  (interactive)
  (let
      ((preload-file (read-file-name "Select a preload file: ")))
    (let
      ((target-file (read-file-name "Select a target file: ")))

    ;; Create or get an existing vterm buffer
    (let ((vterm-buffer (get-buffer-create "*vterm*")))
      (with-current-buffer vterm-buffer
        ;; Start vterm if it's not already running
        (unless (eq major-mode 'vterm-mode)
          (vterm))  ;; Start vterm in the buffer if not already running
        ;; Send the shell command to vterm
        (vterm-send-string
         (concat "cd ..;./main.exe backtest --preload " preload-file " --target " target-file))
        (vterm-send-return))  ;; Simulate pressing Enter

      ;; Switch to the vterm buffer
      (switch-to-buffer vterm-buffer)
      (message "Running command in vterm: %s"
               (concat "dune exec bin/main.exe backtest --preload " preload-file " --target " target-file))))))



(map! :leader
      :desc "Longleaf Backtest" "l" #'longleaf-backtest)
