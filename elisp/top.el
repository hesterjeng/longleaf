(require 'general)

(defvar preload-file nil)
(defvar target-file nil)

(defun longleaf-backtest ()
  "Display a custom menu for selecting a file and running a command in vterm."
  (interactive)
    (setq preload-file (read-file-name "Preload file: "))
    (setq target-file (read-file-name "Target file: "))

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
               (concat "dune exec bin/main.exe backtest --preload " preload-file " --target " target-file))))

(defun longleaf-run-last ()
    (interactive)
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
               (concat "dune exec bin/main.exe backtest --preload " preload-file " --target " target-file)))
)

(map! :leader
      :desc "Longleaf" "l" nil ; Parent menu under "l"
      (:prefix ("l" . "longleaf") ; Create a submenu
       :desc "Run on last inputs" "l" #'longleaf-run-last
       (:prefix ("s" . "Options") ; Nested submenu under "l s"
        :desc "Select new arguments" "n" #'longleaf-backtest
        :desc "Subtask 2" "2" #'my-subtask-2)
       (:prefix ("z" . "Another submenu")
        :desc "do something" "a" #'do-something
                )
       ))
