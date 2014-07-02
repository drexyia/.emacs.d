(defun drexyia-kill-this-buffer () 
  (interactive) 
  (kill-buffer (current-buffer)))

(defun drexyia-revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

(defun drexyia-use-proxy ()
  "sets a default proxy this can be used with fiddler to see tracing information"
  (interactive)
  (setq url-proxy-services  '(("http" . "localhost:8888")
                              ("https" . "localhost:8888"))))

(defun drexyia-words-count-region (start end)
  "displays the number of words in the region"
  (interactive "r")
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (< (point) end)
	(if (forward-word 1)
	    (setq n (1+ n))))
      (message "Region has %d words" n)
      n)))

(defun drexyia-line-select-current ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun drexyia-find-file-name ()
  "calls find-name-dired"
  (interactive)
  (find-name-dired))

(defun drexyia-kill-ring-clear ()
  "clears the kill ring"
  (interactive)
  (setq kill-ring nil))

(defun drexyia-buffer-view-unsaved-changes ()
  "calls ediff-current-file"
  (interactive)
  (ediff-current-file))

(defun drexyia-buffer-word-wrap-toggle ()
  "toggles word wrapping"
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t)))

(defun drexyia-dired-rename-files ()
  "calls wdired-change-to-wdired-mode"
  (interactive)
  (wdired-change-to-wdired-mode))

(defun drexyia-xml-pretty-print ()
  "pretty print an xml document"
  (interactive)
  (sgml-mode)
  (sgml-pretty-print (point-min) (point-max)))

(defun drexyia-split-window-vertically-reduced ()
  "split window vertically with the new window being reduced in size"
  (interactive)
  (split-window-vertically 35))

(defun drexyia-eshell-clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun drexyia-clear-screen ()
  "calls erase-buffer function"
  (interactive)
  (erase-buffer))

;;cycle through buffers left right
(defun drexyia-forward-word-or-buffer-or-windows (&optional arg)
  "Enable <C-left> to call next-buffer if the last command was
next-buffer or previous-buffer, and winner-redo if the last
command was winner-undo or winner-redo."
  (interactive "p")
  (cond ((memq last-command (list 'next-buffer 'previous-buffer))
         (progn (next-buffer)
                (setq this-command 'next-buffer)))
        ((memq last-command (list 'winner-redo 'winner-undo))
         (progn (winner-redo)
                (setq this-command 'winner-redo)))
        (t ;else
         (progn (forward-word arg)
                (setq this-command 'forward-word)))))

(defun drexyia-backward-word-or-buffer-or-windows (&optional arg)
  "Enable <C-left> to call previous-buffer if the last command
was next-buffer or previous-buffer, and winner-undo if the last
command was winner-undo or winner-redo."
  (interactive "p")
  (cond ((memq last-command (list 'next-buffer 'previous-buffer))
         (progn (previous-buffer)
                (setq this-command 'previous-buffer)))
        ((memq last-command (list 'winner-redo 'winner-undo))
         (progn (winner-undo)
                (setq this-command 'winner-undo)))
        (t ;else
         (progn (backward-word arg)
                (setq this-command 'backward-word)))))

(defun drexyia-path-add-directory (path-element)
 "Add the specified path element to the Emacs PATH"
  (interactive "DEnter directory to be added to path: ")
  (if (file-directory-p path-element)
    (setenv "PATH"
       (concat (expand-file-name path-element)
               path-separator (getenv "PATH")))))

(defun drexyia-dired-kill-all-buffers()
 "Kill all dired buffers."
 (interactive)
 (save-excursion
   (let((count 0))
     (dolist(buffer (buffer-list))
       (set-buffer buffer)
       (when (equal major-mode 'dired-mode)
         (setq count (1+ count))
         (kill-buffer buffer)))
     (message "Killed %i dired buffer(s)." count ))))

(defun drexyia-buffer-delete-with-file ()
  "Deletes the current file and buffer, assumes file exists"
  (interactive)
  (delete-file buffer-file-name)
  (kill-buffer (buffer-name)))

;; Originally from stevey, adapted to support moving to a new directory.
(defun drexyia-buffer-rename-with-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
  (message "Renamed to %s." new-name)))

(defun drexyia-windows-swap ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

(defun drexyia-multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(defun drexyia-split-window-quarter ()
  "calls split-window-below and makes the lower window 0.25 of full window"
  (interactive)
  (let (var-three-quarter-window-height)
    (setq var-three-quarter-window-height (round (* (window-body-height) 0.75)))
    (split-window-below var-three-quarter-window-height)
    ))

;;http://stackoverflow.com/questions/2178850/how-to-copy-to-clipboard-in-emacs-lisp
(defun drexyia-copy-buffer-file-name ()
  "copies the full file name to the clipboard"
  (interactive)
  (kill-new (buffer-file-name)))

;;open file for selected region e.g highlight z:/src/elisp/notes.el and run function
(defun drexyia-find-file-for-region ()
  (interactive)
  (find-file (buffer-substring (mark) (point)))
)

(defun drexyia-jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.This is the same as using \\[set-mark-command] with the prefix argument."  
  (interactive)  
  (set-mark-command 1))

(defun drexyia-push-mark-no-activate () 
 "Pushes `point' to `mark-ring' and does not activate the regionEquivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled" 
 (interactive) 
 (push-mark (point) t nil) 
 (message "Pushed mark to ring"))

(defun drexyia-unix-file-line-ending ()
  "Change the current buffer to unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix t))

(defun drexyia-dos-file-line-ending ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun drexyia-collapse-blank-lines ()
  "Deletes all extra blank lines, leaves single lines"
  (interactive)
  (replace-regexp "^\n\\{2,\\}" "\n" nil (point-min) (point-max)))

(defun drexyia-open-folder-in-explorer-from-buffer ()  
  "Call when editing a file in a buffer. Open windows explorer in the current directory and select the current file"  
  (interactive)  
  (w32-shell-execute 
   "open" "explorer"  
   (concat "/e,/select," (convert-standard-filename buffer-file-name))))

(defun drexyia-open-folder-in-explorer ()  
  "Opens a file in windows explorer from dired or from the visited buffer if not in dired mode"
  (interactive)    
  (if (string= "dired-mode" major-mode)
      (dired-w32explore)
    (drexyia-open-folder-in-explorer-from-buffer)))

(defun drexyia-search-open-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(defun drexyia-join-line ()
  (interactive)
  (join-line -1))
               
(defun drexyia-packman-list-install (package-list)
  ;; loop over all packages if not installed
  (dolist (package package-list)          
  (if (package-installed-p package)
    (message "%s installed" package)
    (package-install package))))
