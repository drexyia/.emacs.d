(dolist (file
         '(".emacs.d/elisp/drexyia-core.el"
	   ".emacs.d/elisp/dired+.el"
	   ".emacs.d/elisp/key-chord.el"))
  (load-file file))

(dolist (path
         '(".emacs.d/helm"))
  (add-to-list 'load-path path))

(dolist (feature
         '(key-chord
	   dired+
	   helm-config))
  (require feature))

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           scroll-bar-mode              ; No scroll bars either.
	   menu-bar-mode
           blink-cursor-mode))          ; The blinking cursor gets old.
  (funcall mode 0))

(dolist (mode
         '(show-paren-mode            ; Highlight matching parentheses.
	   cua-selection-mode
           helm-mode
	   key-chord-mode)) 
  (funcall mode 1))

;;;; set emacs prompts

(setq confirm-nonexistent-file-or-buffer nil)
;; removes if you want to kill a buffer with a live process attached to it
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(fset 'yes-or-no-p 'y-or-n-p)

;;;; set dired funcionality

;; auto-refresh dired on file change
(add-hook 'dired-mode-hook '(lambda ()
                              'auto-revert-mode
                              (setq auto-revert-verbose nil)))
(setq dired-dwim-target t)
;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once

;;;; set key bindings

(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "<M-S-up>")    'move-text-up)
(global-set-key (kbd "<M-S-down>")  'move-text-down)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-h l") 'find-library)
(global-set-key (kbd "<f6>") 'bookmark-set)
(global-set-key (kbd "<f12>") 'menu-bar-mode)
(global-set-key (kbd "M-j") 'drexyia-join-line)
(global-set-key (kbd "C-`") (kbd "C-u C-SPC")) ;; navigate mark

;;;; set key chords

(key-chord-define-global "bb" 'helm-buffers-list)
(key-chord-define-global "kk" 'kill-buffer)
(key-chord-define-global "cc" 'comment-or-uncomment-region)
(key-chord-define-global "ss" 'drexyia-switch-to-eshell)
