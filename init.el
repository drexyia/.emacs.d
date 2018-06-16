(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; ; activate all the packages (in particular autoloads)
(package-initialize)

(setq visible-bell 1)
(tool-bar-mode -1)
(ido-mode 1)

(dolist (mode
         '(blink-cursor-mode))          ; The blinking cursor gets old.
  (funcall mode 0))

(dolist (mode
         '(show-paren-mode            ; Highlight matching parethenses.
           key-chord-mode))
  (funcall mode 1))

(setq set-mark-command-repeat-pop t)

(dolist (var-to-set
         '(ido-everywhere
           ido-enable-flex-matching
           set-mark-command-repeat-pop
           dired-dwim-target
           org-src-fontify-natively
           case-fold-search
           inhibit-startup-message
           inhibit-startup-echo-area-message
           ))
  (setq var-to-set t))

;;;; Key Bindings

(windmove-default-keybindings)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-t") 'auto-revert-tail-mode)

;;;; Ibuffer

(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("dired" (or
			 (mode . dired-mode)
			 (mode . wdired-mode)))
	       ("emacs" (or
			 (name . "^\\*")
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (name . "^\\*Completions\\*$")
			 (name . "^\\*grep\\*$")
			 (name . "^\\*Open Recent\\*$")
			 ))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

;;;; KEY CHORD

(key-chord-define-global "bb" 'ido-switch-buffer)
(key-chord-define-global "kk" 'ido-kill-buffer)
(key-chord-define-global "jj" 'ace-jump-mode)
(key-chord-define-global "gg" 'magit-status)
 
(require 'spaceline-config)
(spaceline-emacs-theme)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/organizer.org")))

(setq org-default-notes-file "~/organizer.org")
;; https://orgmode.org/manual/Conflicts.html
(setq org-replace-disputed-keys t)

;;;; set emacs prompts

(setq confirm-nonexistent-file-or-buffer nil)
;; removes if you want to kill a buffer with a live process attached to it
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(fset 'yes-or-no-p 'y-or-n-p)
