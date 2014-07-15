;;; init.el --- init file from https://github.com/drexyia/.emacs.d

;;; Commentary:

;; this init file is used with the following environment
;; windows 7 64x
;; 64bit Emacs builds http://semantic.supelec.fr/popineau/programming-emacs.html#sec-2
;; unix tools are copied to the bin directory of Emacs http://unxutils.sourceforge.net/
;; a HOME environment user variable must be set for example C:\Emacs\emacs-w64-bzr117129\HOME
;; you must setup a autosave-directory directory see NOTE AUTOSAVE
;; if using behind a proxy server use fiddler to access package manager see NOTE PROXY
;; to find sections in this file search for ;;;; or use helm-occur ;;;;
;; first time run set to t for install then set to nil, this does initial setup see NOTE INSTALL
;; I use helm for allot of feature buffer switching occur etc

;;; Code:

(load-file "~/.emacs.d/drexyia-core.el")

;; NOTE PROXY
;;(drexyia-use-proxy)

;;;; install packages
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("MELPA" . "http://melpa.milkbox.net/packages/")))

;; set t to install nil to not install
;; NOTE INSTALL
(if nil
     (let ((package-list '(move-text
		      web-mode
		      csharp-mode
		      helm
		      powerline
		      monokai-theme
		      dired+
		      ace-jump-mode
		      key-chord
		      rainbow-mode
		      dos
		      yaml-mode
		      yasnippet
		      helm-descbinds
		      w32-browser
		      browse-kill-ring)))
       (package-refresh-contents)
       (drexyia-packman-list-install package-list)))

;;;; set ui elements
;; use consolas font on windows
(when (string-equal system-type "windows-nt") 
  (set-frame-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")) 

(set-language-environment "UTF-8")

(setq inhibit-startup-message t)     ; No splash screen please.
(setq initial-scratch-message nil)   ; Clean scratch buffer.

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           scroll-bar-mode              ; No scroll bars either.
	   menu-bar-mode
           blink-cursor-mode))          ; The blinking cursor gets old.
  (funcall mode 0))

;;;; set backup directories
;; NOTE AUTOSAVE  you must create the autosaves directory if it does not exist
(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/"))

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

;; when editing a file emacs creates a file .#filename this file is used to lock the file being edited
;; if another user or instance of emacs tries to open the file a warning will be dispayed
;; the below disables creating these files
(setq create-lockfiles nil)

;;;; set file associations
(add-to-list 'auto-mode-alist '("\\.aspx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))

;;;; set emacs prompts
(setq confirm-nonexistent-file-or-buffer nil)
;; removes if you want to kill a buffer with a live process attached to it
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(fset 'yes-or-no-p 'y-or-n-p)

;;;; set required modes
(dolist (feature
         '(dired+
	   key-chord
	   ibuffer
	   ibuf-ext))
  (require feature))

(dolist (mode
         '(show-paren-mode            ; Highlight matching parentheses.
	   key-chord-mode
	   cua-selection-mode))
  (funcall mode 1))

(dolist (mode
         '(global-auto-revert-mode))
  (funcall mode 0))

;;;; set key bindings
(setq w32-apps-modifier 'super)
(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "s-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "<M-S-up>")    'move-text-up)
(global-set-key (kbd "<M-S-down>")  'move-text-down)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-h l") 'find-library)
(global-set-key (kbd "<f6>") 'bookmark-set)
(global-set-key (kbd "<f7>") 'helm-bookmarks)
(global-set-key (kbd "<f12>") 'menu-bar-mode)
(global-set-key (kbd "M-s o") 'helm-occur) ;; use helm-occur instead of occur
(global-set-key (kbd "M-j") 'drexyia-join-line)
(global-set-key (kbd "C-`") (kbd "C-u C-SPC")) ;; navigate mark

(define-key global-map [?\s-x] 'prelude-exchange-point-and-mark)

;; in dired, you can press a instead of Enter to open the dir. This way, the previous dir will be automatically closed.
;; if you want Enter  and ^ (parent dir) to use the same buffer, put the following in your emacs init file
;; inorder to open a file and not close the dired buffer use f
(put 'dired-find-alternate-file 'disabled nil)
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;;;; set key chords

(key-chord-define-global "bb" 'helm-buffers-list)
(key-chord-define-global "kk" 'kill-buffer)
(key-chord-define-global "ww" 'drexyia-open-folder-in-explorer)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "cc" 'comment-or-uncomment-region)
(key-chord-define-global "ss" 'drexyia-switch-to-eshell)

;;;; set dired funcionality
;; auto-refresh dired on file change
(add-hook 'dired-mode-hook '(lambda ()
                              'auto-revert-mode
                              (setq auto-revert-verbose nil)))
(setq dired-dwim-target t)
;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once

;;;; set ibuffer groupings
;; sort the buffers with dired folders first
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired" (mode . dired-mode))
	       ("Emacs" (name . "^\\*"))
	       ("Temp" (name . "^temp*"))
	       ))))
	     
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;;;; setup helm
(require 'helm-buffers)
;; do not show temp buffers when switching buffers
(add-to-list 'helm-boring-buffer-regexp-list "\\*scratch*")
(add-to-list 'helm-boring-buffer-regexp-list "\\*Messages")
(add-to-list 'helm-boring-buffer-regexp-list "\\*Dired")

;;;; set theme
(load-theme 'monokai t)

;;;; set modeline
(powerline-vim-theme)
(setq-default
 mode-line-format
 '("%e"
   (:eval
    (let* ((active (powerline-selected-window-active))
           ;; left hand side displays Read only or Modified.
           (lhs (list (powerline-raw
                       (cond (buffer-read-only "Read only")
                             ((buffer-modified-p) "Modified")
                             (t "")) nil 'l)))
           ;; right side hand displays (line,column).
           (rhs (list
                 (powerline-raw
                  (concat
                   "(" (number-to-string (line-number-at-pos))
                   "," (number-to-string (current-column)) ")") nil 'r)))
           ;; center displays buffer name.
           (center (list (powerline-raw "%b" nil))))
      (concat (powerline-render lhs)
              (powerline-fill-center nil (/ (powerline-width center) 2.0))
              (powerline-render center)
              (powerline-fill nil (powerline-width rhs))
              (powerline-render rhs))))))


(put 'upcase-region 'disabled nil)

(provide 'init)

;;; init.el ends here
