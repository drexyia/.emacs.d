(load-file "~/.emacs.d/personal/drexyia-common.el")

;;(drexyia-use-proxy)
(prelude-require-packages '(key-chord helm lmc rainbow-mode csharp-mode browse-kill-ring dos yaml-mode powerline))

;;set ui elements
(set-frame-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1") ;;use consolas font
(scroll-bar-mode -1)

;;set file associations
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))

;; disable prompts in emacs
(setq confirm-nonexistent-file-or-buffer nil)

;;start required modes
(powerline-vim-theme)

;;set key bindings
(setq w32-apps-modifier 'super)
(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-k") 'drexyia-kill-this-buffer)
(define-key global-map [?\s-x] 'prelude-exchange-point-and-mark)

(cua-selection-mode t)
(setq prelude-whitespace nil)
(setq prelude-flyspell nil)
(setq prelude-guru nil)
(setq projectile-global-mode nil)
(setq projectile-mode nil)

(server-start)


