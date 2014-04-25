(load-file "~/.emacs.d/personal/drexyia-common.el")

;;(drexyia-use-proxy)
(prelude-require-packages '(key-chord helm lmc rainbow-mode csharp-mode browse-kill-ring dos yaml-mode))

;;set ui elements
(set-frame-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1") ;;use consolas font

;;set file associations
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))

;;set key bindings
(setq w32-apps-modifier 'super)
(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(cua-selection-mode t)

(setq prelude-whitespace nil)
(setq prelude-flyspell nil)
(setq prelude-guru nil)

;;NOTE: prelude should automatically define these however currently this is not working on some machines
;;      due to this i define these manually 
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "JJ" 'prelude-switch-to-previous-buffer)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "yy" 'browse-kill-ring)
(key-chord-mode +1)

(server-start)
