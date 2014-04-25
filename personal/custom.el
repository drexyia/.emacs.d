(defun drexyia-emacsd-install-packages ()
  "installs all packages not installed as part of emacs prelude"
  (let 
      (packages-list  current-package)
    
    ;; all packages not installed within the prelude package
    (setq packages-list '(helm lmc rainbow-mode csharp-mode browse-kill-ring dos))
    
    (while packages-list
      (setq current-package (pop packages-list))

      (if (not (package-installed-p current-package))
          (progn
            (message "installing package: %s" current-package)
            (package-install current-package))))))

(load-file "~/.emacs.d/personal/drexyia-common.el")
(drexyia-use-proxy)
(drexyia-emacsd-install-packages)

;;set ui elements
(set-frame-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1") ;;use consolas font

;;set file associations
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))

;;set key bindings
(setq w32-apps-modifier 'super)
(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(helm-mode t)
(cua-selection-mode t)
(server-start)
