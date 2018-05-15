(message "Starting init-helm-projectile.el...")
;; -----------------------------------------------
;; Set up navigation boosters: Helm and Projectile
;; Inspiration from:
;; https://tuhdo.github.io/helm-projectile.html
;; https://tuhdo.github.io/
;; http://amitp.blogspot.com/2012/10/emacs-helm-for-finding-files.html
;; https://gist.github.com/RickMoynihan/73fff814f394f57d3468
;; helm-imenu
;; brew install ag
;; https://github.com/ggreer/the_silver_searcher
;; https://github.com/syohex/emacs-helm-ag
;;
;; http://agel.readthedocs.org/en/latest/index.html
;; http://camdez.com/blog/2013/11/28/emacs-rapid-buffer-navigation-with-imenu/
;; https://github.com/bbatsov/projectile
;; https://github.com/bbatsov/projectile
;; http://blog.binchen.org/posts/hello-ivy-mode-bye-helm.html
;; -----------------------------------------------

(message "Configuring Helm...")
(require 'helm)
(require 'helm-config)
(require 'helm-projectile)

(helm-projectile-on)

(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

(message "Configuring Projectile...")
(setq projectile-completion-system 'helm)
(add-to-list 'projectile-project-root-files "setup.py")
(add-to-list 'projectile-project-root-files ".env")
(add-to-list 'projectile-project-root-files "requirements.txt")
(setq projectile-indexing-method 'alien)
(setq projectile-globally-ignored-directories '("target" ".virtualenv" ".env" ".idea"))
(setq projectile-globally-ignored-files '(".DS_Store"))
(defun ksm/projectile-neotree()
  (projectile-dired)
  )
(setq projectile-switch-project-action 'ksm/projectile-neotree)

(projectile-global-mode)

(message "Configuring autocompletion defaults...")
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(global-company-mode 1)

(setq imenu-auto-rescan t)

(define-key company-active-map (kbd "C-:") 'helm-company)
(define-key company-mode-map (kbd "C-:") 'helm-company)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key [remap execute-extended-command] #'helm-smex)
(global-set-key (kbd "M-X") #'helm-smex-major-mode-commands)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; M-/       : hippie-expand
;; M-y       : helm-show-kill-ring
;; C-c i     : helm-imenu
;; M-x       : helm-smex-major-mode-commands
;; C-c p h   : helm-projectile-switch-project
;; C-c p f   : helm-projectile-find-file
;; C-c p d   : helm-projectile-find-dir
;; C-c p s s : helm-projectile-ag
;; C-c c-f   : helm-find-files
;; C-x c-f   : helm-find-file
;; C-x b     : helm-mode-switch-to-buffer
;; C-c /     : Find file in directory (within helm-find-files) 

(message "Finished init-helm-projectile.el")
