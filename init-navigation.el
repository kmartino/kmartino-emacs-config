(message "Starting init-navigation.el...")
;; -----------------------------------------------
;; Set up navigation boosters: Ivy and Projectile
;; -----------------------------------------------

(message "Configuring Ivy...")
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(message "Configuring Projectile...")
(projectile-mode +1)
(message "Configuring Projectile...")
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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

(setq imenu-auto-rescan t)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c k") 'counsel-ag)

(message "Finished init-navigation.el")
