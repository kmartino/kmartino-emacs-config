;;; init.el --- kmartino-emacs-config
;;
;; Karl
;; Author: Karl Martino <kmartino@gmail.com>
;; URL: https://github.com/kmartino/kmartino-emacs-config
;; Keywords: personal, configuration, emacs

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code

(message "Defining initialization variables...")

(defvar ksm/EMACS-START-TIME (current-time) "The time EMACS was started up.")

;; Lets get started...
(message "Refreshing packages if needed...")

(load-library "url-handlers")
(require 'cl)
(require 'package)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up

(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar ksm/PACKAGES '( 
                       exec-path-from-shell
                       async
                       company auto-complete yasnippet
                       autopair adaptive-wrap
                       outline-magic                       
                       expand-region
                       dtrt-indent
                       ;; navigation modes
                       ag
                       imenus
                       imenu-list
                       minimap
                       neotree
                       smooth-scrolling
                       helm helm-ag helm-git-grep helm-ls-git helm-projectile helm-gtags helm-smex helm-company
                       openwith projectile sr-speedbar projectile-speedbar
                       extend-dnd                       
                       ;; markdown, pandoc, org-mode, plain-text        
                       pandoc-mode deft
                       markdown-mode yaml-mode                       
                       multi-term
                       paredit flycheck
                       htmlize org-plus-contrib graphviz-dot-mode kanban ox-reveal
                       org-caldav
                       ;; programming modes
                       elpy pyvenv pylint company-jedi
                       direx jedi-direx
                       rvm
                       magit
                       git-gutter-fringe                       
                       ;; web related modes
                       w3m epc deferred web xml-rpc dizzee web-server simple-httpd                       
                       ;; look and feel related
                       ;; themes
                       darkmine-theme
                       solarized-theme
                       twilight-theme
                       monokai-theme
                       hc-zenburn-theme
                       tangotango-theme
                       moe-theme
                       darcula-theme
                       smyx-theme                       
                       )
  "A list of packages to ensure are present at launch.")

(dolist (p ksm/PACKAGES)
  (when (not (package-installed-p p))
    (package-install p)))


(defun ksm/configure-backup-behavior ()
  "Configures Emacs backup behavior"
  (defvar ksm/BACKUP-DIR (expand-file-name "~/.emacs.d/backups/") "Where to store backups.")
  (defvar ksm/AUTOSAVE-DIR (expand-file-name "~/.emacs.d/autosaves/") "Where to store autosaves.")
  (setq backup-directory-alist (list (cons ".*" ksm/BACKUP-DIR))
        auto-save-list-file-prefix ksm/AUTOSAVE-DIR
        auto-save-file-name-transforms `((".*" ,ksm/AUTOSAVE-DIR t))
        delete-old-versions -1
        version-control 1 ; use version numbers with backup files
        vc-make-backup-files t      
        )
  )


;; Configure encoding
(defun ksm/configure-encoding () 
  "Configures Emacs default file encoding"
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (if (boundp 'buffer-file-coding-system)
      (setq-default buffer-file-coding-system 'utf-8)
    (setq default-buffer-file-coding-system 'utf-8))
  (setq-default indent-tabs-mode nil)
  (add-hook 'c-mode-common-hook 
            (lambda()
              (require 'dtrt-indent)
              (dtrt-indent-mode t)))
  )

(defun ksm/launch-initial-look-and-feel ()
  "Configures initial look and feel and behavior"
  (setq initial-scratch-message "" 
        inhibit-startup-message t
        visible-bell nil 
        ring-bell-function 'ignore 
        show-paren-style 'expression
        ns-pop-up-frames nil
        visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
        dired-use-ls-dired nil
        save-interprogram-paste-before-kill t
        select-enable-clipboard t
        select-enable-primary t
        transient-mark-mode 1
        )
  (setq-default term-suppress-hard-newline t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (fset 'yes-or-no-p 'y-or-n-p)
  (show-paren-mode 1)
  (blink-cursor-mode 1)
  (line-number-mode)
  (column-number-mode)
  (size-indication-mode)
  (global-hl-line-mode 1)
  ;; Automagically pair braces and quotes like in TextMate.
  (require 'autopair)
  (autopair-global-mode)
  
  )

(defun ksm/launch-server ()
  "Load server library intializing a reusable emacs process"
  (load "server")
  (server-start)
  )

(defun ksm/override-default-theme-behavior ()
  "Disables any loaded themes before loading a new one (so weird that Emacs does this)"
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))
  )

(defun ksm/launch-env-specific-look-and-feel (&optional frame)
  "Changes the look and feel specific for running in a terminal or a GUI"
  (if (window-system)
      (progn
        (message "Running a GUI")
        (set-face-attribute 'default nil :font ksm/GUI-FONT)
        
        (load-theme 'monokai t)

        (setq smooth-scroll-margin 5)
        (smooth-scrolling-mode)
        (enable-smooth-scroll-for-function previous-line)
        (enable-smooth-scroll-for-function next-line)
        (enable-smooth-scroll-for-function isearch-repeat)
        
        (set-fringe-mode '(10 . 10))
        (setq linum-format "%6d ")
        (add-hook 'prog-mode-hook 'linum-mode)
        
        (setq mouse-wheel-follow-mouse 't
              mouse-wheel-scroll-amount '(1 ((shift) . 1))
              )

        (setq frame-title-format
              '((:eval (if (buffer-file-name)
                           (abbreviate-file-name (buffer-file-name))
                         default-directory)
                       )))
        
        ;; Open splits horizontally
        (setq split-height-threshold 0
              split-width-threshold nil
              )

        ;; Make moving between windows easier
        (windmove-default-keybindings)

        ;; Uniquify buffer names for display and navigation
        (require 'uniquify)
        (setq uniquify-buffer-name-style 'forward)

        (setq x-select-enable-clipboard t
        x-select-enable-primary t
        save-interprogram-paste-before-kill t)
        
        ;; Configure neotree
        (global-set-key [f8] 'neotree-toggle)

        ;; Configure Speedbar
        (setq speedbar-show-unknown-files t
              speedbar-use-imenu-flag t
              sr-speedbar-right-side nil
              speedbar-use-images nil
              )
        (require 'speedbar)
        (speedbar-add-supported-extension ".md")
        
        (toggle-frame-maximized)

        (ksm/default-keymap)

        )
    (progn
      (message "Running in xterm-color... this is kinda mac terminal specific... sorry!")

      (menu-bar-mode -1)
      
      ;; Set some key mappings so that Emacs interprets them correctly in a terminal
      (define-key input-decode-map "\e[1;10A" [M-S-up])
      (define-key input-decode-map "\e[1;10B" [M-S-down])
      (define-key input-decode-map "\e[1;10D" [M-S-left])
      (define-key input-decode-map "\e[1;10C" [M-S-right])
      (define-key input-decode-map "[OA" (kbd "<M-C-up>"))
      (define-key input-decode-map "[OB" (kbd "<M-C-down>"))
      (define-key input-decode-map "[OC" (kbd "<M-C-right>"))
      (define-key input-decode-map "[OD" (kbd "<M-C-left>"))
      (define-key input-decode-map "\e[1;9A" [M-up])
      (define-key input-decode-map "\e[1;9B" [M-down])
      (define-key input-decode-map "\e[1;9C" [M-right])
      (define-key input-decode-map "\e[1;9D" [M-left])
      (define-key input-decode-map "\e[Z" [S-tab])

      ;; get Emacs to respect mouse in terminal
      (require 'mouse)
      (xterm-mouse-mode t)
      (global-set-key [mouse-4] '(lambda ()
                                   (interactive)
                                   (scroll-down 1)))
      (global-set-key [mouse-5] '(lambda ()
                                   (interactive)
                                   (scroll-up 1)))
      (setq mouse-wheel-scroll-amount '(0.01))

      (defun track-mouse (e))
      (setq mouse-sel-mode t)

      (require 'redo+)
      (require 'mac-key-mode)
      (mac-key-mode 1)
      
      (defun ksm/copy-from-osx ()
        (shell-command-to-string "pbpaste"))

      (defun ksm/paste-to-osx (text &optional push)
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc))))

      (setq interprogram-cut-function 'ksm/paste-to-osx)
      (setq interprogram-paste-function 'ksm/copy-from-osx)
      
      )))

(defun ksm/default-keymap()
  "Defines a keymap for Emacs when running in a frame (window)"
  (global-set-key [(hyper a)] 'mark-whole-buffer)
  (global-set-key [(hyper v)] 'yank)
  (global-set-key [(hyper c)] 'kill-ring-save)
  (global-set-key [(hyper s)] 'save-buffer)
  (global-set-key [(hyper l)] 'goto-line)
  (global-set-key [(hyper w)]
                  (lambda () (interactive) (delete-window)))
  (global-set-key [(hyper z)] 'undo)
  (global-set-key [(hyper x)] 'kill-region)
  (global-set-key [(hyper q)] 'save-buffers-kill-terminal)

  )

(defun ksm/extend-environment-paths()
  "Extends the executable path from the shell path"
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))

    ;; Extend the load path for overrides
    (add-to-list 'load-path "~/.emacs.d/ext/")
    (add-to-list 'load-path "~/.emacs.d/local/")

    ;; Your customizations made within Emacs itself
    (setq custom-file "~/.emacs.d/custom.el")
    
    )

(defun ksm/load-personal-init()
  "Creates a personal config file, if doesn't exist, and loads it"
  (defvar ksm/PERSONAL-INIT (concat "~/.emacs.d/init-user-" user-login-name ".el")
    "The path to the init file to add personalized elisp and variables to.")
  (unless (file-exists-p ksm/PERSONAL-INIT)
    (with-temp-buffer
      (insert "(defvar ksm/PLANTUML-RENDER \"java -jar ~/.emacs.d/ext/plantuml.jar %s\" \"Defines the path to PlantUML. Set it to use custom Markdown and org-mode rendering features. Optional.\")\n")
x      (insert "(defvar ksm/MARKDOWN-PREVIEW-GUI \"\" \"Defines the path to the app that previews Markdown. Optional.\")\n")
      (insert "(defvar ksm/GUI-FONT \"Andale Mono-12\" \"Defines the default EMACS font.\")\n")
      (write-region (point-min) (point-max) ksm/PERSONAL-INIT))
    )
  (load ksm/PERSONAL-INIT)
  )

(defun ksm/load-init-files ()
  "Load supplemental initialiation files, prefixed with 'init-'
in ~/.emacs.d/."
  (load ksm/PERSONAL-INIT)
  (mapc 'load
        (directory-files "~/.emacs.d/" t "^init-.*\.el$"))
  )

(defun ksm/init-config ()
  "Runs this entire personalized Emacs configuration. It is meant
to be ran once, from the after-init-hook, to prevent emacsclient
sessions from re-doing this expensive work."
  (ksm/extend-environment-paths)
  (ksm/configure-backup-behavior)
  (ksm/configure-encoding)  
  (ksm/override-default-theme-behavior)
  (ksm/load-personal-init)
  (ksm/load-init-files)
  (ksm/launch-initial-look-and-feel)
  (ksm/launch-env-specific-look-and-feel)
  (ksm/launch-server)
  (message "ksm/init-config () finished!")  
  )


;; Run what is in init-config only once - will not be ran additional
;; times with emacsclient sessions
(add-hook 'after-init-hook 'ksm/init-config) 

(message "init.el finished!")

