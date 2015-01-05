;;; init.el --- kmartino-emacs-config
;;
;; Karl
;; Author: Karl Martino <kmartino@gmail.com>
;; URL: https://github.com/kmartino/kmartino-emacs-config
;; Keywords: personal, configuration

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will b useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
(message "Starting bootstrap process...")

(require 'cl)
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '( marmalade
                       smex autopair  auto-complete ac-nrepl
                       smart-mode-line powerline
                       markdown-mode yaml-mode
                       fuzzy flx-ido ido-ubiquitous ido-vertical-mode
                       openwith helm projectile
                       minimap expand-region sr-speedbar project-explorer
                       helm-projectile
                       pandoc-mode
                       multi-term
                       paredit flycheck
                       htmlize org-plus-contrib graphviz-dot-mode kanban ox-reveal
                       virtualenvwrapper pyflakes pylint jedi elpy
                       direx jedi-direx
                       find-file-in-repository
                       fiplr
                       cider
                       magit magit-push-remote
                       w3m epc deferred web xml-rpc dizzee
                       moe-theme
                       deft web-server
                       adaptive-wrap
                       extend-dnd
                       async
                       screenshot
                       nyan-mode
                       git-gutter-fringe
                       sticky-windows
                       darcula-theme
                       smyx-theme
                       tabbar
                       org-trello
                       neotree
                       rvm
                       )
  "A list of packages to ensure are installed at launch.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun ksm/extend-system-path()
  (message "Extending elisp path...")
  (add-to-list 'load-path "~/.emacs.d/ext")
  (setenv "PATH" (concat (getenv "PATH")))
  (when (file-exists-p "~/.bash_profile")
    (message "Executing shell profile to set environment variables.")
    (setenv "PATH" (shell-command-to-string "source ~/.bash_profile; echo -n $PATH"))
    (loop for path in (split-string (getenv "PATH") ":") do
          (add-to-list 'exec-path path))
    )
  (cd "~/")
)

(message "Setting up server for emacsclient connects...")
(load "server")
(unless (server-running-p)
  (progn
    (ksm/extend-system-path)
    (server-start)
    )
  )

(message "Setting up coding system environment....")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))
(setq-default indent-tabs-mode nil)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(message "Setting up backup behavior...")
(defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosaves/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; -----------------------------------------------
;; Setup initial look and feel
;; -----------------------------------------------
(message "Configuring initial look and feel....")
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(setq visible-bell nil)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(blink-cursor-mode 1)
(setq show-paren-style 'expression)
(setq ns-pop-up-frames nil)
(line-number-mode)
(column-number-mode)
(size-indication-mode)

;; Automagically pair braces and quotes like in TextMate.
(require 'autopair)
(autopair-global-mode)

;; Fix dired for Mac's ls and its lack of --dired option
(setq dired-use-ls-dired nil)

;; Set up fringe and line navigation
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Highlight the current line the cursor is placed on
(global-hl-line-mode 1)

;; Hippie expand is smarter than dabbrev
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; -----------------------------------------------
;; Custom bookmarks menu
;; -----------------------------------------------
(message "Setting up bookmarks menu...")
(define-key global-map [menu-bar bookmarks]
  (cons "Bookmarks" (make-sparse-keymap "Bookmarks")))
(define-key global-map
  [menu-bar bookmarks bookmark-insert]
  '("Insert bookmark into buffer" . bookmark-insert))
(define-key global-map
  [menu-bar bookmarks bookmark-delete]
  '("Delete bookmark" . bookmark-delete))
(define-key global-map
  [menu-bar bookmarks bookmark-save]
  '("Save bookmarks" . bookmark-save))
(define-key global-map
  [menu-bar bookmarks list-bookmarks]
  '("List bookmarks" . list-bookmarks))
(define-key global-map
  [menu-bar bookmarks bookmark-set]
  '("Add bookmark" . bookmark-set))
(define-key global-map
  [menu-bar bookmarks bookmark-jump]
  '("Goto bookmark" . bookmark-jump))

;; -----------------------------------------------
;; Set up GUI look and feel
;; -----------------------------------------------
(defun ksm/look-and-feel (&optional frame)
  (interactive)
  (if (window-system)
      (progn
        (message "Running a GUI")

        (add-hook 'after-init-hook 'toggle-frame-maximized t)

        (setq redisplay-dont-pause t
              scroll-margin 1
              scroll-step 1
              scroll-conservatively 10000
              scroll-preserve-screen-position 1)
        (setq mouse-wheel-follow-mouse 't)
        (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

        (windmove-default-keybindings)

        (load-theme 'zenburn t)

        (set-face-attribute 'default nil
                            :font "Menlo"
                            :height 160
                            :weight 'normal
                            :width 'normal)

        (setq-default line-spacing 2)

        (require 'powerline)
        (powerline-default-theme)
        (setq powerline-arrow-shape 'arrow14) ;; best for small fonts
        (setq-default mode-line-format
                      (cons '(:exec venv-current-name) mode-line-format))

        (setq
         sr-speedbar-width-x 60
         sr-speedbar-width 60
         speedbar-show-unknown-files t
         sr-speedbar-right-side nil)

        (require 'uniquify)
        (setq uniquify-buffer-name-style 'forward)

        )
    (progn
      (message "Running in xterm-color")

      ;; set some key mappings so that Emacs interprets them correctly
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
      (defun track-mouse (e))
      (setq mouse-sel-mode t)

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

(add-hook 'server-visit-hook 'ksm/look-and-feel)

;; -----------------------------------------------
;; Setup EasyPG for file encryption
;; http://epg.sourceforge.jp/
;; -----------------------------------------------
(message "Setting up epa-file for encryption...")
(require 'epa-file)

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; disable auto-save
        (if auto-save-default
            (auto-save-mode -1)))
    ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1)))
  )

;; -----------------------------------------------
;; Setup expand-region
;; https://github.com/magnars/expand-region.el
;; -----------------------------------------------
(require 'expand-region)

;; -----------------------------------------------
;; Setup openwith
;; -----------------------------------------------
(message "Setting up openwith...")
(require 'openwith)
(openwith-mode t)
(setq openwith-associations
      (quote (("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "open -a vlc" (file))
              ("\\.ppt[x]*\\'"  "open" (file))
              ("\\.doc[x]*\\'"  "open" (file))
              ("\\.xls[x]*\\'"  "open" (file))
              )))

;; -----------------------------------------------
;; Setup nxml
;; https://fedoraproject.org/wiki/How_to_use_Emacs_for_XML_editing
;; -----------------------------------------------
(defun ksm/pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

;; -----------------------------------------------
;; Set up navigation boosters: Helm, Projectile
;; Inspiration from:
;; https://tuhdo.github.io/
;; http://amitp.blogspot.com/2012/10/emacs-helm-for-finding-files.html
;; -----------------------------------------------
(require 'helm-config)
(require 'helm-projectile)
(helm-mode 1)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-idle-delay 0.1
      helm-input-idle-delay 0.1)
(loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
      do (add-to-list 'helm-boring-file-regexp-list ext))

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-indexing-method 'alien)
(setq projectile-globally-ignored-directories '("target" ".virtualenv" ".env" ".idea"))
(setq projectile-globally-ignored-files '(".DS_Store"))
(setq projectile-switch-project-action 'projectile-dired)

;; -----------------------------------------------
;; Setup auto-complete
;; http://cx4a.org/software/auto-complete/
;; -----------------------------------------------
(message "Setting up auto-complete")
(require 'fuzzy)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;; -----------------------------------------------
;; Setup python
;; -----------------------------------------------
(message "Setting up python...")
(require 'python)
(setq python-indent-offset 4)

;; -----------------------------------------------
;; Setup ruby
;; -----------------------------------------------
(add-hook 'ruby-mode-hook
          (lambda ()
            (autopair-mode)))

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(rvm-use-default)

;; -----------------------------------------------
;; Setup yaml
;; -----------------------------------------------
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; -----------------------------------------------
;; Execute my personal elisp config files
;; (stuff that is not designed for re-use by others)
;; -----------------------------------------------
(load (concat "~/.emacs.d/" user-login-name ".el"))

(message "All Done!")
