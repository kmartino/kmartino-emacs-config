;;; init.el --- kmartino-emacs-config
;;
;; Karl
;; Author: Karl Martino <kmartino@gmail.com>
;; URL: https://github.com/kmartino/kmartino-emacs-config
;; Keywords: personal, configuration

;; This file is not part of GNU Emacs.

;;; Commentary:

;; emacs-stater-kit provides one of the best sets of defaults you can
;; ask for in an emacs configuration.  This is NOT an attempt at
;; improving it, just adding some additional personal defaults in a project
;; under source control for best practice purposes.  My recommendation
;; is that you use emacs-starter-kit and modify to suit your needs as
;; I have done here.

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
;;

;; -----------------------------------------------
;; Set up emacs-starter-kit and initialize
;;
;; -----------------------------------------------
(message "Setup emacs-starter-kit and initialize package")


(require 'cl)
(require 'package)
(require 'hippie-exp)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings starter-kit-eshell marmalade
                                  markdown-mode yaml-mode
                                  groovy-mode scala-mode2 clojure-mode nrepl ac-nrepl
                                  inf-ruby go-mode js-comint
                                  virtualenv pyflakes pylint ein jedi
                                  geiser quack
                                  magit
                                  htmlize
                                  org-plus-contrib graphviz-dot-mode kanban
                                  epc deferred web xml-rpc dizzee
                                  minimap expand-region sr-speedbar auto-complete escreen helm register-list tabbar tabbar-ruler
                                  smart-mode-line multi-term
                                  w3m elfeed
                                  )
  "A list of packages to ensure are installed at launch.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Add to the path the primary externals directory
(add-to-list 'load-path "~/.emacs.d/ext")

;; Extend exec-path to include system's PATH
(setenv "PATH"
        (concat (getenv "PATH")))
(when (file-exists-p "~/.bash_profile")
  (setenv "PATH" (shell-command-to-string "source ~/.bash_profile; echo -n $PATH"))
  ;; Update exec-path with the contents of $PATH
  (loop for path in (split-string (getenv "PATH") ":") do
        (add-to-list 'exec-path path))
  )

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next.
;; (setq x-select-request-type '(UTF8_STRING COMPOUND))

;; Turn on cua-mode because context switching for copy-paste is hard
;; (cua-mode t)

;; Show matching parens
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Spaces everywhere
(setq-default indent-tabs-mode nil)

;; Nuke trailing whitespaces when writing to a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Turn off the visible bell
(setq visible-bell nil)

;; Set initial working directory
(cd "~/")

;; Show column numbers on mode line
(setq-default column-number-mode t)

;; Backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosaves/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(message "Setup server for emacsclient connects")
(load "server")
(unless (server-running-p) (server-start))

(message "Finished bootstrap!")

;; -----------------------------------------------
;; Create bookmarks menu for quick access
;;
;; -----------------------------------------------
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
;;
;; -----------------------------------------------
(defun ksm/look-and-feel (&optional frame)
  (interactive)
    (if (window-system)
        (progn
          (message "Running a GUI")

          ;; set frame height, width and initial position
          ;;(set-frame-height (selected-frame) 40)
          ;;(set-frame-width (selected-frame) 140)
          ;;(set-frame-position (selected-frame) 20 40)

          ;; No splash screen
          (setq inhibit-startup-message t)
          ;; remove the scroll bar
          (scroll-bar-mode 0)
          ;; turn on the menu bar
          (menu-bar-mode)
          ;; turn off the tool bar
          (tool-bar-mode -1)
          ;; bink the cursor
          (blink-cursor-mode 1)
          ;; set up theme and font
          ;;(load-theme 'tsdh-dark)
          (load-theme 'zenburn t)
          (set-frame-font "Inconsolata-15")

          (windmove-default-keybindings)

          (require 'sr-speedbar nil t)
          (setq speedbar-hide-button-brackets-flag t
                speedbar-show-unknown-files t
                speedbar-smart-directory-expand-flag t
                speedbar-use-images nil
                speedbar-indentation-width 2
                speedbar-update-flag t
                sr-speedbar-width 40
                sr-speedbar-width-x 40
                sr-speedbar-auto-refresh nil
                sr-speedbar-skip-other-window-p t
                sr-speedbar-right-side nil)
          (add-hook 'speedbar-reconfigure-keymaps-hook
                    '(lambda ()
                       (define-key speedbar-mode-map [S-up]
                         'speedbar-up-directory)
                       (define-key speedbar-mode-map [right]
                         'speedbar-flush-expand-line)
                       (define-key speedbar-mode-map [left]
                         'speedbar-contract-line)))
          (add-hook 'speedbar-mode-hook '(lambda () (hl-line-mode 1)))
          (autoload 'sr-speedbar-toggle "sr-speedbar"
            "Toggle sr-speedbar window" t)
          (autoload 'sr-speedbar-open   "sr-speedbar"
            "Open the sr-speedbar window" t)

          ;; Turn on visual-line mode
          (add-hook 'text-mode-hook 'turn-on-visual-line-mode)

          ;; Turn on smart-mode-line
          (require 'smart-mode-line)
          (add-hook 'after-init-hook 'sml/setup)
          (set-face-attribute  'mode-line
                 nil
                 :foreground "gray80"
                 :background "gray25"
                 :box '(:line-width 1 :style released-button))
          (set-face-attribute  'mode-line-inactive
                 nil
                 :foreground "gray28"
                 :background "gray25"
                 :box '(:line-width 1 :style released-button))
          (set-face-background 'modeline-inactive "gray10")

          ;; set up clipboard
          ;;(setq x-select-enable-clipboard t)

          ;; turn on tab bar
          ;;(tabbar-mode 1)
          ;;(tabbar-ruler-up)
          )

          (progn
            ;; Set up xterm look and feel
            (message "running in xterm-color")

            ;; Running global highlight line mode
            (global-hl-line-mode 1)

            ;; Set some key mappings so that Emacs interprets them correctly
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
            (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
            (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))

            ;; Set some terminal encoding preferences
            (set-terminal-coding-system 'utf-8)
            (set-keyboard-coding-system 'utf-8)
            (prefer-coding-system 'utf-8)

            (defun copy-from-osx ()
              (shell-command-to-string "pbpaste"))

            (defun paste-to-osx (text &optional push)
              (let ((process-connection-type nil))
                (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
                  (process-send-string proc text)
                  (process-send-eof proc))))

            (setq interprogram-cut-function 'paste-to-osx)
            (setq interprogram-paste-function 'copy-from-osx)
            ;; (setq x-select-enable-clipboard t)

            ;; Get Emacs to respect mouse in Terminal
            (require 'mouse)
            (xterm-mouse-mode t)
            (defun track-mouse (e))
            (setq mouse-sel-mode t)

            )))
(ksm/look-and-feel)

(add-hook 'server-visit-hook 'ksm/look-and-feel)

;; -----------------------------------------------
;; Some basic methods that don't require external
;; libraries
;;
;; -----------------------------------------------

(defun ksm/move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun ksm/move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<S-s-up>") 'ksm/move-line-up)
(global-set-key (kbd "<S-s-down>") 'ksm/move-line-down)

(defun ksm/toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )

(defun ksm/no-flyspell-mode (&optional rest)
  (flyspell-mode -1))

(defun ksm/opacity-modify (&optional dec)
  "Modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))
(global-set-key (kbd "M--") '(lambda()(interactive)(ksm/opacity-modify t)))
(global-set-key (kbd "M-=") '(lambda()(interactive)(ksm/opacity-modify)))
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(global-set-key (kbd "s-{") 'other-window)
(global-set-key (kbd "s-}") 'other-window)
;;  M-x toggle-frame-fullscreen or M-x toggle-frame-maximized

(defun ksm/insert-blockquote ()
  "Insert blockquote at cursor point."
  (interactive)
  (insert ":<a href=\"\"></a>:<blockquote></blockquote>")
  (backward-char 4))

(defun ksm/insert-paragraph-tag ()
  "Insert <p></p> at cursor point."
  (interactive)
  (insert "<p></p>")
  (backward-char 4))

 (defun ksm/uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((end (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
          (replace-match "\\1\n\\2")))))

  (defun ksm/uniquify-all-lines-buffer ()
    "Delete duplicate lines in buffer and keep first occurrence."
    (interactive "*")
    (ksm/uniquify-all-lines-region (point-min) (point-max)))


(defun ksm/google-region (&optional flags)
  "Google the selected region."
  (interactive)
  (let ((query (buffer-substring (region-beginning) (region-end))))
    (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))

(defun ksm/google-mail (&optional flags)
  "Open Google mail on the selected region."
  (interactive)
  (browse-url "https://mail.google.com/mail/?view=cm&fs=1&tf=1&to=&su=")
  )

(defun ksm/diff-current-buffer-with-disk ()
  "Compare the current buffer with it's disk file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(defun ksm/unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs,
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

(defun ksm/word-count-region (startPos endPos)
  "Do some text processing on region.
This command calls the external script “wc”."
  (interactive "r")
  (let (scriptName)
    (setq scriptName "/usr/bin/wc") ; full path to your script
    (shell-command-on-region startPos endPos scriptName nil t nil t)
    ))

(defun ksm/surf-news ()
  (interactive)
  (progn
    (browse-url "http://news.ycombinator.com")
    (browse-url "http://stackoverflow.com")
    ))

(defun ksm/todo-compile ()
  "Example funcion that will run an external compile command,
and switch to a buffer containing its contents.
from http://tychoish.com/rhizome/emacs-thoughts-and-a-lisp-function/"
  (interactive)
  (if (get-buffer "*todo-compile*")
      (progn
        (switch-to-buffer-other-window (get-buffer "*todo-compile*"))
        (recompile))
    (progn
      (compile "make -j -k -C ~/wiki")
      (switch-to-buffer-other-window "*compilation*")
      (rename-buffer "*todo-compile*")))
  (revbufs))

(defun ksm/sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun ksm/string-replace (this withthat in)
  "replace THIS with WITHTHAT' in the string IN"
  (with-temp-buffer
    (insert in)
    (goto-char (point-min))
    (replace-string this withthat)
    (buffer-substring (point-min) (point-max))))




(defun show-some-buffers (buffer-list)
  (split-window-horizontally)
  (other-window 1)
  (dolist (buffer buffer-list)
    (split-window-vertically)
    (switch-to-buffer (get-buffer buffer))
    (other-window 1))
  ;; at the end we have one extra window we need to delete
  (delete-window)
  (balance-windows))

(defun show-erc-buffers ()
  (interactive)
  (show-some-buffers '("#emacs" "#clojure")))

(defun ksm/shell-command-on-region-to-string (start end command)
  (with-output-to-string
    (shell-command-on-region start end command standard-output)))

(defun ksm/shell-command-on-region-with-output-to-end-of-buffer (start end command)
  (interactive
   (let ((command (read-shell-command "Shell command on region: ")))
     (if (use-region-p)
         (list (region-beginning) (region-end) command)
       (list (point-min) (point-max) command))))
  (save-excursion
    (goto-char (point-max))
    (insert (ksm/shell-command-on-region-to-string start end command))))

(defun ksm/do-something-region (startPos endPos)
  "Do some text processing on region.
This command calls the external script “wc”."
  (interactive "r")
  (let (scriptName)
    (setq scriptName "/usr/bin/wc") ; full path to your script
    (shell-command-on-region startPos endPos scriptName nil t nil t)))

(defun ksm/switch-to-scratch-and-insert-text ()
  (interactive)
  (save-excursion
    (set-buffer "*scratch*")
    (insert "Hello, World")))
;;(global-set-key (kbd "C-c i") 'switch-to-scratch-and-insert-text)

;; beautify JSON
(defun ksm/beautify-json ()
  "Beautifies any region containing valid JSON data, using Python"
  (interactive)
  (when (use-region-p)
    (let (buf)
      (shell-command-on-region (mark) (point)
                               "python -m json.tool"
                               (current-buffer) t t))))

(message "Finished creating basic methods")


;; -----------------------------------------------
;; Setup elfeed
;; https://github.com/skeeto/elfeed
;; -----------------------------------------------
(global-set-key (kbd "C-x w") 'elfeed)

;; -----------------------------------------------
;; Setup EasyPG for file encryption
;; http://epg.sourceforge.jp/
;; -----------------------------------------------
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
	(auto-save-mode 1))))

;; -----------------------------------------------
;; Setup auto-complete
;; http://cx4a.org/software/auto-complete/
;; -----------------------------------------------
(message "Setting up auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)

;; -----------------------------------------------
;; Setup ielm
;; http://www.emacswiki.org/emacs/InferiorEmacsLispMode
;; -----------------------------------------------
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;; -----------------------------------------------
;; Setup python:
;; https://github.com/tkf/emacs-jedi
;; -----------------------------------------------
(message "Setting up python")
(require 'python)
(setq python-shell-interpreter (or (getenv "IPYTHON") "ipython")
      ;; python-shell-interpreter-args "--colors Linux --no-autoindent"
      ;; python-shell-interpreter-args "--colors Linux --no-autoindent --gui=wx --pylab=wx"
      python-shell-interpreter-args "--colors Linux"
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
(add-hook 'python-mode-hook 'electric-pair-mode)
;;Jedi and EIN
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(require 'jedi)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;; -----------------------------------------------
;; Set up clojure-mode
;; https://github.com/clojure-emacs/clojure-mode
;; -----------------------------------------------
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)

(setq nrepl-tab-command 'indent-for-tab-command)

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; -----------------------------------------------
;; Setup nxml
;; http://www.emacswiki.org/NxmlMode
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
;; Set up expand-region
;; https://github.com/magnars/expand-region.el
;; -----------------------------------------------
(require 'expand-region)

;; -----------------------------------------------
;; Set up js-comint
;; http://js-comint-el.sourceforge.net/
;; -----------------------------------------------
(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".1G.*3G" "..."
                                                 (replace-regexp-in-string
                                                  ".*1G.*3G" "> " output))))
        ))

;; -----------------------------------------------
;; Set up mozrepl
;; https://github.com/bard/mozrepl/wiki/Emacs-integration
;; -----------------------------------------------
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

;; -----------------------------------------------
;; Enable appt alerts
;; http://www.emacswiki.org/emacs/AppointmentMode
;; -----------------------------------------------
(setq appt-activate 1
      appt-message-warning-time 5
      appt-display-mode-line t
      appt-display-format 'window)

;; -----------------------------------------------
;; Setup org-mode
;; -----------------------------------------------
(require 'htmlize)
(require 'kanban)
(require 'ox-reveal)
(require 'org-protocol)
(require 'org-crypt)
(require 'org-info)
(require 'org-jsinfo)
(require 'org-habit)
(require 'org-latex)
(require 'org-drill)
(require 'org-element)
(require 'org-mac-iCal)
(require 'ob)
(require 'ob-js)
(setq org-ditaa-jar-path
      "~/.emacs.d/ext/ditaa.jar")
(setq org-plantuml-jar-path
      "~/.emacs.d/ext/plantuml.jar")
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
	 (calc . t)
         (plantuml . t)
         (latex . t))))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(global-set-key "\M-S-up" 'org-toggle-timestamp-type)

;; Turn on org-crypt
(org-crypt-use-before-save-magic)
;; Syntax highlight org-babel blocks
(setq org-src-fontify-natively t)
;; With an export, display inline images
(setq org-export-html-inline-images t)
;; Hitting RETURN over a link executes it
(setq org-return-follows-link t)
;; Have Agenda window open in same window
(setq org-agenda-window-setup 'current-window)
;; Where to refile items
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3) (nil :maxlevel . 3)))
;; Add notes in reverse order
(setq org-reverse-note-order t)
;; Remove timestamps from default exports
(setq org-export-with-timestamps nil)
;; Log TODO state changes into property drawer
(setq org-log-into-drawer t)
;; Set how far into the past and future to see org-habit chart
(setq org-habit-following-days 7)
(setq org-habit-preceding-days 7)
;; Highlight line underneath cursor in agenda views
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
;; Include the diary in the agenda
(setq org-agenda-include-diary t)
;; Show all agenda dates even if they are empty.
(setq org-agenda-show-all-dates t)
;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)
;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)
;; See deadlines in the agenda 30 days before the due date.
(setq org-deadline-warning-days 30)
;; Setup org-goto to use outline-path-completion
(setq org-goto-interface 'outline-path-completion org-goto-max-level 10)

;; Setup default todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "STARTED(s!)" "EVENT(e!)" "WAITING(w!)" "SOMEDAY(f!)"
                  "|" "SKIPPED(k!)" "DONE(d!)" "ACCEPTED(a!)" "CANCELED(c!)")))
;; Setup default tags
(setq org-tag-alist '( (:startgroup . nil) ("@WORK" . ?w) ("@HOME" . ?h) (:endgroup .nil)
                       ("PROJECT" . ?p) ("INBOX" . ?i) ("JOURNAL" . ?j) ("REFERENCE" . ?r)
                       ("QUESTION" . ?q) ("HIGHLIGHT" . ?h) ("STORY" . ?s) ("DEFECT" . ?d) ("TASK" . ?t)
                       ))

;; Define stuck projects
(setq org-tags-exclude-from-inheritance '("PROJECT"))
(setq org-stuck-projects '("+LEVEL=2+PROJECT/-MAYBE-DONE-SOMEDAY" ("NEXT" "WAITING" "STARTED")))

;; Create Custom Agenda Commands
(setq org-agenda-custom-commands
      '(
        ("R" "Weekly Review"
         ((agenda "+@WORK" ((org-agenda-span 'day)
                            (org-agenda-prefix-format "  %-18:T %?-8:t")
                            (org-agenda-remove-tags 'prefix)
                            ))
           ;; type "l" in the agenda to review logged items
          (tags "LEVEL=3+INBOX" ((org-agenda-prefix-format "  ")
                                  (org-agenda-show-inherited-tags nil)
                                  ))
          (todo "EVENT" ((org-agenda-prefix-format "  %-18:T %?-10:s ")
                          (org-agenda-remove-tags 'prefix)
                          (org-agenda-todo-keyword-format "")
                          (org-agenda-sorting-strategy '(tag-up category-up))))
          (todo "WAITING" ((org-agenda-prefix-format "  %-18:T %?-10:s ")
                                  (org-agenda-remove-tags 'prefix)
                                  (org-agenda-todo-keyword-format "")
                                  (org-agenda-sorting-strategy '(priority-up effort-down))
                                  (org-agenda-sorting-strategy '(tag-up category-up))))
          (todo "STARTED" ((org-agenda-prefix-format "  %-18:T %?-10:s ")
                                 (org-agenda-remove-tags 'prefix)
                                 (org-agenda-todo-keyword-format "")
                                 (org-agenda-sorting-strategy '(tag-up category-up))))
          (todo "NEXT" ((org-agenda-prefix-format "  %-18:T %?-10:s ")
                              (org-agenda-remove-tags 'prefix)
                              (org-agenda-todo-keyword-format "")
                              (org-agenda-sorting-strategy '(tag-up category-up))))
          (todo "TODO" ((org-agenda-prefix-format "  %-18:T %?-10:s ")
                              (org-agenda-remove-tags 'prefix)
                              (org-agenda-todo-keyword-format "")
                              (org-agenda-sorting-strategy '(tag-up category-up))))
          (todo "SOMEDAY" ((org-agenda-prefix-format "  %-18:T %?-10:s ")
                                 (org-agenda-remove-tags 'prefix)
                                 (org-agenda-todo-keyword-format "")
                                 (org-agenda-sorting-strategy '(tag-up category-up))))
          (stuck "" ((org-agenda-prefix-format "  %-18:T %?-10:s ")
                           (org-agenda-remove-tags 'prefix)
                      (org-agenda-sorting-strategy '(tag-up))))
          ))
        ))


;; -----------------------------------------------
;; Setup term and comint
;; -----------------------------------------------
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(defun ksm/term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))
(add-to-list 'term-bind-key-alist '("C-c C-e" . ksm/term-send-escape))

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)

;; Move the point to the prompt after command output
(setq comint-move-point-for-output t)


(message "All Done!")
