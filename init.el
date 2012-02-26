;;; init.el --- kmartino-emacs-config
;;
;; Karl Martino
;; Copyright (C) 2011 Karl Martino
;; Author: Karl Martino <kmartino@gmail.com>
;; URL: https://github.com/kmartino/kmartino-emacs-config
;; Version: 1.0.0
;; Keywords: personal, configuration

;; This file is not part of GNU Emacs.

;;; Commentary:

;; emacs-stater-kit provides one of the best sets of defaults you can
;; ask for in an emacs configuration.  This is NOT an attempt at
;; improving it, just adding some additional defaults in a project
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

;;; Code:

;; -----------------------------------------------
;; Overide the following in your personal
;; {user}.el file
;; -----------------------------------------------

;; location of MIT-Scheme executable. This is the one variable you'll
;; want to override in your local config.
(setq MIT_SCHEME_INSTALL "/Applications/mit-scheme.app/Contents/Resources/mit-scheme")


;; -----------------------------------------------
;; Set up emacs-starter-kit and initialize
;; -----------------------------------------------

;; Enable package
(require 'package)
;; Add marmalade repository to package manager
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Initialize packages
(when (not package-archive-contents)
  (package-refresh-contents))
;; Install default packages including emacs-starter-kit
(defvar my-packages '(starter-kit
                      starter-kit-lisp 
                      starter-kit-bindings
                      starter-kit-eshell
                      markdown-mode 
                      yaml-mode
                      marmalade
                      js-comint
                      zenburn-theme
                      color-theme-ir-black
                      graphviz-dot-mode
                      groovy-mode
                      yasnippet
                      python-mode
                      inf-ruby
                      ipython
                      magit 
                      clojure-mode
                      pastels-on-dark-theme
                      )
  "A list of packages to ensure are installed at launch.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; -----------------------------------------------
;; Some basic setup.
;; Don't need to do much since emacs-starter-kit
;; has so many decent defaults!
;; -----------------------------------------------

;; Visible bell
(setq visible-bell nil)

;; turn on linum-mode
;;(setq linum-format "%4d ")
;; (fringe-mode 8)
;;(global-linum-mode +1)

;; Enable Python-Mode
(require 'python-mode)

;; Set initial working directory
(cd "~/")

;; Backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosaves/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; -----------------------------------------------
;; Bookmarks custom menu
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
;; Some general methods
;; -----------------------------------------------
(defun ksm/opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))
(global-set-key (kbd "C-9") '(lambda()(interactive)(ksm/opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)(ksm/opacity-modify)))

(defun ksm/zoom (n)
  "Increase or decrease font size based upon argument"
  (set-face-attribute 'default (selected-frame) :height
                      (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))
(global-set-key (kbd "C-7")      '(lambda nil (interactive) (ksm/zoom -1)))
(global-set-key (kbd "C-8")      '(lambda nil (interactive) (ksm/zoom 1)))
(global-set-key (kbd "C-=") 'text-scale-increase)


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
(global-set-key (kbd "C-c d") 'ksm-diff-current-buffer-with-disk)

(defun ksm/unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs,
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

(defun ksm/process-region (startPos endPos)
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
;; Setup term and comint
;; -----------------------------------------------
;; http://kanis.fr/nterm.html
;; http://blog.nguyenvq.com/2011/03/07/escreen-instead-of-elscreen-for-screen-like-features-in-emacs/
;; http://snarfed.org/why_i_run_shells_inside_emacs
;; http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
;; http://technical-dresese.blogspot.com/2010/02/saner-ansi-term-in-emacs.html
;; https://github.com/dimitri/emacs-kicker/blob/master/init.el
;; -----------------------------------------------
;; Setup comint 
;; -----------------------------------------------
;; set maximum-buffer size for shell-mode (useful if some program that you're debugging spews out large amounts of output).
(setq comint-buffer-maximum-size 10240)
;; always add output at the bottom
(setq comint-scroll-to-bottom-on-output t) 
;; scroll to show max possible output
(setq comint-scroll-show-maximum-output t)
;; if this is t, it breaks shell-command
(setq comint-prompt-read-only nil)         
;; will disalllow passwords to be shown in clear text (this is useful, for example, 
;; if you use the shell and then, login/telnet/ftp/scp etc. to other machines).
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;; will remove ctrl-m from shell output.
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
;; will truncate shell buffer to comint-buffer-maximum-size
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun ksm/set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'ksm/set-scroll-conservatively)

(setq ansi-color-names-vector
       ["black" "red4" "green4" "yellow4"
        "blue3" "magenta4" "cyan4" "white" ])
(setq ansi-term-color-vector
      (vector 'unspecified "#3f3f3f"
              "#cc9393" "#7f9f7f"
              "#f0dfaf" "#94bff3"
              "#dc8cc3" "#93e0e3"))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;(require 'term)
; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
;(define-key term-raw-map (kbd "C-'") 'term-line-mode)
;(define-key term-mode-map (kbd "C-'") 'term-char-mode)
;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
;(define-key term-raw-map (kbd "C-y") 'term-paste)

;; -----------------------------------------------
;; Set up GUI look and feel
;; -----------------------------------------------
(defun ksm/look-and-feel (&optional frame)
    (if (window-system)
        (progn
          (message "Running a GUI")
          ;; set frame height, width and initial position
          (set-frame-height (selected-frame) 40)
          (set-frame-width (selected-frame) 140)
          (set-frame-position (selected-frame) 20 40)
          ;; remove the scroll bar
          (scroll-bar-mode 0)
          ;; turn on the menu bar
          (menu-bar-mode)
          ;; set up theme and font
          ;(load-theme 'tango-dark t)
          (load-theme 'zenburn t)
          (set-frame-font "Monaco-14")
          ;; turn off fringe
          ;; (fringe-mode 0)
          ;; turn on the server
          (server-start)
          )
      (progn
        ;; Set up xterm look and feel
        (message "running in xterm-color")
        ;; Load theme
        (load-theme 'zenburn t)
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
        (setq x-select-enable-clipboard t)

        ;; Get Emacs to respect mouse in Terminal
        (require 'mouse)
        (xterm-mouse-mode t)
        (defun track-mouse (e))
        (setq mouse-sel-mode t)

        )
      ))
(ksm/look-and-feel)
(add-hook 'server-visit-hook 'ksm/look-and-feel)

;; -----------------------------------------------
;; Externals specific setup
;; -----------------------------------------------

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

;; Set up Scheme using MIT_SCHEME_INSTALL
(setq scheme-program-name MIT_SCHEME_INSTALL)
(defun load-xscheme () (require 'xscheme))
(add-hook 'scheme-mode-hook 'load-xscheme)
(setq exec-path
      (cons MIT_SCHEME_INSTALL exec-path))

;; Set up js-comint
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

;; Set up mozrepl
;; https://github.com/bard/mozrepl/wiki/Emacs-integration
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

;; Add a personal directory for info file loader
;; http://www.neilvandyke.org/sicp-texi/
(add-to-list 'Info-default-directory-list
             (expand-file-name "~/.emacs.d/ext/info/"))

;; Initialize yasnippet and add personal snippets directory
(require 'yasnippet) 
(yas/initialize)
(yas/load-directory "~/.emacs.d/ext/yasnippet/")

;; Setup EasyPG for file encryption 
(require 'epa-file)

;; Setup autocomplete
;; http://cx4a.org/software/auto-complete/index.html
(add-to-list 'load-path "~/.emacs.d/ext/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ext/auto-complete/ac-dict")
(ac-config-default)
;; http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/
;; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
;; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
;; Case sensitivity is important when finding matches
(setq ac-ignore-case nil)
;; Let's have yasnippets in the auto-complete dropdown
(add-to-list 'ac-sources 'ac-source-yasnippet)

;; Setup ruby-mode
(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (require 'inf-ruby)
                               (require 'ruby-compilation))))

;; -----------------------------------------------
;; Set up org-mode
;; -----------------------------------------------

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(global-set-key "\M-S-up" 'org-toggle-timestamp-type)

;; Enable org-mode to use encryption
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(require 'org-info)
(require 'org-jsinfo)
(require 'org-habit)

;; Set up org-babel
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

;; Disable auto-fill-mode for org-mode editting
(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode 0)))

;; With an export, display inline images
(setq org-export-html-inline-images t)
;; Hitting RETURN over a link executes it
(setq org-return-follows-link t)
;; Where to refile items
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)
			   (nil :maxlevel . 5)))
;; Use path-like completion when refiling
(setq org-refile-use-outline-path '(file))
;; When refiling an item, add it to the top of a list, not the end
(setq org-reverse-note-order t)
;; Log done time on TODOs
(setq org-log-done 'time)
;; Log repeat time on TODOs
(setq org-log-repeat 'time)
;; Log times to a LOGBOOK property
(setq org-log-into-drawer "LOGBOOK")
;; Set default timer time
(setq org-timer-default-timer 25)
;; http://stackoverflow.com/questions/1851390/custom-agenda-view-in-org-mode-combining-dates-and-tags
;; Set clock in behavior
(add-hook 'org-clock-in-hook '(lambda () 
				(if (not org-timer-current-timer) 
                                    (org-timer-set-timer '(16)))))
;; Remove timestamps from default exports
(setq org-export-with-timestamps nil)

;; Set how far into the past and future to see org-habit chart
(setq org-habit-following-days 7)
(setq org-habit-preceding-days 7)
;; Highlight line underneath cursor in agenda views
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
;; Include the diary in the agenda
(setq org-agenda-include-diary t)
;; Show all agenda dates even if they are empty.
(setq org-agenda-show-all-dates t)
;; Remove tags from agenda display when they are manipulated in org-agenda-prefix-format
(setq org-agenda-remove-tags 'prefix)
;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)
;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)
;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)
;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)
;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)
;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)
;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)
;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)
;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)
;; See deadlines in the agenda 30 days before the due date.
(setq org-deadline-warning-days 30)

;; Setup TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)"
                  "STARTED(s!)" "EVENT(e!)"
                  "WAITING(w!)" 
                  "|" "CANCELED(c!)" "DONE(d!)")))
;; Setup Tags
(setq org-tag-alist
      '((:startgroup) 
	("@WORK" . ?w) 
	("@LIFE" . ?l)
	(:endgroup)
	(:startgroup)
	("@DAILY" . ?d)
	("@YEARLY" . ?y)
	(:endgroup)
	("THINKING" . ?t)
	("EDU" . ?e)
	("VOLUNTEER" . ?v)
	("NOTES" . ?n)
	(:startgroup)
	("COMM" . ?c)
	("WATCH" . ?w)
	("READ" . ?r)
	("BUY" . ?b)
))
;; How we define stuck projects
(setq org-stuck-projects '("+LEVEL=2+PROJECTS/-MAYBE-DONE-SOMEDAY" ("NEXT") ("@DAILY") ""))

;; Simple method to narrow the subtree to TODO headlines
(defun ksm/org-todo ()
  "Narrow the subtree to TODO entries"
  (interactive)
  (org-narrow-to-subtree)
  (org-show-todo-tree nil)
  (widen))

;; Wraps a region with EXAMPLE escapes
(defun ksm/quote-org (start end)
  "Wraps a region with BEGIN_EXAMPLE and END_EXAMPLE."
  (interactive "r")
  (let ()
    (message "Region starts: %d, end at: %d" start end)
    (goto-char (+ 1 end))
    (insert "#+END_EXAMPLE\n")
    (goto-char start)
    (insert "#+BEGIN_EXAMPLE\n")
    )
  )

;; Create Custom Agenda Commands
(setq org-agenda-custom-commands
      '(
	("R" "Weekly Review"
         ( (agenda "" ((org-agenda-ndays 1)  
		       )) 
	  ;; type "l" in the agenda to review logged items 
          (todo "WAITING" ((org-agenda-prefix-format "%-10T%-25c")
                            (org-agenda-todo-keyword-format "")
                            (org-agenda-sorting-strategy '(tag-up category-up))))
           (todo "STARTED" ((org-agenda-prefix-format "%-10T%-25c")
                            (org-agenda-todo-keyword-format "")
                            (org-agenda-sorting-strategy '(tag-up category-up))))
           (todo "NEXT" ((org-agenda-prefix-format "%-10T%-25c")
                         (org-agenda-todo-keyword-format "")
                         (org-agenda-sorting-strategy '(tag-up category-up))))
           (stuck "" ((org-agenda-prefix-format "%-10T")
                      (org-agenda-sorting-strategy '(tag-up)))) 
           (todo "TODO" ((org-agenda-prefix-format "%-10T%-25c")
                        (org-agenda-todo-keyword-format "")
                        (org-agenda-sorting-strategy '(tag-up category-up))))
           (todo "SOMEDAY" ((org-agenda-prefix-format "%-10T%-25c")
                            (org-agenda-todo-keyword-format "")
                            (org-agenda-sorting-strategy '(tag-up category-up))))
           )) 
        ))


