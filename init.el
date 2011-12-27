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

(defconst MIT_SCHEME_INSTALL "/Applications/mit-scheme.app/Contents/Resources/mit-scheme")

;; enable package
(require 'package)
;; add marmalade repository to package manager
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; initialize packages
(when (not package-archive-contents)
  (package-refresh-contents))
;; install default packages including emacs-starter-kit
(defvar my-packages '(starter-kit
                      starter-kit-lisp 
                      starter-kit-bindings
                      starter-kit-eshell
                      markdown-mode 
                      yaml-mode
                      marmalade
                      color-theme
                      color-theme-sanityinc-solarized
                      color-theme-tango
                      color-theme-tangotango
                      color-theme-zenburn
                      color-theme-ir-black
                      graphviz-dot-mode
                      groovy-mode
                      yasnippet
                      sr-speedbar
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

;; visible bell
(setq visible-bell nil)

;; turn on the menu bar
(menu-bar-mode)

;; turn on ir-black color-theme 
(require 'color-theme-ir-black)
(color-theme-ir-black)

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

(defun ksm-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))
(global-set-key (kbd "C-8") '(lambda()(interactive)(ksm-opacity-modify t)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(ksm-opacity-modify)))

(defun ksm-zoom (n)
  "Increase or decrease font size based upon argument"
  (set-face-attribute 'default (selected-frame) :height
                      (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))
(global-set-key (kbd "C-6")      '(lambda nil (interactive) (ksm-zoom -1)))
(global-set-key (kbd "C-7")      '(lambda nil (interactive) (ksm-zoom 1)))

(defun ksm-maximize-frame ()
  "Maximizes the frame"
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))
;(global-set-key (kbd "C-0") 'ksm-maximize-frame)


;; http://www.brool.com/index.php/using-org-mode-with-gtd
;; [[elisp:(open-encrypted-file "~/org/passwords.txt.bfe")][Passwords]]
(defun ksm-open-encrypted-file (fname)
  (interactive "FFind file: \n")
  (let ((buf (create-file-buffer fname)))
    (shell-command
     (concat "echo " (read-passwd "Decrypt password: ") " | bcrypt -o " fname)
     buf)
    (set-buffer buf)
    (kill-line)(kill-line)
    (toggle-read-only)
    (not-modified))
  )

(defun ksm-insert-blockquote ()
  "Insert blockquote at cursor point."
  (interactive)
  (insert ":<a href=\"\"></a>:<blockquote></blockquote>")
  (backward-char 4))

(defun ksm-insert-paragraph-tag ()
  "Insert <p></p> at cursor point."
  (interactive)
  (insert "<p></p>")
  (backward-char 4))

(defun ksm-google-region (&optional flags)
  "Google the selected region."
  (interactive)
  (let ((query (buffer-substring (region-beginning) (region-end))))
    (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))

(defun ksm-google-mail (&optional flags)
  "Open Google mail on the selected region."
  (interactive)
  (browse-url "https://mail.google.com/mail/?view=cm&fs=1&tf=1&to=&su=")
  )

(defun ksm-diff-current-buffer-with-disk ()
  "Compare the current buffer with it's disk file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))
(global-set-key (kbd "C-c d") 'ksm-diff-current-buffer-with-disk)

(defun ksm-unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs,
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

(defun ksm-process-region (startPos endPos)
  "Do some text processing on region.
This command calls the external script “wc”."
  (interactive "r")
  (let (scriptName)
    (setq scriptName "/usr/bin/wc") ; full path to your script
    (shell-command-on-region startPos endPos scriptName nil t nil t)
    ))

(defun ksm-surf-news ()
  (interactive)
  (progn
    (browse-url "http://news.ycombinator.com")
    (browse-url "http://stackoverflow.com")
    ))

(defun ksm-pretty-print-xml-region (begin end)
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

;; -----------------------------------------------
;; Setup comint 
;; -----------------------------------------------
;; set maximum-buffer size for shell-mode (useful if some program that you're debugging spews out large amounts of output).
(setq comint-buffer-maximum-size 10240)
;; always insert at the bottom
(setq comint-scroll-to-bottom-on-input t)
;; always add output at the bottom
(setq comint-scroll-to-bottom-on-output nil) 
;; scroll to show max possible output
(setq comint-scroll-show-maximum-output t)
;; if this is t, it breaks shell-command
(setq comint-prompt-read-only nil)         
;; will disalllow passwords to be shown in clear text (this is useful, for example, 
;;  if you use the shell and then, login/telnet/ftp/scp etc. to other machines).
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;; will remove ctrl-m from shell output.
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
;; will truncate shell buffer to comint-buffer-maximum-size
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; -----------------------------------------------
;; Setup shell
;; -----------------------------------------------
; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun ksm-set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'ksm-set-scroll-conservatively)

;; -----------------------------------------------
;; Setup ansi-term
;; -----------------------------------------------
;; a saner ansi-term
;; http://technical-dresese.blogspot.com/2010/02/saner-ansi-term-in-emacs.html
(defun ksm-term-hooks ()
  ;; dabbrev-expand in term
  (define-key term-raw-escape-map "/"
    (lambda ()
      (interactive)
      (let ((beg (point)))
        (dabbrev-expand nil)
        (kill-region beg (point)))
      (term-send-raw-string (substring-no-properties (current-kill 0)))))
  ;; yank in term (bound to C-c C-y)
  (define-key term-raw-escape-map "\C-y"
    (lambda ()
      (interactive)
      (term-send-raw-string (current-kill 0))))
  (setq term-default-bg-color (face-background 'default))
  (setq term-default-fg-color (face-foreground 'default)))
(add-hook 'term-mode-hook 'ksm-term-hooks)

(defun ksm-term-switch-to-line-mode ()
  (interactive)
  (term-line-mode))

(add-hook 'term-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))
(setq multi-term-scroll-show-maximum-output t)
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq ansi-term-color-vector
      (vector 'unspecified "#3f3f3f"
              "#cc9393" "#7f9f7f"
              "#f0dfaf" "#94bff3"
              "#dc8cc3" "#93e0e3"))

;; -----------------------------------------------
;; Externals specific setup
;; -----------------------------------------------
;; Setup PATH and add scheme interpreter 
;; LINUX and OSX specific unfortunately 
(when (file-exists-p "~/.bash_profile") 
    (setenv "PATH" (shell-command-to-string "source ~/.bash_profile; echo -n $PATH"))
    ;; Update exec-path with the contents of $PATH
    (loop for path in (split-string (getenv "PATH") ":") do
 	(add-to-list 'exec-path path))

    ;; Set up Scheme interpreter MIT-SCHEME
    ;; http://www.cs.rpi.edu/academics/courses/fall00/ai/scheme/starting.html
    (setq scheme-program-name MIT_SCHEME_INSTALL)
    (defun load-xscheme () (require 'xscheme)) 
    (add-hook 'scheme-mode-hook 'load-xscheme)

)

;; Add a personal driectory to info file loader list (SICP in info is here)
;; http://www.neilvandyke.org/sicp-texi/
(add-to-list 'Info-default-directory-list
             (expand-file-name "~/.emacs.d/ext/info/"))

;; Initialize yasnippet and add personal snippets directory
(require 'yasnippet) 
(yas/initialize)
(yas/load-directory "~/.emacs.d/ext/yasnippet/")

;; Load EasyPG for file encryption 
(require 'epa-file)

;; Set up GUI look-and-feel
(when window-system
  ;; set frame height
  (set-frame-height (selected-frame) 50)
  (set-frame-width (selected-frame) 140)
  ;; add the scroll bar
  (scroll-bar-mode 1)

  ;; sr-speedbar
  ;; http://www.emacswiki.org/emacs/SrSpeedbar
  ;; https://gist.github.com/776856
  (require 'sr-speedbar)
  (setq speedbar-frame-parameters
    '((minibuffer)
    (width . 60)
    (border-width . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (unsplittable . t)
    (left-fringe . 0)))
  ;; (when window-system
  ;;   (defadvice sr-speedbar-open (after sr-speedbar-open-resize-frame activate)
  ;;     (set-frame-width (selected-frame)
  ;;                      (+ (frame-width) sr-speedbar-width)))
  ;;   (ad-enable-advice 'sr-speedbar-open 'after 'sr-speedbar-open-resize-frame)

  ;;   (defadvice sr-speedbar-close (after sr-speedbar-close-resize-frame activate)
  ;;     (sr-speedbar-recalculate-width)
  ;;     (set-frame-width (selected-frame)
  ;;                      (- (frame-width) sr-speedbar-width)))
  ;;   (ad-enable-advice 'sr-speedbar-close 'after 'sr-speedbar-close-resize-frame))
  )


;; -----------------------------------------------
;; Set up org-mode
;; -----------------------------------------------

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "M-i") 'ido-goto-symbol)

;; add org-babel externals to load path
(setq org-ditaa-jar-path
      "~/.emacs.d/ext/ditaa.jar")
(setq org-plantuml-jar-path
      "~/.emacs.d/ext/plantuml.jar")

;; enable org-mode to use encryption
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(require 'org-info)
(require 'org-jsinfo)
(require 'org-habit)

;; set up org-babel
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

;; with an export, don't output timestamps in table of contents
(setq org-export-remove-timestamps-from-toc t)
;; with an export, display inline images
(setq org-export-html-inline-images t)
;; with an export, don't export latex by default
(setq org-export-with-sub-superscripts nil)
;; with an export, have a single level table of contents
(setq org-export-with-toc 1)
;; with an export, turn off priority cookies
(setq org-export-with-priority nil)
;; with an export, turn of tags in table of contents
(setq org-export-with-tags 'not-in-toc)
;; hitting return over a link executes it
(setq org-return-follows-link t)
;; Where to refile items
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)
			   (nil :maxlevel . 5)))
;; Use path-like completion when refiling
(setq org-refile-use-outline-path '(file))
;; when refiling an item, add it to the top of a list, not the end
(setq org-reverse-note-order t)

(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "STARTED(s!)" "EVENT(e!)" "WAITING(w!)" "SOMEDAY(f!)" "|" "CANCELED(c!)" "DONE(d!)")))
  
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

(defun my-org-todo ()
  "Narrow the subtree to TODO entries"
  (interactive)
  (org-narrow-to-subtree)
  (org-show-todo-tree nil)
  (widen))

(defun my-quote-org (start end)
  "Prints region starting and ending positions."
  (interactive "r")
  (let ()
    (message "Region starts: %d, end at: %d" start end)
    (goto-char (+ 1 end))
    (insert "#+END_EXAMPLE\n")
    (goto-char start)
    (insert "#+BEGIN_EXAMPLE\n")
    )
  )

;; set default timer time
(setq org-timer-default-timer 25)
;; set clock in behavior
(add-hook 'org-clock-in-hook '(lambda () 
				(if (not org-timer-current-timer) 
				      (org-timer-set-timer '(16)))))
;; Include the diary in the agenda
(setq org-agenda-include-diary t)
;; log done time on TODOs
(setq org-log-done 'time)
;; log repeat time on TODOs
(setq org-log-repeat 'time)
;; How we define stuck projects
(setq org-stuck-projects '("+LEVEL=2+PROJECTS/-MAYBE-DONE" ("NEXT") ("@DAILY") ""))
;; Highlight line underneath cursor in agenda views
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
;; Set how far into the past and future to see org-habit chart
(setq org-habit-following-days 7)
(setq org-habit-preceding-days 7)
;; Show all agenda dates even if they are empty.
(setq org-agenda-show-all-dates t)


;; http://stackoverflow.com/questions/1851390/custom-agenda-view-in-org-mode-combining-dates-and-tags
;; http://blog.edencardim.com/2011/06/gtd-with-org-mode-part-3/
(setq org-agenda-custom-commands
      '(
	("R" "Weekly Review"
         ( (agenda "" ((org-agenda-ndays 1)  
		       )) 
	  ;; type "l" in the agenda to review logged items 
          (todo "WAITING")
          (todo "STARTED")
	  (todo "NEXT")
          (stuck "" ((org-agenda-prefix-format ""))) 
          (todo "TODO")
	  (todo "SOMEDAY")
	  ;(tags "PROJECTS+LEVEL=2" ((org-agenda-prefix-format "")
				    ;(org-agenda-filter-preset '("-@DAILY" "-NOTES"))))
	  )) 

	("x" "With deadline columns" alltodo ""
         ((org-agenda-overriding-columns-format "%20ITEM %DEADLINE")
          (org-agenda-view-columns-initially t)))

	("P" "Printed agenda"
         ((agenda "" ((org-agenda-ndays 3)                      ;; daily agenda
                      (org-deadline-warning-days 7)             ;; 7 day advanced warning for deadlines
                      (org-agenda-todo-keyword-format "[ ]")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-prefix-format "%t%s")))

          (todo "WAITING"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T %-12:c   ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
		 (org-agenda-filter-preset '("-@DAILY"))
                 (org-agenda-overriding-header "\nWaiting Tasks\n------------------\n")))

          (todo "STARTED"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T %-12:c   ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
		 (org-agenda-filter-preset '("-@DAILY"))
                 (org-agenda-overriding-header "\nStarted Tasks\n------------------\n")))

          (todo "NEXT"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T %-12:c  ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
		 (org-agenda-filter-preset '("-@DAILY"))
                 (org-agenda-overriding-header "\nNext Tasks\n------------------\n")))

	  (todo "TODO"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T %-12:c\t")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
		 (org-agenda-filter-preset '("-@DAILY"))
                 (org-agenda-overriding-header "\nTODO\n------------------\n")))

	  (tags "PROJECTS+LEVEL=2"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T: \t")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
		 (org-agenda-filter-preset '("-@DAILY" "-NOTES"))
                 (org-agenda-overriding-header "\nProjects\n------------------\n")))
	  )
         ((org-agenda-with-colors nil)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 4)
	  (ps-landscape-mode t))
         ("~/Dropbox/_notes/agenda.html"))


      ("D" "Upcoming deadlines" agenda ""
         ((org-agenda-entry-types '(:deadline))
          (org-agenda-ndays 1)
          (org-deadline-warning-days 60)
          (org-agenda-time-grid nil)))

	("W" "Agenda and Work-related tasks and headlines"
	 ((agenda "" ((org-agenda-ndays 7)))
	  (tags-todo "+@WORK")
	  (tags "+@WORK")))

	("C" "Calendar" agenda ""
         ((org-agenda-ndays 7)                          
          (org-agenda-start-on-weekday 0)               
          (org-agenda-time-grid nil)                    
          (org-agenda-repeating-timestamp-show-all t)   ;; ensures that repeating events appear on all relevant dates
          (org-agenda-entry-types '(:timestamp :sexp))))

        ))


;; -----------------------------------------------
;; Setup autocomplete
;; http://cx4a.org/software/auto-complete/index.html
;; -----------------------------------------------
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
;; case sensitivity is important when finding matches
(setq ac-ignore-case nil)
;; Let's have yasnippets in the auto-complete dropdown
(add-to-list 'ac-sources 'ac-source-yasnippet)

(message "All Done!")

;;; init.el ends here
