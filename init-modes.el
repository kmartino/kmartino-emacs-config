(message "Starting init-modes.el...")

;; -----------------------------------------------
;; ChordPro
;; -----------------------------------------------

(setq auto-mode-alist (cons '("\\.pro$" . chordpro-mode) auto-mode-alist))
(autoload 'chordpro-mode "chordpro-mode")

;; -----------------------------------------------
;; Setup EasyPG for file encryption
;; http://epg.sourceforge.jp/
;; -----------------------------------------------
(message "Setting up epa-file for encryption...")
(require 'epa-file)
;(setf epa-pinentry-mode 'loopback)
(setq epg-gpg-program "gpg1")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(epg-gpg-program "gpg1"))

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
;; Setup auto-complete
;; http://cx4a.org/software/auto-complete/
;; -----------------------------------------------
(message "Setting up auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;; -----------------------------------------------
;; Setup elisp and eldoc
;; -----------------------------------------------
(message "Setting up elisp...")

(defun ksm/lisp-hook ()
  (turn-on-eldoc-mode)
  )

(add-hook 'emacs-lisp-mode-hook 'ksm/lisp-hook)

;; -----------------------------------------------
;; Setup python
;; https://github.com/jorgenschaefer/elpy
;; 
;; https://github.com/jorgenschaefer/pyvenv
;; -----------------------------------------------
(message "Setting up python...")
(require 'python)
(setq python-indent-offset 4)

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'python-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))


(elpy-enable)
(setq elpy-rpc-backend "jedi")

(setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))


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

(setq yaml-imenu-generic-expression '((nil "^\\(:?[_a-zA-Z_-\\s-]+\\):" 1)
(nil "^[-]+[ ]+\\([_a-z]+\\): \\([ a-zA-Z0-9]+\\)" 0)
))

;; -----------------------------------------------
;; Setup js-comint
;; http://js-comint-el.sourceforge.net/
;; -----------------------------------------------
(message "Setting up js-comint...")
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



(message "Finished init-modes.el")


