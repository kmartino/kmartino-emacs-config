(message "Starting init-markdown.el...")

(defun ksm/markdown-insert-screenshot()
  "Run screencapture in interactive mode and saves to a directory named after the buffer file name."
  (interactive)
  (let* ((dir-name (file-name-as-directory(concat buffer-file-name ".static")))
         (timestamp (number-to-string (floor (time-to-seconds (current-time)))))
         (file-name (file-relative-name(concat dir-name timestamp ".png")))
         )

    (unless (file-exists-p dir-name)
      (make-directory dir-name))

    (call-process-shell-command (concat "/usr/sbin/screencapture -s " file-name))
    (insert "\n![](" file-name ")\n")
    )
  (save-buffer)
  )

(defun ksm/markdown-insert-quoted-block ()
  "Created a fenced in code block"
  (interactive)
  (insert "```\n")
  (insert "```\n")
  )

(defun ksm/markdown-insert-uml ()
  "Creates a PlantUML block for markdown processing and creates a directory for its rendering."
  (interactive)
  (let* ((dir-name (file-name-as-directory(concat buffer-file-name ".static")))
         (timestamp (number-to-string (floor (time-to-seconds (current-time)))))
         (file-name (file-relative-name(concat dir-name timestamp ".png")))
         )
    (unless (file-exists-p dir-name)
      (make-directory dir-name))
    (insert "\n```\n")
    (insert "@startuml " file-name "\n")
    (insert "\n")
    (insert "@enduml\n")
    (insert "```\n")
    (insert "![](" file-name ")\n")    
    )
    (save-buffer)
    )

(defun ksm/mkcurrent()
  "Create a markdown file named after the current date"
  (interactive)
  (let*  ((dir-name default-directory)
          (timestamp (format-time-string "%Y%m%d"))
          (file-name (file-relative-name (concat dir-name timestamp ".md")))
          )
    (message file-name)
    (unless (file-exists-p file-name)
      (write-region (format-time-string "# [%Y-%m-%d]\n\n") nil file-name t)
      (make-directory dir-name)
      )
    (find-file file-name)
    (goto-char (point-min))
    (end-of-line)
    )
  )

(defun ksm/markdown-preview-file ()
  "Run external app to preview and process the buffer"
  (interactive)
  (save-buffer)
  (shell-command
   (format ksm/MARKDOWN-PREVIEW-GUI
           (shell-quote-argument (buffer-file-name))))
  )

(defun ksm/markdown-process-and-preview-file ()
  "Process PlantUML and preview using external app"
  (interactive)
  (save-buffer)
  (shell-command
   (format ksm/PLANTUML-RENDER
           (shell-quote-argument (buffer-file-name))))
  
  (ksm/markdown-preview-file)
  )

(defun ksm/insert-datestamp()
  "Inserts a datestamp into the current buffer"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d]"))
  )

(defun ksm/insert-timestamp()
  "Insters a timestamp into the current buffer"
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %H:%M:%S]"))
  )

;; via https://github.com/yuandisheng/.emacs.d/blob/e9d403624a78f01b316a2729fd5c8d5335c0f080/init-markdown.el
(defun ksm/markdown-imenu-create-index ()
  "Extends imenu to include headers from a markdown file"
  (let* ((root '(nil . nil))
         cur-alist
         (cur-level 0)
         (pattern "^\\(\\(#+\\)[ \t]*\\(.+\\)\\|\\([^# \t\n=-].*\\)\n===+\\|\\([^# \t\n=-].*\\)\n---+\\)$")
         (empty-heading "-")
         (self-heading ".")
         hashes pos level heading)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern (point-max) t)
        (cond
         ((setq hashes (match-string-no-properties 2))
          (setq heading (match-string-no-properties 3)
                pos (match-beginning 1)
                level (length hashes)))
         ((setq heading (match-string-no-properties 4))
          (setq pos (match-beginning 4)
                level 1))
         ((setq heading (match-string-no-properties 5))
          (setq pos (match-beginning 5)
                level 2)))
        (let ((alist (list (cons heading pos))))
          (cond
           ((= cur-level level)		; new sibling
            (setcdr cur-alist alist)
            (setq cur-alist alist))
           ((< cur-level level)		; first child
            (dotimes (i (- level cur-level 1))
              (setq alist (list (cons empty-heading alist))))
            (if cur-alist
                (let* ((parent (car cur-alist))
                       (self-pos (cdr parent)))
                  (setcdr parent (cons (cons self-heading self-pos) alist)))
              (setcdr root alist))		; primogenitor
            (setq cur-alist alist)
            (setq cur-level level))
           (t				; new sibling of an ancestor
            (let ((sibling-alist (last (cdr root))))
              (dotimes (i (1- level))
                (setq sibling-alist (last (cdar sibling-alist))))
              (setcdr sibling-alist alist)
              (setq cur-alist alist))
            (setq cur-level level)))))
      (cdr root))))

(setq markdown-imenu-generic-expression
      '(("title"  "^\\(.*\\)[\n]=+$" 1)
        ("h2-"    "^\\(.*\\)[\n]-+$" 1)
        ("h1"   "^# \\(.*\\)$" 1)
        ("h2"   "^## \\(.*\\)$" 1)
        ("h3"   "^### \\(.*\\)$" 2)
        ("h4"   "^#### \\(.*\\)$" 3)
        ("h5"   "^##### \\(.*\\)$" 4)
        ("h6"   "^###### \\(.*\\)$" 5)
        ("fn"   "^\\[\\^\\(.*\\)\\]" 6)
        ))

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (local-set-key (kbd "C-c .") 'ksm/insert-datestamp)
            (local-set-key (kbd "C-c ,") 'ksm/insert-timestamp)            
            (local-set-key (kbd "C-c m") 'ksm/markdown-process-and-preview-file)
            (local-set-key (kbd "C-c v") 'ksm/markdown-preview-file) 
            (local-set-key (kbd "C-c /") 'ksm/markdown-insert-screenshot)
            (local-set-key (kbd "C-c u") 'ksm/markdown-insert-uml)
            (markdown-cycle t)
            (setq imenu-create-index-function 'ksm/markdown-imenu-create-index)
            (setq markdown-gfm-use-electric-backquote 'nil)
            (setq markdown-enable-wiki-links t)
            )
          )

;; (eval-after-load 'outline
;;   '(progn
;;     (require 'outline-magic)
;;     ))

(message "Finished init-markdown.el")


