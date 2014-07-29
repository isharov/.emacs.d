;; packages
(setq pkg-root "/home/isharov/.emacs.d/el-get/")

(defun load-packages (pkgs)
  (dolist (pkg pkgs)
    (add-to-list 'load-path (concat pkg-root (symbol-name pkg)))
    (require pkg)))

;; common editor customization
(setq c-default-style "linux"
      c-basic-offset 4
      backup-inhibited t
      require-final-newline t
      kill-whole-line t)
(setq-default tab-width 4)
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-12"))
(fset 'yes-or-no-p 'y-or-n-p) ; type y/n instead of yes/no

;; common modes
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(global-auto-revert-mode t)
(show-paren-mode 1)
(desktop-save-mode 1)
(electric-pair-mode 1)

;; window splitting
(when (not (daemonp))
  (split-window-vertically)
  (other-window 1)
  (split-window-horizontally)
  (other-window 2)
  (split-window-horizontally))

;; window navigation
(windmove-default-keybindings 'meta)
;(setq windmove-wrap-around t)
(winner-mode 1)

;; enable some commands
(put 'erase-buffer 'disabled nil)

;; ido mode
(require 'ido) ; interactively do things with buffers and files
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(load-packages '(smex))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x

;; uniquify mode
(require 'uniquify) ; making buffer names unique
(setq uniquify-buffer-name-style 'post-forward)

;; dired
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "<return>")
              'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
            ))

;; multiple cursors
(load-packages '(multiple-cursors))
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C->") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-c C-m") 'mc/mark-all-in-region)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-S-c C-SPC") 'set-rectangular-region-anchor)

;; fast cursor move
(load-packages '(ace-jump-mode))
(global-set-key (kbd "C-c SPC") 'ace-jump-word-mode)
;(global-set-key (kbd "C-u C-c SPC") 'ace-jump-char-mode)
;(global-set-key (kbd "C-u C-u C-c SPC") 'ace-jump-line-mode)

;; find-things-fast retrieves project files by git or hg
(load-packages '(find-things-fast))
(setq ftf-filetypes '("*"))
(global-set-key [f6] 'ftf-find-file)
(global-set-key [f7] 'ftf-grepsource)
(global-set-key [M-f7] 'isharov/rgrep)
(global-set-key [f9] 'ftf-compile)

;; text selection
(load-packages '(expand-region))
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(global-set-key (kbd "S-M-SPC") 'isharov/select-current-line)

;; TAGS
(load-packages '(ggtags))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(global-set-key [f10] 'isharov/compile-gtags)
;(global-set-key (kbd "M-.") 'isharov/find-tag)

;; spell checking
(global-set-key [f8] 'isharov/toggle-flyspell)

;; scala
(load-packages '(scala-mode2))
(add-hook 'scala-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e")
                           (lambda()
                             (interactive)
                             (buffer/create-send-region "*sbt-console*" "sbt console-quick")))))

;; C++
;;(load-file (concat pkg-root "cedet/common/cedet.el"))
;;(add-hook 'c-mode-common-hook 'isharov/cedet-hook)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key "\C-ct" 'isharov/toggle-source)
            (modify-syntax-entry ?_ "w") ; now '_' is not considered a word-delimiter
            ))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; using c++ mode for *.h files

;; Python
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "")
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-hook 'python-mode-hook
          (lambda ()
            (defvar python-shell-directory)
            (add-to-list 'python-shell-setup-codes 'python-shell-directory)
            (local-set-key (kbd "C-c C-v") 'isharov/python-check)
            (local-set-key (kbd "C-c V") 'isharov/python-check-dir)
            (local-set-key (kbd "C-x C-e") (lambda () (interactive) (buffer/create-send-region "*shell*" "")))
            (local-set-key (kbd "C-c C-z") (isharov/python-cd-project 'python-shell-switch-to-shell))
            (local-set-key (kbd "C-c C-c") (isharov/python-cd-project 'python-shell-send-buffer))
            (modify-syntax-entry ?_ "w") ; now '_' is not considered a word-delimiter
            ))

;; git
(add-to-list 'load-path (concat pkg-root "git-modes"))
(load-packages '(magit git-gutter))
(global-git-gutter-mode t)
;(custom-set-variables '(git-gutter:hide-gutter t))
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-c v g") 'magit-status)
(add-to-list 'git-gutter:update-hooks 'magit-revert-buffer-hook)
(add-to-list 'git-gutter:update-commands 'previous-buffer)
(add-to-list 'git-gutter:update-commands 'next-buffer)

;; cvs
(global-set-key (kbd "C-c v c") 'isharov/cvs-status)

;; color-theme
(add-to-list 'custom-theme-load-path (concat pkg-root "color-theme-solarized"))
(when (window-system)
  (load-theme 'solarized-dark t))

;; el-get
(load-packages '(el-get))


;; custom functions
(defun isharov/cvs-status ()
  (interactive)
  (cvs-examine (ftf-project-directory) nil))

(defun isharov/toggle-source ()
  "Toggle between source and implementation files"
  (interactive)
  (let* ((root-dir    (ftf-project-directory))
         (current-dir (path/related-dir))
         (include-dir (path/replace-first current-dir "src" "include"))
         (source-dir  (path/replace-first current-dir "include" "src")))
    (setq ff-search-directories
          (append (list (path/join root-dir include-dir)
                        (path/join root-dir source-dir))
                  (path/subdirs-rec root-dir))
          ff-always-try-to-create nil)
    (ff-find-other-file)))

(defun isharov/toggle-flyspell ()
  "Toggle spell-checking mode"
  (interactive)
  (if flyspell-mode
      (flyspell-mode -1)
    (progn
      (flyspell-mode)
      (ispell-change-dictionary "english"))))

(defun isharov/rgrep ()
  "Recursive grep search"
  (interactive)
  (let ((term (completing-read "rgrep: " nil nil nil (thing-at-point 'word))))
    (grep-compute-defaults)
    (rgrep term "*" (ftf-project-directory))))

(defun isharov/diff-region ()
  "Select a region to compare"
  (interactive)
  (when (use-region-p) ; there is a region
    (let (buf)
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (region-beginning) (region-end)))
    )
  (message "Now select other region to compare and run `diff-region-now`"))

(defun isharov/diff-region-now ()
  "Compare current region with region already selected by `diff-region`"
  (interactive)
  (when (use-region-p)
    (let (bufa bufb)
      (setq bufa (get-buffer-create "*Diff-regionA*"))
      (setq bufb (get-buffer-create "*Diff-regionB*"))
      (save-current-buffer
        (set-buffer bufb)
        (erase-buffer))
      (append-to-buffer bufb (region-beginning) (region-end))
      (ediff-buffers bufa bufb))))

(defun isharov/select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun isharov/compile-gtags ()
  (interactive)
  (ggtags-create-tags (ftf-project-directory)))

(defun isharov/remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun isharov/python-check ()
  (interactive)
  (let ((buf (file-name-nondirectory (buffer-file-name (current-buffer)))))
    (python-check (python/check-command buf))))

(defun isharov/python-check-dir ()
  (interactive)
  (let ((dir (read-directory-name "Python check dir: " (ftf-project-directory))))
    (python-check (python/check-command dir))))

(defun isharov/python-cd-project (fun)
  (lexical-let ((fun fun))
    (lambda ()
      (interactive)
      (setq python-shell-directory (format "import os\nos.chdir(os.path.expanduser('%s'))" (ftf-project-directory)))
      (funcall fun))))

;; utils
(defun some (pred elems)
  (let ((e (car elems)))
    (if (or (not e) (funcall pred e))
        e
      (some pred (cdr elems)))))

(defun path/replace-first (str old new)
  (replace-regexp-in-string (concat "\\(/" old "/\\).*\\'")
                            (concat "/" new "/") str nil nil 1))

(defun path/join (&rest elems)
  (mapconcat 'identity elems "/"))

(defun path/parent (path)
  (expand-file-name (path/join path "..")))

(defun path/subdirs (base)
  (let ((res nil))
    (dolist (f (directory-files base))
      (let ((name (path/join base f)))
        (when (and (file-directory-p name)
                   (not (equal (substring f 0 1) ".")))
          (add-to-list 'res name))))
    res))

(defun path/subdirs-rec (base)
  (let ((res nil))
    (dolist (s (path/subdirs base))
      (add-to-list 'res s)
      (dolist (r (path/subdirs-rec s))
        (add-to-list 'res r)))
    res))

(defun path/current-dir ()
  (file-name-directory (or (buffer-file-name (current-buffer)) default-directory)))

(defun path/related-dir ()
  (substring (path/current-dir) (length (ftf-project-directory))))

(defun buffer/send-region (buffer)
  (let ((current-buffer (current-buffer)))
    (if (use-region-p)
        (append-to-buffer buffer (region-beginning) (region-end))
      (append-to-buffer buffer (line-beginning-position) (line-beginning-position 2)))
    (switch-to-buffer-other-window buffer)
    (execute-kbd-macro "\C-m")
    (switch-to-buffer-other-window current-buffer)))

(defun buffer/create-shell (buffer cmd)
  (when (not (get-buffer buffer))
    (let ((current-buffer (current-buffer))
          (cmd (completing-read "command: " nil nil nil cmd)))
      (shell buffer)
      (previous-buffer)
      (switch-to-buffer-other-window buffer)
      (insert (concat "cd " (ftf-project-directory) " && " cmd))
      (execute-kbd-macro "\C-m")
      (switch-to-buffer-other-window current-buffer))))

(defun buffer/create-send-region (shell-name start-command)
  (buffer/create-shell shell-name start-command)
  (buffer/send-region shell-name))

(defun python/check-command (path)
  (format "flake8 --ignore=E501 %s" path))

;; deprecated functions
;(defun isharov/find ()
;  "Recursive file search by name"
;  (interactive)
;  (let ((term (completing-read "find: " nil nil nil (concat "*" (thing-at-point 'word) "*"))))
;    (find-name-dired (heu/project-root) term)))
;
;(defun isharov/cedet-hook ()
;  (semantic-load-enable-code-helpers) ; Prototype help and smart completion
;  (global-srecode-minor-mode 1) ; Template insertion menu
;  (require 'semantic-ia) ; Names completion, displaying of info for tags & classes
;  (require 'semantic-gcc) ; Auto find path, where system include files are located
;
;  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
;  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
;  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
;  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
;  (local-set-key "\C-cr" 'semantic-symref)
;  (local-set-key "\C-cf" 'semantic-symref-symbol))
;
;(defun ede/project-root ()
;  "Determines the project root directory for the current buffer"
;  (ede-project-root-directory (ede/current-project)))
;
;(defun ede/get-local (var)
;  "Fetch given variable var from :local-variables of project of file fname"
;  (let ((prj (ede/current-project)))
;    (when prj
;      (let* ((ov (oref prj local-variables))
;             (lst (assoc var ov)))
;        (when lst
;          (cdr lst))))))
;
;(defun ede/get-local (var)
;  "Fetch given variable var from :local-variables of project of file fname"
;  (let ((prj (ede/current-project)))
;    (when prj
;      (let* ((ov (oref prj local-variables))
;             (lst (assoc var ov)))
;        (when lst
;          (cdr lst))))))
;
;(defun heu/project-root ()
;  (or (path/find-up (path/current-dir) '(".hg" ".git" ".svn" "CMakeLists.txt" "Jamroot"))
;      (path/current-dir)))
;
;(defun path/find-up (base attrs)
;  (cond ((string= base "/") nil)
;        ((some (lambda (attr) (file-exists-p (path/join base attr))) attrs) base)
;        (t (path/find-up (path/parent base) attrs))))
;
;(defun isharov/compile ()
;  "Saves all unsaved buffers, and runs 'compile'."
;  (interactive)
;  (save-some-buffers t)
;  (cd (ftf-project-directory))
;  (compile compile-command))
;
;(defun isharov/compile-etags ()
;  (interactive)
;  (cd (ftf-project-directory))
;  (compile "find -regex '.*\\.\\(c\\|cpp\\|h\\|hpp\\|java\\|scala\\|py\\)$' -print | etags -"))
;
;(defun isharov/find-tag ()
;  (interactive)
;  (let ((tags    (path/join (ftf-project-directory) "TAGS"))
;        (tagname (completing-read "Find tag: " nil nil nil (thing-at-point 'word))))
;    (when (file-exists-p tags)
;      (setq tags-revert-without-query t)
;      (setq tags-file-name tags))
;    (find-tag tagname)))
