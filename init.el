;; packages
(setq pkg-root "~/.emacs.d/el-get/")

;; el-get
(add-to-list 'load-path (concat pkg-root "el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; common editor customization
(setq c-default-style "linux"
      c-basic-offset 4
      backup-inhibited t
      require-final-newline t
      kill-whole-line t
      recentf-max-saved-items 50)
(setq-default tab-width 4
              indent-tabs-mode nil
              show-trailing-whitespace nil)
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
;(key-chord-mode 1)
(recentf-mode 1)

;; window navigation
(windmove-default-keybindings 'meta)
;(setq windmove-wrap-around t)
(winner-mode 1)

;; buffer moving
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; enable some commands
(put 'erase-buffer 'disabled nil)

;; auto-delete trailing whitespace
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; helm mode
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-c h /") 'helm-find)
(global-set-key (kbd "C-c h l") 'helm-locate)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h a") 'helm-apropos)
(global-set-key (kbd "C-c h b") 'helm-resume)
(define-key minibuffer-local-map (kbd "M-r") 'helm-minibuffer-history)
(add-to-list 'desktop-globals-to-save 'extended-command-history)

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
(require 'multiple-cursors)
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
(global-set-key (kbd "C-c SPC") 'ace-jump-word-mode)

;; project
(global-set-key (kbd "C-c c") 'project/compile)
(global-set-key (kbd "C-c f") 'helm-git-files)
(global-set-key (kbd "C-c g") 'helm-git-grep-at-point)
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

;; text selection
(require 'expand-region)
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(global-set-key (kbd "S-M-SPC") 'isharov/select-current-line)

;; semantic navigation
(global-set-key (kbd "C-<up>") 'smart-up)
(global-set-key (kbd "C-<down>") 'smart-down)
(global-set-key (kbd "C-<left>") 'smart-backward)
(global-set-key (kbd "C-<right>") 'smart-forward)

;; handy pairs
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-)") 'delete-pair)

;; TAGS
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1)
              (local-set-key (kbd "C-c t") 'tag/compile-gtags))))

(global-set-key (kbd "C-c t") 'tag/compile-etags)
(global-set-key (kbd "M-.") 'tag/find-tag)

;; spell checking
(global-set-key (kbd "C-c s") 'isharov/toggle-flyspell)

;; tramp mode
(setq password-cache-expiry nil)

;; scala
(require 'scala-mode2)
(add-hook 'scala-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r")
                           (lambda()
                             (interactive)
                             (buffer/create-send-region "*sbt-console*" "sbt console-quick")))))

;; clojure
(require 'cider)

;; C++
;;(load-file (concat pkg-root "cedet/common/cedet.el"))
;;(add-hook 'c-mode-common-hook 'isharov/cedet-hook)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-x t") 'isharov/toggle-source)
            (modify-syntax-entry ?_ "w") ; now '_' is not considered a word-delimiter
            ))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; using c++ mode for *.h files

;; Python
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-hook 'python-mode-hook
          (lambda ()
            (defvar python-shell-directory)
            (add-to-list 'python-shell-setup-codes 'python-shell-directory)
            (local-set-key (kbd "C-c C-v") 'python/check)
            (local-set-key (kbd "C-c V") 'python/check-dir)
            (local-set-key (kbd "C-c C-z") (python/with-project 'python-shell-switch-to-shell))
            (local-set-key (kbd "C-c C-c") (python/with-project 'python-shell-send-buffer))
            (modify-syntax-entry ?_ "w") ; now '_' is not considered a word-delimiter
            ))
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (comint/turn-on-history)
            (define-key inferior-python-mode-map (kbd "M-r") 'helm-comint-input-ring)
            ))

;; git
(require 'magit)
(require 'git-gutter)
(global-git-gutter-mode -1)
;(custom-set-variables '(git-gutter:hide-gutter t))
(global-set-key (kbd "C-x C-g") 'git-gutter-mode)
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

;; svn
(require 'dsvn)
(global-set-key (kbd "C-c v s") 'isharov/svn-status)

;; color-theme
(when (window-system)
  (load-theme 'zenburn t))

;; shell
(add-hook 'shell-mode-hook
          (lambda ()
            (setq comint-input-ring-file-name "~/.emacs.d/.shell_history"
                  comint-input-ignoredups t
                  comint-input-ring-size 1000)
            (comint-read-input-ring t)))
;(add-hook 'shell-mode-hook (lambda () (goto-address-mode)))
;(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(add-hook 'kill-buffer-hook 'comint-write-input-ring)
(add-hook 'kill-emacs-hook 'comint/write-input-ring-all-buffers)
(define-key shell-mode-map (kbd "M-r") 'helm-comint-input-ring)


(defun isharov/cvs-status ()
  (interactive)
  (cvs-examine (project/root) nil))

(defun isharov/svn-status ()
  (interactive)
  (svn-status (project/root)))

(defun isharov/toggle-source ()
  "Toggle between source and implementation files"
  (interactive)
  (let* ((root-dir    (project/root))
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
      (ispell-change-dictionary "english")
      (flyspell-mode))))

(defun isharov/select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun tag/compile-gtags ()
  (interactive)
  (ggtags-create-tags (project/root)))

(defun tag/compile-etags ()
  (interactive)
  (cd (project/root))
  (compile "find -regex '.*\\.\\(c\\|cpp\\|h\\|hpp\\|java\\|scala\\|py\\)$' -print | etags -"))

(defun tag/find-tag ()
  (interactive)
  (let ((tags (path/join (project/root) "TAGS"))
        (tagname (completing-read "Find tag: " nil nil nil (thing-at-point 'word))))
    (when (file-exists-p tags)
      (setq tags-revert-without-query t)
      (setq tags-file-name tags)
      (setq tags-table-list nil))
    (find-tag tagname)))

(defun sudo/edit-current-file ()
  (interactive)
  (let ((my-file-name) ; fill this with the file to open
        (position))    ; if the file is already open save position
    (if (equal major-mode 'dired-mode) ; test if we are in dired-mode 
        (progn
          (setq my-file-name (dired-get-file-for-visit))
          (find-alternate-file (tramp/prepare-sudo-string my-file-name)))
      (setq my-file-name (buffer-file-name); hopefully anything else is an already opened file
            position (point))
      (find-alternate-file (tramp/prepare-sudo-string my-file-name))
      (goto-char position))))

(defun tramp/prepare-sudo-string (tempfile)
  (if (file-remote-p tempfile)
      (let ((vec (tramp-dissect-file-name tempfile)))
        (tramp-make-tramp-file-name
         "sudo"
         (tramp-file-name-user nil)
         (tramp-file-name-host vec)
         (tramp-file-name-localname vec)
         (format "ssh:%s@%s|"
                 (tramp-file-name-user vec)
                 (tramp-file-name-host vec))))
    (concat "/sudo:root@localhost:" tempfile)))

(defun python/check ()
  (interactive)
  (let ((buf (file-name-nondirectory (buffer-file-name (current-buffer)))))
    (python-check (python/check-command buf))))

(defun python/check-dir ()
  (interactive)
  (let ((dir (read-directory-name "Python check dir: " (project/root))))
    (python-check (python/check-command dir))))

(defun python/check-command (path)
  (format "flake8 --ignore=E501 %s" path))

(defun python/with-project (fun)
  (lexical-let ((fun fun))
    (lambda ()
      (interactive)
      (setq python-shell-directory (format "import os\nos.chdir(os.path.expanduser('%s'))" (project/root)))
      (funcall fun))))

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
  (substring (path/current-dir) (length (project/root))))

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
      (insert (concat "cd " (project/root) " && " cmd))
      (execute-kbd-macro "\C-m")
      (switch-to-buffer-other-window current-buffer))))

(defun buffer/create-send-region (shell-name start-command)
  (buffer/create-shell shell-name start-command)
  (buffer/send-region shell-name))

(defun buffer/remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun buffer/diff-region ()
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

(defun buffer/diff-region-now ()
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

(defun buffer/mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

(defun comint/write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))

(defun comint/turn-on-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (format "~/.emacs.d/.inferior-%s-history"
                    (process-name process)))
      (comint-read-input-ring)
      (set-process-sentinel process 'comint/write-history-on-exit))))

(defun comint/write-input-ring-all-buffers ()
  (buffer/mapc-buffers 'comint-write-input-ring))

(defun project/root ()
  (if (tramp-tramp-file-p (buffer-file-name))
      (path/current-dir)
    (or (project/locals-root)
        (project/git-root default-directory)
        default-directory)))

(defun project/locals-root ()
  (defun parent-dir (path)
    (file-name-directory (directory-file-name path)))
  (defun dir-has-project-file (path)
    (or (file-exists-p (concat path "/.dir-locals.el"))
        (file-exists-p (concat path "/.emacs-project"))))
  (let ((path default-directory)
        (return-path nil))
    (while path
      (cond ((string-equal path "/")
             (setq return-path nil
                   path nil))
            ((dir-has-project-file path)
             (setq return-path path
                   path nil))
            (t (set 'path (parent-dir path)))))
    return-path))

(defun project/git-root (dir)
  (with-temp-buffer
    (if (eq 0 (call-process "git" nil t nil "rev-parse"))
        (let ((cdup (with-output-to-string
                      (with-current-buffer standard-output
                        (cd dir)
                        (call-process "git" nil t nil
                                      "rev-parse" "--show-cdup")))))
          (expand-file-name (concat (file-name-as-directory dir)
                                    (car (split-string cdup "\n")))))
      nil)))

(defmacro project/with-root (&rest body)
  `(let ((default-directory (project/root)))
     ,@body))

(defun project/compile ()
  (interactive)
  (project/with-root (call-interactively 'compile)))

(defun project/rgrep ()
  "Recursive grep search"
  (interactive)
  (let ((term (completing-read "rgrep: " nil nil nil (thing-at-point 'word))))
    (grep-compute-defaults)
    (rgrep term "*" (project/root))))
