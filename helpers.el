(eval-when-compile (require 'cl))

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
  (if (not (use-region-p))
      (progn
        (set-mark (line-end-position))
        (back-to-indentation))
    (beginning-of-line)))

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

(defun buffer/tag-region (tag)
  "'tag' a region"
  (interactive "stag: ")
  (if (use-region-p)
      (let ((b (region-beginning))
            (e (region-end)))
        (save-excursion
          (goto-char e)
          (insert (format "</%s>" tag))
          (goto-char b)
          (insert (format "<%s>" tag))))
    (progn
      (save-excursion
        (insert (format "</%s>" tag)))
      (insert (format "<%s>" tag)))))

(defun buffer/occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun buffer/ansi-colorize ()
  "Colorize ansi escape sequences in the current buffer."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun buffer/shell-command (&optional cmd)
  "Do shel command on current buffer file"
  (interactive "P")
  (when (not cmd)
    (setq cmd (read-from-minibuffer "Command: ")))
  (shell-command (format "%s %s" cmd (shell-quote-argument buffer-file-name))))

(defun comint/write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))

(defun comint/turn-on-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name (format "~/.emacs.d/.inferior-%s-history" (process-name process))
            comint-input-ignoredups t
            comint-input-ring-size 1000)
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

(defun project/ag ()
  (interactive)
  (helm-do-ag (project/root)))
