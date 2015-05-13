(require 'dash)
(require 'ansi-color)
(require 'compile)

(defvar mutant-project-root-files
  '(".git" "Gemfile" ".projectile")
  "A list of files that might indicate the root directory of a project.")

(defvar mutant-use-bundle t
  "Run mutant through bundle exec command.")

(defvar mutant-cmd-base "mutant"
  "The command used to run mutant")

(defvar mutant-cmd-strategy
  "--use rspec"
  "The strategy to be used in mutation.")

(defvar mutant-regexp-alist
  '(("\\(_spec\\)?\\.rb" . "")
    ("^\\(app\\|spec\\|test\\)\\/.+?\\/" . "")
    ("^lib\\/" . "")
    ("/" . "::")
    ("_" . ""))
  "A list of regular expressions to be applied upon a file name.")

(defun mutant-cmd-builder (&optional match-exp)
  "Build each part of the mutant command."
  (-> (mutant-cmd-bundle)
       (mutant-join mutant-cmd-base)
       (mutant-join (mutant-cmd-rails-env))
       (mutant-join mutant-cmd-strategy)
       (mutant-join match-exp)))

(defun mutant-cmd-bundle ()
  "Returns 'bundle exec' if `mutant-use-bundle` is non-nil.'"
  (when mutant-use-bundle "bundle exec"))

(defun mutant-cmd-rails-env ()
  "Boot Rails environment, if available."
  (when (file-exists-p
         (expand-file-name "config/environment.rb" (mutant-project-root)))
    "--require ./config/environment"))

(defun mutant-project-root ()
  "Retrieve the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (or (->> mutant-project-root-files
        (--map (locate-dominating-file default-directory it))
        (-remove #'null)
        (car))
      (error "You're not into a project")))

(defun mutant-guess-class-name (file-name)
  "Guess the name of a class based on FILE-NAME."
  (let* ((relative-name (file-relative-name file-name (mutant-project-root)))
         (class-name (capitalize relative-name)))
    (->> mutant-regexp-alist
         (--reduce-from (replace-regexp-in-string (car it) (cdr it) acc nil t)
                        class-name))))

(defun mutant-check-file (&optional file-name)
  "Run Mutant over a single file.
If none is given, than `buffer-file-name` is used."
  (interactive)
  (let* ((file-name (or file-name (buffer-file-name)))
         (class-name (mutant-guess-class-name file-name)))
    (mutant-run class-name)))

(defun mutant-check-from-dired ()
  "Run Mutant over all marked files in dired.
If there are no files marked, use that under cursor."
  (interactive)
  (--> (dired-get-marked-files)
       (mapconcat 'mutant-guess-class-name it " ")
       (mutant-run it)))

(defun mutant-check-custom (&optional match-exp)
  "Run Mutant over MATCH-EXP.
When called without argument, prompt user."
  (interactive)
  (let ((match-exp (or match-exp (read-input "Match expression: "))))
    (mutant-run match-exp)))

(defun mutant-run (match-exp)
  "Execute mutant command under compilation mode with given MATCH-EXP."
  (let ((default-directory (or (mutant-project-root) default-directory))
        (full-cmd (mutant-cmd-builder match-exp)))
    (compile full-cmd 'mutant-compilation-mode)))

(defun mutant-join (&rest args)
  (--> args
       (-remove #'null it)
       (mapconcat 'identity it " ")))

(define-compilation-mode mutant-compilation-mode "Mutant Compilation"
  "Compilation mode for Mutant output."
  (add-hook 'compilation-filter-hook 'mutant-colorize-compilation-buffer nil t))

(defun mutant-colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(provide 'mutant)
