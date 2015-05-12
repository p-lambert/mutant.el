(require 'dash)

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
  "Guess the name of the class based on file name."
  (let* ((relative-name (file-relative-name file-name (mutant-project-root)))
         (class-name (capitalize relative-name)))
    (->> mutant-regexp-alist
         (--reduce-from (replace-regexp-in-string (car it) (cdr it) acc nil t)
                        class-name))))

(defun mutant-join (&rest args)
  (--> args
       (-remove #'null it)
       (mapconcat 'identity it " ")))

(provide 'mutant)
