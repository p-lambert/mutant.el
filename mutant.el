(require 'dash)

(defvar mutant-project-root-files
  '(".git" "Gemfile" ".projectile")
  "A list of files that might indicate the root directory of a project.")

(defvar mutant-regexp-alist
  '(("\\(_spec\\)?\\.rb" . "")
    ("^\\(app\\|spec\\|test\\)\\/.+?\\/" . "")
    ("^lib\\/" . "")
    ("/" . "::")
    ("_" . ""))
  "A list of regular expressions to be applied upon a file name.")

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

(provide 'mutant)
