(require 'json)
(require 'seq)

(require 'a)
(require 'dash)
(require 'dash-functional)

(defvar melpa/archive-json-url "https://melpa.org/archive.json")
(defvar melpa/packages nil
  "MELPA packages, read from archive.json.")

(defun melpa/author-package-counts ()
  (interactive)
  (let ((counts (->> (melpa/packages)
                     (--map (melpa/package-field '(props authors) it))
                     (--map (seq-into it 'list))
                     (-flatten)
                     (-sort #'string<)
                     (melpa/count #'identity))))
    (if (called-interactively-p 'any)
        (pp-display-expression counts "*MELPA Authors*")
      counts)))

(defun melpa/maintainer-package-counts ()
  (interactive)
  (let ((counts (->> (melpa/packages)
                     (--map (melpa/package-field '(props maintainer) it))
                     (-sort #'string<)
                     (melpa/count #'identity))))
    (if (called-interactively-p 'any)
        (pp-display-expression counts "*MELPA Maintainers*")
      counts)))

(defun melpa/packages (&optional refresh)
  (when (or refresh (not melpa/packages))
    (setf melpa/packages (melpa/retrieve-json melpa/archive-json-url)))
  melpa/packages)

(defun melpa/retrieve-json (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (re-search-forward "\n\n")
    (prog1 (json-read)
      (kill-buffer))))

(defun melpa/count (fn list)
  (->> list
       (seq-group-by fn)
       (--map (cons (car it) (length (cdr it))))
       (-sort (-on #'> #'cdr))))

(defun melpa/package-field (field package)
  (a-get-in (cdr package) field))
