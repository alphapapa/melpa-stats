(require 'json)
(require 'seq)

(require 'a)
(require 'dash)
(require 'dash-functional)

(defvar melpa/archive-json-url "https://melpa.org/archive.json")
(defvar melpa/downloads-json-url "https://melpa.org/download_counts.json")
(defvar melpa/packages nil
  "MELPA packages, read from archive.json.")
(defvar melpa/downloads nil
  "MELPA package downloads, read from download_counts.json.")

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

(defun melpa/downloads (&optional refresh)
  (when (or refresh (not melpa/downloads))
    (setf melpa/downloads (melpa/retrieve-json melpa/downloads-json-url)))
  melpa/downloads)

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

(defun melpa/package-version-and-downloads (package-name)
  (let* ((package-name (cl-typecase package-name
                         (string (intern package-name))
                         (symbol package-name)))
         (package (->> (melpa/packages)
                       (--select (equal package-name (car it)))
                       (car))))
    (a-list 'version (melpa/package-field '(ver) package)
            'downloads (a-get (melpa/downloads) package-name))))
