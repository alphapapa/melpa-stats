;;; melpa-stats.el --- MELPA package statistics      -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/melpa-stats
;; Keywords: packages, MELPA
;; Package-Requires: ((emacs) (a) (anaphora) (dash) (dash-functional) (ht) (s))

;;; Commentary:

;;

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'json)
(require 'pp)
(require 'seq)

(require 'a)
(require 'anaphora)
(require 'dash)
(require 'dash-functional)
(require 'ht)

;;;; Variables

(defvar melpa-stats/archive-json-url "https://melpa.org/archive.json")
(defvar melpa-stats/downloads-json-url "https://melpa.org/download_counts.json")

(defvar melpa-stats/packages nil
  "MELPA packages, read from archive.json.")
(defvar melpa-stats/downloads nil
  "MELPA package downloads, read from download_counts.json.")

;;;; Customization


;;;; Commands

(defun melpa-stats/author-maintainer-package-counts ()
  "Return list of people by number of packages authored or maintained."
  (interactive)
  (let* ((people (ht))
         (counts))
    (cl-loop for package in (melpa-stats/packages)
             for authors = (-flatten (seq-into (melpa-stats/package-field '(props authors) package) 'list))
             when authors
             do (cl-loop for author in authors
                         do (push package (gethash author people))))
    (cl-loop for package in (melpa-stats/packages)
             for maintainer = (melpa-stats/package-field '(props maintainer) package)
             when maintainer
             do (push package (gethash maintainer people)))
    (setf counts (->> (cl-loop for person being the hash-keys of people
                               using (hash-values packages)
                               do (setf packages (-uniq packages))
                               collect (cons person (length packages)))
                      (-sort (-on #'> #'cdr))))
    (if (called-interactively-p 'any)
        (pp-display-expression counts "*MELPA Authors and Maintainers*")
      counts)))

(defun melpa-stats/author-package-counts ()
  "Return list of authors by package count.
Interactively, display with `pp-display-expression'."
  (interactive)
  (let ((counts (->> (melpa-stats/packages)
                     (--map (melpa-stats/package-field '(props authors) it))
                     (--map (seq-into it 'list))
                     (-flatten)
                     (-sort #'string<)
                     (melpa-stats/count #'identity))))
    (if (called-interactively-p 'any)
        (pp-display-expression counts "*MELPA Authors*")
      counts)))

(defun melpa-stats/maintainer-package-counts ()
  "Return list of maintainers by package count.
Interactively, display with `pp-display-expression'."
  (interactive)
  (let ((counts (->> (melpa-stats/packages)
                     (--map (melpa-stats/package-field '(props maintainer) it))
                     (-sort #'string<)
                     (melpa-stats/count #'identity))))
    (if (called-interactively-p 'any)
        (pp-display-expression counts "*MELPA Maintainers*")
      counts)))

;;;; Functions

(cl-defun melpa-stats/select-packages (&key authors maintainers urls depends-on
                                            (test-fn #'string-match))
  "Return packages matching AUTHORS, MAINTAINERS, URLS, or DEPENDS-ON.
Each argument may be one or a list of strings, which is tested
against the corresponding field in each package in the MELPA
archive.  If any field matches any argument (boolean OR), the
package is returned.  Each of DEPENDS-ON is compared as a string
against the names of packages' dependencies."
  (let* ((authors (cl-typecase authors
                    (list authors)
                    (string (list authors))))
         (maintainers (cl-typecase maintainers
                        (list maintainers)
                        (string (list maintainers))))
         (urls (cl-typecase urls
                 (list urls)
                 (string (list urls))))
         (depends-on (cl-typecase depends-on
                       (list depends-on)
                       (string (list depends-on))))
         (author-packages (when authors
                            (->> (melpa-stats/packages)
                                 (--select (cl-loop with package-authors = (melpa-stats/package-field '(props authors) it)
                                                    for pa across package-authors
                                                    thereis (cl-loop for a in authors
                                                                     thereis (funcall test-fn a pa)))))))
         (maintainer-packages (when maintainers
                                (->> (melpa-stats/packages)
                                     (--select (awhen (melpa-stats/package-field '(props maintainer) it)
                                                 (cl-loop for m in maintainers
                                                          thereis (funcall test-fn m it)))))))
         (url-packages (when urls
                         (->> (melpa-stats/packages)
                              (--select (awhen (melpa-stats/package-field '(props url) it)
                                          (cl-loop for u in urls
                                                   thereis (funcall test-fn u it)))))))
         (depends-on-packages (when depends-on
                                (--select (-some--> (melpa-stats/package-field '(deps) it)
                                                    (--map (->> it car symbol-name) it)
                                                    (cl-loop for d in depends-on
                                                             thereis (cl-member d it :test test-fn)))
                                          (melpa-stats/packages)))))
    (->> (append author-packages maintainer-packages url-packages depends-on-packages)
         (-sort (-on #'string< (-compose #'symbol-name #'car)))
         -uniq)))

(defun melpa-stats/count (fn list)
  "Return alist of elements of LIST with counts, sorted with `>'.
LIST is, first, grouped with `seq-group-by' by applying FN to
each element."
  ;; FIXME: This can probably be done in a much better way.
  (->> list
       (seq-group-by fn)
       (--map (cons (car it) (length (cdr it))))
       (-sort (-on #'> #'cdr))))

(defun melpa-stats/downloads (&optional refresh)
  "Return MELPA package data, read from download_counts.json"
  (when (or refresh (not melpa-stats/downloads))
    (setf melpa-stats/downloads (melpa-stats/retrieve-json melpa-stats/downloads-json-url)))
  melpa-stats/downloads)

(defun melpa-stats/package-field (field package)
  "Return value of FIELD in PACKAGE data.
PACKAGE should be a package's data structure as returned by
`melpa-stats/packages'.  FIELD should be a list of nested map keys as
expected by `a-get-in' which correspond to the structure of the
JSON data."
  (a-get-in (cdr package) field))

(defun melpa-stats/package-version-and-downloads (package)
  "Return version and download count for PACKAGE.
PACKAGE should be a package data list as returned by
`melpa-stats/packages'.  Returns an alist like:

    ((version . [20170909 631])
     (downloads . 431))"
  (a-list 'version (melpa-stats/package-field '(ver) package)
          'downloads (a-get (melpa-stats/downloads) (car package))))

(defun melpa-stats/packages (&optional refresh)
  "Return MELPA package data, read from archive.json."
  (when (or refresh (not melpa-stats/packages))
    (setf melpa-stats/packages (melpa-stats/retrieve-json melpa-stats/archive-json-url)))
  melpa-stats/packages)

(defun melpa-stats/packages-by-author ()
  "Return alist of packages by author."
  (cl-loop with people = (ht)
           for package in (melpa-stats/packages)
           for authors = (-flatten (seq-into (melpa-stats/package-field '(props authors) package) 'list))
           when authors
           do (cl-loop for author in authors
                       do (push package (gethash author people)))
           finally return (ht->alist people)))

(defun melpa-stats/packages-by-maintainer ()
  "Return alist of packages by maintainer."
  (cl-loop with people = (ht)
           for package in (melpa-stats/packages)
           for maintainer = (melpa-stats/package-field '(props maintainer) package)
           when maintainer
           do (push package (gethash maintainer people))
           finally return (ht->alist people)))

(defun melpa-stats/retrieve-json (url)
  "Return parsed JSON object from URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (re-search-forward "\n\n")
    (prog1 (json-read)
      (kill-buffer))))

;;;; Footer

(provide 'melpa-stats)

;;; melpa-stats.el ends here
