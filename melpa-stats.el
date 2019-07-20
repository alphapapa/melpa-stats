;;; melpa-stats.el --- MELPA package statistics      -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/melpa-stats
;; Keywords: packages, MELPA
;; Package-Requires: ((emacs) (a) (anaphora) (dash) (dash-functional) (s))

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

;;;; Variables

(defvar melpa/archive-json-url "https://melpa.org/archive.json")
(defvar melpa/downloads-json-url "https://melpa.org/download_counts.json")

(defvar melpa/packages nil
  "MELPA packages, read from archive.json.")
(defvar melpa/downloads nil
  "MELPA package downloads, read from download_counts.json.")

;;;; Customization


;;;; Commands

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

;;;; Functions

(cl-defun melpa/select-packages (&key authors maintainers urls (test-fn #'string-match))
  "Return packages matching AUTHORS, MAINTAINERS, or URLs.
Each argument may be one or a list of strings, which is tested
against the corresponding field in each package in the MELPA
archive.  If any field matches any argument (boolean OR), the
package is returned."
  (let* ((authors (cl-typecase authors
                    (list authors)
                    (string (list authors))))
         (maintainers (cl-typecase maintainers
                        (list maintainers)
                        (string (list maintainers))))
         (urls (cl-typecase urls
                 (list urls)
                 (string (list urls))))
         (author-packages (when authors
                            (->> (melpa/packages)
                                 (--select (cl-loop with package-authors = (melpa/package-field '(props authors) it)
                                                    for pa across package-authors
                                                    thereis (cl-loop for a in authors
                                                                     thereis (funcall test-fn a pa)))))))
         (maintainer-packages (when maintainers
                                (->> (melpa/packages)
                                     (--select (awhen (melpa/package-field '(props maintainer) it)
                                                 (cl-loop for m in maintainers
                                                          thereis (funcall test-fn m it)))))))
         (url-packages (when urls
                         (->> (melpa/packages)
                              (--select (awhen (melpa/package-field '(props url) it)
                                          (cl-loop for u in urls
                                                   thereis (funcall test-fn u it))))))))
    (-uniq (append author-packages maintainer-packages url-packages))))

(defun melpa/packages (&optional refresh)
  "Return MELPA package data, read from archive.json."
  (when (or refresh (not melpa/packages))
    (setf melpa/packages (melpa/retrieve-json melpa/archive-json-url)))
  melpa/packages)

(defun melpa/downloads (&optional refresh)
  "Return MELPA package data, read from download_counts.json"
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
  "Return value of FIELD in PACKAGE data.
PACKAGE should be a package's data structure as returned by
`melpa/packages'.  FIELD should be a list of nested map keys as
expected by `a-get-in' which correspond to the structure of the
JSON data."
  (a-get-in (cdr package) field))

(defun melpa/package-version-and-downloads (package)
  "Return version and download count for PACKAGE.
PACKAGE should be a package data list as returned by
`melpa/packages'.  Returns an alist like:

    ((version . [20170909 631])
     (downloads . 431))"
  (a-list 'version (melpa/package-field '(ver) package)
          'downloads (a-get (melpa/downloads) (car package))))

;;;; Footer

(provide 'melpa-stats)

;;; melpa-stats.el ends here
