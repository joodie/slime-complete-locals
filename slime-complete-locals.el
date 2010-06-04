;; Copyright (C) 2010 Joost Diepenmaat, Zeekat Softwareontwikkeling
;;
;; Author: Joost Diepenmaat - joost@zeekat.nl
;; Version: 0.1
;; Keywords: abbrev, convenience, slime
;; URL: http://github.com/joodie/slime-complete-locals
;; Compatibility: GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; this code modifies the slime-simple-completions function
;; to take into account any locally mentioned symbols
;;
;; that way function arguments and let bindings are expanded
;; as well as the usual 'globally reachable' vars in the
;; inferior lisp

;; to configure
;; put this file somewhere in your load-path
;; then
;;
;; (slime-setup 
;;  '(slime-fancy ;; optional, turns on fancy inspector, autodoc and other useful stuff
;;   slime-company ;; optional, needs additional install
;;   slime-complete-locals)) 

;; note that for this to work in the standard slime configuration,
;; for now, you also need to set
;; slime-complete-symbol-function to 'slime-simple-complete-symbol
;;
;; it will work "out of the box" with slime-company mode, though.
;; see 
;; http://www.emacswiki.org/emacs/CompanyMode
;; and
;; http://www.emacswiki.org/emacs/CompanyModeBackends


(setq slime-string-literal-regexp
  "\".*?[^\\]\"")

(setq slime-comment-regexp
  ";.*")

(setq slime-not-a-symbol-regexp
  "[][(){}]+")

(defun slime-get-local-symbols-buffer
  ()
  (save-excursion
    (let ((b (get-buffer-create "*slime-local-symbols-buffer*")))
      (set-buffer b)
      (erase-buffer)
      b)))

(defun slime-uniq-list (list)
  (let ((result '()))
    (dolist (elem list)
      (when (not (member elem result))
        (push elem result)))
    (nreverse result)))

;; this is pretty hackish. should be fairly quick, though
(defun slime-local-symbols
  ()
  (interactive)
  (save-excursion
    (destructuring-bind (s e) (slime-region-for-defun-at-point)
      (let ((tmp (slime-get-local-symbols-buffer)))
        (copy-to-buffer tmp s e)
        (set-buffer tmp)
        (set-text-properties 1 (point-max)  nil)
        (replace-regexp slime-string-literal-regexp " ")
        (beginning-of-buffer)
        (replace-regexp slime-comment-regexp "")
        (beginning-of-buffer)
        (replace-regexp slime-not-a-symbol-regexp " ")
        (slime-uniq-list (split-string (buffer-string)))))))

(defun slime-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun slime-simple-local-completions (prefix)
  (let ((len (length prefix)))
    (slime-filter (lambda (str) 
                    (and (<= len (length x))
                         (string= prefix (substring str 0 len))))
                  (slime-local-symbols))))

(defun slime-add-simple-local-completions (prefix lst)
  (cons 
   (sort (slime-uniq-list (append (slime-simple-local-completions prefix) (car lst))) 
         (lambda (a b) (compare-strings a 0 nil b 0 nil))) 
   (cdr lst)))


(defadvice slime-simple-completions (around include-local-symbols (prefix))
  "include symbols from the current top-level form in the completion suggestions"
  ad-do-it
  (setq ad-return-value (slime-add-simple-local-completions prefix ad-return-value)))

(defun slime-complete-locals-init ()
  (ad-activate 'slime-simple-completions))

(provide 'slime-complete-locals)
