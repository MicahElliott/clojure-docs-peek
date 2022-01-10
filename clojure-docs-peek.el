;;; clojure-docs-peek --- Quick-peek for clojure-docs -*- lexical-binding: t -*-

;; Copyright (C) 2022 Micah Elliott

;; Author: Micah Elliott <mde@micahelliott.com
;; Created: 10 Jan 2022
;; Homepage: https://github.com/micahelliott/clojure-docs-peek
;; Keywords: extensions
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: MIT
;; Version: 1.0

;;; Commentary:

;; Use quick-peek and cider to pop up a simple guide to function or
;; namespace at point.

;; Please see https://github.com/micahelliott/clojure-docs-peek for
;; more information.

;; Quick-Peek for Clojure-docs via Cider
;; Original seed: https://github.com/clojure-emacs/cider/issues/2968

;;; Code:

(require 'cider)
(require 'quick-peek)

(defvar cider-inline-docs-opened-p nil
  "Toggle to support an inline help being open (expanded) or not.")

(defun cider-inline-docs-toggle ()
  "Show a fn or ns docstring's key pieces as inline overlay."
  ;; Another way to do this is to use "popup" instead of quick-peek.
  (interactive)
  (if cider-inline-docs-opened-p
      (progn
	(setq cider-inline-docs-opened-p nil)
	(quick-peek-hide))
    ;; If not on a word, let it error
    (setq my-cidt-ns-p nil)
    (let* ((info    (cider-var-info (thing-at-point 'word 'no-properties)))
           ;; Built-in fns' info are indexed differently from user-defined.
	   (arglist-str (nrepl-dict-get info "arglists-str"))
	   (arglist (when arglist-str (propertize arglist-str 'face 'clojure-keyword-face)))
	   (ns      (propertize (nrepl-dict-get info "ns") 'face 'font-lock-type-face))
	   (name-str (nrepl-dict-get info "name"))
	   (name    (if (not (string-match "\\." name-str)) ; not an NS
			(propertize name-str 'face 'font-lock-function-name-face)
		      ;; (propertize name-str 'face 'font-lock-type-face)
		      (setq my-cidt-ns-p t)
		      nil))
	   (doc     (nrepl-dict-get info "doc"))
	   (seealso (propertize
		     (concat "\n\nSee-also:\n- "
			     (mapconcat 'identity
					(nrepl-dict-get (cider-var-info (thing-at-point 'word 'no-properties)) "see-also")
					"\n- "))
		     'face font-lock-comment-face))
	   (alltext (concat
		     ;; NS/name heading
		     (if (string= (cider-current-ns) ns)
			 name
		       (if my-cidt-ns-p
			   ns
			 (concat ns "/" name)))
                     "\n"
		     (if arglist (concat arglist "\n") "")
		     (when doc (replace-regexp-in-string "^  " "" doc))
		     ;; Include see-also if universal arg
		     (when current-prefix-arg seealso))))
      (if doc
	  (progn
	    (setq max-h 30)
	    (quick-peek-show
	     alltext
             nil 20
	     )
            ;; (put-text-property 1 200 'face (cons 'foreground-color "red"))
	    (add-text-properties 1 300 '(comment t face highlight))
	    ;; (propertize str-before 'face 'quick-peek-padding-face)
	    (setq cider-inline-docs-opened-p t))
	(message "Missing docstring or invalid thing at point")))))

(provide 'clojure-docs-peek)

;;; clojure-docs-peek ends here
