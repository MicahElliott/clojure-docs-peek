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

(defvar cdp--peek-opened-p nil
  "Toggle to support an inline help being open (expanded) or not.")

;; Get private data
;; (cider-interactive-eval "(:private (meta #'crawlingchaos.domain.homeowner.borrower/sfdc-lookup-payments))")

;;;###autoload
(defun clojure-docs-peek-toggle ()
  "Show a fn or ns docstring's key pieces as inline overlay."
  (interactive)
  (if cdp--peek-opened-p
      (progn
	(setq cdp--peek-opened-p nil)
	(quick-peek-hide))
    (setq cdp--ns-p nil)
    ;; If not on a word, let it error
    (let ((info0    (cider-var-info (thing-at-point 'word 'no-properties))))
      (if (not (nrepl-dict-get info0 "ns"))
	  (message "Nothing to do with a java class; try calling: cider-javadoc.")
	(if (not (nrepl-dict-get info0 "doc"))
	    (message "Missing docstring or invalid thing at point.")
	  (let* ((info (if (cdp--looking-at-fn-p)
			   info0
			 (cider-var-info (nrepl-dict-get info0 "ns"))))
		 (ns      (propertize (nrepl-dict-get info "ns") 'face 'font-lock-type-face))
		 ;; Built-in fns' info are indexed differently from user-defined.
		 (arglist-str (nrepl-dict-get info "arglists-str"))
		 (arglist (when arglist-str (propertize arglist-str 'face 'clojure-keyword-face)))
		 (file-parts (split-string (nrepl-dict-get info "file") ":"))
		 (is-jar-p   (string-equal "jar" (car file-parts)))
		 (fname      (car (last file-parts)))
		 (name-str (nrepl-dict-get info "name"))
		 (name    (if (not (string-match "\\." name-str)) ; not an NS
			      (propertize name-str 'face 'font-lock-function-name-face)
			    ;; (propertize name-str 'face 'font-lock-type-face)
			    (setq cdp--ns-p t)
			    nil))
		 (doc     (propertize (nrepl-dict-get info "doc")
				      'face 'font-lock-doc-face))
		 (stats (if (cdp--looking-at-fn-p)
			    (cdp--fn-stats ns name)
                          (cdp--ns-stats ns is-jar-p)))
		 (seealso-str (nrepl-dict-get (cider-var-info (thing-at-point 'word 'no-properties)) "see-also"))
		 (seealso (and seealso-str
			       (propertize
				(concat "\n\nSee-also:\n- "
					(mapconcat 'identity
						   (nrepl-dict-get (cider-var-info (thing-at-point 'word 'no-properties)) "see-also")
						   "\n- "))
				'face font-lock-comment-face)))
		 (alltext (concat
			   ;; NS/name heading
			   (if (string= (cider-current-ns) ns)
			       name
			     (if cdp--ns-p
				 ns
			       (concat ns "/" name)))
			   "\n"
			   (if arglist (concat arglist "\n") "")
			   "\n"
			   (when doc (replace-regexp-in-string "^  " "" doc))
			   "\n\n"
			   stats
			   ;; Include see-also if universal arg
			   (when current-prefix-arg seealso))))
	    (if doc
		(progn
		  (setq max-h 30)
		  (quick-peek-show alltext nil 50)
		  ;; (put-text-property 1 200 'face (cons 'foreground-color "red"))
		  (add-text-properties 1 300 '(comment t face highlight))
		  ;; (propertize str-before 'face 'quick-peek-padding-face)
		  (setq cdp--peek-opened-p t))
	      (message "Not sure how we got here!"))))))))

(defun cdp--colorize-stat (label num &optional face-override)
  "Add face color properties to LABEL and NUM, with ability to FACE-OVERRIDE."
  (when num
    (concat (propertize label 'face (or face-override 'cider-repl-stdout-face)) ":"
	    (propertize num   'face 'cider-repl-input-face))))

(defun cdp--looking-at-fn-p ()
  "Check if point is on a function or namespace."
  (let* ((thing (thing-at-point 'word))
	 (sepi (s-index-of "/" thing))
         (i0 (car (bounds-of-thing-at-point 'word)))
	 (i2 (cdr (bounds-of-thing-at-point 'word)))
	 (pt (- (point) i0)))
    (when (not (string-match-p "\\." thing)) ; assume no fully qualified dotted NSs used
      (if (string-match-p "/" thing)
	  (< sepi pt)))))

(defun cdp--make-tips (tip-list)
  "Build a TIP-LIST that can be enabled or not."
  (concat "\n\n" (string-join tip-list "\n- ")))

(defun cdp--fn-stats (ns name)
  "Gather function stats on NS and NAME."
  (concat (cdp--colorize-stat "refs" (int-to-string (length (cider-sync-request:fn-refs ns name))))
	  (cdp--make-tips
	   (list "Tips (you may have bindings already, else make them):"
		 "cider-browse-ns:           view complete NS"
		 "cider-find-var:            visit file, and then..."
		 "cider-xref-fn-refs-select: see and visit project-wide references"))
	  ))

(defun cdp--ns-stats (ns is-jar-p)
  "Gather namespace stats on NS, looking out for IS-JAR-P.

Build a single summary line showing counts of various interesting
things, such as lines/vars and problems like missing docs and
flycheck flags."
  (save-window-excursion
    ;; Might need this to avoid read-only prompt
    ;; https://emacs.stackexchange.com/a/19747/11025
    ;; (find-file fname)
    (concat
     (string-join
      (remq nil (list
		 (cdp--colorize-stat "lines"  (int-to-string (count-lines (point-min) (point-max))))

		 (cdp--colorize-stat "vars"   (int-to-string (length (cider-browse-ns--items ns))))
		 (when (not is-jar-p)
		   (cdp--colorize-stat "issues" (let ((issues (split-string
							       ;; " FlyC" or " FlyC:0|1"
							       (flycheck-mode-line-status-text) ":")))
						  (when (= 2 (length issues))
						    (car (last issues))))
				       'error))
		 (when (not is-jar-p)
		   (cdp--colorize-stat "nodocs" (int-to-string
						 (length (remq nil (mapcar (lambda (x) (string-match "Not documented" x))
									   (cider-browse-ns--items ns) ))))
				       'error))
		 ))
      " — ")
     (cdp--make-tips
      (list "Tips:"
	    "cider-find-var:  visit file, and then..."
	    "cider-browse-ns: view complete NS")))))

;; (cdp--find-test-file "/home/mde/work/cc/src/clj/crawlingchaos/domain/homeowner/borrower.clj")
;; Borrowed from and depends on toggle-test.el
(defun cdp--find-test-file (file)
  "Find a test file given source FILE."
  (let ((proj (tgt-proj-for file)))
    (cond (proj
	   (multiple-value-bind
	       (src-file-rel-path test-file-rel-path)
	       (tgt-find-project-file-in-dirs file proj)
	     (cond
	      (test-file-rel-path (tgt-all-toggle-paths
				   test-file-rel-path proj :src-dirs
				   #'tgt-possible-src-file-names))
	      (src-file-rel-path (tgt-all-toggle-paths
				  src-file-rel-path proj :test-dirs
				  #'tgt-possible-test-file-names))))))))

(provide 'clojure-docs-peek)

;;; clojure-docs-peek ends here
