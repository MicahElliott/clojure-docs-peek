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

;; Get private data (future feature)
;; (cider-interactive-eval "(:private (meta #'some.ns))")

;; To test, try out the following symbols:
;; - [clojure.string :as str]
;; - somens/somefn
;; - somefn
;; - somemacro
;; - UUID/fromString
;; - UUID

;;;###autoload
(defun clojure-docs-peek-toggle ()
  "Show a fn or ns docstring's key pieces as inline overlay."
  ;; If not on a word, let it error
  (interactive)
  (if cdp--peek-opened-p
      (progn
	(setq cdp--peek-opened-p nil)
	(quick-peek-hide))
    (setq cdp--ns-p nil)
    (let ((info0    (cider-var-info (thing-at-point 'word 'no-properties))))
      (if (not info0)
	  (message "Invalid thing at point.")
	;; if (not (nrepl-dict-get info0 "doc"))
        ;; (message "TODO This looks like java")
	;; (if (not (nrepl-dict-get info0 "ns"))
	(if (not (nrepl-dict-get info0 "doc"))
	    (message "Missing docstring!")
	  (let* ((info (if (or (cdp--looking-at-fn-p info0)
			       (cdp--java-p info0)) ; already have everything we need
			   info0
			 ;; Do another full lookup to get NS info
			 (cider-var-info (nrepl-dict-get info0 "ns"))))
		 ;; If java, won't have ns
		 ;; (ns      (propertize (nrepl-dict-get info "ns") 'face 'font-lock-type-face))
		 (ns      (let ((ns-str (or (nrepl-dict-get info "ns")
					    (nrepl-dict-get info "class"))))
			    (when ns-str
			      (propertize ns-str
					  'face 'font-lock-type-face))))
		 ;; No arglist if not a fn (variable)
		 (arglist-str (nrepl-dict-get info "arglists-str"))
		 (arglist (when (and arglist-str
				     ;; In java, arglist can be string containing "nil"!
				     (not (string-equal arglist-str "nil")))
			    (propertize arglist-str 'face 'clojure-keyword-face)))
		 ;; (file-parts (split-string (nrepl-dict-get info "file") ":"))
		 ;; (is-lib-p   (string-equal "jar" (car file-parts)))

		 (is-lib-p (let ((file-csv (nrepl-dict-get info "file")))
                             (when file-csv
			       (string-equal "jar" (car (split-string file-csv ":"))))))

		 ;; (fname      (car (last file-parts))) ; not used
		 (name-str (or (nrepl-dict-get info "name")
			       (nrepl-dict-get info "member")
			       (nrepl-dict-get info "class")))
		 (name    (when (not (cdp--ns-p info)) ; not an NS
			    (propertize name-str 'face 'font-lock-function-name-face)))
		 (doc     (propertize (nrepl-dict-get info "doc")
				      'face 'font-lock-doc-face))
		 (stats (if (cdp--looking-at-fn-p info)
			    (cdp--fn-stats ns name)
			  ;; Don't do stats if java'
			  (if (and (not (cdp--java-p info))
				   (not (nrepl-dict-get info "macro")))
			      (cdp--ns-stats ns is-lib-p))))
		 (seealso-str (nrepl-dict-get
			       (cider-var-info (thing-at-point 'word 'no-properties))
			       "see-also"))
		 (seealso (and seealso-str
			       (propertize
				(concat "\n\nSee-also:\n- "
					(mapconcat 'identity
						   (nrepl-dict-get
						    (cider-var-info
						     (thing-at-point 'word 'no-properties))
						    "see-also")
						   "\n- "))
				'face font-lock-comment-face)))
		 (alltext (concat
			   ;; NS/name heading
			   (if (string= (cider-current-ns) ns)
			       name
			     (if (cdp--ns-p info)
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
	    (setq max-h 30)
	    (quick-peek-show alltext nil 50)
	    ;; (put-text-property 1 200 'face (cons 'foreground-color "red"))
	    (add-text-properties 1 300 '(comment t face highlight))
	    ;; (propertize str-before 'face 'quick-peek-padding-face)
	    (setq cdp--peek-opened-p t)))))))

(defun cdp--ns-p (info)
  "Check if INFO indicates this is a namespace.
Only NSs have same name and ns, even for aliases"
  (let ((ns (nrepl-dict-get info "ns"))
	(name (nrepl-dict-get info "name")))
    (and name (string-equal ns name))))

(defun cdp--java-p (info)
  "Detect if we're looking at java by inspecting INFO."
  (nrepl-dict-get info "class"))

(defun cdp--colorize-stat (label num &optional face-override)
  "Add face color properties to LABEL and NUM, with ability to FACE-OVERRIDE."
  (when num
    (concat (propertize label 'face (or face-override 'cider-repl-stdout-face)) ":"
	    (propertize num   'face 'cider-repl-input-face))))

;; FIXME better name, and expand to java symbols
(defun cdp--looking-at-fn-p (info)
  "Check in INFO if point is on fn or ns in case shape is somens/somefn.

Because then we want to decide based on point whether to show ns
or fn.

Thing could be any of:
- somefn
- somens
- somens/somefn
- some.dotted.ns"
  (when (and (not (cdp--ns-p info))
	     (not (cdp--java-p info))
	     ;; (not (nrepl-dict-get info "macro"))
	     )
    (let* ((thing (thing-at-point 'word))
	   (sepi (s-index-of "/" thing))
	   (i0 (car (bounds-of-thing-at-point 'word)))
	   (i2 (cdr (bounds-of-thing-at-point 'word)))
	   (pt (- (point) i0)))
      (if (string-match-p "/" thing)
	  (< sepi pt)
	t)
      ;; (when (not (string-match-p "\\." thing)) ; assume no fully qualified dotted NSs used
      ;; 	)
      )))

(defun cdp--make-tips (tip-list)
  "Build a TIP-LIST that can be enabled or not."
  (when current-prefix-arg
    (concat "\n\n" (string-join tip-list "\n- "))))

(defun cdp--fn-stats (ns name)
  "Gather function stats on NS and NAME."
  (concat (cdp--colorize-stat "refs" (int-to-string
				      (length (cider-sync-request:fn-refs ns name))))
	  (cdp--make-tips
	   (list "Tips (you may have bindings already, else make them):"
		 "cider-browse-ns:           view complete NS"
		 "cider-find-var:            visit file, and then..."
		 "cider-xref-fn-refs-select: see and visit project-wide references"))
	  ))

(defun cdp--ns-stats (ns is-lib-p)
  "Gather namespace stats on NS, looking out for IS-LIB-P.

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
		 (when (not is-lib-p)
		   (cdp--colorize-stat "issues" (let ((issues (split-string
							       ;; " FlyC" or " FlyC:0|1"
							       (flycheck-mode-line-status-text) ":")))
						  (when (= 2 (length issues))
						    (car (last issues))))
				       'error))
		 (when (not is-lib-p)
		   (cdp--colorize-stat "nodocs" (int-to-string
						 (length
						  (remq nil (mapcar (lambda (x) (string-match "Not documented" x))
								    (cider-browse-ns--items ns) ))))
				       'error))))
      " — ")
     (cdp--make-tips
      (list "Tips:"
	    "cider-find-var:  visit file, and then..."
	    "cider-browse-ns: view complete NS")))))

;; (cdp--find-test-file "/home/mde/work/cc/src/clj/crawlingchaos/domain/homeowner/borrower.clj")
;; Borrowed from and depends on toggle-test.el
;; Not in use yet.
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
