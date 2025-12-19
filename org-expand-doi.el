;;; org-expand-doi.el --- Org Expand DOI library                  -*- lexical-binding: t; -*-

;; - First get JSON data using doi-utils-get-json-metadata from doi-utils in org-ref https://github.com/jkitchin/org-ref/blob/master/doi-utils.el#L960
;; - Somehow add the json data to a JSON CSL
;;   - See citeproc docs: https://citeproc-js.readthedocs.io/en/latest/csl-json/markup.html
;;   - is just a jsonfile with a list of citations [ {"doi":"123", .... }, {"doi":"abc", .... },]
;;   - This jsonfile can be added to the =citar-bibliography= of =org-cite-global-bibliography= variable
;; - Change the doi export function:
;;   - doi:123456 becomes =author et al, title [cite:@author]=
;;   - Because of the cite, automatically export to bibliography


;; TESTING
(when nil
  ;; (setq test-cache '(("10.1016/j.ijscr.2023.108044" :publisher "Elsevier BV" :is-referenced-by-count 16 :DOI "doi123")
  ;; 		     ("10.1259/bjr/98449202" :publisher "Oxfort" :is-referenced-by-count 9 :DOI "doi123")))
  ;; (setq test-metadata '(:publisher "Oxfort" :is-referenced-by-count 9 :id "doi123" :DOI "doi123" :author ((:family "famauth") (:family "second")) :published (:date-parts ((2023 4)))))
  ;; (setq org-doi-cache test-cache)

  (org-expand-doi--cache-to-json)
  (org-expand-doi-save-json)

  ;; 1. Get doi (and add to cache `org-doi-cache' )
  ;; TODO: add an 'id' field (just make it doi). This is compulsory!!!!
  (org-expand-doi-get-json-metadata "10.1259/bjr/98449202")
  (org-expand-doi-get-json-metadata "10.1016/j.ijscr.2023.108044")

  ;; 2. convert cache into a json list and save as csl json
  (org-expand-doi-save-json)

  ;; 3. save cache to disk
  (org-expand-doi-save-cache)

  ;; 4. make cite key from doi: just cite the 'id' key we added when importing
  ;; DONE, implemented in json downloader function

  ;; 5. Expansion variables:
  ;; Follow format of 'org-expand-doi-format' and 'org-expand-doi-export-format'

  ;; 6 Expansion function
  (org-expand-doi-buffer)
  (org-expand-doi-at-point)
  (org-expand-doi--expand "10.1259/bjr/98449202")
  (citar-copy-reference '("10.1259/bjr/98449202"))

  ;; 7 Implementation for publish functions
  ;; DONE: just use `(org-expand-doi-setup)'

  ;; 8 load cache function
  ;; DONE: just use `(org-expand-doi-setup)'
  (org-expand-doi-load-cache)

  ;; 9. Hook to save cached doi's on emacs exit
  ;; See org-id.el (c:/Users/cvmarc2/scoop/apps/emacs/current/share/emacs/30.2/lisp/org/org-id.el)
  ;; DONE: Implemented in `(org-expand-doi-setup)'

  ;; 10 Function to download doi metadata if a [cite:@] contains
  ;; DONE, implemented in (org-expand-doi-export-hook)

  ;; 11 TODO:
  ;; Support for local json CSL (eg in the same folder), which can locally be applied with #+BIBLIOGRAPHY
  ;; For this, we will need to make a cache from the local bibliography
  ;; Or before getting the new data, check if such a key already exists!
  (org-cite-basic--get-entry "Idontexist")
  (setq entry-test (org-cite-basic--get-entry "scholzCTFindingsAdult2011"))
  (setq json-test (json-encode entry-test))
  (setq json-test2 (json-encode (org-expand-doi-get-json-metadata "10.1259/bjr/98449202")))
  (setq entry-test2 (org-expand-doi-get-json-metadata "10.1259/bjr/98449202"))
  ;; Can convert alist to plist, but problems with :keyword

  ;; WIll probably have problems with author.....
  (alist-entry-to-plist entry-test)
  (defun alist-entry-to-plist (alist)
    (let ((res '()))
      (dolist (x alist)
	(push (intern (concat ":" (symbol-name (car x)))) res)
	(push (cdr x) res))
      (nreverse res)))

  ;; Problem: JSON data is plist with :keyword, while cite data is alist
  ;; However, saving as json DOES work, but this will not work for cache.
  ;; Therefore: when getting info for expansion: FIRST try to get from org-cite by doi, if unsuccesfull, get from cache
  ;; What if we overwrite the json file in /var? Best load from json and make cache that way.

  )

;; Variables
(defcustom org-expand-doi-format "${first-author} et al (${year})"
  "The format to expand doi links")
(defcustom org-expand-doi-export-format "${first-author} et al (${year})"
  "The format to expand doi links during export")

(defcustom org-doi-cache-file
  (expand-file-name (convert-standard-filename "var/org/org-doi-cache.el") user-emacs-directory)
  "List of files with IDs in those files.")

(defcustom org-doi-json-csl-file
  (expand-file-name (convert-standard-filename "var/org/org-doi-csl.json") user-emacs-directory)
  "List of files with IDs in those files.")

(defcustom org-expand-doi-auto-save-cache t
  "If non-nil, auto save the cache on emacs exit."
  :group 'org-expand-doi
  :type '(choice (const :tag "Auto save cache on exit" t)
                 (const :tag "Do not auto save cache on exit" nil))
  :safe #'booleanp)

(defcustom org-expand-doi-add-citation t
  "When non-nil, add a citation after the doi link in the form of '[cite:@doinumber]'."
  :group 'org-expand-doi
  :type '(choice (const :tag "Add citation" t)
                 (const :tag "Do not add citation" nil))
  :safe #'booleanp)

(defcustom org-expand-doi-citation-prefix ""
  "The prefix to set after inserting a citation."
  :group 'org-expand-doi)

(defcustom org-expand-doi-citation-suffix ""
  "The suffix to set after inserting a citation."
  :group 'org-expand-doi)

(defcustom org-expand-doi-default-expansion 'all
  "When non-nil, when expanding doi links, replace the description.
When nil, keep the description as-is."
  :group 'org-expand-doi
  :type '(choice (const :tag "Replace link and add citation." 'all)
                 (const :tag "Only append a citation after doi link." 'citation)
		 (const :tag "Only replace the doi link." 'link))
  :safe #'symbolp)

(defcustom org-expand-doi-export-backend 'any
  "For which backend the export process should be allowed.
If 'any is selected, always apply. Otherwise, choose your backend."
  :group 'org-expand-doi
  :safe #'symbolp)

(defcustom org-expand-doi-replace-existing-description t
  "When non-nil, when expanding doi links, replace the description.
When nil, keep the description as-is." 
  :group 'org-expand-doi
  :safe #'booleanp)

(defvar org-doi-cache nil
  "Cache variable for storing data we can reuse.
A-list (doi . data) where doi is doi string, and data is what is
retrieved from it. This is transient, and disappears when you
restart Emacs. This mostly exists to prevent
repeatedly downloading the data for
every field.")

(defvar org-doi-re "\\b\\(10[.][0-9]\\{4,\\}\\(?:[.][0-9]+\\)*/\\(?:[^\"&\'\[<>[:space:]]\\)+\\)\\b"
  "The regex for a doi number.
Does not include 'doi:', 'cite:@', or brackets.")
(defvar org-doi-cite-re (concat "\\[cite:@" org-doi-re "\\]")
  "Regex for a doi number inside a cite block")

;;; FUNCTIONS
(require 'cl-lib)
(require 'json)
(require 'oc)
(require 'oc-basic)
(require 'org-link-doi)

;;;###autoload
(defun org-expand-doi-setup ()
  (interactive)
  ;; Load cache from disk, if not already populated.
  (unless org-doi-cache 
    (org-expand-doi-load-cache))
  ;; Add the CSL json file to the bibliography.
  (add-to-list 'org-cite-global-bibliography org-doi-json-csl-file)
  ;; Add hook to save the cache on emacs exit
  (add-hook 'kill-emacs-hook (lambda ()
			       (when org-expand-doi-auto-save-cache
				 (org-expand-doi-save-cache))))
  ;; Add hook to the export processor
  (add-hook 'org-export-before-processing-hook #'org-expand-doi-export-hook))

(defun org-expand-doi-cleanup ()
  "Remove all hooks and variable modifications."
  (interactive)
  (setq org-cite-global-bibliography
	(remove org-doi-json-csl-file org-cite-global-bibliography))
  (remove-hook 'kill-emacs-hook (lambda ()
				  (when org-expand-doi-auto-save-cache
				    (org-expand-doi-save-cache))))
  (remove-hook 'org-export-before-processing-hook #'org-expand-doi-export-hook))

(defun org-expand-doi-export-hook (backend)
  "This function is hooked into `org-export-before-processing-hook'
to expand all doi links in a buffer before export.
You can change the backends to which it will apply in the variable `org-expand-doi-export-backend'"
  (when (or (equal 'any org-expand-doi-export-backend)
	    (equal backend org-expand-doi-export-backend))
    (let ((org-expand-doi-format org-expand-doi-export-format))
      (add-to-list 'org-cite-global-bibliography org-doi-json-csl-file)
      (org-expand-doi-buffer)
      (org-expand-doi--get-missing-citations-buffer))))

;;;###autoload
(defun org-expand-doi-buffer ()
  (interactive)
  (org-with-point-at 1
    (while (and (org-next-link)
		(not org-link--search-failed))
      (org-expand-doi-at-point))))

;;;###autoload
(defun org-expand-doi-at-point (&optional expand-what)
  (interactive)
  (when (string-equal "doi" (org-element-property :type (org-element-context)))
    (let* ((expand-what (or expand-what org-expand-doi-default-expansion))
	   (context (org-element-context))
	   (doi (org-element-property :path context))
	   (begin (org-element-property :begin context))
	   (end (org-element-property :end context))
	   (desc (and (org-element-property :contents-begin context)
                      (org-element-property :contents-end context)
                      (buffer-substring-no-properties
                       (org-element-property :contents-begin context)
                       (org-element-property :contents-end context)))) )
      (cond
       ((eq expand-what 'citation)
	(org-expand-doi--add-citation end doi))
       ((eq expand-what 'link)
	(org-expand-doi--link begin end doi desc))
       ((eq expand-what 'all)
	(org-expand-doi--add-citation end doi)
	(org-expand-doi--link begin end doi desc))))))

(defun org-expand-doi--link (begin end doi desc)
  (let ((desc (if (or org-expand-doi-replace-existing-description
		      (not desc))
		  (org-expand-doi--expand doi)
		desc))) 
    (goto-char begin)
    (delete-region begin end)
    (insert (format "[[%s][%s]]"
		    (concat "doi:" doi)
		    desc))))

(defun org-expand-doi--add-citation (end doi)
  (goto-char end)
  (insert (format "%s[cite:@%s]%s"
		  org-expand-doi-citation-prefix
		  doi
		  org-expand-doi-citation-suffix)))


(defun org-expand-doi--expand (doi)
  "Expand a doi number by its metadata, according to the template `org-expand-doi-format'."
  ;; Get data, either from cache or from internet
  (let ((data (org-expand-doi-get-json-metadata doi)))
    (if data
	(s-format org-expand-doi-format 'org-expand-doi--pget-replacer data)
      ;; If no doi metadata: return doi.
      doi)))

(defun org-expand-doi--pget-replacer (keyword plist)
  "Helper function for s-format.
Get the value of KEYWORD from the doi plist.
Case sensitive!"
  (cond ((string-equal keyword "first-author")
	 ;; Author is in a vector, so need to use 'aref'
	 (plist-get (aref (plist-get plist :author 'equal) 0) :family 'equal))
	((string-equal keyword "first-author-surname")
	 ;; Author is in a vector, so need to use 'aref'
	 (plist-get (aref (plist-get plist :author 'equal) 0) :given 'equal))
	((string-equal keyword "first-author-surname-initial")
	 (substring 
	  (plist-get (aref (plist-get plist :author 'equal) 0) :given 'equal)
	  0 1))
	((string-equal keyword "cite")
	 (format "[cite:@%s]" (plist-get plist :id 'equal)))
	((string-equal keyword "year")
	 (aref (aref 
	       (plist-get (plist-get plist :published 'equal) :date-parts 'equal) 0) 0))
	(t
	 (plist-get plist (intern (concat ":" keyword)) 'equal))))

(defun org-expand-doi--get-missing-citations-buffer ()
  "Look for all doi styled citations (eg [cite:@doinumber])
and if any of them are not known by the citation manager, get them."
  (let ((keys (org-cite-basic--all-keys))
	(citations (org-expand-doi--matches-in-buffer org-doi-cite-re 1)))
    (dolist (doi citations)
      (when (not (member doi keys))
	(if (org-expand-doi-get-json-metadata doi)
	    (org-expand-doi-save-json)
	  (message (format "Metadata for %s not found" doi)))))))

(defun org-expand-doi--matches-in-buffer (regexp &optional match-group buffer)
  "Return a list of matches of REGEXP in BUFFER or the current buffer if not given."
  (let ((matches)
	(match-group (or match-group 0)))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (push (match-string-no-properties match-group) matches)))))
      (delete-dups matches))))

(defun org-expand-doi--cache-to-json ()
  "Convert the doi cache to a json CSL file."
  ;; We need to convert the cache (an alist of 'doi' . 'metadata')
  ;; into a plain list of just the metadata.
  (let ((doi-list (mapcar
		   (lambda (el) (cdr el))
		   org-doi-cache)))
    ;; Then transform the list into a vector with 'vconcat'
    ;; Otherwise json-encode will not encode properly.
    (json-encode (vconcat doi-list))))

;; (defun org-expand-doi--transform-cache (doi-cache)
;;   "Transforms the cache (an assoc list of doi and doi-metadata)
;;  into a list of the doi metadata."
;;   (mapcar (lambda (el) (cdr el)) org-doi-cache)
;;   (mapcar (lambda (el) (cdr el)) doi-cache))

;;;###autoload
(defun org-expand-doi-load-cache ()
  "Load `org-doi-cache' from `org-doi-cache-file'."
  (interactive)
  (when (and org-doi-cache-file
	     (file-exists-p org-doi-cache-file))
    (org-expand-doi-clear-cache)
    (with-temp-buffer
      (condition-case nil
	  (progn
	    (insert-file-contents org-doi-cache-file)
	    (setq org-doi-cache (read (current-buffer))))
	(error
         (message "Could not read `org-doi-cache' from %s, setting it to nil"
		  org-doi-cache-file))))))

(defun org-expand-doi-save-cache ()
  "Save `org-doi-cache' in `org-doi-cache-file'."
  (interactive)
  (when (and org-doi-cache-file org-doi-cache)
    (with-temp-file org-doi-cache-file
      (let ((print-level nil)
	    (print-length nil))
	(print org-doi-cache (current-buffer))))))

(defun org-expand-doi-clear-cache ()
  "Clear the doi metadata cache."
  (interactive)
  (setq org-doi-cache nil))

(defun org-expand-doi-save-json ()
  "Export the `org-doi-cache' to a CSL json file, located at `org-doi-csl-file'."
  (interactive)
  (when (and org-doi-json-csl-file org-doi-cache)
    (with-temp-file org-doi-json-csl-file
      (let ((print-level nil)
	    (print-length nil))
	;; alternative: json-insert
	(insert (org-expand-doi--cache-to-json))))))

(defun org-expand-doi-get-json-metadata (doi)
  "Try to get json metadata for DOI.  Open the DOI in a browser if we do not get it.
Afterwards, add an id equal to the doi number so we can reference it with org-cite.
Also cache the doi metadata if we need it later.

Function adapted from `doi-utils' by 'jkitchin'.
See 'https://github.com/jkitchin/org-ref'."
  (if-let* ((data (cdr (assoc doi org-doi-cache))))
      ;; We have the data already, so we return it.
      data
    ;; TODO: else check in bibliography if it already exists
    ;; (if (member doi (org-cite-basic--all-keys))
    ;; 	;; Somehow add the data to the cache
    ;; 	)
    ;; ELSE get it from internet
    (let ((url-request-method "GET")
          (url-mime-accept-string "application/citeproc+json")
          (json-object-type 'plist)
          (json-data)
	  (url (concat org-link-doi-server-url doi)))
      (with-temp-buffer
	(url-insert
	 (url-retrieve-synchronously url))
	(setq json-data (buffer-string))

	(when (or (string-match "<title>Error: DOI Not Found</title>" json-data)
		  (string-match "Resource not found" json-data)
		  (string-match "Status *406" json-data)
		  (string-match "400 Bad Request" json-data))
	  ;; (browse-url (concat org-link-doi-server-url doi))
	  (error "Something went wrong.  We got this response:
%s

Check if %s is a valid doi." json-data url))
	(setq data (json-read-from-string json-data))
	(plist-put data :id doi)
	(cl-pushnew (cons doi data) org-doi-cache)
	data))))

(provide 'org-expand-doi)
;;; org-expand-doi.el ends here
