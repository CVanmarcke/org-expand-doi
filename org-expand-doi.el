;;; org-expand-doi.el --- Org DOI expander and extender  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Bruce D'Arcus

;; Author: C Vanmarcke
;; Version: 0.1
;; Homepage: https://github.com/cvanmarcke/org-expand-doi

;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;;  An Emacs library designed to streamline the use of DOIs in Org-mode.
;;  It automatically fetches metadata for DOI links, expands them into
;;  human-readable formats (like "Author et al. (Year)"),
;;  and integrates them with org-cite or citar by generating
;;  a local CSL-JSON bibliography on the fly.
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url-http)
(require 'url-handlers)
(require 'oc-basic)
(require 'ol-doi)
(require 'ox)

;;; Declare variables and functions for byte compiler

(declare-function s-format "s")
(declare-function url-insert "url-handlers")
(declare-function org-element-context "org-element")
(defvar url-request-method)
(defvar url-mime-accept-string)

;:; Variables
(defcustom org-expand-doi-format "${first-author} et al (${year})"
  "The format to expand doi links.

By default set to \"${first-author} et al (${year})\":
this will replace doi:XXXXX with \"Author et al (2025)\".

This can be customized using the following placeholders:
- ${any_json_key}: any key in the JSON CSL without colon:
      eg publisher, issue, type, page, volume, language, ...
- ${title}: Fetches the corresponding metadata field.
- ${first-author}: The family name of the first author.
- ${year}: The publication year.
- ${DOI}: The DOI number (note: capital letters!).
- ${cite}: Inserts a citation in the form of [cite:@doi_number].
   Note that you cannot have a citation inside a link.
   If you want a citation and a link, either
   (1) make sure org-expand-doi-default-expansion is set to `all' or
       `citation' or
   (2) set `org-expand-doi-make-link' to nil.

The related variable `org-expand-doi-export-format' is used
for the export process."
  :group 'org-expand-doi
  :type 'string)

(defcustom org-expand-doi-export-format "${first-author} et al (${year})"
  "The format to expand doi links during export.
See `org-expand-doi-format' for more information."
  :group 'org-expand-doi
  :type 'string)

(defcustom org-doi-cache-file
  (expand-file-name (convert-standard-filename "var/org/org-doi-cache.el") user-emacs-directory)
  " Deprecated. Now the cache is loaded and saved from/to the JSON CSL file.

Location of the doi cache (only used between emacs restarts)."
  :group 'org-expand-doi
  :type 'file)

(defcustom org-doi-json-csl-file
  (expand-file-name (convert-standard-filename "var/org/org-doi-csl.json") user-emacs-directory)
  "Location of the json csl file.
This file can be added to `org-cite-global-bibliography'
  to expand org citations with doi numbers.

This file is also used as cache between emacs restarts."
  :group 'org-expand-doi
  :type 'file)

(defcustom org-expand-doi-auto-save t
  "If non-nil, auto save the cache on emacs exit."
  :group 'org-expand-doi
  :type '(choice (boolean :tag "Auto save cache on exit" t)
                 (boolean :tag "Do not auto save cache on exit" nil))
  :safe #'booleanp)

(defcustom org-expand-doi-citation-prefix ""
  "The prefix to set before inserting a citation."
  :type 'string
  :group 'org-expand-doi)

(defcustom org-expand-doi-citation-suffix ""
  "The suffix to set after inserting a citation."
  :type 'string
  :group 'org-expand-doi)

(defcustom org-expand-doi-default-expansion 'all
  "When non-nil, when expanding doi links, replace the description.
When nil, keep the description as-is."
  :group 'org-expand-doi
  :type '(choice (symbol :tag "Replace link and add citation." 'all)
                 (symbol :tag "Only append a citation after doi link." 'citation)
		 (symbol :tag "Only replace the doi link." 'link))
  :safe #'symbolp)

(defcustom org-expand-doi-export-backend 'any
  "For which backend the export process should be allowed.
If `'any' is selected, always apply. Otherwise, choose your backend."
  :group 'org-expand-doi
  :type 'symbol
  :safe #'symbolp)

(defcustom org-expand-doi-replace-existing-description t
  "When non-nil, when expanding doi links, replace the description.
When nil, keep the description as-is." 
  :group 'org-expand-doi
  :type 'boolean
  :safe #'booleanp)

(defcustom org-expand-doi-make-link t
  "When non-nil and `org-expand-doi-default-expansion' is `'all' or `'link',
when a doi link is expanded, create a link in the form of `[[doi:XXXX][descr]]'
(with description according to `org-expand-doi-format'
or `org-expand-doi-export-format').
If nil, expand without creating a link.

Note that if expansion is performed without link and the buffer is saved,
the link to the reference might be lost as the doi number will disappear.
This will not occur if the expansion is only during export
as modifications occur in a temporary buffer."
  :group 'org-expand-doi
  :type 'boolean
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
Does not include `doi:', `cite:@', or brackets.")
(defvar org-doi-cite-re (concat "\\[cite:@" org-doi-re "\\]")
  "Regex for a doi number inside a cite block")

;;; Functions

;;;###autoload
(defun org-expand-doi-setup ()
  "Set up the hooks and variables.

An org export hook is set so doi links are automatically expanded on export.

The cache is automatically populated the first time
`org-expand-doi-get-json-metadata' is run, either by loading
an existing json file from disk or getting the doi data from the internet.

The CSL JSON file, if it exists, is added to the bibliography.

A hook is set to save the JSON cache when emacs exits. "
  (interactive)
  ;; Add the CSL json file to the bibliography.
  (when (and org-doi-json-csl-file (file-exists-p org-doi-json-csl-file))
    (add-to-list 'org-cite-global-bibliography org-doi-json-csl-file))
  ;; Add hook to save the cache on emacs exit
  (add-hook 'kill-emacs-hook (lambda ()
			       (when org-expand-doi-auto-save
				 (org-expand-doi-save-json))))
  ;; Add hook to the export processor
  (add-hook 'org-export-before-processing-functions #'org-expand-doi-export-hook))

(defun org-expand-doi-cleanup ()
  "Remove all hooks and variable modifications set by `org-expand-doi-setup'."
  (interactive)
  (setq org-cite-global-bibliography
	(remove org-doi-json-csl-file org-cite-global-bibliography))
  (remove-hook 'kill-emacs-hook (lambda ()
				  (when org-expand-doi-auto-save
				    (org-expand-doi-save-json))))
  (remove-hook 'org-export-before-processing-functions #'org-expand-doi-export-hook))

(defun org-expand-doi-export-hook (backend)
  "This function is hooked into `org-export-before-processing-hook'
to expand all doi links in a buffer before export.
You can change the backends to which it will apply
in the variable `org-expand-doi-export-backend'"
  (when (or (equal 'any org-expand-doi-export-backend)
	    (equal backend org-expand-doi-export-backend))
    (let ((org-expand-doi-format org-expand-doi-export-format))
      (add-to-list 'org-cite-global-bibliography org-doi-json-csl-file)
      (org-expand-doi-buffer)
      (org-expand-doi--get-missing-citations-buffer))))

;;;###autoload
(defun org-insert-doi (&optional doi)
  "Insert a doi link with automatic metadata retrieval and expansion,
according to the settings of `org-expand-doi'.

DOI is a DOI id string. If nil, prompt the user for a DOI id."
  (interactive)
  (let* ((doi (or doi (read-from-minibuffer "Enter doi number: ")))
	(desc (org-expand-doi--expand doi)))
    (cond
     ((eq 'citation org-expand-doi-default-expansion)
      (org-expand-doi-insert-citation doi))
     ((eq 'link org-expand-doi-default-expansion)
      (org-expand-doi--insert-link doi desc))
     ((eq 'all org-expand-doi-default-expansion)
      (org-expand-doi--insert-link doi desc)
      (org-expand-doi-insert-citation doi)))))

;;;###autoload
(defun org-expand-doi-buffer ()
  (interactive)
  (let ((regexp (concat "doi:" org-doi-re)))
    (org-with-point-at 1
      (while (re-search-forward regexp nil t)
	(org-expand-doi-at-point)))))

;;;###autoload
(defun org-expand-doi-at-point (&optional expand-what)
  (interactive)
  (when (string-equal "doi" (org-element-property :type (org-element-context)))
    (let* ((expand-what (or expand-what org-expand-doi-default-expansion))
	   (context (org-element-context))
	   (doi (org-element-property :path context))
	   (begin (org-element-property :begin context))
	   (end (- (org-element-property :end context)
		   ;; Substract 1 if there is a whitespace after the link,
		   ;; because otherwise :end is the start of the next word
		   (org-element-property :post-blank context)))
	   (desc (and (org-element-property :contents-begin context)
                      (org-element-property :contents-end context)
                      (buffer-substring-no-properties
                       (org-element-property :contents-begin context)
                       (org-element-property :contents-end context))))
	   ;; Prepare the bounds for `citation-after'
	   (search-bound (+ end (length org-expand-doi-citation-prefix)
			    7 5  ;; 7 for [cite:@], 5 extra to make sure
			    (length doi)))
	   ;; Look for citation after link. If present, no citation needs to be added.
	   (citation-after (search-forward (concat "[cite:@" doi) search-bound t)))
      (cond
       ((eq expand-what 'citation)
	(unless citation-after
	  (goto-char end)
	  (org-expand-doi-insert-citation doi t)))
       ((eq expand-what 'link)
	(org-expand-doi--link-overwrite begin end doi desc))
       ((eq expand-what 'all)
	(unless citation-after
	  (goto-char end)
	  (org-expand-doi-insert-citation doi t))
	(org-expand-doi--link-overwrite begin end doi desc))))))

(defun org-expand-doi--link-overwrite (begin end doi &optional desc)
  (let ((desc (if (or org-expand-doi-replace-existing-description
		      (not desc))
		  (org-expand-doi--expand doi)
		desc))) 
    (goto-char begin)
    (delete-region begin end)
    (org-expand-doi--insert-link doi desc)))

(defun org-expand-doi--insert-link (doi desc)
  (insert 
   (if org-expand-doi-make-link
       (format "[[%s][%s]]" (concat "doi:" doi) desc)
     (format "%s" desc))))

;;;###autoload
(defun org-expand-doi-insert-citation (&optional doi insert-only)
  "Insert a citation to a DOI id at point.
If DOI is nil, prompt the user for a doi number.

When insert-only is nil, automatically retrieves the doi metadata
if it is not already in the cache."
  (interactive)
  (let ((doi (or doi (read-from-minibuffer "Enter doi number: ")))) 
    (insert (format "%s[cite:@%s]%s"
		    org-expand-doi-citation-prefix
		    doi
		    org-expand-doi-citation-suffix))
    (unless (or insert-only (org-cite-basic--get-entry doi))
      (if (org-expand-doi-get-json-metadata doi)
	  (org-expand-doi-save-json)
	(message (format "Metadata for %s not found" doi))))))

(defun org-expand-doi--expand (doi)
  "Expand a doi number by its metadata,
according to the template `org-expand-doi-format'."
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
  (or 
   (condition-case nil
       (cond ((string-equal keyword "first-author")
	      (plist-get (org-expand-doi--get-first-author plist) :family 'equal))
	     ((string-equal keyword "first-author-surname")
	      (plist-get (org-expand-doi--get-first-author plist) :given 'equal))
	     ((string-equal keyword "first-author-surname-initial")
	      (substring 
	       (plist-get (org-expand-doi--get-first-author plist) :given 'equal)
	       0 1))
	     ((string-equal keyword "cite")
	      (format "[cite:@%s]" (plist-get plist :id 'equal)))
	     ((string-equal keyword "year")
	      (let ((published (or (plist-get plist :published 'equal)
				   (plist-get plist :issued 'equal))))
		(aref (aref (plist-get published :date-parts 'equal) 0) 0)))
	     (t
	      (plist-get plist (intern (concat ":" keyword)) 'equal)))
     (error (message "Error retrieving JSON fields for keyword %s from doi %s"
		     keyword (plist-get plist :id 'equal))
	    nil))
   ;; If the keyword did not exist or error return an empty string.
   ""))

(defun org-expand-doi--get-first-author (entry)
  (let ((authors (or (plist-get entry :author 'equal)
		     (plist-get entry :editor 'equal)))
	(first))
    ;; Author is in a vector, so we need to convert it first into a list.
    (cl-dolist (author (seq--into-list authors))
      (when (or (equal 1 (length authors)) ;; if only 1 author
		(string-equal (plist-get author :sequence) "first"))
	(setq first author)))
    first))

(defun org-expand-doi--get-missing-citations-buffer ()
  "Look for all doi styled citations (eg [cite:@doinumber])
and if any of them are not known by the citation manager, get them."
  (let ((citations (org-expand-doi--doi-citations-in-buffer org-doi-cite-re 1)))
    (dolist (doi citations)
      (unless (org-cite-basic--get-entry doi)
	(if (org-expand-doi-get-json-metadata doi)
	    (org-expand-doi-save-json)
	  (message (format "Metadata for %s not found" doi)))))))

(defun org-expand-doi--doi-citations-in-buffer (regexp &optional match-group buffer)
  "Return a list of all the doi citations in the buffer.
A doi citation is in the form of [cite:@doi_number].
There is support for combination citations, eg [cite:@citation1;@citation2].
Duplicates will automatically be filtered."
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
      (delete-dups
	   (flatten-list
		;; Split any combination doi numbers (eg [cite:@123;@abc])
		(mapcar (lambda (el) (string-split el ";@")) matches))))))

(defun org-expand-doi--cache-to-json ()
  "Convert the doi cache to a json CSL file."
  ;; We need to convert the cache (an alist of `doi' . `metadata')
  ;; into a plain list of just the metadata.
  (let ((doi-list (mapcar
		   (lambda (el) (cdr el))
		   org-doi-cache)))
    ;; Then transform the list into a vector with `vconcat'
    ;; Otherwise json-encode will not encode properly.
    (json-encode (vconcat doi-list))))

;;;###autoload
(defun org-expand-doi-load-json ()
  "Load `org-doi-cache' from `org-doi-json-csl-file'."
  (interactive)
  (if (and org-doi-json-csl-file
	   (file-exists-p org-doi-json-csl-file))
      (condition-case nil 
	  (if-let ((data (with-temp-buffer
			   (insert-file-contents org-doi-json-csl-file)
			   (json-parse-buffer :object-type 'plist))))
	      (progn 
		(setq org-doi-cache 
		      (mapcar (lambda (el)
				(let ((DOI (plist-get el :DOI 'equal)))
				  (cons DOI el)))
			      data))
		(message "Loaded doi data from %s" org-doi-json-csl-file))
	    (message "Could not load data from %s, it returned nil" org-doi-json-csl-file))
	(error
         (message "Error parsing `org-doi-json-csl-file' from %s" org-doi-json-csl-file)))
    (message "Error opening the CSL file: does the file exist?")))

(defun org-expand-doi-save-json ()
  "Export the `org-doi-cache' to a CSL json file, located at `org-doi-csl-file'."
  (interactive)
  (when (and org-doi-json-csl-file org-doi-cache)
    (make-directory (file-name-parent-directory org-doi-json-csl-file) t)
    (with-temp-file org-doi-json-csl-file
      (let ((print-level nil)
	    (print-length nil))
	;; alternative: json-insert
	(insert (org-expand-doi--cache-to-json))))))

(defun org-expand-doi-load-cache ()
  "Deprecated. Now the cache is loaded and saved from/to the JSON CSL file.

Load `org-doi-cache' from `org-doi-cache-file'."
  ;; DEPRACATED, use 'org-expand-doi-load-json'
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
         (message "Could not read `org-doi-cache' from %s"
		  org-doi-cache-file))))))

(defun org-expand-doi-save-cache ()
  "Deprecated. Now the cache is loaded and saved from/to the JSON CSL file.

Save `org-doi-cache' in `org-doi-cache-file'."
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

(defun org-expand-doi-get-json-metadata (doi)
  "Try to get json metadata for DOI.
Add the DOI number as id so we can reference it with org-cite.
Also cache the doi metadata if we need it later.

Function adapted from `doi-utils' by `jkitchin'.
See `https://github.com/jkitchin/org-ref'."
  ;; If the cache is nil and the CSL JSON file already exists
  ;; Load the cache from the JSON file
  (when (and (not org-doi-cache)
	     org-doi-json-csl-file
	     (file-exists-p org-doi-json-csl-file))
    (org-expand-doi-load-json))
  (if-let* ((data (cdr (assoc doi org-doi-cache))))
      ;; We have the data already, so we return it.
      data
    ;; ELSE get it from internet
    (condition-case nil 
	(let ((url-request-method "GET") ;; necessary to get the right response
	      (url-mime-accept-string "application/citeproc+json") ;; necessary
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
	      (error "Something went wrong.  We got this response:
%s

Check if %s is a valid doi." json-data url))
	    (setq data (json-read-from-string json-data))
	    (plist-put data :id doi)
	    (cl-pushnew (cons doi data) org-doi-cache)
	    data))
      ;; Catch an error and return nil.
      (error
       (message "There was an error getting or parsing the json data of doi %s." doi)
	     nil))))

;;; Test area
(when nil
  (org-expand-doi-clear-cache)

  (setq testdoi "10.1007/978-3-642-13327-5")
  (setq testdoi "10.1259/bjr/98449202")
  (setq testdoi-data (org-expand-doi-get-json-metadata testdoi))
  (setq testdoi-expansion (org-expand-doi--expand testdoi))

  (setq json-cache (org-expand-doi--cache-to-json))

  (org-expand-doi-save-json)
  (org-expand-doi-load-json))

(provide 'org-expand-doi)
;;; org-expand-doi.el ends here
