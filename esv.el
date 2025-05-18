;;; esv.el --- Retrieve ESV Bible verses via api.esv.org -*- lexical-binding: t; -*-

;; Requires:
;; - API token from https://api.esv.org/

(require 'json)
(require 'url)
(require 'url-util)
(require 's)

(require 'bibleutils)

(defvar esv-api-token nil
  "API token for accessing the ESV passage API.")

(defun esv--require-token ()
  (unless esv-api-token
    (error "ESV API token not set. Please customize `esv-api-token`.")))

(defun esv--url (reference)
  "Construct a URL query for REFERENCE"
  (let* ((base-url "https://api.esv.org/v3/passage/text/")
         (params `(("q" . ,(list reference))
                   ("include-passage-references" . ,(list "false"))
                   ("include-footnotes" . ,(list "false"))
                   ("include-headings" . ,(list "false"))))
         (query-string (url-build-query-string params)))
    (concat base-url "?" query-string)))


(defun esv-format-verses-text (reference)
  "Fetch the passage text for REFERENCE from the ESV API."
  (esv--require-token)
  (let* ((url-request-extra-headers `(("Authorization" . ,(concat "Token " esv-api-token))))
         (body (bibleutils--get-html-body-raw (esv--url reference))))
    (when body
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (let* ((data (json-parse-buffer))
                 (passages (gethash "passages" data)))
            (string-join passages "\n\n")))))))


;; (defun esv-format-verses-text (reference)
;;   "Fetch the passage text for REFERENCE from the ESV API."
;;   (esv--require-token)
;;   (let ((url-request-extra-headers
;;          `(("Authorization" . ,(concat "Token " esv-api-token)))))
;;     (with-current-buffer (url-retrieve-synchronously (esv--url reference) t t 10)
;;       (goto-char (point-min))
;;       (re-search-forward "\n\n") ;; Skip HTTP headers
;;       (let* ((json-object-type 'alist)
;;              (json-array-type 'list)
;;              (json-key-type 'symbol)
;;              (data (json-parse-buffer))
;;              (passages (gethash "passages" data)))
;;         (string-join passages "\n\n")))))


(defun esv--versify (s)
  "Replace [N] verse markers in S with LaTeX \\vs{N} macros."
  (s-replace-regexp "\\[\\([0-9]+\\)\\]" "\\\\vs{\\1}" s))

(defun esv--stripesv (s)
  "Remove (ESV) note from text.  We add it back below."
  (s-replace "(ESV)" "" s))

(defun esv-format-verses-latex (reference)
  "Return LaTeX-formatted ESV passage for REFERENCE string."
  (let* ((raw (esv-format-verses-text reference))
         (clean (esv--stripesv raw))
         (versified (esv--versify clean)))
    (format "\\begin{scripture}[%s (ESV)]\n%s\n\\end{scripture}"
            reference
            versified)))

(provide 'esv)
