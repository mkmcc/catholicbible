(provide 'bibleutils)

(require 's)

(defvar bible-cachedir
  (expand-file-name ".biblecache/" (getenv "HOME"))
  "Root directory for cached Bible chapters.")

(defun bibleutils--ensure-dir (path)
  (make-directory (file-name-directory path) t))

(defun bibleutils--fix-encoding (raw &optional coding-system)
  "Decode RAW string using CODING-SYSTEM (default UTF-8), strip carriage returns.
Returns a clean, decoded string safe for parsing."
  (let* ((decoded (decode-coding-string raw (or coding-system 'utf-8) t))
         (cleaned (s-replace "\r" "" decoded)))
    cleaned))

(defun bibleutils--get-html-body-raw (url)
  "Fetch URL synchronously, skipping headers. Return raw body as string or nil.
Suitable for HTML, JSON, or plain text responses."
  (let ((buf (url-retrieve-synchronously url t t 10)))
    (if (not (buffer-live-p buf))
        (progn
          (message "Failed to fetch: %s" url)
          nil)
      (with-current-buffer buf
        (goto-char (point-min))
        (if (not (re-search-forward "^$" nil t))
            (progn
              (message "Malformed HTTP response (no headers/body): %s" url)
              (kill-buffer buf)
              nil)
          ;; check status code
          (goto-char (point-min))
          (if (not (looking-at "HTTP/1.1 200 OK"))
              (progn
                (message "Non-200 HTTP response: %s" url)
                (message "\n\n%s" (buffer-substring-no-properties (point-min) (point-max)))
                (kill-buffer buf)
                nil)
            ;; extract body
            (re-search-forward "\n\n" nil 'move)
            (prog1
                (bibleutils--fix-encoding
                 (buffer-substring-no-properties (point) (point-max)))
              (kill-buffer buf))))))))

(defun bibleutils--get-html-body (url path)
  "Fetch HTML body from URL.
If PATH is provided and the file exists, return its contents instead.
If PATH is provided and the file does not exist, download and cache it to that location.
Returns the UTF-8 decoded, cleaned body string."
  (when path
    (bibleutils--ensure-dir path))
  (if (and path (file-exists-p path))
      ;; load from file
      (when path
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-substring-no-properties (point-min) (point-max))))
    ;; download and cache
    (let ((html (bibleutils--get-html-body-raw url)))
      (unless html
        (error "Download failed for url `%s'" url))
      (when path
        (with-temp-buffer
          (set-buffer-file-coding-system 'utf-8)
          (insert html)
          (write-region (point-min) (point-max) path nil 'quiet)))
      html)))
