;; weblogger-dummy.el
;;
;; This is just a stub for possibly implementing other blogs.

(defun install-dummy ()
  (interactive)
  (setq weblogger-api-send-edits 'weblogger-api-dummy-send-edits
        weblogger-api-new-entry 'weblogger-api-dummy-new-entry
        weblogger-api-list-categories 'weblogger-api-dummy-list-categories
        weblogger-api-list-entries 'weblogger-api-dummy-list-entries
        weblogger-new-entry-hook nil
        ))

(defun install-blogger ()
  (interactive)
  (setq weblogger-api-send-edits 'weblogger-api-blogger-send-edits
        weblogger-api-new-entry 'weblogger-api-blogger-new-entry
        weblogger-api-list-categories 'weblogger-api-blogger-list-categories
        weblogger-api-list-entries 'weblogger-api-blogger-list-entries
        ))

(defun weblogger-api-dummy-send-edits (struct &optional publishp)
  "Blogger API method to post edits to an entry specified by
STRUCT.  If PUBLISHP is non-nil, publishes the entry as well."
  (message "Dummy send-edits."))

(defun weblogger-api-dummy-new-entry (struct publishp)
  "Post a new entry from STRUCT.  If PUBLISHP is non-nil, publishes the
entry as well."
  (message "Dummy new-entry."))

(defun weblogger-api-dummy-list-categories ()
  "Return a list of categories that the weblog server has. (Not supported yet)"
  (message "Dummy list-categories.")
  (setq weblogger-category-list nil))

(defun weblogger-api-dummy-list-entries (&optional count)
  "Return a list of entries that the weblog server has.  COUNT specifies
how many of the most recent entries to get.  If COUNT is not
specified, then the default is weblogger-max-entries-in-ring."
  (message "Dummy list-entries.")
  (let ((entry '(("title" . "Dumb Title")                  ; Subject
                 ("authorName" . "Shane")                  ; From
                 ("dateCreated" . "Dumb date")             ; Date
                 ("content" . "Dumb content")              ; 
                 ("url" . "http://thisurlisdumb.com")      ; X-Url
                 ("texttype" . "") ; X-TextType
                 ("categories" . ("dumb" "silly"))             ; Keywords
                 ("mt_keywords" . ("awkward" "silent"))        ; Summary
;                 ("trackbacks" . "http://")                ; In-Reply-To
                 ("entry-id" . "//123jsdf")                      ; Message-Id
                 ("userid" . "shane.celis")                ; 
                 )))
    (setq weblogger-entry-list 
          (mapcar (lambda (entry)
                    (ring-insert-at-beginning weblogger-entry-ring
                                              entry)) (list entry)))
    ))

(provide 'weblogger-dummy)
