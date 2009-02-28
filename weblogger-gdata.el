;; weblogger-gdata.el
;;
;; I want to break this into a couple different things, the UI,
;; XML-RPC which has two parts
;;                                              +--------------+
;;                                +-----------+ |   blogger old|
;;        +----------------+      |           | +--------------+
;;        |                |      |  XML-RPC  |
;;        |   UI           |      |           | +--------------+
;;        |                |      +-----------+ |  meta        |
;;        |                |                    +--------------+
;;        +----------------+
;;                                +-----------+
;;                                | GData     |
;;                                +-----------+
;;
;;
;; (g-app-get-entry gblogger-auth-handle gblogger-base-url)
;; (g-app-get-entry gblogger-auth-handle "http://gnufool.blogspot.com/feeds/posts/default")

;; random e-blog code notes
;; (setq feed (e-blog-parse-xml (e-blog-fetch-bloglist)))
;; (e-blog-get-titles feed)
;; (setq entry (e-blog-get-entry "gnufool" feed))

(defun install-gdata ()
  (interactive)
  (setq weblogger-api-list-entries 'weblogger-api-gdata-list-entries)
  )

(defun weblogger-api-blogger-send-edits (struct &optional publishp)
  "Blogger API method to post edits to an entry specified by
STRUCT.  If PUBLISHP is non-nil, publishes the entry as well."
  (xml-rpc-method-call
   weblogger-server-url
   'blogger.editPost
   weblogger-blogger-app-key
   (cdr (assoc "entry-id" struct))
   (weblogger-server-username)
   (weblogger-server-password)
   (weblogger-api-blogger-get-content struct)
   publishp))

(defun weblogger-api-blogger-new-entry (struct publishp)
  "Post a new entry from STRUCT.  If PUBLISHP is non-nil, publishes the
entry as well."
  (xml-rpc-method-call
   weblogger-server-url
   'blogger.newPost
   weblogger-blogger-app-key
   (weblogger-weblog-id)
   (weblogger-server-username)
   (weblogger-server-password)
   (weblogger-api-blogger-get-content struct)
   publishp))

(defun weblogger-api-blogger-list-categories ()
  "Return a list of categories that the weblog server has. (Not supported yet)"
  (setq weblogger-category-list nil))

(defun parse-xml (string)
  "Parses XML to be represented in elisp."
  (let (parsed)
    (save-excursion
      (set-buffer (get-buffer-create e-blog-tmp-buffer))
      (erase-buffer)
      (insert string)
      (setq parsed (xml-parse-region (point-min) (point-max)))
      parsed)))

(defun parse-xml-buffer (buffer)
  "Parses XML to be represented in elisp."
  (save-excursion
    (set-buffer buffer)
    (xml-parse-region (point-min) (point-max))))

(defun entry-to-struct (entry)
  `(("title" . ,(e-blog-get-title entry))
    ("content" . ,(e-blog-get-content entry)))
  )

;; Let's just get the content and the title
(defun weblogger-api-gdata-list-entries (&optional count)
  "Return a list of entries that the weblog server has.  COUNT specifies
how many of the most recent entries to get.  If COUNT is not
specified, then the default is weblogger-max-entries-in-ring."
  (let* ((url "http://gnufool.blogspot.com/feeds/posts/default")
         (xml-buffer (g-app-get-entry gblogger-auth-handle url))
         (lispml (parse-xml-buffer xml-buffer))
         (entries (e-blog-get-entries lispml))
         )
  (setq weblogger-entry-list
	(mapcar
	 (lambda (entry)
	   (ring-insert-at-beginning weblogger-entry-ring
                                 (entry-to-struct entry)))
     entries
	 ))))

(provide 'weblogger-blogger)
