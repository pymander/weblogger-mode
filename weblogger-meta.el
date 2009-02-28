;; weblogger-meta.el

(defun weblogger-api-meta-send-edits (struct &optional publishp)
  "MetaWeblog API method to post edits to a entry specified by
STRUCT.  If PUBLISHP is non-nil, publishes the entry as well."
  (xml-rpc-method-call
   weblogger-server-url
   'metaWeblog.editPost
   (cdr (assoc "entry-id" struct))
   (weblogger-server-username)
   (weblogger-server-password)
   (weblogger-struct-to-request struct)
   publishp))

(defun weblogger-api-meta-new-entry (struct publishp)
  "Post a new entry (STRUCT).  If PUBLISHP is non-nil, publishes
the entry as well."
  (xml-rpc-method-call
   weblogger-server-url
   'metaWeblog.newPost
   (weblogger-weblog-id)
   (weblogger-server-username)
   (weblogger-server-password)
   (weblogger-struct-to-request struct)
   publishp))

(defun weblogger-api-meta-list-categories ()
  "Return a list of entries that the weblog server has."
  (setq weblogger-category-list
	(mapcar 
	 (lambda (category)
	   (ring-insert-at-beginning
            weblogger-category-ring (cdr (assoc "categoryName"
                                                category))))
	 (xml-rpc-method-call
	  weblogger-server-url
	  'metaWeblog.getCategories
	  (weblogger-weblog-id)
	  (weblogger-server-username)
	  (weblogger-server-password)
	  ))))

(defun weblogger-api-meta-list-entries (&optional count)
  "Return a list of entries that the weblog server has.  COUNT specifies
how many of the most recent entries to get.  If COUNT is not
specified, then the default is weblogger-max-entries-in-ring."
  (setq weblogger-entry-list 
	(mapcar 
	 (lambda (entry)
	   (ring-insert-at-beginning  weblogger-entry-ring
				      (weblogger-response-to-struct entry)))
	 (xml-rpc-method-call
	  weblogger-server-url
	  'metaWeblog.getRecentPosts
	  (weblogger-weblog-id)
	  (weblogger-server-username)
	  (weblogger-server-password)
	  (or count weblogger-max-entries-in-ring)))))

(provide 'weblogger-meta)
