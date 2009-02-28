;; weblogger-blogger.el
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
(defun weblogger-api-blogger-get-content (struct)
  "Return the content for this post, optionally inserting the
title in the first row if weblogger-blogger-firstline-title is
set."
  (if weblogger-blogger-firstline-title
      (concat "<title>"
	      (cdr (assoc "title" struct))
	      "</title>\n"
	      (cdr (assoc "content" struct)))
      (cdr (assoc "content" struct))))

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

(defun weblogger-api-blogger-list-entries (&optional count)
  "Return a list of entries that the weblog server has.  COUNT specifies
how many of the most recent entries to get.  If COUNT is not
specified, then the default is weblogger-max-entries-in-ring."
  (setq weblogger-entry-list
	(mapcar
	 (lambda (entry)
	   (ring-insert-at-beginning weblogger-entry-ring
				     (weblogger-response-to-struct entry)))
	 (xml-rpc-method-call
	  weblogger-server-url
	  'blogger.getRecentPosts
	  weblogger-blogger-app-key
	  (weblogger-weblog-id)
	  (weblogger-server-username)
	  (weblogger-server-password)
	  (or count weblogger-max-entries-in-ring)))))

(provide 'weblogger-blogger)
