;; weblogger-gdata.el
;;
;; I want to break this into a couple different things, the UI,
;; XML-RPC which has two parts.
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

(require 'g)
(require 'g-app)
(require 'gblogger)

(defvar weblogger-gdata-fetch-bloglist-url "http://www.blogger.com/feeds/default/blogs")
(defvar weblogger-gdata-fetch-entries-url-format "http://www.blogger.com/feeds/%s/posts/full")
(defvar weblogger-gdata-draft-tag 
"<app:control xmlns:app='http://www.w3.org/2007/app'>
  <app:draft>yes</app:draft>
</app:control>")

;; defsubst is an inline function.

(defun gblogger-auth-handle ()
  (weblogger-cache 'gdata-auth-handle
                   (progn
                     (gblogger-sign-in)
                     gblogger-auth-handle)))
;;; I really have no good explanation for why I'm not able to make this
;;; work without gblogger directly.

;;                    (let ((handle (make-g-auth :server "blogger"
;;                                               :email (weblogger-server-username)
;;                                               :password (weblogger-server-password))))
;;                      (g-authenticate handle)
;;                      handle)))

(defun weblogger-install-gdata ()
  (interactive)
  (setq weblogger-api-list-entries 'weblogger-api-gdata-list-entries
        weblogger-api-send-edits   'weblogger-api-gdata-send-edits
        weblogger-api-new-entry    'weblogger-api-gdata-new-entry
        weblogger-api-delete-entry 'weblogger-api-gdata-delete-entry
        weblogger-api-list-categories nil
        weblogger-api-weblog-alist  'weblogger-api-gdata-weblog-alist
        weblogger-api-server-userid   nil))

(defun weblogger-api-gdata-send-edits (struct &optional publishp)
  "Blogger API method to post edits to an entry specified by
STRUCT.  If PUBLISHP is non-nil, publishes the entry as well."
  (setq last-struct struct)
  (let* ((buffer     (current-buffer))
         (url        (cdr (assoc "url" struct)))
         (xml-buffer (gblogger-edit-entry url)) 
         (entry      (car (parse-xml-buffer xml-buffer)))
         (title      (cdr (assoc "title" struct)))
         (content    (cdr (assoc "content" struct)))
         (categories (cdr (assoc "categories" struct)))
         )
    (save-excursion
      (weblogger-gdata-change-title entry title)
      (weblogger-gdata-change-content entry content)
      (weblogger-gdata-change-labels entry categories)
      (weblogger-gdata-set-draft entry (if publishp "no" "yes"))
      (weblogger-gdata-elisp-to-xml entry xml-buffer)
      (set-buffer xml-buffer)
      (g-app-publish)
      )   
    (set-buffer buffer)
    (set-buffer-modified-p nil)
  ))

(defun weblogger-gdata-set-unmodified ()
  (interactive)
  (set-buffer-modified-p nil))

(defun weblogger-gdata-change-labels (entry labels)
  (let* ((term (assoc 'term (cadar (xml-get-children last-entry 'category))))
         (labels-string (mapconcat #'identity labels ", ")))
    (and term (setcdr term labels-string))))

(defun weblogger-gdata-xml-parse-string (string)
  (with-temp-buffer
    (insert string)
    (xml-parse-region (point-min) (point-max))))

(defun weblogger-gdata-change-content (entry content)
  "Accepts the lispml entry and a string of content."
  (let* ((old-content (cdar (xml-get-children entry 'content)))
         (lispml (or (weblogger-gdata-xml-parse-string content) (list content)))
         )
    (setcdr old-content lispml)))

(defun weblogger-gdata-elisp-to-xml (elisp buffer)
  (save-excursion
    (set-buffer (get-buffer-create buffer))
    (erase-buffer)
    (xml-debug-print-internal elisp " ")))

(defun weblogger-gdata-get-post-url ()
  (let* ((buffer (g-app-get-entry (gblogger-auth-handle) gblogger-base-url))
         (feed (car (parse-xml-buffer buffer)))
         (entries (xml-get-children feed 'entry))
         (links (weblogger-gdata-get-links (car entries))))
    (if (> (length entries) 1)
        (message "can't handle multiple blogger blogs yet"))
    (cdr (assoc "post" links))))
         
(defun weblogger-api-gdata-new-entry (struct publishp)
  "Post a new entry from STRUCT.  If PUBLISHP is non-nil, publishes the
entry as well."
  (let* ((post-url   (weblogger-gdata-get-post-url))
         (title      (cdr (assoc "title" struct)))
         (content    (cdr (assoc "content" struct)))
         (categories (cdr (assoc "categories" struct)))
         (xml-buffer (gblogger-new-entry post-url title))
         (entry      (car (parse-xml-buffer xml-buffer))))
    (setq last-xml-buffer xml-buffer)
    (setq last-entry entry)
    (weblogger-gdata-change-title entry title)
    (weblogger-gdata-change-content entry content)
    (weblogger-gdata-change-labels entry categories)
    (weblogger-gdata-set-draft entry (if publishp "no" "yes"))
    (weblogger-gdata-elisp-to-xml entry xml-buffer)
    (g-app-publish)     
    (set-buffer-modified-p nil)
  ))

(defun weblogger-api-gdata-list-categories ()
  "Return a list of categories that the weblog server has. (Not supported yet)"
  (setq weblogger-category-list nil))

(defun parse-xml-buffer (buffer)
  "Parses XML to be represented in elisp."
  (save-excursion
    (set-buffer buffer)
    (xml-parse-region (point-min) (point-max))))

(defun entry-to-struct (entry)
  (let* ((links (weblogger-gdata-get-links entry))
         (url (cdr (assoc "alternate" links)))
         (edit-url (cdr (assoc "edit" links)))
         (author (car (xml-get-children entry 'author)))
         (author-name (caddar (xml-get-children author 'name)))
         (entry-id (caddar (xml-get-children entry 'id)))
         )
    `(("weird-thing" . "blah")          ; Custom entries aren't
                                        ; carried over, boo!
      ("title" . ,(weblogger-gdata-get-title entry))
      ("content" . ,(weblogger-gdata-get-content entry))
      ("categories" . ,(weblogger-gdata-get-labels entry))
      ("url" . ,edit-url)
      ("authorName" . ,author-name)
      ("entry-id" . ,entry-id)
      ("dateCreated" . ,(weblogger-gdata-get-date entry))
      )))

(defun weblogger-api-gdata-weblog-alist ()
  "We need to return something that looks like this:
  ((('url' . 'http://semios.livejournal.com/')
    ('blogName' . 'Shane')
    ('blogid' . 'semios'))
  (('url' . 'http://community.livejournal.com/_geek_love/')
    ('blogName' . 'geek love')
    ('blogid' . '_geek_love')))"
  (let* ((url weblogger-gdata-fetch-bloglist-url)
         (xml-buffer (g-app-get-entry (gblogger-auth-handle) url))
         (lispml (parse-xml-buffer xml-buffer))
         (entries (xml-get-children (xml-node-name lispml) 'entry))
         )
    (mapcar 'weblogger-gdata-entry-to-weblog-alist entries)))

(defun weblogger-gdata-entry-to-weblog-alist (entry)
  (let* ((title (car (xml-node-children (car (xml-get-children entry 'title)))))
         (url (cdr (assoc "alternate" (weblogger-gdata-get-links entry))))
         (id (nth 2 (car (xml-get-children entry 'id))))
         (better-id (replace-regexp-in-string ".*-\\([0-9]*\\)$" "\\1" id))
         )
    `(("blogName" . ,title)
      ("url" . ,url)
      ("blogid" .,better-id))))

(defun weblogger-api-gdata-list-entries (&optional count)
  "Return a list of entries that the weblog server has.  COUNT specifies
how many of the most recent entries to get.  If COUNT is not
specified, then the default is weblogger-max-entries-in-ring."
  (let* ((url (format weblogger-gdata-fetch-entries-url-format 
                      (weblogger-weblog-id)))
         (url* (if count (format "%s?max-results=%d" url count) url))
         (xml-buffer (g-app-get-entry (gblogger-auth-handle) url*))
         (lispml (parse-xml-buffer xml-buffer))
         (entries (xml-get-children (xml-node-name lispml) 'entry))
         )
  (setq weblogger-entry-list
	(mapcar
	 (lambda (entry)
	   (ring-insert-at-beginning weblogger-entry-ring
                                 (entry-to-struct entry)))
     entries))))

(defun weblogger-api-gdata-delete-entry (struct)
  (let* ((url (cdr (assoc "url" struct)))
         )
    (save-excursion
      (g-app-delete-entry (gblogger-auth-handle) url))))

(defun weblogger-gdata-change-title (entry title)
  "Given an elisp representation of an entry and a title, will
modify the title to given title."
  (setcar (xml-node-children
           (xml-node-name
            (xml-get-children entry 'title)))
          title))

(defun weblogger-gdata-get-links (entry)
  "Given an elisp representation of an ENTRY, returns a list of
the links associated with that ENTRY."
  (let* ((links (xml-get-children entry 'link))
         )
    (mapcar (lambda (link) 
              (cons (weblogger-gdata-strip-schema (xml-get-attribute link 'rel))
                    (xml-get-attribute link 'href))) links)))

(defun weblogger-gdata-strip-schema (url)
  (replace-regexp-in-string ".*#" "" url))

(defun weblogger-gdata-get-content (entry)
  "Given an elisp representation of an ENTRY, returns the content
for that ENTRY."
  (nth 0
       (xml-node-children
        (xml-node-name
         (xml-get-children entry 'content)))))

(defun weblogger-gdata-get-labels (entry)
  "Given an elisp representation of an ENTRY, returns a list of
labels for that ENTRY."
  (let ((categories (xml-get-children entry 'category)))
    (mapcar (lambda (label) (xml-get-attribute label 'term))
            categories)))

(defun weblogger-gdata-get-title (entry)
  "Given an  elisp representation of an ENTRY,  returns the title
of that ENTRY."
  (let* ((title-tag (xml-get-children entry 'title))
         (title (nth 0
                     (xml-node-children
                      (xml-node-name title-tag)))))
    title))

(defun weblogger-gdata-get-draft (entry)
  (let* ((control (car (xml-get-children entry 'app:control)))
         (draft   (car (xml-get-children control 'app:draft))))
    (string-equal "yes" (caddr draft))))

(defun weblogger-gdata-set-draft (entry string &optional recur)
  (let* ((control (car (xml-get-children entry 'app:control)))
         (draft   (car (xml-get-children control 'app:draft))))
    (if (cddr draft)
        (setcar (cddr draft) string)
        (if (string-equal "no" string)
            nil ; good, we don't need to do anything.
            (let* ((draft-xml (weblogger-gdata-xml-parse-string weblogger-gdata-draft-tag)))
              (if recur
                  (error "unable to add the draft tag")
                  (weblogger-gdata-set-draft (nconc entry draft-xml) string t)))))))

(defun weblogger-gdata-is-draft (entry)
  (string-equal "yes" (weblogger-gdata-get-draft entry)))

(defun weblogger-gdata-get-date (entry)
  (let* ((published (car (xml-get-children entry 'published))))
    (caddr published)))

(provide 'weblogger-gdata)
