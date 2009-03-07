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
(require 'g)
(require 'g-app)
(require 'gblogger)

(defun install-gdata ()
  (interactive)
  (setq weblogger-api-list-entries 'weblogger-api-gdata-list-entries)
  (setq weblogger-api-send-edits 'weblogger-api-gdata-send-edits)
  (setq weblogger-api-new-entry 'weblogger-api-gdata-new-entry)
  )

(defun weblogger-api-gdata-send-edits (struct &optional publishp)
  "Blogger API method to post edits to an entry specified by
STRUCT.  If PUBLISHP is non-nil, publishes the entry as well."
  (message "gdata-send-edits")
  (setq last-struct struct)
  (let* ((buffer     (current-buffer))
         (url        (setq last-url (cdr (assoc "url" struct))))
         (xml-buffer (gblogger-edit-entry url)) ;(g-app-get-entry gblogger-auth-handle url))
         (entry      (car (parse-xml-buffer xml-buffer)))
         (title      (cdr (assoc "title" struct)))
         (content    (cdr (assoc "content" struct)))
         (categories (cdr (assoc "categories" struct)))
         )
    (save-excursion
      (setq last-xml-buffer xml-buffer)
      (setq last-entry entry)
      (setq last-categories categories)
      (e-blog-change-title entry title)
      (e-blog-change-content entry content)
      (my-change-labels entry categories)
      ;; The rest of this function is performed in `e-blog-tmp-buffer'
      ;; since the `e-blog-elisp-to-xml' did a `set-buffer'.
      ;(e-blog-elisp-to-xml entry "*atom entry*")
      (my-elisp-to-xml entry xml-buffer)
      ;(e-blog-change-labels categories "*atom entry*")
      (g-app-publish)
      )   
    (set-buffer buffer)
    (set-buffer-modified-p nil)
  ))

(defun my-set-unmodified ()
  (interactive)
  (set-buffer-modified-p nil))

(defun my-change-labels (entry labels)
  (let* ((term (assoc 'term (cadar (xml-get-children last-entry 'category))))
         (labels-string (mapconcat #'identity labels " ")))
    (and term (setcdr term labels-string))))

(defun my-elisp-to-xml (elisp buffer)
  (set-buffer (get-buffer-create buffer))
  (erase-buffer)
  (xml-debug-print-internal elisp " "))
         
(defun weblogger-api-gdata-new-entry (struct publishp)
  "Post a new entry from STRUCT.  If PUBLISHP is non-nil, publishes the
entry as well."
  (message "gdata-new-entry")
  (let* ((post-url "http://www.blogger.com/feeds/5594832128999528489/posts/default")
         (title      (cdr (assoc "title" struct)))
         (content    (cdr (assoc "content" struct)))
         (categories (cdr (assoc "categories" struct)))
         (xml-buffer (gblogger-new-entry post-url title))
         (entry      (car (parse-xml-buffer xml-buffer)))
         )
    (setq last-xml-buffer xml-buffer)
    (setq last-entry entry)
    (e-blog-change-title entry title)
    (e-blog-change-content entry content)
    (my-change-labels entry categories)
    (my-elisp-to-xml entry xml-buffer)
    (g-app-publish)     
    (set-buffer-modified-p nil)
  ))

(defun weblogger-api-gdata-list-categories ()
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
  (let* ((links (e-blog-get-links entry))
         (url (cadr (assoc "alternate" links)))
         (edit-url (cadr (assoc "edit" links)))
         (author (car (xml-get-children entry 'author)))
         (author-name (caddar (xml-get-children author 'name)))
         (entry-id (caddar (xml-get-children entry 'id)))
         )
    `(("weird-thing" . "blah")          ; Custom entries aren't
                                        ; carried over, boo!
      ("title" . ,(e-blog-get-title entry))
      ("content" . ,(e-blog-get-content entry))
      ("categories" . ,(e-blog-get-labels entry))
      ;;("url" . ,url)
      ("url" . ,edit-url)
      ("authorName" . ,author-name)
      ("entry-id" . ,entry-id)
      ;;("mt_keywords" . ("what" "stuff"))
      ;;("categories" . ("what2" "stuff2"))
      )))

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

(defun e-blog-get-labels (entry)
  "Given an elisp representation of an ENTRY, returns a list of
labels for that ENTRY."
  (let (post-labels)
    (setq post-labels ())
    (dolist (label (xml-get-children entry 'category))
      (add-to-list 'post-labels
		   (xml-get-attribute label 'term)))
    post-labels))

(provide 'weblogger-gdata)
