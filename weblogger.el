;;; weblogger.el - Weblog maintenance via XML-RPC and GData APIs

;; Original Author: Mark A. Hershberger - See AUTHORS for more details.
;; Version: 2.0
;; Created: 2002 Oct 11
;; Keywords: weblog blogger cms movabletype openweblog blog gdata
;; URL: http://www.github.com/scelis/weblogger/tree/master

;; This file is not yet part of GNU Emacs.

;; weblogger.el free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; weblogger.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; weblogger.el implements the Blogger, MetaWeblog, Movable Type, and
;; LiveJournal APIs to talk to server-side weblog software.
;;
;;; Starting Out:
;;
;; If you don't yet have a weblog, you can set one up for free on
;; various services mentioned above.
;;
;;; Installation:
;;
;; I use the following commands in my .emacs file to load up
;; weblogger:
;;
;; (add-to-list 'load-path "/path/to/weblogger-directory")
;; (require 'weblogger)
;;
;;; Setup:
;;
;; To set up your profile:
;;
;;    M-x customize-group RET weblogger RET
;;
;; Make sure to pay special attention to "Weblog Configurations"
;; and "Weblogger Servers".
;;
;;; Keymaps:
;;
;; I use the following command in my .emacs file:
;;
;; (global-set-key "\C-cbs" 'weblogger-start-entry)
;;
;; Then C-c b s will then switch to a new buffer where you can compose
;; a entry.  
;;
;; C-x C-s    -- post-and-publish current buffer to the weblog.
;;               Calling weblogger-publish-entry with an prefix argument
;;               (i.e. C-u C-x C-s) will prompt for which weblog
;;               to use.
;;
;; C-c C-c    -- save as draft and bury the buffer.
;;
;; C-c C-n    -- post (but not publish) the current entry and
;;               load the next entry.
;;
;; C-c C-p    -- post (but not publish) the current entry and
;;               load the previous entry.
;;
;; C-c C-k    -- delete the current entry.
;;
;; M-g        -- synchronise weblogger.el's idea of the entries available
;;               with the weblog server.
;;
;; C-c C-t m  -- edit the main template.
;;
;; C-c C-t a  -- edit the Archive Index template
;;
;; C-c C-s s  -- Change the server being used.
;;
;; C-c C-s w  -- Change the weblog.
;;
;; C-c C-s u  -- Change the user (re-login).
;;
;; See the TODO and BUGS file to see the currently issues and
;; potential features.
;;

(require 'xml-rpc)
(require 'message)
(require 'ring)

;(require 'weblogger-dummy)
(require 'weblogger-blogger)
(require 'weblogger-gdata)
(require 'weblogger-meta)

(defgroup weblogger nil
  "Edit weblogs with Emacs."
  :group 'emacs
  :link '(url-link "http://www.github.com/scelis/weblogger/tree/master"))

(defconst weblogger-version "2.0"
  "Current version of weblogger.el")

(defvar weblogger-blogger-app-key "07C72E6970E0FBA5DE21BA9F4800C44534C19870"
  "The appkey to send to weblog server.  Generally this shouldn't be changed.")

(defvar weblogger-config-alist-default '(("myblog" ("weblog"   . "weblogid")
                                                   ("username" . "your username")
                                                   ("server"   . "blogger")))
  "The default configuration.  Gives us something to check
  against if someone tries to connect without doing anything.")

(defcustom weblogger-config-alist weblogger-config-alist-default
  "IMPORTANT!  This stores the username, server, blog id, and
  password*.  The server is the name specified below in the
  Weblogger Servers.  The URL and other server specific
  information is provided in the Weblog Servers section.

* If you're sure you want to store it in plaintext in your .emacs
  file.  It is not recommended."
  :group 'weblogger
  :tag "Weblog Configurations"
  :type '(alist :key-type string :value-type (alist :key-type string :value-type string :options ("weblog" "username" "server"))))

(defcustom weblogger-config-name "default"
  "Name of the default configuration."
  :tag "Default Weblog Configuration Name"
  :group 'weblogger
  :type 'string)

(defcustom weblogger-servers
  '(
    (blogger     "Blogger (GData)" (gdata "http://www.blogger.com/feeds/") nil)
    (blogger-old "Blogger (XMLRPC) old" (xmlrpc "http://www2.blogger.com/api") t)
;    (movabletype "Movable Type" (xmlrpc "") nil)
    (openweblog  "Open Weblog" 
     (xmlrpc "http://www.openweblog.com/xmlrpc/") nil)
    (cms         "Blog CMS" 
     (xmlrpc "http://www.yourserver.com/yourpath/nucleus/xmlrpc/server.php") nil)
;    (metaweblog  "MetaWeblog" (xmlrpc "") nil)
    (livejournal "Livejournal (Blogger API)" 
     (xmlrpc "http://www.livejournal.com/interface/blogger") t)
    ;; The livejournal XMLRPC API is not supported yet.
    ;;(livejournal "Livejournal" (xmlrpc "http://www.livejournal.com/interface/xmlrpc"))
    )
  "Each server has a name that can be referenced in Weblog
Configurations, a descrption, whether its XML-RPC or GData along
with a URL, and whether it handles titles like Blogger's XML-RPC
API, which is to place the title on the first line.  "
  :tag "Weblog Servers"
  :type '(repeat (group 
                  (symbol :tag "Server")
                  (string :tag "Description")
                  (choice :tag "RPC Type"
                   (group :tag "XML-RPC" (const xmlrpc) (string :tag "URL"))
                   (group :tag "GData" (const gdata) (string :tag "URL")))
                  (boolean :tag "Title in first line")
                   ))
  :group 'weblogger)

; If we do save the passwords, we should do some half-ass encryption
; at least.
(defcustom weblogger-save-password nil
  "Whether to save to the password to disk or not.  (Passwords
will still be cached in the session even if this is set to nil.)"
  :group 'weblogger
  :type 'boolean)

(defcustom weblogger-default-title ""
  "The default title to use when making an entry.  This is added
if your weblog server supports titles on entries but you haven't
set one.  Set to \"\" for no title."
  :type 'string
  :group 'weblogger)

(defcustom weblogger-default-categories nil
  "The default list of categories when making an entry.  This is
added if your weblog server supports categories on entries but you
haven't set one.  Set to nil for no category."
  :type '(repeat (string :tag "category")) 
  :group 'weblogger)

(defcustom weblogger-default-tags nil
  "The default list of tags when making an entry. This is added
if your weblog server supports tags on entries but you haven't
set one.  Set to nil for no tags."
  :type '(repeat (string :tag "tag")) 
  :group 'weblogger)


(defvar weblogger-entry-list nil
  "List of weblog entries that we know about. Chronological
order, with newest first.")

(defvar weblogger-category-list nil
  "List of categories that we know about.")

(defvar weblogger-tag-list nil
  "List of tags that we know about.")

(defvar *weblogger-entry* nil
  "The buffer where we compose entries")

(defvar weblogger-entry-mode-hook nil
  "Hook to run after starting up weblogger mode.")

(defcustom weblogger-start-edit-entry-hook 
  (lambda ()
    (message-goto-body)
    (replace-string "\r" "" nil (point) (point-max)))
  "Hook to run after loading an entry in buffer for editting."
  :group 'weblogger)

(defcustom weblogger-new-entry-hook '(weblogger-ping-weblogs)
  "Hook to run after sending a new entry.  Typically, this is
where you would put weblogger-ping-weblogs to let weblog
aggregators know that you have updated."
  :group 'weblogger)

(defcustom weblogger-max-entries-in-ring 20
  "Maximum number of entries that will be retrieved from the
server.  There may be a server-side limitation to this number."
  :group 'weblogger
  :type 'integer)

(defcustom weblogger-ping-urls '("http://rpc.weblogs.com/RPC2")
  "List of URLs to ping using the XML-RPC interface defined at 
<http://www.xmlrpc.com/weblogsCom>."
  :group 'weblogger
  :type '(repeat (string :tag "URL")))


(defvar weblogger-cache nil
  "Holds a cache of all the values associated with the current server.")

(defcustom weblogger-edit-entry-hook nil
  "Hook to run after updating an entry."
  :group 'weblogger)

(defvar weblogger-entry-mode-map nil
  "Keymap for weblogger-entry-mode.")

(defvar weblogger-template-mode-map nil
  "Keymap for weblogger-template-mode.")

(defvar weblogger-entry-ring nil
  "Ring that holds all the entries.")

(defvar weblogger-category-ring nil
  "Ring that holds all the categories.")

;; tag-ring is never used.  It should be removed.
;; (defvar weblogger-tag-ring nil
;;   "Ring that holds all the tags.")

(defvar weblogger-ring-index 0
  "Pointer to the index on the ring")

;; Known capabilities
(defconst weblogger-no-capabilities '(("blogger.newPost" . nil)
                                      ("blogger.getPost" . nil)
                                      ("blogger.editPost" . nil)
                                      ("blogger.getRecentPosts" . nil)
                                      ("blogger.getUsersBlogs" . nil)
                                      ("blogger.getUserInfo" . nil)
                                      ("blogger.deletePost" . nil)
                                      ("blogger.getTemplate" . nil)
                                      ("blogger.setTemplate" . nil)
                                      ("metaWeblog.getPost" . nil)
                                      ("metaWeblog.newPost" . nil)
                                      ("metaWeblog.editPost" . nil)	
                                      ("metaWeblog.newMediaObject" . nil)	
                                      ("metaWeblog.getRecentPosts" . nil)
                                      ("metaWeblog.getCategories" . nil)
                                      ("metaWeblog.newMediaObject" . nil)
                                      ("metaWeblog.deletePost" . nil)
                                      ("metaWeblog.getTemplate" . nil)
                                      ("metaWeblog.setTemplate" . nil)
                                      ("metaWeblog.getUsersBlogs" . nil)
                                      ("mt.getCategoryList" . nil)
                                      ("mt.getRecentPostTitles" . nil)
                                      ("mt.setPostCategories" . nil)
                                      ("mt.getPostCategories" . nil)
                                      ("mt.getTrackbackPings" . nil)
                                      ("mt.supportedMethods" . nil)
                                      ("mt.publishPost" . nil)
                                      ("mt.supportedTextFilters" . nil)))

(defvar weblogger-capabilities weblogger-no-capabilities
  "Known capabilities of the remote host")

;; Here are the functions that need to be implemented to support other
;; blogs.
(defvar weblogger-api-new-entry nil)
(defvar weblogger-api-send-edits nil)
(defvar weblogger-api-list-entries nil)
(defvar weblogger-api-list-categories nil)
(defvar weblogger-api-delete-entry nil)
(defvar weblogger-api-weblog-alist nil)
(defvar weblogger-api-server-userid nil)

(defvar menu-bar-weblogger-menu nil)

(unless weblogger-entry-mode-map
  (setq weblogger-entry-mode-map
        (let ((map (copy-keymap message-mode-map))
              (server-map (make-sparse-keymap))
              (template-map (make-sparse-keymap)))
          (define-key map "\C-c\C-c" 'weblogger-send-entry)
          (define-key map "\C-x\C-s" 'weblogger-publish-entry)
          (when (fboundp 'unicode-smart-double-quote)
            (define-key map "\"" 'unicode-smart-double-quote)
            (define-key map "'" 'unicode-smart-single-quote)
            (define-key map "-" 'unicode-smart-hyphen)
            (define-key map "." 'unicode-smart-period))
          (define-key map "\C-c\C-n" 'weblogger-next-entry)
          (define-key map "\C-c\C-p" 'weblogger-prev-entry)
          (define-key map "\C-c\C-k" 'weblogger-delete-entry)
          (define-key map "\M-g"     'weblogger-fetch-entries)
          (define-key template-map "m" 'weblogger-edit-main-template)
          (define-key template-map "a" 'weblogger-edit-archive-template)
          (define-key map "\C-c\C-t" template-map)
;          (define-key map "\C-c\C-o" 'weblogger-change-server)
          (define-key map "\C-c\C-w" 'weblogger-change-weblog)
          (define-key map "\C-c\C-u" 'weblogger-change-user)
          map)))

(unless weblogger-template-mode-map
  (setq weblogger-template-mode-map (copy-keymap text-mode-map))
  (define-key weblogger-template-mode-map "\C-x\C-s" 'weblogger-save-template))

(unless menu-bar-weblogger-menu 
  (easy-menu-define
      menu-bar-weblogger-menu weblogger-entry-mode-map ""
      '("Weblogger"
        ["Send weblog entry" weblogger-send-entry t]
        ["Save weblog entry" weblogger-save-entry nil t]
        ["--" nil nil]
        ["Delete entry"      weblogger-delete-entry t]
        ["--" nil nil]
        ["Previous entry"    weblogger-prev-entry t]
        ["Next entry"        weblogger-next-entry t]
        ["--" nil nil]
        ["Edit Main Template" weblogger-edit-main-template t]
        ["Edit Archive Template" weblogger-edit-main-template t]
        ["--" nil nil]
        ["Change Weblog"    weblogger-change-weblog t])))

(defun weblogger-cache-get (key)
  (cdr (assoc key weblogger-cache)))

(defun weblogger-cache-set (key value)
  (let ((cell (assoc key weblogger-cache)))
    (if cell
        (setcdr cell value)
        (setq weblogger-cache (nconc weblogger-cache (list (cons key value))))
        value
        )))

(defun weblogger-cache-invalidate (key)
  (setq weblogger-cache (assq-delete-all key weblogger-cache)))

(defmacro weblogger-cache (key &rest body)
  "We see if the value is already cached.  If not we run body,
and consider its return value to be the cacheable value.  This
way we can invalidate the cache very easily."
  `(or (weblogger-cache-get ,key)
       (weblogger-cache-set ,key (progn ,@body))))

(defun weblogger-config-server (&optional key)
  (let* ((keys '(symbol name rpc title))
         (index (position key keys))
         (server-sym (read (weblogger-config-param weblogger-config-name "server")))
         (server (assoc server-sym weblogger-servers)))
    (if (eq key 'url)
        (nth 1 (nth (position 'rpc keys) server))
        (if key
            (and index (nth index server))
            server))))

(defun weblogger-blogger-firstline-title ()
  "Look for the title in the first line surrounded by <title>
tags when using the Blogger API."
  (weblogger-config-server title))

(defun weblogger-select-configuration (config)
  "Select a previously saved configuration."
  (interactive (list (let* ((completion-ignore-case t)
                            (seq 0)
                            (configs (mapcar
                                      (lambda (config)
                                        (cons (car config) (setq seq (1+ seq))))
                                      weblogger-config-alist))
                            (conf    (if (= 1 (length configs))
                                         (caar configs)
                                         (completing-read 
                                          "Config Name: " configs nil t))))
                       (weblogger-check-configuration)
                            conf)))
  ;(unless (string-equal weblogger-config-name config)
    (setq weblogger-cache nil);)
  (setq weblogger-config-name config)
  (weblogger-determine-capabilities)
  (weblogger-api-weblog-alist t))

(defun weblogger-change-user ()
  "Change username and password."
  (interactive)
  (weblogger-check-configuration)
  (weblogger-server-username t)
  (weblogger-server-password t))

(defun weblogger-change-weblog ()
  "Change the weblog."
  (interactive)
  (weblogger-check-configuration)
  (let ((point-save (point)))
    (weblogger-weblog-id t)
    (message-remove-header "Weblog") 
    (message-add-header (concat "Weblog: " 
                                (weblogger-weblog-name-from-id 
                                 (weblogger-weblog-id))))
    (goto-char point-save)))

(defun weblogger-change-texttype ()
  "Change Text Type."
  (interactive)
  (weblogger-check-configuration)
  (let ((point-save (point)))
    (message-remove-header "X-TextType")
    (message-add-header (concat "X-TextType: " 
                                (weblogger-texttype-name-from-id 
                                 (weblogger-select-texttype))))
    (goto-char point-save)))

(defun weblogger-entry-mode ()
  "Major mode for editing text for Weblogger.  Based on message-mode."
  (interactive)
  (weblogger-check-configuration)
  (message-mode)
  (message-disassociate-draft)
  (use-local-map weblogger-entry-mode-map)
  (setq mode-name "weblogger-entry")
  (setq major-mode 'weblogger-entry-mode)
  (unless weblogger-entry-ring
    (setq weblogger-entry-ring (make-ring weblogger-max-entries-in-ring)))
  (run-hooks 'weblogger-entry-mode-hook))

(defun weblogger-template-mode ()
  "Major mode for editing templates for Weblogger. Based on text-mode."
  (interactive)
  (weblogger-check-configuration)
  (text-mode)
  (use-local-map weblogger-template-mode-map)
  (setq mode-name "weblogger-template")
  (setq major-mode 'weblogger-template-mode))

(defun weblogger-edit-template (type)
  "Edit a Template. TYPE indicates which one."
  (setq *weblogger-template* (switch-to-buffer "*weblogger-template*"))
  (weblogger-template-mode)
  (erase-buffer)
  (insert (xml-rpc-method-call
           (weblogger-server-url)
           'blogger.getTemplate 
           weblogger-blogger-app-key
           (weblogger-weblog-id)
           (weblogger-server-username)
           (weblogger-server-password)
           type))
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (setq weblogger-template-type type))

(defun weblogger-save-template ()
  "Save a Template. TYPE indicates which one."
  (interactive)
  (weblogger-check-configuration)
  (if (buffer-modified-p)
      (progn (xml-rpc-method-call
              (weblogger-server-url)
              'blogger.setTemplate 
              weblogger-blogger-app-key
              (weblogger-weblog-id)
              (weblogger-server-username)
              (weblogger-server-password)
              (buffer-substring-no-properties (point-min) (point-max))
              weblogger-template-type)
             (set-buffer-modified-p nil))))

(defun weblogger-edit-main-template ()
  "Edit the main template"
  (interactive)
  (weblogger-check-configuration)
  (weblogger-edit-template "main"))

(defun weblogger-edit-archive-template ()
  "Edit the template for archive listings"
  (interactive)
  (weblogger-check-configuration)
  (weblogger-edit-template "archive"))

(defun weblogger-start-entry (&optional prompt)
  "Start creating a weblog entry in the *weblogger-entry* buffer.
With a prefix, it will check the available weblogs on the server
and prompt for the weblog to post to if multiple ones are
available."
  (interactive "P")
  (weblogger-check-configuration)
  (if prompt (weblogger-weblog-id prompt))
  (unless weblogger-entry-ring
    (setq weblogger-entry-ring (make-ring weblogger-max-entries-in-ring)))
  (ring-insert weblogger-entry-ring '(("content" "")))
  (setq weblogger-ring-index 0)
  (weblogger-edit-entry))

(defun weblogger-entry-setup-headers (entry &optional body-line)
  "Add any pertinant headers to the weblog entry."
  (let ((entry-id (when (cdr (assoc  "entry-id" entry))
                    (if (stringp (cdr (assoc  "entry-id" entry)))
                        (cdr (assoc  "entry-id" entry))
                        (int-to-string (cdr (assoc  "entry-id" entry))))))
        (content  (or (cdr (assoc "content"     entry)) ""))
        (title    (cdr (assoc "title"       entry))))

    (mapcar 'message-add-header
            (delq nil
                  (mapcar
                   (lambda (bit)
                     (when (car (cdr-safe bit))
                       (concat (car bit) ": "
                               (cadr bit))))
                   (list
                    (list "Message-ID"
                          (when entry-id
                            (format "<%s/%s@%s>"
                                    entry-id
                                    (weblogger-weblog-id)
                                    (url-host (url-generic-parse-url (weblogger-server-url))))))
                    (list "Date"
                          (cdr (assoc "dateCreated" entry)))
                    (list "In-Reply-To"
                          (let ((hold nil))
                            (mapcar
                             (lambda (p)
                               (setq hold (concat hold p ", ")))
                             (cdr (assoc "trackbacks"  entry)))
                            (when hold hold)))
                    (list "X-URL"
                          (cdr (assoc "url" entry)))
                    (list "X-TextType"
                          (cdr (assoc "texttype" entry)))
                    (list "Subject" title)
                    (list "Keywords"
                          (let ((cats (cdr (assoc "categories"  entry))))
                            (when (> (length cats) 0)
                              (mapconcat
                               (lambda (p) p)
                               cats ", "))))
                    (list "Summary" 	;tags support
                          (let ((cats (cdr (assoc "mt_keywords"  entry))))
                            (when (> (length cats) 0)
                              (mapconcat
                               (lambda (p) p)
                               cats ", "))))
                    (list "From"
                          (or (cdr (assoc "authorName"  entry))
                              (weblogger-server-username)))
                    (list "Weblog"
                          (concat (weblogger-weblog-name-from-id 
                                   (weblogger-weblog-id))))))))
    (goto-char (point-max))
    (when body-line
      (insert mail-header-separator "\n"))))

(defun weblogger-send-entry (&optional arg)
  "Send but not publish the current entry.  With optional argument, prompts
for the weblog to use."
  (interactive)
  (weblogger-check-configuration)
  (weblogger-save-entry nil arg)
  ;(bury-buffer)
  )

(defun weblogger-publish-entry (&optional arg)
  "Publish the current entry.  With optional argument, prompts
for the weblog to use."
  (interactive)
  (weblogger-check-configuration)
  (set-buffer-modified-p t)
  (weblogger-save-entry t arg)
  ;(bury-buffer)
  )

(defun weblogger-save-entry (&optional publishp &optional arg)
  "Publish the current entry is publishp is set.  With optional
argument, prompts for the weblog to use."
  (interactive)
  (weblogger-check-configuration)
  (if (not (equal (current-buffer) *weblogger-entry*))
      (message 
       "You are not in the *weblogger-entry* buffer.")
      (let ((entry (weblogger-entry-buffer-to-struct)))
        (cond ((and (buffer-modified-p)
                    (not (string-equal (cdr (assoc "content" entry)) "")))
               (weblogger-server-username arg)
               (weblogger-server-password arg)
               (weblogger-weblog-id arg)
               (cond ((cdr (assoc "entry-id" entry))
                      (weblogger-update-ring entry)
                      (weblogger-api-send-edits entry publishp)
                      (set-buffer-modified-p nil))
                     (t
                      (weblogger-entry-setup-headers 
                       (weblogger-api-new-entry entry publishp)))))
              (t (message "Nothing to post."))))))

(defun weblogger-update-ring (entry)
  "Update the entry ring with the contents of ENTRY"
  (let ((ring-entry (ring-ref
                     weblogger-entry-ring
                     weblogger-ring-index)))
    (mapcar (lambda (el)
              (let ((field (assoc (car el) ring-entry)))
                (when field
                  (setcdr field (cdr el)))))
            entry)))

(defun weblogger-server-username (&optional prompt)
  "Get the username.  If you've not yet logged in then prompt for
it."
  (when prompt
    (weblogger-cache-invalidate 'username))
  (or (weblogger-config-param weblogger-config-name "username")
      (weblogger-cache 'username (read-from-minibuffer "Username: "))))

(defun weblogger-server-password (&optional prompt)
  "Get the password.  If you've not yet logged in then prompt for
it."
  (when prompt
    (weblogger-cache-invalidate 'password))
  (or (weblogger-config-param weblogger-config-name "password")
      (weblogger-cache 'password (read-passwd (format "Password for %s: " (weblogger-server-url))))))

(defun weblogger-weblog-id (&optional prompt)
  "Get the weblog ID."
  (when prompt
    (weblogger-cache-invalidate 'weblog))
  (or (weblogger-config-param weblogger-config-name "weblog")
      (weblogger-cache 'weblog 
                       (weblogger-select-weblog prompt))))

(defun weblogger-api-new-entry (struct publishp)
  "Publish a new entry (STRUCT) using the best method available."
  (run-hooks 'weblogger-new-entry-hook)
  (unless weblogger-api-new-entry
    (weblogger-determine-capabilities))
  (ring-insert
   weblogger-entry-ring
   (add-to-list
    'struct 
    (cons "entry-id" (eval `(,weblogger-api-new-entry struct publishp)))))
  (setq weblogger-ring-index 0)
  (ring-ref weblogger-entry-ring weblogger-ring-index))

(defun weblogger-api-send-edits (struct publishp)
  "Update an entry (in STRUCT) using the best method available."
  (run-hooks 'weblogger-edit-entry-hook)
  (unless weblogger-api-send-edits
    (weblogger-determine-capabilities))
  (eval `(,weblogger-api-send-edits struct publishp)))

(defun weblogger-api-list-entries (count)
  "Get a list of entries."
  (unless weblogger-api-list-entries
    (weblogger-determine-capabilities))
  (when weblogger-api-list-entries
    (eval `(,weblogger-api-list-entries count))))
;; This is an interesting means of polymorphism. I was wrong about
;; what it was doing initially.  weblogger-api-list-entries is a
;; variable that holds a function name, so there is no redefining of
;; weblogger-api-list-entries happening here, which is what I
;; initially thought.

(defun weblogger-api-list-categories ()
  "Get a list of categories."
  (unless weblogger-api-list-categories
    (weblogger-determine-capabilities))
  (when weblogger-api-list-categories
    (eval `(,weblogger-api-list-categories))))

(defun weblogger-select-weblog (&optional fetch)
  "Allows the user to select a weblog and returns the weblog ID.
If there is only one weblog owned by the user on the server, then
that weblog is returned.  With FETCH defined, the server is
re-queried for a list of weblogs the user owns"
  (interactive)
  (weblogger-check-configuration)
  (weblogger-weblog-id-from-weblog-name
   (let* ((completion-ignore-case t)
          (seq 0)
          (webloglist (mapcar
                       (lambda (weblog)
                         (cons weblog (setq seq (1+ seq))))
                       (weblogger-list-weblog-names fetch))))
     (if (= 1 (length webloglist))
         (caar webloglist)
         (completing-read 
          "Weblog: " webloglist nil t)))))

(defun weblogger-weblog-id-from-weblog-name (name)
  "Returns the weblog id given the name."
  (cdr (assoc name
              (mapcar 
               (lambda (weblog)
                 (cons (cdr (assoc "blogName" weblog))
                       (cdr (assoc "blogid" weblog))))
               (weblogger-api-weblog-alist)))))

(defun weblogger-weblog-name-from-id (id)
  "Returns the weblog name given the id."
  (cdr (assoc id
              (mapcar 
               (lambda (weblog)
                 (cons (cdr (assoc "blogid" weblog))
                       (cdr (assoc "blogName" weblog))))
               (weblogger-api-weblog-alist)))))

(defun weblogger-texttype-name-from-id (id)
  "Returns the texttype name given the id."
  (cdr (assoc id
              (mapcar 
               (lambda (texttype)
                 (cons (cdr (assoc "key" texttype))
                       (cdr (assoc "label" texttype))))
               (weblogger-texttype-alist)))))

(defun weblogger-texttype-id-from-name (name)
  "Returns the texttype id given the name."
  (cdr (assoc name
              (mapcar 
               (lambda (texttype)
                 (cons (cdr (assoc "label" texttype))
                       (cdr (assoc "key" texttype))))
               (weblogger-texttype-alist)))))

(defun weblogger-list-texttype-names (&optional fetch)
  "Returns a list of texttype names."
  (mapcar 
   (lambda (texttype)
     (cdr (assoc "label" texttype)))
   (weblogger-texttype-alist fetch)))

(defun weblogger-texttype-alist (&optional fetch)
  "Returns the alist of texttypes allowed by the server."
  (when (cdr (assoc "mt.supportedTextFilters" weblogger-capabilities))
    (when fetch
      (weblogger-cache-invalidate 'texttype-alist))
    (weblogger-cache 'texttype-alist
                     (xml-rpc-method-call 
                      (weblogger-server-url)
                      'mt.supportedTextFilters))))

(defun weblogger-select-texttype (&optional fetch)
  "Allows the user to select a texttype for entries."
  (weblogger-texttype-id-from-name
   (let* ((completion-ignore-case t)
          (seq 0)
          (ttlist (mapcar
                   (lambda (texttype)
                     (cons texttype (setq seq (1+ seq))))
                   (weblogger-list-texttype-names fetch))))
     (if (= 1 (length ttlist))
         (caar ttlist)
         (completing-read 
          "TextType: " ttlist nil t)))))

(defun weblogger-config-param (config name)
  (let* ((alist (cdr (assoc config weblogger-config-alist)))
         (value (cdr (assoc name alist))))
    value))

(defun weblogger-server-url ()
  "Returns the server url."
  (weblogger-config-server 'url))

(defun weblogger-config-server-type ()
  "Returns the server url."
  (nth 0 (weblogger-config-server 'rpc)))
  ;(weblogger-config-param weblogger-config-name "server-url"))

(defun weblogger-server-url-from-id (id)
  "Returns the weblog URL given the id."
  (cdr (assoc id
              (mapcar
               (lambda (weblog)
                 (cons (cdr (assoc "blogid" weblog))
                       (cdr (assoc "url" weblog))))
               (weblogger-api-weblog-alist)))))

(defun weblogger-list-weblog-names (&optional fetch)
  "Returns a list of weblog names."
  (mapcar 
   (lambda (blog)
     (cdr (assoc "blogName" blog)))
   (weblogger-api-weblog-alist fetch)))

(defun weblogger-api-weblog-alist (&optional fetch)
  "Returns the alist of weblogs owned by a user on the server."
  (when fetch
    (weblogger-cache-invalidate 'weblog-alist))
  (when weblogger-api-weblog-alist
    (weblogger-cache 'weblog-alist (funcall weblogger-api-weblog-alist))))

(defun weblogger-ping-weblogs (&optional id)
  "Ping the weblog aggregators listed in weblogger-ping-urls."
  (mapcar
   (lambda (url)
     (xml-rpc-method-call-async
      'weblogger-handle-weblog-ping-response
      url
      'weblogUpdates.ping
      (weblogger-weblog-name-from-id 
       (or id weblogger-weblog-id)				)
      (weblogger-server-url-from-id 
       (or id weblogger-weblog-id))))
   weblogger-ping-urls))

(defun weblogger-handle-weblog-ping-response (&optional resp)
  "Handle the response from a weblog ping.  Print a entry with the result.

For old w3.el, resp is expected.  Otherwise current-buffer is expected to
contain the http result."
  (if resp
      (message (cdr (assoc "message" (cdr resp))))
      (message (cdr 
                (assoc "message" 
                       (cdr 
                        (xml-rpc-xml-to-response
                         (xml-rpc-request-process-buffer (current-buffer)))))))))

(defun weblogger-goto-entry (num &optional relativep)
  "Move to the entry identified by NUM in the ring.  If RELATIVE
is set, then add it to the current index and go to that entry."
  (if (buffer-modified-p)
      (weblogger-save-entry nil nil))
  (unless weblogger-entry-list
    (weblogger-api-list-entries weblogger-max-entries-in-ring))
  (let ((entry-id (if relativep
                      (+ (if weblogger-ring-index weblogger-ring-index 
                             -1)
                         num)
                      num)))
    (setq weblogger-ring-index entry-id))
  (if (ring-empty-p weblogger-entry-ring)
      (weblogger-api-list-entries weblogger-max-entries-in-ring))
  (weblogger-edit-entry
   (ring-ref weblogger-entry-ring weblogger-ring-index)))

(defun weblogger-next-entry ()
  "Edit the contents of the next entry."
  (interactive)
  (weblogger-goto-entry -1 t))

(defun weblogger-prev-entry ()
  "Edit the contents of the previous entry."
  (interactive)
  (weblogger-goto-entry +1 t))

(defun weblogger-delete-entry ()
  "Delete the entry."
  (interactive)
  (unless weblogger-ring-index
    (message "You must have an entry loaded first."))
  (when (y-or-n-p "Do you really want to delete this entry? ")
    ;; I wonder why eval is used in the other ones and not funcall?
    (funcall weblogger-api-delete-entry (ring-ref weblogger-entry-ring 
                                                  weblogger-ring-index))
    (ring-remove weblogger-entry-ring weblogger-ring-index)
    (weblogger-edit-entry
     (ring-ref weblogger-entry-ring weblogger-ring-index))))

(defun weblogger-edit-entry (&optional entry)
  "Edit a entry.  If ENTRY is specified, then use that entry.
Otherwise, open a new entry."
  (setq *weblogger-entry* (switch-to-buffer "*weblogger-entry*"))
  (setq buffer-read-only nil)
  (weblogger-entry-mode)
  (erase-buffer)
  (weblogger-entry-setup-headers entry t)
  (if (and entry (cdr (assoc "content" entry)))
      (insert (cdr (assoc "content" entry))))
  (run-hooks 'weblogger-start-edit-entry-hook)
  (set-buffer-modified-p nil)
  (message-goto-keywords) ;; Create Keywords field in new entries
  (if (message-fetch-field "Subject")
      (message-goto-body) ;; If Subject exists, move cursor to message body
      (message-goto-subject)) ;; Else, drop cursor on Subject header
  (message-fetch-field "Keywords")
  (pop-to-buffer *weblogger-entry*)
  ;; We just set up this buffer, so it hasn't been modified.
  (set-buffer-modified-p nil)
  )

(defun weblogger-response-to-struct (response)
  "Convert the result of the xml-rpc call to a structure we
like."
  (let ((postid      (cdr (assoc-ignore-case "postid" response)))
        (authorName  (cdr (assoc-ignore-case "authorname" response)))
        (authorID    (cdr (assoc-ignore-case "authorid" response)))
        (userid      (cdr (assoc-ignore-case "userid" response)))
        (title       (cdr (assoc-ignore-case "title" response)))
        (dateCreated (cdr (assoc-ignore-case "datecreated" response)))
        (content          (assoc-ignore-case "content" response))
        (trackbacks  (cdr (assoc-ignore-case "mt_tb_ping_urls" response)))
        (textType    (cdr (assoc-ignore-case "mt_convert_breaks" response)))
        (url         (cdr (assoc-ignore-case "link" response)))
        (description      (assoc-ignore-case "description" response))
        (extended         (assoc-ignore-case "mt_text_more" response))
        (mt_keywords (cdr (assoc-ignore-case "mt_keywords" response)))
        (categories  (cdr (assoc-ignore-case "categories" response))))
    
    (cond (content
           (delq nil (list
                      (when postid
                        (cons "entry-id"     postid))
                      (if title
                          (cons "title"        title)
                          ;; See if we can extract the title from the first line of the
                          ;; message body if it wasn't in a header.
                          (when (and (weblogger-blogger-firstline-title)
                                     (string-match "^<title>\\(.*\\)</title>.*\n" (cdr content)))
                            (setq title (match-string 1 (cdr content)))
                            (setcdr content
                                    (with-temp-buffer
                                      (insert (cdr content))
                                      (goto-char (point-min))
                                      (replace-string (match-string 0 (cdr content)) "")
                                      (buffer-string)))
                            (cons "title" title)))
                      (when authorName
                        (cons "authorName"   authorName))
                      (when userid
                        (cons "userid"       userid))
                      (when dateCreated
                        (cons "dateCreated"  dateCreated))
                      (when content
                        (cons "content"      (cdr content))))))
          (description
           (delq nil (list
                      (when authorName
                        (cons "authorName"   authorName))
                      (when postid
                        (cons "entry-id"     postid))
                      (when trackbacks
                        (cons "trackbacks"   trackbacks))
                      (when description
                        (cond ((cdr extended)
                               (cons "content"   (concat (cdr description)
                                                         "<!--more-->"
                                                         (cdr extended))))
                              (t 
                               (cons "content" (cdr description)))))
                      (when title
                        (cons "title"        title))
                      (when url
                        (cons "url"          url))
                      (when dateCreated
                        (cons "dateCreated"  dateCreated))
                      (when categories
                        (cons "categories"   categories))
                      (when mt_keywords
                        (cons "mt_keywords"   mt_keywords))
                      (when textType
                        (cons "texttype"     textType)))))
          (t
           (error "bogosity!")))))

(defun weblogger-struct-to-request (entry)
  "Convert the struct to something that can be used in an xml-rpc request."
  (delq nil
        (list
         (assoc "title"        entry)
         (assoc "authorName"   entry)
         (assoc "userid"       entry)
         (assoc "dateCreated"  entry)
         (cons "mt_tb_ping_urls"   (cdr (assoc "trackbacks"  entry)))
         (cons "mt_convert_breaks" (weblogger-texttype-id-from-name
                                   (cdr (assoc "texttype"    entry))))
         (cons "link"              (cdr (assoc "url"         entry)))
         (cons "description"       (cdr (assoc "content"     entry)))
         (cons "categories"        (cdr (assoc "categories"  entry)))
         (cons "mt_keywords"       (cdr (assoc "mt_keywords" entry))))))

(defun weblogger-server-userid ()
  "Get information on user."
  (when weblogger-api-blogger-server-userid
    (funcall weblogger-api-blogger-server-userid)))

(defun weblogger-fetch-entries ()
  "Sync the entry ring with what is on the weblog server."
  (interactive)
  (weblogger-check-configuration)
  (setq weblogger-entry-ring (make-ring weblogger-max-entries-in-ring))
  (setq weblogger-category-ring (make-ring 20))
  (weblogger-api-list-categories)
  (weblogger-api-list-entries weblogger-max-entries-in-ring)
  (setq weblogger-ring-index 0)
  (weblogger-edit-entry
   (ring-ref weblogger-entry-ring weblogger-ring-index)))

(defun weblogger-check-configuration ()
  (interactive)
  (if (equal weblogger-config-alist weblogger-config-alist-default)
      ;; We should do something here.
      ;(if (yes-or-no-p "Weblogger has not been configured.  Do you want to configure it? ")
      ;(call-interactively (lambda () (customize-group 'weblogger)))
      (error "Not configured yet.  Use M-x customize-group RET weblogger RET")
          ;)
      ))

(defun weblogger-determine-capabilities ()
  "Determine the capabilities of the remote weblog server."
  (weblogger-check-configuration)
  (if (eq (weblogger-config-server-type) 'gdata)
      (weblogger-install-gdata)
      ;; otherwise do everything else.
      (weblogger-install-blogger)
  (setq weblogger-capabilities weblogger-no-capabilities)
  (let ((has-meta-api t)
        (has-mt-api t)
        (has-blogger-api t))
    (condition-case nil
        (progn (mapcar
                (lambda (method)
                  (and (assoc method weblogger-capabilities) 
                       (setcdr (assoc method weblogger-capabilities) t)))
                (xml-rpc-method-call
                 (weblogger-server-url)
                 'mt.supportedMethods)))
      (error (setq has-mt-api nil))))
  (cond ((cdr (assoc "metaWeblog.editPost" weblogger-capabilities))
         (setq weblogger-api-send-edits 'weblogger-api-meta-send-edits))
        (t
         (setq weblogger-api-send-edits 'weblogger-api-blogger-send-edits)))
  (cond ((cdr (assoc "metaWeblog.newPost" weblogger-capabilities))
         (setq weblogger-api-new-entry 'weblogger-api-meta-new-entry))
        (t
         (setq weblogger-api-new-entry 'weblogger-api-blogger-new-entry)))
  (cond ((cdr (assoc "metaWeblog.getCategories" weblogger-capabilities))
         (setq weblogger-api-list-categories
               'weblogger-api-meta-list-categories))
        (t
         (setq weblogger-api-list-categories
               'weblogger-api-blogger-list-categories)))
  (cond ((cdr (assoc "metaWeblog.getRecentPosts" weblogger-capabilities))
         (setq weblogger-api-list-entries 'weblogger-api-meta-list-entries))
        (t
         (setq weblogger-api-list-entries
               'weblogger-api-blogger-list-entries)))))

(defun weblogger-entry-buffer-to-struct (&optional encode buffer)
  "Convert an entry BUFFER to a struct (which is then used
internally).  If BUFFER is not given, use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (delq nil 
          (list
           (cons "authorName"   (message-fetch-field "From"))
           (cons "dateCreated"  (message-fetch-field "Date"))
           (cons "texttype"      (message-fetch-field "X-TextType"))
           (cons "url"           (message-fetch-field "X-Url"))
           (cons "title"     (or (message-fetch-field "Subject") 
                                 weblogger-default-title))
           (cons "categories"  (vconcat (or (message-tokenize-header 
                                             (message-fetch-field "Keywords") ", ")
                                            weblogger-default-categories)))
           (cons "mt_keywords"  (vconcat (or (message-tokenize-header 
                                              (message-fetch-field "Summary") ", ")
                                             weblogger-default-tags)))

           (when (message-fetch-field "In-Reply-To")
             (cons "trackbacks" (or (message-tokenize-header 
                                     (message-fetch-field "Keywords") ", ")
                                    weblogger-default-categories)))
           (when (and weblogger-ring-index
                      (> (ring-length weblogger-entry-ring) 0))
             (cons "entry-id"
                   (let ((msgid (message-fetch-field "Message-ID")))
                     (if (and msgid (string-match "<\\([0-9]+\\)/" msgid))
                         (match-string 1 msgid)
                         (cdr (assoc "entry-id"
                                     (ring-ref
                                      weblogger-entry-ring
                                      weblogger-ring-index)))))))
           (cons "content"
                 (progn
                   (message-goto-body)
                   (if encode
                       (url-insert-entities-in-string
                        (buffer-substring-no-properties (point) (point-max)))
                       (buffer-substring-no-properties (point) (point-max)))))))))

;; TODO -- Support for toolbar
;; (eval-when-compile (defvar tool-bar-map))
;; (if (featurep 'xemacs)
;;     nil					; no XEmacs support just yet.
;;   (when (and (fboundp 'tool-bar-add-item-from-menu)
;;  	     tool-bar-mode)
;;     (defvar weblogger-tool-bar-map
;;       (let ((tool-bar-map (copy-keymap tool-bar-map)))
;;  	;; Zap some items which aren't so relevant and take up space.
;;  	(dolist (key '(print-buffer kill-buffer save-buffer write-file
;;  				    dired open-file))
;;  	  (define-key tool-bar-map (vector key) nil))
 
;;  	(tool-bar-add-item-from-menu
;;  	 'message-send-and-exit "mail_send" message-mode-map)
;;  	(tool-bar-add-item-from-menu
;;  	 'message-kill-buffer "close" message-mode-map)
;;  	(tool-bar-add-item-from-menu
;;  	 'message-dont-send "cancel" message-mode-map)
;;  	(tool-bar-add-item-from-menu
;;  	 'mml-attach-file "attach" message-mode-map)
;;  	(tool-bar-add-item-from-menu
;;  	 'ispell-message "spell" message-mode-map)
;;  	tool-bar-map))))

(provide 'weblogger)
