Commentary
==========

 weblogger.el implements the Blogger, MetaWeblog, Movable Type, and
 LiveJournal APIs to talk to server-side weblog software.

Starting Out
------------

 If you don't yet have a weblog, you can set one up for free on
 various services mentioned above.

Download
--------

    $ git clone git://github.com/secelis/weblogger.git


Installation
------------

 I use the following commands in my .emacs file to load up
 weblogger:

    (add-to-list 'load-path "/path/to/weblogger-directory")
    (require 'weblogger)

Setup
-----

 To set up your profile:

    M-x customize-group RET weblogger RET

 Make sure to pay special attention to "Weblog Configurations"
 and "Weblogger Servers".

Keymaps
-------

 I use the following command in my .emacs file:

    (global-set-key "\C-cbs" 'weblogger-start-entry)

 Then `C-c b s` will then switch to a new buffer where you can compose
 a entry.  

    C-x C-s    -- post-and-publish current buffer to the weblog.
                  Calling weblogger-publish-entry with an prefix argument
                  (i.e. C-u C-x C-s) will prompt for which weblog
                  to use.
    
    C-c C-c    -- save as draft and bury the buffer.
    
    C-c C-n    -- post (but not publish) the current entry and
                  load the next entry.
    
    C-c C-p    -- post (but not publish) the current entry and
                  load the previous entry.
    
    C-c C-k    -- delete the current entry.
    
    M-g        -- synchronise weblogger.el's idea of the entries available
                  with the weblog server.
    
    C-c C-t m  -- edit the main template.
    
    C-c C-t a  -- edit the Archive Index template
    
    C-c C-w    -- Change the weblog.
    
    C-c C-u    -- Change the user (re-login).

 See the TODO and BUGS file to see the currently issues and potential
 features.  Please use the
 [wiki](http://wiki.github.com/secelis/weblogger) on github to ask
 questions or report issues.

Screenshot
----------

<pre style='color: #bebebe; background-color: #262626; font-size: 8pt; width: 80em;'>
<span style='color: #add8e6;'>Subject:</span> <span style='color: #e0ffff; font-weight: bold;'>weblogger.el v2.0</span>
<span style='color: #add8e6;'>Keywords:</span> <span style='color: #66cdaa;'>emacs blog weblogger</span>
<span style='color: #add8e6;'>From:</span> <span style='color: #66cdaa;'>shane.celis@gmail.com</span>
<span style='color: #add8e6;'>Weblog:</span> <span style='color: #66cdaa;'>gnufool</span>
<span style='color: #d2691e;'>--text follows this line--</span>
I worked with &lt;a href='http://code.google.com/p/e-blog/'&gt;e-blog&lt;/a&gt; for...
</pre>
