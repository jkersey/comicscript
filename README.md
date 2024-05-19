comicscript
===========

An Emacs major mode for writing formatted comic book scripts, based on Vance L. Simpson's screenplay.el

Instructions:

Start a new page: TAB-ENTER
Start a new panel: TAB-TAB-ENTER:

Adding dialog:
Working on this, when you type a ':' the text before it is uppercased and the cursor drops down to the next line

Code is in place to renumber the page and panels, will be automatic but right now you have to use comicscript-do-repaginate (binding for C-x o is commented out)

WARNING: Anything on the same line as 'PAGE XXX' is destroyed by the formatter.  'Page' lines are safe and are not destroyed.

