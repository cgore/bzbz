;;;; Copyright (c) 2005 -- 2011, Christopher Mark Gore,
;;;; All rights reserved.
;;;; 
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;; 
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;; 
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;; 
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;; 
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(load "utilities")
(defpackage :html
  (:use :common-lisp #+sbcl :sb-ext :utilities)
  (:export :!-- :abbr :acronym :b :base :basic-webpage :body :book-title :br
    :canonical-site-address ; NOTE: You need to redefine this one locally.
    :cite :command-line :cr :dd :dfn :div :dl :dl-from-pairs :dl-from-rest :dt
    :em :h1 :h2 :h3 :h4 :head :hidden-input :hier-h1 :hr :href :html
    :html-comment :html-comment :html-content-header :html-quote :html-var :i
    :img :javascript :kbd :label :li :link :link-css :load-javascript :mailto
    :multiline-tag?  :newline :newline-tag?  :ol :ol-from-list :ol-from-rest :p
    :password-input :p-left :pre :radio-input :radio-inputs :samp
    :simple-tag-functor
    :site-css ; NOTE: You need to redefine this one locally.
    :site-name ; NOTE: You need to redefine this one locally.
    :source :standard-head :strong :sub :submit-input :sup :table :tag-equal?
    :tag-must-close?  :td :text-input :th :title?  :to-string :tr :tt :ul
    :ul-from-list :ul-from-rest :ul-href :url-decode :webpage-without-login))
(in-package :html)

(defun newline ()
  (format t "~%"))

(defun to-string (s)
  "Converts common types of things into a string."
  (cond ((null s) "")
        ((symbolp s) (string-downcase (symbol-name s)))
        ((stringp s) s)
        (t (format nil "~A" s))))

(defun tag-equal? (a b)
  "This is a comparator for two tags."
  (string-equal (to-string a) (to-string b)))

(defun multiline-tag? (tag)
  "This tests to see if this tag should be formatted over
  multiple lines in the actual HTML output."
  (member tag '(html head script body form p div ul ol dl table)
          :test #'tag-equal?))

(defun newline-tag? (tag)
  "This tests to see if this tag should be on a line by itself in the actual
  HTML output."
  (or (multiline-tag? tag)
      (member tag '(title h1 h2 h3 h4 h5 h6 br hr li dd dt input tr td)
              :test #'tag-equal?)))

(defun tag-must-close? (tag)
  "This predicate indicates if the tag passed in must have a closing tag
  instead of just self-closing.  That is, you MUST do <tag></tag> even if
  there isn't any body contained in the tag, and you can't just do <tag/>.
  As far as I know only the 'link' and 'script' tags are this stupid."
  (member tag '(link script) :test #'tag-equal?))

(defun html (tag &rest rest)
  ;; TODO?: (html :tag . fred # bob rest)
  ;; should do . --> class
  ;;           # --> id
  ;; Both should be optional, and order shouldn't matter, but both before 
  ;; any of the rest is done.  This is loosly based on HAML in Ruby.  Should
  ;; we do this?
  (let ((args nil)
        (args-count 0)
        (body nil)
        (result ""))
    ;; Any non-NIL lists at the front of the arguments are tag arguments.
    (loop for i in rest
          while (and (listp i)
                     (not (null i)))
          do (incf args-count)
          (setf args (append args i)))
    ;; All of the rest of the arguments is the body of the tag.
    (setf body (nthcdr args-count rest))
    ;; Turn NILs into empty strings.
    (setf body (mapcar (lambda (i)
                         (if i i ""))
                       body))
    (labels ((html-format (&rest rest)
                          "This is our local format."
                          (setf result (strcat result
                                               (apply #'format nil rest)))))
      (setf tag (to-string tag))
      ;;; The args should be a list of arguments.  This is for
      ;;; things like the href used with the a tag, so that
      ;;; <a href="foo.html">Get Foo!</a>
      ;;; would be generated from
      ;;; (html 'a '((href "foo.html")) "Get Foo!").
      ;; N.B., NIL is a list too, so we don't need a special
      ;; check for it.
      (assert (listp args))
      (mapcar (lambda (arg)
                (if (listp arg)
                  (progn (setf (first arg)
                               (to-string (first arg)))
                         (if (second arg)
                           (setf (second arg)
                                 (to-string (second arg)))))
                  (setf arg (to-string arg))))
              args)
      (when (or (multiline-tag? tag)
                (newline-tag? tag))
        (html-format "~%"))
      (html-format "<~A" tag)
      (when args
        (mapcar (lambda (arg)
                  (if (listp arg)
                    (html-format " ~A=\"~A\""
                                 (first arg)
                                 (second arg))
                    ;; XXX: I don't know that this second form
                    ;; is valid XHTML.
                    (html-format " ~A" arg)))
                args))
      (when (and (not body)
                 (not (tag-must-close? tag)))
        (html-format "/"))
      (html-format ">")
      (when (multiline-tag? tag)
        (html-format "~%"))
      (when body
        (mapcar (lambda (line)
                  (html-format (escape-tildes line)))
                body))
      (when (or body (tag-must-close? tag))
        (when (multiline-tag? tag)
          (html-format "~%"))
        (html-format "</~A>" tag))
      (when (or (multiline-tag? tag)
                (newline-tag? tag))
        (html-format "~%"))
      result)))

(defun html-comment (&rest rest)
  "This generates and HTML style inline comment.  Usually you don't want to
  even really use this, you should just be using normal Lisp comments in your
  code, but hey, do what you want."
  (apply #'strcat (append '("<!-- ") (mapcar #'to-string rest) '(" -->"))))

(function-alias 'html-comment '!--)

(defun html-content-header nil
  (format t "Content-type: text/html~%"))

(defun html-xml-encoding-header nil
  (format t "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%"))

(defun html-xml-doctype-header nil
  (format t "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1.dtd\">~%"))

(defmacro simple-tag-functor (function-name &optional tag)
  "This macro is used to construct the vast majority of the HTML tag helpers.

  If you want to have:
    (foo \"some stuff\")
  give you:
    \"<foo>some stuff</foo>\"
  then all you need to do is:
    (simple-tag-functor :foo)
  
  Optionally, if you need to have the function named something different, use
  the form:
    (simple-tag-functor :bar :foo)
  and then
    (bar \"some stuff\")
  will generate:
    \"<foo>some stuff</foo>\""
  ;;; TODO: There has to be a simpler way to write this macro.
  `(let* ((name (symbol-name ,function-name)) ; The string representation.
          (tag (string-downcase (if ,tag ,tag name)))) ; The tag as a string.
     (intern name) ; Intern it as a symbol.
     (let ((function (find-symbol name))) ; Get the newly-interned symbol.
       (eval `(defun ,function (&rest rest) ; Define the function.
                (apply #'html ,tag rest))))))

;; All of these functions correspond to their appropriate HTML tags.
(mapcar (lambda (tag)
          (simple-tag-functor tag))
        '(:b :base :body :br :cite :dd :dl :dfn :dt :em :h1 :h2 :h3 :h4 :hr
             :head :i :kbd :li :link :ol :p :pre :samp :strong :sub :sup :table
             :td :th :tr :tt :ul))

;; The HTML <quote> element.
;; QUOTE is already a (very important) function in Lisp, so we will not be able
;; to redefine it here: instead we will define a function named HTML-QUOTE to
;; avoid a naming confict.
(simple-tag-functor :html-quote :quote)

;; The HTML <var> element.
;; This styles a computer program variable.
;; VAR is used by SBCL's debugging system, so we won't redefine it here:
;; instead we'll define a function named HTML-VAR to avoid a naming confict.
(simple-tag-functor :html-var :var)

(defun abbr (lang title &rest rest)
  (apply #'html :abbr `(("lang" ,lang) ("title" ,title)) rest))

(defun acronym (lang title &rest rest)
  (apply #'html :acronym `(("lang" ,lang) ("title" ,title)) rest))

;; The HTML <blink> element.
;; BLINK was never a standard tag, and we will therefore not include it.  The
;; correct way to cause the text to blink is now to use CSS, specifically
;; {text-decoration: blink}, or even better not at all.

(defun div (id &rest rest)
  (apply #'html :div `(("id" ,id)) rest))

(defun hidden-input (name &optional (value ""))
  (html :input `(("type" "hidden") ("name" ,name) ("value" ,value))))

(defun password-input (name)
  (html :input `((type password) (name ,name))))

(defun radio-input (name value &key text checked)
  (unless text (setf text value))
  (if checked
    (html :input `((type radio) (name ,name) (value ,value) "checked") text)
    (html :input `((type radio) (name ,name) (value ,value)) text)))

(defun radio-inputs (name values &optional checked-value)
  (apply #'strcat
         (mapcar (lambda (v)
                   (let* ((value (if (listp v)
                                   (first v)
                                   v))
                          (text (if (listp v)
                                  (second v)
                                  v))
                          (checked? (and checked-value
                                         (string-equal value checked-value))))
                     (radio-input name value :text text :checked checked?)))
                 values)))

(defun submit-input (value &key (name "submit-input") (onclick nil))
  (html :input (if onclick 
                 `((type submit) (name ,name) (value ,value) (onclick ,onclick))
                 `((type submit) (name ,name) (value ,value)))))

(defun text-input (name &optional (value "") (size 16))
  (html :input `((type text) (name ,name) (value ,value) (size ,size))))

(defun label (for &rest rest)
  (apply #'html :label `(("for" ,for)) rest))

(defun p-left (&rest rest)
  (apply #'p '((:style "text-align: left")) rest))

(defun cr ()
  "
")

;;; TODO: Move this out of here, it isn't basic HTML stuff, it is a shortcut.
(defun command-line (&rest rest)
  (apply #'pre '((:class "cli")) rest))

;;; TODO: Move this out of here, it isn't basic HTML stuff, it is a shortcut.
(defun source (&rest rest)
  (apply #'pre '((:class "source")) rest))

(defun ol-from-list (list)
  (assert (listp list))
  (apply #'ol (mapcar #'li list)))

(defun ol-from-rest (&rest rest)
  (ol-from-list rest))

(defun ul-from-list (list)
  (assert (listp list))
  (apply #'ul (mapcar #'li list)))

(defun ul-from-rest (&rest rest)
  (ul-from-list rest))

(defun dl-from-pairs (pairs)
  (apply #'dl (mapcar (lambda (pair)
                        (strcat (dt (first pair))
                                (dd (second pair))))
                      pairs)))

(defun dl-from-rest (&rest rest)
  (labels ((recursor (&rest rest)
                     (cond ((<= 2 (length rest))
                            (strcat (dt (first rest))
                                    (dd (second rest))
                                    (apply #'recursor (cddr rest))))
                           ((= 1 (length rest))
                            (dt (first rest)))
                           (t ""))))
    (dl (apply #'recursor rest))))

(defun href (link &rest rest)
  (if rest
    (apply #'html :a `(("href" ,link)) rest)
    (apply #'html :a `(("href" ,link)) (list link))))

;; TODO: factor out some sort of pairing function.  This is used in
;; dl-from-pairs too, and there isn't any reason for the code duplication.
(defun ul-href (&rest rest)
  (labels ((recursor (&rest rest)
                     (cond ((<= 2 (length rest))
                            (strcat (li (href (first rest)
                                              (second rest)))
                                    (apply #'recursor (cddr rest))))
                           ((= 1 (length rest))
                            (li (href (first rest))))
                           (t ""))))
    (ul (apply #'recursor rest))))

(defun mailto (email &rest rest)
  (if rest
    (apply #'href (strcat "mailto:" email) rest)
    (apply #'href (strcat "mailto:" email) (list email))))

(defun img (src &optional (title "foo"))
    (html :img `(("src" ,src)
                 ("alt" ,title)
                 ("title" ,title))))

(defun javascript (&rest rest)
  (apply #'html :script '(("type" "text/javascript")) rest))

(defun load-javascript (js)
  (html :script `(("type" "text/javascript")
                  ("src" ,js))))

(defun link-css (css)
  (link `(("rel" "stylesheet")
          ("href" ,css)
          ("type" "text/css"))))

(defun url-decode-recursor (remaining)
  (cond ((null remaining) nil)
        ((eq (first remaining) #\+)
         (cons #\Space (url-decode-recursor (rest remaining))))
        ((and (eq (first remaining) #\%)
              (>= (length remaining) 3))
         (cons (code-char (read-from-string
                            (format nil "#x~A~A"
                                    (second remaining) (third remaining))))
               (url-decode-recursor (nthcdr 3 remaining))))
        (t (cons (first remaining) (url-decode-recursor (rest remaining))))))

;;; TODO: Move this out of here, it isn't basic HTML, it is BzBz-specific.
(defun url-decode (encoded-string)
  (coerce (url-decode-recursor (coerce encoded-string 'list)) 'string))

;;; TODO: Move this out of here, it isn't basic HTML, it is BzBz-specific.
(defun site-name () "Default Site Name")

;;; TODO: Move this out of here, it isn't basic HTML, it is BzBz-specific.
(defun canonical-site-address () "http://www.example.com")

;;; TODO: Move this out of here, it isn't basic HTML, it is BzBz-specific.
(defun site-css () (link-css "/default.css"))

(defun title? (title)
  (or (stringp title)
      (and (listp title)
           (every #'stringp title))))

;;; TODO: Move this out of here, it isn't basic HTML, it is BzBz-specific.
(defun hier-h1 (index? &rest title)
  "This generates a linked hierarchal H1 header."
  (assert (title? title))
  (cond ((zerop (length title))
         (h1 (site-name)))
        ((stringp title)
         (hier-h1 (list title)))
        (t (apply #'h1
                  (let ((i (if index? 
                             (length title)
                             (1- (length title))))
                        (first? t))
                    (mapcar (lambda (title-segment)
                              (prog1 (strcat
                                       (if first?
                                         (setf first? nil)
                                         ": ")
                                       (cond ((or (< i 0)
                                                  (and (< i 1) index?))
                                              title-segment)
                                             ((and (= i 0) (not index?))
                                              (href "./" title-segment))
                                             (t (href (strmult i "../")
                                                      title-segment))))
                                (decf i)))
                            (cons (site-name) title)))))))

;;; TODO: Move this out of here, it isn't basic HTML, it is BzBz-specific.
(defun basic-webpage (&rest rest)
  (html-content-header)
  (newline)
  (html-xml-encoding-header)
  (html-xml-doctype-header)
  (format t (apply #'html :html
                   '((xmlns "http://www.w3.org/1999/xhtml")
                     ("xml:lang" en))
                   (mapcar #'escape-tildes rest))))

(defun webpage-without-login (&rest rest)
  (html-content-header)
  (apply #'basic-webpage rest))

;;; TODO: Move this out of here, it isn't basic HTML stuff, it is a shortcut.
(defun standard-head (title &rest rest)
  (assert (title? title))
  (apply #'head
         (html :title (cond ((zerop (length title))
                             (site-name))
                            ((stringp title)
                             (strcat (site-name) ": " title))
                            (t (string-join (cons (site-name) title) ": "))))
         (site-css)
         rest))

;;; TODO: Move this out of here, it isn't basic HTML stuff, it is a shortcut.
(defun book-title (&rest rest)
  (apply #'b rest))
