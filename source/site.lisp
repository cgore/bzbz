;;;; Copyright (c) 2005-2010, Christopher Mark Gore,
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

(load "utilities/utilities")
(load "html")
(defpackage :site
  (:use :common-lisp
        #+sbcl :sb-ext
        :utilities
        :html)
  (:export :left-menu
           :*include-advertisements*
           :wide-skyscraper-ad
           :webpage-template
           :webpage))
(in-package :site)

(defun site-name () "New BzBz Installation")

(defun canonical-site-address () "http://www.example.com")

(defun site-css () (link-css "/site.css"))

(defun left-menu ()
  (div :leftbar
       (href "http://www.cgore.com" "cgore.com")
       (mailto "cgore@cgore.com" "cgore@cgore.com")
       (href "http://www.cgore.com/programming/lisp/bzbz/" "BzBz")
       (href "https://github.com/cgore/bzbz" "BzBz on Github")))

(defparameter *include-advertisements* t)

(defun wide-skyscraper-ad ()
  (when *include-advertisements*
    ;; Include an ad here if you want.  I do.
    (div :wide-skyscraper-ad "")))

(defun webpage-template (index? title &rest rest)
  (webpage-without-login
    (standard-head title)
    (body 
      (left-menu)
      (wide-skyscraper-ad)
      (apply #'div (if *include-advertisements* :main-with-ad :main)
             (apply #'hier-h1 index? (if (listp title) title (list title)))
             rest))))

(defmacro webpage (index? title &rest rest)
  `(webpage-template ,index? ,title ,@rest))
