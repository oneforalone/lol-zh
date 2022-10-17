.. _production-code:

================
生产代码
================

GitHub Repo: https://github.com/thephoeron/let-over-lambda

项目目录结构


.. code-block::

    (in-package cl-user)

    (defpackage let-over-lambda-test
    (:use cl let-over-lambda prove)
    (:import-from named-readtables
                    in-readtable))

    (in-package let-over-lambda-test)

    (in-readtable lol-syntax)

    ;; NOTE: To run this test file, execute `(asdf:test-system :let-over-lambda)' in your Lisp.

    (plan 8)

    (defun! fn! ()
    `(let ((,g!test 123))
        ,g!test))

    (defmacro fn-macro ()
    (fn!))

    (deftest defun!-test
    (is-expand (fn-macro)
                (LET (($TEST 123))
                $TEST)))

    (defparameter flatten-list `(D (E (F ,'(G)))))

    (deftest flatten-test
    (is (flatten '((A . B) (C D (E) (F (G)))))
        '(A B C D E F G)
        "FLATTEN function works as expected.")
    (is (flatten `(A B C ,flatten-list))
        '(A B C D E F G)
        "FLATTEN on quasiquotes works as expected."))

    (defparameter heredoc-string #>END
    I can put anything here: ", , "# and ># are
    no problem. The only thing that will terminate
    the reading of this string is...END)

    (deftest heredoc-read-macro-test
    (is heredoc-string
        "I can put anything here: \", , \"# and ># are
    no problem. The only thing that will terminate
    the reading of this string is..."
        "SHARP-GREATER-THEN read macro works as expected."))

    (deftest pilfered-perl-regex-syntax-test
    (is-expand '#~m|\w+tp://|
                '(lambda ($str) (cl-ppcre:scan-to-strings "\\w+tp://" $str))
                "#~m expands correctly.")
    (is-expand '#~s/abc/def/
                '(lambda ($str) (cl-ppcre:regex-replace-all "abc" $str "def"))
                "#~s expands correctly.")
    (is-values (#~m/abc/ "123abc")
                '("abc" #())
                "#~m runs correctly."
                :test #'equalp)
    (is (#~s/abc/def/ "Testing abc testing abc")
        "Testing def testing def"
        "#~s runs correctly."))

    (deftest read-anaphor-sharp-backquote-test
    (is '#`((,a1))
        '(lambda (a1) `((,a1)))
        "SHARP-BACKQUOTE expands correctly."
        :test #'equalp)
    (is-expand #.(#3`(((,@a2)) ,a3 (,a1 ,a1))
                    (gensym)
                    '(a b c)
                    'hello)
                (((a b c)) hello ($g $g))
                "SHARP-BACKQUOTE runs correctly, respecting order, gensyms, nesting, numarg, etc."))

    (deftest sharp-f-test
    (is '#f
        '(declare (optimize (speed 3) (safety 0)))
        "Default numarg SHARP-F expands correctly.")
    (is '#0f
        '(declare (optimize (speed 0) (safety 3)))
        "Numarg = 3 SHARP-F expands correctly.")
    (is '(#1f #2f)
        '((declare (optimize (speed 1) (safety 2)))
            (declare (optimize (speed 2) (safety 1))))
        "SHARP-F correctly expands into rarely used compiler options."))

    (deftest |test-#""#-read-macro|
    (is #"Contains " and \."#
        "Contains \" and \\."
        "SHARP-QUOTE read macro works as expected."))

    (deftest if-match-test
    (is (if-match (#~m_a(b)c_ "abc")
            $1)
        "b"
        "IF-MATCH correctly returns the single capture.")
    (is-error (if-match (#~m_a(b)c_ "abc")
                    $2)
                'simple-error
                "IF-MATCH throws an error when $2 is unbound.")
    (is (if-match (#~m_a(b)c_ "def")
            $1
            :else)
        :else
        "When IF-MATCH test is false it goes to the else body.")
    (is (if-match (#~m_a(b)c_ "abc")
            (if-match (#~m_(d)(e)f_ "def")
                (list $1 $2)
                :no-second-match)
            $1)
        '("d" "e")
        "IF-MATCH works with nested IF-MATCHs.")
    (is (if-match (#~m_a(b)c_ "abc")
            (if-match (#~m_(d)(e)f_ "d ef")
                (list $1 $2)
                :no-second-match)
            $1)
        :no-second-match
        "IF-MATCH works with nested IF-MATCHs.")
    (is-error (if-match (#~m_a(b)c_ "ab c")
                    (if-match (#~m_(d)(e)f_ "d ef")
                        (list $1 $2)
                        :no-second-match)
                    $1)
                'simple-error
                "IF-MATCH throws an error, even when nested.")
    (is-error (if-match (#~m_a(b)c_ "ab c")
                    (if-match (#~m_(d)(e)f_ "d ef")
                        (list $1 $2)
                        :no-second-match)
                    $2)
                'simple-error
                "IF-MATCH throws an error, even when nested."))

    (run-test-all)

.. toctree::
   :maxdepth: 2

   t
     let-over-lambda.lisp
   .travis.yml
   circle.yml
   let-over-lambda.asd
   let-over-lambda.lisp
   let-over-lambda-test.asd
   LICENSE
   package.lisp
   README.md
   

.. _License 说明:

License 说明
---------------

Antiweb (C) Doug Hoyte

This is a "production" version of LOL with bug-fixes
and new features in the spirit of the book.

See http://letoverlambda.com

This is the source code for the book
_Let_Over_Lambda_ by Doug Hoyte.
This code is (C) 2002-2008, Doug Hoyte.

You are free to use, modify, and re-distribute
this code however you want, except that any
modifications must be clearly indicated before
re-distribution. There is no warranty,
expressed nor implied.

Attribution of this code to me, Doug Hoyte, is
appreciated but not necessary. If you find the
code useful, or would like documentation,
please consider buying the book!

Modifications by "the Phoeron" Colin J.E. Lupton, 2012--2014
- Support for ASDF/Quicklisp
- Cheap hacks to support new Backquote implementation in SBCL v1.2.2

Safety feature for SBCL>=v1.2.2


.. _8-1-weired-by-design:

代码架构
---------------

 .. image:: production-code.png

 .. image:: production-code2.png

 .. image:: production-code3.png


t
~~~~~~~~~~~~~~~~
 
:ref:`let-over-lambda.lisp` 

:ref:`.travis.yml` 

:ref:`circle.yml` 

:ref:`let-over-lambda.asd` 

:ref:`let-over-lambda.lisp` 

:ref:`let-over-lambda-test.asd` 
      
:ref:`LICENSE` 

:ref:`package.lisp` 

:ref:`README.md` 



.. code-block:: lisp
    :linenos:

    (defvar forth-registers
      '(pstack rstack pc
        dict compiling dtable))
;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.
;;
;; You are free to use, modify, and re-distribute
;; this code however you want, except that any
;; modifications must be clearly indicated before
;; re-distribution. There is no warranty,
;; expressed nor implied.
;;
;; Attribution of this code to me, Doug Hoyte, is
;; appreciated but not necessary. If you find the
;; code useful, or would like documentation,
;; please consider buying the book!


(defvar lol-edition "1.0")


.. _产品代码:

产品代码
---------------

t
~~~~~~~~~~~~~~~~

.. _let-over-lambda.lisp:

let-over-lambda.lisp
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: lisp
    :linenos:

 

    ;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.lisp

    (in-package #:let-over-lambda)

    ;; Antiweb (C) Doug Hoyte

    ;; This is a "production" version of LOL with bug-fixes
    ;; and new features in the spirit of the book.

    ;; See http://letoverlambda.com

    ;; This is the source code for the book
    ;; _Let_Over_Lambda_ by Doug Hoyte.
    ;; This code is (C) 2002-2008, Doug Hoyte.
    ;;
    ;; You are free to use, modify, and re-distribute
    ;; this code however you want, except that any
    ;; modifications must be clearly indicated before
    ;; re-distribution. There is no warranty,
    ;; expressed nor implied.
    ;;
    ;; Attribution of this code to me, Doug Hoyte, is
    ;; appreciated but not necessary. If you find the
    ;; code useful, or would like documentation,
    ;; please consider buying the book!

    ;; Modifications by "the Phoeron" Colin J.E. Lupton, 2012--2014
    ;; - Support for ASDF/Quicklisp
    ;; - Cheap hacks to support new Backquote implementation in SBCL v1.2.2

    ;; Safety feature for SBCL>=v1.2.2
    #+sbcl
    (eval-when (:compile-toplevel :execute)
    (handler-case
        (progn
            (sb-ext:assert-version->= 1 2 2)
            (setq *features* (remove 'old-sbcl *features*)))
        (error ()
        (pushnew 'old-sbcl *features*))))

.. _group2:

``group``
------------

.. code-block:: lisp
    :linenos:

 
    (defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                (if (consp rest)
                    (rec rest (cons
                                (subseq source 0 n)
                                acc))
                    (nreverse
                        (cons source acc))))))
        (if source (rec source nil) nil)))

    (eval-when (:compile-toplevel :execute :load-toplevel)
    
.. _mkstr2:

``mkstr``
------------

.. code-block:: lisp
    :linenos:

 
 
    (defun mkstr (&rest args)
        (with-output-to-string (s)
        (dolist (a args) (princ a s))))

.. _symb2:

``symb``
------------

.. code-block:: lisp
    :linenos:

 
 
    (defun symb (&rest args)
        (values (intern (apply #'mkstr args))))

.. _flatten2:

``flatten``
------------

.. code-block:: lisp
    :linenos:

 
 
    (defun flatten (x)
        (labels ((rec (x acc)
                    (cond ((null x) acc)
                            #+(and sbcl (not lol::old-sbcl))
                            ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                            ((atom x) (cons x acc))
                            (t (rec
                                (car x)
                                (rec (cdr x) acc))))))
        (rec x nil)))

.. _g!-symbol-p2:

``g!-symbol-p``
------------------------
 
.. code-block:: lisp
    :linenos:

 
    (defun g!-symbol-p (s)
        (and (symbolp s)
            (> (length (symbol-name s)) 2)
            (string= (symbol-name s)
                    "G!"
                    :start1 0
                    :end1 2)))

.. _o!-symbol-p2:

``o!-symbol-p``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun o!-symbol-p (s)
        (and (symbolp s)
            (> (length (symbol-name s)) 2)
            (string= (symbol-name s)
                    "O!"
                    :start1 0
                    :end1 2)))

.. _o!-symbol-to-g!-symbol2:

``o!-symbol-to-g!-symbol``
------------------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun o!-symbol-to-g!-symbol (s)
        (symb "G!"
            (subseq (symbol-name s) 2))))

.. _defmacro/g!2:

``defmacro/g!``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro defmacro/g! (name args &rest body)
    (let ((syms (remove-duplicates
                (remove-if-not #'g!-symbol-p
                                (flatten body)))))
        (multiple-value-bind (body declarations docstring)
            (parse-body body :documentation t)
        `(defmacro ,name ,args
            ,@(when docstring
                (list docstring))
            ,@declarations
            (let ,(mapcar
                    (lambda (s)
                    `(,s (gensym ,(subseq
                                    (symbol-name s)
                                    2))))
                    syms)
            ,@body)))))

.. _defmacro!2:

``defmacro!``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro defmacro! (name args &rest body)
    (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
            (gs (mapcar #'o!-symbol-to-g!-symbol os)))
        (multiple-value-bind (body declarations docstring)
            (parse-body body :documentation t)
        `(defmacro/g! ,name ,args
            ,@(when docstring
                (list docstring))
            ,@declarations
            `(let ,(mapcar #'list (list ,@gs) (list ,@os))
                ,(progn ,@body))))))

.. _defun!2:

``defun!``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro defun! (name args &body body)
    (let ((syms (remove-duplicates
                (remove-if-not #'g!-symbol-p
                                (flatten body)))))
        (multiple-value-bind (body declarations docstring)
            (parse-body body :documentation t)
        `(defun ,name ,args
            ,@(when docstring
                (list docstring))
            ,@declarations
            (let ,(mapcar (lambda (s)
                            `(,s (gensym ,(subseq (symbol-name s)
                                                2))))
                        syms)
            ,@body)))))

    ;; Nestable suggestion from Daniel Herring
    (eval-when (:compile-toplevel :load-toplevel :execute)
    
.. _|#"-reader|2:

``|#"-reader|``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun |#"-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars (state 'normal) (depth 1))
        (loop do
            (let ((curr (read-char stream)))
                (cond ((eq state 'normal)
                    (cond ((char= curr #\#)
                            (push #\# chars)
                            (setq state 'read-sharp))
                            ((char= curr #\")
                            (setq state 'read-quote))
                            (t
                            (push curr chars))))
                    ((eq state 'read-sharp)
                    (cond ((char= curr #\")
                            (push #\" chars)
                            (incf depth)
                            (setq state 'normal))
                            (t
                            (push curr chars)
                            (setq state 'normal))))
                    ((eq state 'read-quote)
                    (cond ((char= curr #\#)
                            (decf depth)
                            (if (zerop depth) (return))
                            (push #\" chars)
                            (push #\# chars)
                            (setq state 'normal))
                            (t
                            (push #\" chars)
                            (if (char= curr #\")
                                (setq state 'read-quote)
                                (progn
                                    (push curr chars)
                                    (setq state 'normal)))))))))
        (coerce (nreverse chars) 'string))))

    ; (set-dispatch-macro-character #\# #\" #'|#"-reader|)

    ; This version is from Martin Dirichs
    (eval-when (:compile-toplevel :load-toplevel :execute)
    
.. _|#>-reader|2:

``|#>-reader|``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun |#>-reader| (stream sub-char numarg)
        (declare (ignore sub-char numarg))
        (let (chars)
        (do ((curr (read-char stream)
                    (read-char stream)))
            ((char= #\newline curr))
            (push curr chars))
        (let ((pattern (nreverse chars))
                output)
            (labels ((match (pos chars)
                    (if (null chars)
                        pos
                        (if (char= (nth pos pattern) (car chars))
                            (match (1+ pos) (cdr chars))
                            (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
            (do (curr
                (pos 0))
                ((= pos (length pattern)))
                (setf curr (read-char stream)
                    pos (match pos (list curr)))
                (push curr output))
            (coerce
            (nreverse
                (nthcdr (length pattern) output))
            'string))))))

    ; (set-dispatch-macro-character #\# #\> #'|#>-reader|)

.. _segment-reader2:

``segment-reader``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun segment-reader (stream ch n)
    (if (> n 0)
        (let ((chars))
        (do ((curr (read-char stream)
                    (read-char stream)))
            ((char= ch curr))
            (push curr chars))
        (cons (coerce (nreverse chars) 'string)
                (segment-reader stream ch (- n 1))))))


.. _match-mode-ppcre-lambda-form2:

``match-mode-ppcre-lambda-form``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    #+cl-ppcre
    (defmacro! match-mode-ppcre-lambda-form (o!args o!mods)
    ``(lambda (,',g!str)
        (ppcre:scan-to-strings
        ,(if (zerop (length ,g!mods))
            (car ,g!args)
            (format nil "(?~a)~a" ,g!mods (car ,g!args)))
        ,',g!str)))

    #+cl-ppcre
    (defmacro! subst-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
        (cl-ppcre:regex-replace-all
        ,(car ,g!args)
        ,',g!str
        ,(cadr ,g!args))))

.. _|#~-reader|2:

``|#~-reader|``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    #+cl-ppcre
    (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun |#~-reader| (stream sub-char numarg)
        (declare (ignore sub-char numarg))
        (let ((mode-char (read-char stream)))
        (cond
            ((char= mode-char #\m)
            (match-mode-ppcre-lambda-form
            (segment-reader stream
                            (read-char stream)
                            1)
            (coerce (loop for c = (read-char stream)
                        while (alpha-char-p c)
                        collect c
                        finally (unread-char c stream))
                    'string)))
            ((char= mode-char #\s)
            (subst-mode-ppcre-lambda-form
            (segment-reader stream
                            (read-char stream)
                            2)))
            (t (error "Unknown #~~ mode character"))))))

    ; #+cl-ppcre (set-dispatch-macro-character #\# #\~ #'|#~-reader|)

.. _dlambda2:

``dlambda``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro! dlambda (&rest ds)
    `(lambda (&rest ,g!args)
        (case (car ,g!args)
        ,@(mapcar
            (lambda (d)
                `(,(if (eq t (car d))
                    t
                    (list (car d)))
                (apply (lambda ,@(cdr d))
                        ,(if (eq t (car d))
                            g!args
                            `(cdr ,g!args)))))
            ds))))

.. _alambda2:

``alambda``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    ;; Graham's alambda
    (defmacro alambda (parms &body body)
    `(labels ((self ,parms ,@body))
        #'self))

.. _aif2:

``aif``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    ;; Graham's aif
    (defmacro aif (test then &optional else)
    `(let ((it ,test))
        (if it ,then ,else)))

.. _|#`-reader|2:

``|#`-reader|``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (eval-when (:compile-toplevel :execute :load-toplevel)
    (defun |#`-reader| (stream sub-char numarg)
        (declare (ignore sub-char))
        (unless numarg (setq numarg 1))
        `(lambda ,(loop for i from 1 to numarg
                    collect (symb 'a i))
        ,(funcall
            (get-macro-character #\`) stream nil)))

.. _|#f-reader|2:

``|#f-reader|``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun |#f-reader| (stream sub-char numarg)
        (declare (ignore stream sub-char))
        (setq numarg (or numarg 3))
        (unless (<= numarg 3)
        (error "Bad value for #f: ~a" numarg))
        `(declare (optimize (speed ,numarg)
                            (safety ,(- 3 numarg)))))

.. _lol-syntax2:

``lol-syntax``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defreadtable lol-syntax
        (:merge :standard)
        (:dispatch-macro-char #\# #\" #'|#"-reader|)
        (:dispatch-macro-char #\# #\> #'|#>-reader|)
        #+cl-ppcre
        (:dispatch-macro-char #\# #\~ #'|#~-reader|)
        (:dispatch-macro-char #\# #\` #'|#`-reader|)
        (:dispatch-macro-char #\# #\f #'|#f-reader|)))

.. _nlet-tail2:

``nlet-tail``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (in-readtable lol-syntax)

    (defmacro! nlet-tail (n letargs &body body)
    (let ((gs (loop for i in letargs
                collect (gensym))))
        `(macrolet
            ((,n ,gs
                `(progn
                (psetq
                    ,@(apply #'nconc
                            (mapcar
                            #'list
                            ',(mapcar #'car letargs)
                            (list ,@gs))))
                (go ,',g!n))))
        (block ,g!b
            (let ,letargs
            (tagbody
                ,g!n (return-from
                        ,g!b (progn ,@body))))))))

.. _alet%s2:

``alet%``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro alet% (letargs &rest body)
    `(let ((this) ,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        this))

.. _alet2:

``alet``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro alet (letargs &rest body)
    `(let ((this) ,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        (lambda (&rest params)
        (apply this params))))

.. _let-binding-transform2:

``let-binding-transform``
------------------------------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun let-binding-transform (bs)
    (if bs
        (cons
        (cond ((symbolp (car bs))
                (list (car bs)))
                ((consp (car bs))
                (car bs))
                (t
                (error "Bad let bindings")))
        (let-binding-transform (cdr bs)))))

.. _pandoriclet2:

``pandoriclet``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro pandoriclet (letargs &rest body)
    (let ((letargs (cons
                    '(this)
                    (let-binding-transform
                        letargs))))
        `(let (,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        (dlambda
            (:pandoric-get (sym)
            ,(pandoriclet-get letargs))
            (:pandoric-set (sym val)
            ,(pandoriclet-set letargs))
            (t (&rest args)
            (apply this args))))))

.. _pandoriclet-get2:

``pandoriclet-get``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun pandoriclet-get (letargs)
    `(case sym
        ,@(mapcar #`((,(car a1)) ,(car a1))
                letargs)
        (t (error
            "Unknown pandoric get: ~a"
            sym))))

.. _pandoriclet-set2:

``pandoriclet-set``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun pandoriclet-set (letargs)
    `(case sym
        ,@(mapcar #`((,(car a1))
                    (setq ,(car a1) val))
                letargs)
        (t (error
            "Unknown pandoric set: ~a"
            sym))))

.. _get-pandoric2:

``get-pandoric``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (declaim (inline get-pandoric))

    (defun get-pandoric (box sym)
    (funcall box :pandoric-get sym))

.. _get-pandoric2:

``get-pandoric``
------------------------

 
.. code-block:: lisp
    :linenos:

 
    (defsetf get-pandoric (box sym) (val)
    `(progn
        (funcall ,box :pandoric-set ,sym ,val)
        ,val))

.. _with-pandoric2:

``with-pandoric``
------------------------
 
.. code-block:: lisp
    :linenos:

 
    (defmacro with-pandoric (syms box &rest body)
    (let ((g!box (gensym "box")))
        `(let ((,g!box ,box))
        (declare (ignorable ,g!box))
        (symbol-macrolet
            (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                        syms))
            ,@body))))

.. _pandoric-hotpatch2:

``pandoric-hotpatch``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun pandoric-hotpatch (box new)
    (with-pandoric (this) box
        (setq this new)))

.. _pandoric-recode2:

``pandoric-recode``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro pandoric-recode (vars box new)
    `(with-pandoric (this ,@vars) ,box
        (setq this ,new)))

.. _plambda2:

``plambda``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro plambda (largs pargs &rest body)
    (let ((pargs (mapcar #'list pargs)))
        `(let (this self)
        (setq
            this (lambda ,largs ,@body)
            self (dlambda
                    (:pandoric-get (sym)
                    ,(pandoriclet-get pargs))
                    (:pandoric-set (sym val)
                    ,(pandoriclet-set pargs))
                    (t (&rest args)
                    (apply this args)))))))

.. _pandoric-eval-tunnel2:

``pandoric-eval-tunnel``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defvar pandoric-eval-tunnel)

.. _pandoric-eval2:

``pandoric-eval``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro pandoric-eval (vars expr)
    `(let ((pandoric-eval-tunnel
            (plambda () ,vars t)))
        (eval `(with-pandoric
                ,',vars pandoric-eval-tunnel
                ,,expr))))

  
.. _Chapter 7:

Chapter 7
''''''''''''

.. code-block:: lisp
    :linenos:


    ;; Chapter 7


.. _fast-progn2:

``fast-progn``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro fast-progn (&rest body)
    `(locally #f ,@body))

.. _safe-progn2:

``safe-progn``
------------------------
 
.. code-block:: lisp
    :linenos:

 
    (defmacro safe-progn (&rest body)
    `(locally #0f ,@body))

.. _fformat2:

``fformat``
------------------------
 
.. code-block:: lisp
    :linenos:

 
    (defun fformat (&rest all)
    (apply #'format all))

.. _fformat2:

``fformat``
------------------------
 
.. code-block:: lisp
    :linenos:

 
    (define-compiler-macro fformat
                        (&whole form
                            stream fmt &rest args)
    (if (constantp fmt)
        (if stream
        `(funcall (formatter ,fmt)
            ,stream ,@args)
        (let ((g!stream (gensym "stream")))
            `(with-output-to-string (,g!stream)
            (funcall (formatter ,fmt)
                ,g!stream ,@args))))
        form))

.. _inline make-tlist tlist-left tlist-right tlist-empty-p2:

``inline make-tlist tlist-left tlist-right tlist-empty-p``
------------------------------------------------------------------------

.. code-block:: lisp
    :linenos:

 
 
    (declaim (inline make-tlist tlist-left
                    tlist-right tlist-empty-p))

.. _make-tlist2:

``make-tlist``
------------------------
 
.. code-block:: lisp
    :linenos:

 
    (defun make-tlist () (cons nil nil))
    
.. _tlist-left2:

``tlist-left``
------------------------
 
.. code-block:: lisp
    :linenos:

 
    (defun tlist-left (tl) (caar tl))
    
.. _tlist-right2:

``tlist-right``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun tlist-right (tl) (cadr tl))
    
.. _tlist-empty-p2:

``tlist-empty-p``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun tlist-empty-p (tl) (null (car tl)))

.. _inline tlist-add-left tlist-add-right2:

``inline tlist-add-left tlist-add-right``
------------------------------------------------

.. code-block:: lisp
    :linenos:

 
 
    (declaim (inline tlist-add-left
                    tlist-add-right))

.. _tlist-add-left2:

``tlist-add-left``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun tlist-add-left (tl it)
    (let ((x (cons it (car tl))))
        (if (tlist-empty-p tl)
        (setf (cdr tl) x))
        (setf (car tl) x)))

.. _tlist-add-right2:

``tlist-add-right``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun tlist-add-right (tl it)
    (let ((x (cons it nil)))
        (if (tlist-empty-p tl)
        (setf (car tl) x)
        (setf (cddr tl) x))
        (setf (cdr tl) x)))

.. _tlist-rem-left2:

``tlist-rem-left``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (declaim (inline tlist-rem-left))
    (defun tlist-rem-left (tl)
    (if (tlist-empty-p tl)
        (error "Remove from empty tlist")
        (let ((x (car tl)))
        (setf (car tl) (cdar tl))
        (if (tlist-empty-p tl)
            (setf (cdr tl) nil)) ;; For gc
        (car x))))

.. _tlist-update2:

``tlist-update``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (declaim (inline tlist-update))

    (defun tlist-update (tl)
    (setf (cdr tl) (last (car tl))))

.. _build-batcher-sn2:

``build-batcher-sn``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defun build-batcher-sn (n)
    (let* (network
            (tee (ceiling (log n 2)))
            (p (ash 1 (- tee 1))))
        (loop while (> p 0) do
        (let ((q (ash 1 (- tee 1)))
                (r 0)
                (d p))
            (loop while (> d 0) do
            (loop for i from 0 to (- n d 1) do
                (if (= (logand i p) r)
                (push (list i (+ i d))
                        network)))
            (setf d (- q p)
                    q (ash q -1)
                    r p)))
        (setf p (ash p -1)))
        (nreverse network)))

.. _sortf2:

``sortf``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro! sortf (comparator &rest places)
    (if places
        `(tagbody
        ,@(mapcar
            #`(let ((,g!a #1=,(nth (car a1) places))
                    (,g!b #2=,(nth (cadr a1) places)))
                (if (,comparator ,g!b ,g!a)
                    (setf #1# ,g!b
                        #2# ,g!a)))
            (build-batcher-sn (length places))))))

.. _dollar-symbol-p2:

``dollar-symbol-p``
------------------------

.. code-block:: lisp
    :linenos:

  
 
    ;;;;;; NEW CODE FOR ANTIWEB
    #+cl-ppcre
    (defun dollar-symbol-p (s)
    (and (symbolp s)
        (> (length (symbol-name s)) 1)
        (string= (symbol-name s)
                    "$"
                    :start1 0
                    :end1 1)
        (ignore-errors (parse-integer (subseq (symbol-name s) 1)))))

.. _if-match2:

``if-match``
------------------------

.. code-block:: lisp
    :linenos:

 
 
    (defmacro! if-match ((match-regex str) then &optional else)
    (let* ((dollars (remove-duplicates
                    (remove-if-not #'dollar-symbol-p
                                    (flatten then))))
            (top (or (car (sort (mapcar #'dollar-symbol-p dollars) #'>))
                    0)))
        `(multiple-value-bind (,g!matches ,g!captures) (,match-regex ,str)
        (declare (ignorable ,g!matches ,g!captures))
        (let ((,g!captures-len (length ,g!captures)))
            (declare (ignorable ,g!captures-len))
            (symbol-macrolet ,(mapcar #`(,(symb "$" a1)
                                        (if (< ,g!captures-len ,a1)
                                            (error "Too few matchs: ~a unbound." ,(mkstr "$" a1))
                                            (aref ,g!captures ,(1- a1))))
                                    (loop for i from 1 to top collect i))
            (if ,g!matches
                ,then
                ,else))))))

.. _when-match2:

``when-match``
------------------------

.. code-block:: lisp
    :linenos:

 
 

    (defmacro when-match ((match-regex str) &body forms)
    `(if-match (,match-regex ,str)
        (progn ,@forms)))

    ;; EOF

.. _End:

.. _.travis.yml:

.travis.yml
------------------------

.. code-block:: lisp
    :linenos:

  
    language: common-lisp

    sudo: required

    env:
    matrix:
        - LISP=sbcl COVERALLS=true
        - LISP=ccl
        # - LISP=clisp
        # - LISP=ecl
        # - LISP=abcl

    install:
    - if [ -x ./install.sh ] && head -2 ./install.sh | grep '^# cl-travis' > /dev/null;
        then
        ./install.sh;
        else
        curl https://raw.githubusercontent.com/luismbo/cl-travis/master/install.sh | sh;
        fi
    # Coveralls support
    - git clone https://github.com/fukamachi/cl-coveralls ~/lisp/cl-coveralls

    script:
    - cl -l prove -l cl-coveralls
        -e '(in-package :cl-user)'
        -e '(ql:quickload :let-over-lambda)'
        -e '(setf prove:*debug-on-error* t)'
        -e '(setf *debugger-hook*
                    (lambda (c h)
                    (declare (ignore c h))
                    (uiop:quit -1)))'
        -e '(coveralls:with-coveralls (:exclude (list "t"))
                (or (prove:run :let-over-lambda-test)
                    (uiop:quit -1)))'

    notifications:
    webhooks:
        urls:
        - https://webhooks.gitter.im/e/fba7308ceb6194ceb9ff
        on_success: change  # options: [always|never|change] default: always
        on_failure: always  # options: [always|never|change] default: always
        on_start: false     # default: false


.. _circle.yml:

circle.yml
------------------------

.. code-block:: yml
    :linenos:

    machine:
    environment:
        PATH: ~/.roswell/bin:$PATH

    dependencies:
    pre:
        - curl -L https://raw.githubusercontent.com/snmsts/roswell/release/scripts/install-for-ci.sh | sh
        - case $CIRCLE_NODE_INDEX in
            0) ros config set default.lisp sbcl-bin ;;
            1) ros install ccl-bin;
            ros config set default.lisp ccl-bin ;;
        esac
        - ros run -- --version
    override:
        - git clone https://github.com/fukamachi/cl-coveralls ~/lisp/cl-coveralls
        - git clone https://github.com/fukamachi/prove ~/lisp/prove
        - ros -l ~/lisp/prove/prove.asd install prove

    test:
    override:
        - if [ "$CIRCLE_NODE_INDEX" = 0 ]; then COVERALLS=true run-prove let-over-lambda-test.asd; else run-prove let-over-lambda-test.asd; fi: {parallel: true}


.. _let-over-lambda.asd:
  
let-over-lambda.asd
------------------------

.. code-block:: lisp
    :linenos:

    ;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.asd

    (in-package :cl-user)

    (defpackage let-over-lambda-asd
    (:use :cl :asdf)
    (:export #:*lol-version*))

    (in-package :let-over-lambda-asd)

    (defparameter *lol-version* "1.0.1"
    "A string denoting the current version of LET-OVER-LAMBDA.  Used for diagnostic output.")

    (defsystem #:let-over-lambda
    :serial t
    :description "The Production version code from letoverlambda.com, conveniently wrapped in an ASDF System for Quicklisp."
    :version #.*lol-version*
    :author "Doug Hoyte <doug@hoytech.com>"
    :maintainer "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
    :license "BSD Simplified"
    :depends-on (#:alexandria
                #:cl-ppcre
                #:named-readtables)
    :components ((:file "package")
                (:file "let-over-lambda"))
    :in-order-to ((test-op (test-op let-over-lambda-test))))

    ;; EOF


.. _let-over-lambda.lisp(whole):

let-over-lambda.lisp(whole)
------------------------------------------------

.. code-block:: lisp
    :linenos:

    ;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.lisp

    (in-package #:let-over-lambda)

    ;; Antiweb (C) Doug Hoyte

    ;; This is a "production" version of LOL with bug-fixes
    ;; and new features in the spirit of the book.

    ;; See http://letoverlambda.com

    ;; This is the source code for the book
    ;; _Let_Over_Lambda_ by Doug Hoyte.
    ;; This code is (C) 2002-2008, Doug Hoyte.
    ;;
    ;; You are free to use, modify, and re-distribute
    ;; this code however you want, except that any
    ;; modifications must be clearly indicated before
    ;; re-distribution. There is no warranty,
    ;; expressed nor implied.
    ;;
    ;; Attribution of this code to me, Doug Hoyte, is
    ;; appreciated but not necessary. If you find the
    ;; code useful, or would like documentation,
    ;; please consider buying the book!

    ;; Modifications by "the Phoeron" Colin J.E. Lupton, 2012--2014
    ;; - Support for ASDF/Quicklisp
    ;; - Cheap hacks to support new Backquote implementation in SBCL v1.2.2

    ;; Safety feature for SBCL>=v1.2.2
    #+sbcl
    (eval-when (:compile-toplevel :execute)
    (handler-case
        (progn
            (sb-ext:assert-version->= 1 2 2)
            (setq *features* (remove 'old-sbcl *features*)))
        (error ()
        (pushnew 'old-sbcl *features*))))

    (defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                (if (consp rest)
                    (rec rest (cons
                                (subseq source 0 n)
                                acc))
                    (nreverse
                        (cons source acc))))))
        (if source (rec source nil) nil)))

    (eval-when (:compile-toplevel :execute :load-toplevel)
    (defun mkstr (&rest args)
        (with-output-to-string (s)
        (dolist (a args) (princ a s))))

    (defun symb (&rest args)
        (values (intern (apply #'mkstr args))))

    (defun flatten (x)
        (labels ((rec (x acc)
                    (cond ((null x) acc)
                            #+(and sbcl (not lol::old-sbcl))
                            ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                            ((atom x) (cons x acc))
                            (t (rec
                                (car x)
                                (rec (cdr x) acc))))))
        (rec x nil)))

    (defun g!-symbol-p (s)
        (and (symbolp s)
            (> (length (symbol-name s)) 2)
            (string= (symbol-name s)
                    "G!"
                    :start1 0
                    :end1 2)))

    (defun o!-symbol-p (s)
        (and (symbolp s)
            (> (length (symbol-name s)) 2)
            (string= (symbol-name s)
                    "O!"
                    :start1 0
                    :end1 2)))

    (defun o!-symbol-to-g!-symbol (s)
        (symb "G!"
            (subseq (symbol-name s) 2))))

    (defmacro defmacro/g! (name args &rest body)
    (let ((syms (remove-duplicates
                (remove-if-not #'g!-symbol-p
                                (flatten body)))))
        (multiple-value-bind (body declarations docstring)
            (parse-body body :documentation t)
        `(defmacro ,name ,args
            ,@(when docstring
                (list docstring))
            ,@declarations
            (let ,(mapcar
                    (lambda (s)
                    `(,s (gensym ,(subseq
                                    (symbol-name s)
                                    2))))
                    syms)
            ,@body)))))

    (defmacro defmacro! (name args &rest body)
    (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
            (gs (mapcar #'o!-symbol-to-g!-symbol os)))
        (multiple-value-bind (body declarations docstring)
            (parse-body body :documentation t)
        `(defmacro/g! ,name ,args
            ,@(when docstring
                (list docstring))
            ,@declarations
            `(let ,(mapcar #'list (list ,@gs) (list ,@os))
                ,(progn ,@body))))))

    (defmacro defun! (name args &body body)
    (let ((syms (remove-duplicates
                (remove-if-not #'g!-symbol-p
                                (flatten body)))))
        (multiple-value-bind (body declarations docstring)
            (parse-body body :documentation t)
        `(defun ,name ,args
            ,@(when docstring
                (list docstring))
            ,@declarations
            (let ,(mapcar (lambda (s)
                            `(,s (gensym ,(subseq (symbol-name s)
                                                2))))
                        syms)
            ,@body)))))

    ;; Nestable suggestion from Daniel Herring
    (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun |#"-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars (state 'normal) (depth 1))
        (loop do
            (let ((curr (read-char stream)))
                (cond ((eq state 'normal)
                    (cond ((char= curr #\#)
                            (push #\# chars)
                            (setq state 'read-sharp))
                            ((char= curr #\")
                            (setq state 'read-quote))
                            (t
                            (push curr chars))))
                    ((eq state 'read-sharp)
                    (cond ((char= curr #\")
                            (push #\" chars)
                            (incf depth)
                            (setq state 'normal))
                            (t
                            (push curr chars)
                            (setq state 'normal))))
                    ((eq state 'read-quote)
                    (cond ((char= curr #\#)
                            (decf depth)
                            (if (zerop depth) (return))
                            (push #\" chars)
                            (push #\# chars)
                            (setq state 'normal))
                            (t
                            (push #\" chars)
                            (if (char= curr #\")
                                (setq state 'read-quote)
                                (progn
                                    (push curr chars)
                                    (setq state 'normal)))))))))
        (coerce (nreverse chars) 'string))))

    ; (set-dispatch-macro-character #\# #\" #'|#"-reader|)

    ; This version is from Martin Dirichs
    (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun |#>-reader| (stream sub-char numarg)
        (declare (ignore sub-char numarg))
        (let (chars)
        (do ((curr (read-char stream)
                    (read-char stream)))
            ((char= #\newline curr))
            (push curr chars))
        (let ((pattern (nreverse chars))
                output)
            (labels ((match (pos chars)
                    (if (null chars)
                        pos
                        (if (char= (nth pos pattern) (car chars))
                            (match (1+ pos) (cdr chars))
                            (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
            (do (curr
                (pos 0))
                ((= pos (length pattern)))
                (setf curr (read-char stream)
                    pos (match pos (list curr)))
                (push curr output))
            (coerce
            (nreverse
                (nthcdr (length pattern) output))
            'string))))))

    ; (set-dispatch-macro-character #\# #\> #'|#>-reader|)

    (defun segment-reader (stream ch n)
    (if (> n 0)
        (let ((chars))
        (do ((curr (read-char stream)
                    (read-char stream)))
            ((char= ch curr))
            (push curr chars))
        (cons (coerce (nreverse chars) 'string)
                (segment-reader stream ch (- n 1))))))

    #+cl-ppcre
    (defmacro! match-mode-ppcre-lambda-form (o!args o!mods)
    ``(lambda (,',g!str)
        (ppcre:scan-to-strings
        ,(if (zerop (length ,g!mods))
            (car ,g!args)
            (format nil "(?~a)~a" ,g!mods (car ,g!args)))
        ,',g!str)))

    #+cl-ppcre
    (defmacro! subst-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
        (cl-ppcre:regex-replace-all
        ,(car ,g!args)
        ,',g!str
        ,(cadr ,g!args))))

    #+cl-ppcre
    (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun |#~-reader| (stream sub-char numarg)
        (declare (ignore sub-char numarg))
        (let ((mode-char (read-char stream)))
        (cond
            ((char= mode-char #\m)
            (match-mode-ppcre-lambda-form
            (segment-reader stream
                            (read-char stream)
                            1)
            (coerce (loop for c = (read-char stream)
                        while (alpha-char-p c)
                        collect c
                        finally (unread-char c stream))
                    'string)))
            ((char= mode-char #\s)
            (subst-mode-ppcre-lambda-form
            (segment-reader stream
                            (read-char stream)
                            2)))
            (t (error "Unknown #~~ mode character"))))))

    ; #+cl-ppcre (set-dispatch-macro-character #\# #\~ #'|#~-reader|)

    (defmacro! dlambda (&rest ds)
    `(lambda (&rest ,g!args)
        (case (car ,g!args)
        ,@(mapcar
            (lambda (d)
                `(,(if (eq t (car d))
                    t
                    (list (car d)))
                (apply (lambda ,@(cdr d))
                        ,(if (eq t (car d))
                            g!args
                            `(cdr ,g!args)))))
            ds))))

    ;; Graham's alambda
    (defmacro alambda (parms &body body)
    `(labels ((self ,parms ,@body))
        #'self))

    ;; Graham's aif
    (defmacro aif (test then &optional else)
    `(let ((it ,test))
        (if it ,then ,else)))

    (eval-when (:compile-toplevel :execute :load-toplevel)
    (defun |#`-reader| (stream sub-char numarg)
        (declare (ignore sub-char))
        (unless numarg (setq numarg 1))
        `(lambda ,(loop for i from 1 to numarg
                    collect (symb 'a i))
        ,(funcall
            (get-macro-character #\`) stream nil)))

    (defun |#f-reader| (stream sub-char numarg)
        (declare (ignore stream sub-char))
        (setq numarg (or numarg 3))
        (unless (<= numarg 3)
        (error "Bad value for #f: ~a" numarg))
        `(declare (optimize (speed ,numarg)
                            (safety ,(- 3 numarg)))))

    (defreadtable lol-syntax
        (:merge :standard)
        (:dispatch-macro-char #\# #\" #'|#"-reader|)
        (:dispatch-macro-char #\# #\> #'|#>-reader|)
        #+cl-ppcre
        (:dispatch-macro-char #\# #\~ #'|#~-reader|)
        (:dispatch-macro-char #\# #\` #'|#`-reader|)
        (:dispatch-macro-char #\# #\f #'|#f-reader|)))

    (in-readtable lol-syntax)

    (defmacro! nlet-tail (n letargs &body body)
    (let ((gs (loop for i in letargs
                collect (gensym))))
        `(macrolet
            ((,n ,gs
                `(progn
                (psetq
                    ,@(apply #'nconc
                            (mapcar
                            #'list
                            ',(mapcar #'car letargs)
                            (list ,@gs))))
                (go ,',g!n))))
        (block ,g!b
            (let ,letargs
            (tagbody
                ,g!n (return-from
                        ,g!b (progn ,@body))))))))

    (defmacro alet% (letargs &rest body)
    `(let ((this) ,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        this))

    (defmacro alet (letargs &rest body)
    `(let ((this) ,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        (lambda (&rest params)
        (apply this params))))

    (defun let-binding-transform (bs)
    (if bs
        (cons
        (cond ((symbolp (car bs))
                (list (car bs)))
                ((consp (car bs))
                (car bs))
                (t
                (error "Bad let bindings")))
        (let-binding-transform (cdr bs)))))

    (defmacro pandoriclet (letargs &rest body)
    (let ((letargs (cons
                    '(this)
                    (let-binding-transform
                        letargs))))
        `(let (,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        (dlambda
            (:pandoric-get (sym)
            ,(pandoriclet-get letargs))
            (:pandoric-set (sym val)
            ,(pandoriclet-set letargs))
            (t (&rest args)
            (apply this args))))))

    (defun pandoriclet-get (letargs)
    `(case sym
        ,@(mapcar #`((,(car a1)) ,(car a1))
                letargs)
        (t (error
            "Unknown pandoric get: ~a"
            sym))))

    (defun pandoriclet-set (letargs)
    `(case sym
        ,@(mapcar #`((,(car a1))
                    (setq ,(car a1) val))
                letargs)
        (t (error
            "Unknown pandoric set: ~a"
            sym))))

    (declaim (inline get-pandoric))

    (defun get-pandoric (box sym)
    (funcall box :pandoric-get sym))

    (defsetf get-pandoric (box sym) (val)
    `(progn
        (funcall ,box :pandoric-set ,sym ,val)
        ,val))

    (defmacro with-pandoric (syms box &rest body)
    (let ((g!box (gensym "box")))
        `(let ((,g!box ,box))
        (declare (ignorable ,g!box))
        (symbol-macrolet
            (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                        syms))
            ,@body))))

    (defun pandoric-hotpatch (box new)
    (with-pandoric (this) box
        (setq this new)))

    (defmacro pandoric-recode (vars box new)
    `(with-pandoric (this ,@vars) ,box
        (setq this ,new)))

    (defmacro plambda (largs pargs &rest body)
    (let ((pargs (mapcar #'list pargs)))
        `(let (this self)
        (setq
            this (lambda ,largs ,@body)
            self (dlambda
                    (:pandoric-get (sym)
                    ,(pandoriclet-get pargs))
                    (:pandoric-set (sym val)
                    ,(pandoriclet-set pargs))
                    (t (&rest args)
                    (apply this args)))))))

    (defvar pandoric-eval-tunnel)

    (defmacro pandoric-eval (vars expr)
    `(let ((pandoric-eval-tunnel
            (plambda () ,vars t)))
        (eval `(with-pandoric
                ,',vars pandoric-eval-tunnel
                ,,expr))))

    ;; Chapter 7


    (defmacro fast-progn (&rest body)
    `(locally #f ,@body))

    (defmacro safe-progn (&rest body)
    `(locally #0f ,@body))

    (defun fformat (&rest all)
    (apply #'format all))

    (define-compiler-macro fformat
                        (&whole form
                            stream fmt &rest args)
    (if (constantp fmt)
        (if stream
        `(funcall (formatter ,fmt)
            ,stream ,@args)
        (let ((g!stream (gensym "stream")))
            `(with-output-to-string (,g!stream)
            (funcall (formatter ,fmt)
                ,g!stream ,@args))))
        form))

    (declaim (inline make-tlist tlist-left
                    tlist-right tlist-empty-p))

    (defun make-tlist () (cons nil nil))
    (defun tlist-left (tl) (caar tl))
    (defun tlist-right (tl) (cadr tl))
    (defun tlist-empty-p (tl) (null (car tl)))

    (declaim (inline tlist-add-left
                    tlist-add-right))

    (defun tlist-add-left (tl it)
    (let ((x (cons it (car tl))))
        (if (tlist-empty-p tl)
        (setf (cdr tl) x))
        (setf (car tl) x)))

    (defun tlist-add-right (tl it)
    (let ((x (cons it nil)))
        (if (tlist-empty-p tl)
        (setf (car tl) x)
        (setf (cddr tl) x))
        (setf (cdr tl) x)))

    (declaim (inline tlist-rem-left))

    (defun tlist-rem-left (tl)
    (if (tlist-empty-p tl)
        (error "Remove from empty tlist")
        (let ((x (car tl)))
        (setf (car tl) (cdar tl))
        (if (tlist-empty-p tl)
            (setf (cdr tl) nil)) ;; For gc
        (car x))))

    (declaim (inline tlist-update))

    (defun tlist-update (tl)
    (setf (cdr tl) (last (car tl))))

    (defun build-batcher-sn (n)
    (let* (network
            (tee (ceiling (log n 2)))
            (p (ash 1 (- tee 1))))
        (loop while (> p 0) do
        (let ((q (ash 1 (- tee 1)))
                (r 0)
                (d p))
            (loop while (> d 0) do
            (loop for i from 0 to (- n d 1) do
                (if (= (logand i p) r)
                (push (list i (+ i d))
                        network)))
            (setf d (- q p)
                    q (ash q -1)
                    r p)))
        (setf p (ash p -1)))
        (nreverse network)))

    (defmacro! sortf (comparator &rest places)
    (if places
        `(tagbody
        ,@(mapcar
            #`(let ((,g!a #1=,(nth (car a1) places))
                    (,g!b #2=,(nth (cadr a1) places)))
                (if (,comparator ,g!b ,g!a)
                    (setf #1# ,g!b
                        #2# ,g!a)))
            (build-batcher-sn (length places))))))

    ;;;;;; NEW CODE FOR ANTIWEB
    #+cl-ppcre
    (defun dollar-symbol-p (s)
    (and (symbolp s)
        (> (length (symbol-name s)) 1)
        (string= (symbol-name s)
                    "$"
                    :start1 0
                    :end1 1)
        (ignore-errors (parse-integer (subseq (symbol-name s) 1)))))

    (defmacro! if-match ((match-regex str) then &optional else)
    (let* ((dollars (remove-duplicates
                    (remove-if-not #'dollar-symbol-p
                                    (flatten then))))
            (top (or (car (sort (mapcar #'dollar-symbol-p dollars) #'>))
                    0)))
        `(multiple-value-bind (,g!matches ,g!captures) (,match-regex ,str)
        (declare (ignorable ,g!matches ,g!captures))
        (let ((,g!captures-len (length ,g!captures)))
            (declare (ignorable ,g!captures-len))
            (symbol-macrolet ,(mapcar #`(,(symb "$" a1)
                                        (if (< ,g!captures-len ,a1)
                                            (error "Too few matchs: ~a unbound." ,(mkstr "$" a1))
                                            (aref ,g!captures ,(1- a1))))
                                    (loop for i from 1 to top collect i))
            (if ,g!matches
                ,then
                ,else))))))


    (defmacro when-match ((match-regex str) &body forms)
    `(if-match (,match-regex ,str)
        (progn ,@forms)))

    ;; EOF


.. _let-over-lambda-test.asd:

let-over-lambda-test.asd
------------------------------------------------

.. code-block:: lisp
    :linenos:

    ;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA-TEST-ASD; Base: 10 -*-
    ;;;; file: let-over-lambda-test.asd

    (in-package :cl-user)

    (defpackage let-over-lambda-test-asd
    (:use :cl :asdf))

    (in-package :let-over-lambda-test-asd)

    (defsystem #:let-over-lambda-test
    :serial t
    :version "1.0.1"
    :description "The test code for Let Over Lambda."
    :author "Andr茅 Miranda <andremiramor@gmail.com>"
    :maintainer "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
    :license "BSD Simplified"
    :depends-on (#:let-over-lambda
                #:prove
                #:named-readtables)
    :components ((:module "t"
                    :components
                    ((:test-file "let-over-lambda"))))

    :defsystem-depends-on (prove-asdf)
    :perform (test-op :after (op c)
                        (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                        (asdf:clear-system c)))

    ;; EOF

  
 .. _LICENSE:

LICENSE
------------------------

.. code-block:: lisp
    :linenos:

    The BSD License (BSD Simplified)

    Copyright (c) 2002--2008, Doug Hoyte, HCSW
    All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

    Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

    Neither the name of HCSW nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

  
 .. _package.lisp:

package.lisp
------------------------

.. code-block:: lisp
    :linenos:

    ;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: package.lisp

    (defpackage #:let-over-lambda
    (:nicknames #:lol)
    (:use #:cl #:cl-user #:cl-ppcre)
    (:import-from #:alexandria
                    #:parse-body)
    (:import-from #:named-readtables
                    #:defreadtable
                    #:in-readtable)
    (:export #:lol-syntax
            #:mkstr
            #:symb
            #:group
            #:flatten
            #:fact
            #:choose
            #:g!-symbol-p
            #:defmacro/g!
            #:o!-symbol-p
            #:o!-symbol-to-g!-symbol
            #:defmacro!
            #:defun!
            #:|#"-reader|
            #:segment-reader
            #:match-mode-ppcre-lambda-form
            #:subst-mode-ppcre-lambda-form
            #:|#~-reader|
            #:dlambda
            #:alambda
            #:aif
            #:|#`-reader|
            #:|#f-reader|
            #:nlet-tail
            #:alet%
            #:alet
            #:it
            #:this
            #:self
            #:let-binding-transform
            #:pandoriclet
            #:pandoriclet-get
            #:pandoriclet-set
            #:get-pandoric
            #:with-pandoric
            #:pandoric-hotpatch
            #:pandoric-recode
            #:plambda
            #:pandoric-eval
            #:fast-progn
            #:safe-progn
            #:fformat
            #:make-tlist
            #:tlist-left
            #:tlist-right
            #:tlist-empty-p
            #:tlist-add-left
            #:tlist-add-right
            #:tlist-rem-left
            #:tlist-update
            #:build-batcher-sn
            #:sortf
            #:dollar-symbol-p
            #:prune-if-match-bodies-from-sub-lexical-scope
            #:if-match
            #:when-match))

    ;; EOF

.. _README.md:

README.md
------------------------

.. code-block:: md
    :linenos:

    # LET-OVER-LAMBDA

    [![Build Status](https://circleci.com/gh/thephoeron/let-over-lambda.svg?style=shield)](https://circleci.com/gh/thephoeron/let-over-lambda)
    [![Build Status](https://travis-ci.org/thephoeron/let-over-lambda.svg?branch=master)](https://travis-ci.org/thephoeron/let-over-lambda)
    [![Coverage Status](https://coveralls.io/repos/thephoeron/let-over-lambda/badge.svg?branch=master)](https://coveralls.io/r/thephoeron/let-over-lambda)
    [![Quicklisp](http://quickdocs.org/badge/let-over-lambda.svg)](http://quickdocs.org/let-over-lambda/)
    [![BSD Simplified License](https://img.shields.io/badge/license-BSD%20Simplified-blue.svg)](./LICENSE)
    [![Join the chat at https://gitter.im/thephoeron/let-over-lambda](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/thephoeron/let-over-lambda?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

    Doug Hoyte's "Production" version of macros from Let Over Lambda, including community updates; available from Quicklisp.

    Read more about the book and code at: http://letoverlambda.com

    ## News &amp; Updates

    ##### 3/19/2015

    Add symbols for anaphoric macro internals, `IT`, `THIS`, and `SELF` to package exports for better end-user experience.  Will be available in April 2015 release of Quicklisp.

    ##### 8/14/2014

    Issue with incompatible change to backquote syntax in SBCL 1.2.2 resolved; tested against and builds on SBCL 1.2.0-1 and 1.2.2.  Will be available in the August release of Quicklisp.

    ##### 12/18/2013

    Now available in the December 2013 distribution of Quicklisp

    ## Usage

    Make sure you have the latest Quicklisp distribution, then include it as a dependency in your system definition, or from the REPL evaluate `(ql:quickload "let-over-lambda")`.

    ```lisp
    (ql:quickload "let-over-lambda")
    (lol:flatten '((A . B) (C . D) (E . (F G H (I . J) . K))))
    => (A B C D E F G H I J K)
    ```

    LET-OVER-LAMBDA now uses the `named-readtables` library instead of modifying the global readtable. To use LOL reader macros in your Lisp source files, you will have to add both `let-over-lambda` and `named-readtables` to your project dependencies, and the following line after your call to `in-package`, in every source file you wish to use LOL syntax:

    ```lisp
    (named-readtables:in-readtable lol:lol-syntax)
    ```

    ## Contributors

    - [Doug Hoyte](https://github.com/hoytech)
    - ["the Phoeron" Colin J.E. Lupton](https://github.com/thephoeron)
    - [Jorge Gajon](https://github.com/gajon)
    - [Andr茅 Miranda](https://github.com/EuAndreh/)
  
End
---------------
