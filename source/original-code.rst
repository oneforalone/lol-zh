.. _original-code:

****************
源代码
****************

::

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


.. _chapter01-src-code:

Chapter01
===================

.. _mkstr:

``mkst``
------------

.. code-block:: lisp

    (defun mkstr (&rest args)
    (with-output-to-string (s)
        (dolist (a args) (princ a s))))

.. _symb:

``symb``
------------

.. code-block:: lisp

    (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

.. _group:

``group``
------------------

.. code-block:: lisp

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

.. _flatten:

``flatten``
----------------

.. code-block:: lisp

    (defun flatten (x)
    (labels ((rec (x acc)
                (cond ((null x) acc)
                    ((atom x) (cons x acc))
                    (t (rec
                            (car x)
                            (rec (cdr x) acc))))))
        (rec x nil)))

.. _fact:

``fact``
-----------------

.. code-block:: lisp

    (defun fact (x)
    (if (= x 0)
        1
        (* x (fact (- x 1)))))

.. _choose:

``choose``
------------------

.. code-block:: lisp

    (defun choose (n r)
    (/ (fact n)
        (fact (- n r))
        (fact r)))


.. _chapter02-src-code:

Chapter02
=======================

.. _block-scanner:

``block-scanner``
----------------------

.. code-block:: lisp

    (defun block-scanner (trigger-string)
    (let* ((trig (coerce trigger-string 'list))
            (curr trig))
        (lambda (data-string)
        (let ((data (coerce data-string 'list)))
            (dolist (c data)
            (if curr
                (setq curr
                    (if (char= (car curr) c)
                        (cdr curr) ; next char
                        trig))))   ; start over
            (not curr))))) ; return t if found

.. _chapter03-src-code:

Chapter03
========================

.. _sleep-uints%:

``sleep-uints%``
---------------------

.. code-block:: lisp

    (defun sleep-units% (value unit)
    (sleep
        (* value
        (case unit
            ((s) 1)
            ((m) 60)
            ((h) 3600)
            ((d) 86400)
            ((ms) 1/1000)
            ((us) 1/1000000)))))

.. _sleep-units:

``sleep-uints``
----------------------

.. code-block:: lisp

    (defmacro sleep-units (value unit)
    `(sleep
        (* ,value
            ,(case unit
            ((s) 1)
            ((m) 60)
            ((h) 3600)
            ((d) 86400)
            ((ms) 1/1000)
            ((us) 1/1000000)))))

.. _unit-of-time:

``unit-of-time``
----------------------

.. code-block:: lisp

    (defmacro unit-of-time (value unit)
    `(* ,value
        ,(case unit
            ((s) 1)
            ((m) 60)
            ((h) 3600)
            ((d) 86400)
            ((ms) 1/1000)
            ((us) 1/1000000))))

.. _nlet:

``nlet``
--------------------

.. code-block:: lisp

    (defmacro nlet (n letargs &rest body)
    `(labels ((,n ,(mapcar #'car letargs)
                ,@body))
        (,n ,@(mapcar #'cadr letargs))))

.. _g!-symbol-p:

``g!-symbol-p``
---------------------

.. code-block:: lisp

    (defun g!-symbol-p (s)
    (and (symbolp s)
        (> (length (symbol-name s)) 2)
        (string= (symbol-name s)
                    "G!"
                    :start1 0
                    :end1 2)))

.. _defmacro/g!:

``defmacro/g!``
-----------------------

.. code-block:: lisp

    (defmacro defmacro/g! (name args &rest body)
    (let ((syms (remove-duplicates
                    (remove-if-not #'g!-symbol-p
                                (flatten body)))))
        `(defmacro ,name ,args
        (let ,(mapcar
                (lambda (s)
                    `(,s (gensym ,(subseq
                                    (symbol-name s)
                                    2))))
                syms)
            ,@body))))

.. _o!-symbol-p:

``o!-symbol-p``
----------------------

.. code-block:: lisp

    (defun o!-symbol-p (s)
    (and (symbolp s)
        (> (length (symbol-name s)) 2)
        (string= (symbol-name s)
                    "O!"
                    :start1 0
                    :end1 2)))

.. _o!-symbol-to-g!-symbol:

``o!-symbol-to-g!-symbol``
----------------------------

.. code-block:: lisp

    (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
            (subseq (symbol-name s) 2)))

.. _defmacro!:

``defmacro!``
------------------------------

.. code-block:: lisp

    (defmacro defmacro! (name args &rest body)
    (let* ((os (remove-if-not #'o!-symbol-p args))
            (gs (mapcar #'o!-symbol-to-g!-symbol os)))
        `(defmacro/g! ,name ,args
        `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn ,@body)))))

.. _nif:

``nif``
-----------------------------

.. code-block:: lisp

    (defmacro! nif (o!expr pos zero neg)
    `(cond ((plusp ,g!expr) ,pos)
            ((zerop ,g!expr) ,zero)
            (t ,neg)))

.. _chapter04-src-code:

Chapter04
======================

.. _#"-reader:

``#"-reader``
-----------------------------

.. code-block:: lisp

    (defun |#"-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars)
        (do ((prev (read-char stream) curr)
            (curr (read-char stream) (read-char stream)))
            ((and (char= prev #\") (char= curr #\#)))
        (push prev chars))
        (coerce (nreverse chars) 'string)))

    (set-dispatch-macro-character
    #\# #\" #'|#"-reader|)

.. _#>-reader:

``#>-reader``
-----------------------------

.. code-block:: lisp

    (defun |#>-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars)
        (do ((curr (read-char stream)
                (read-char stream)))
            ((char= #\newline curr))
        (push curr chars))
        (let* ((pattern (nreverse chars))
            (pointer pattern)
            (output))
        (do ((curr (read-char stream)
                    (read-char stream)))
            ((null pointer))
            (push curr output)
            (setf pointer
                (if (char= (car pointer) curr)
                    (cdr pointer)
                    pattern))
            (if (null pointer)
            (return)))
        (coerce
            (nreverse
            (nthcdr (length pattern) output))
            'string))))

    (set-dispatch-macro-character
    #\# #\> #'|#>-reader|)

.. _segment-reader:

``segment-reader``
-----------------------------

.. code-block:: lisp

    (defun segment-reader (stream ch n)
    (if (> n 0)
        (let ((chars))
        (do ((curr (read-char stream)
                    (read-char stream)))
            ((char= ch curr))
            (push curr chars))
        (cons (coerce (nreverse chars) 'string)
                (segment-reader stream ch (- n 1))))))

.. _match-mode-ppcre-lambda-form:

``match-mode-ppcre-lambda-form``
----------------------------------------

.. code-block:: lisp

    #+cl-ppcre
    (defmacro! match-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
        (cl-ppcre:scan
        ,(car ,g!args)
        ,',g!str)))

.. _subst-mode-ppcre-lambda-form:

``subst-mode-ppcre-lambda-form``
---------------------------------------

.. code-block:: lisp

    #+cl-ppcre
    (defmacro! subst-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
        (cl-ppcre:regex-replace-all
        ,(car ,g!args)
        ,',g!str
        ,(cadr ,g!args))))

.. _#~-reader:

``#~-reader``
-----------------------------

.. code-block:: lisp

    #+cl-ppcre
    (defun |#~-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let ((mode-char (read-char stream)))
        (cond
        ((char= mode-char #\m)
            (match-mode-ppcre-lambda-form
            (segment-reader stream
                            (read-char stream)
                            1)))
        ((char= mode-char #\s)
            (subst-mode-ppcre-lambda-form
            (segment-reader stream
                            (read-char stream)
                            2)))
        (t (error "Unknown #~~ mode character")))))

    #+cl-ppcre
    (set-dispatch-macro-character #\# #\~ #'|#~-reader|)

.. _cyclic-p:

``cyclic-p``
-----------------------------

.. code-block:: lisp

    (defun cyclic-p (l)
    (cyclic-p-aux l (make-hash-table)))

.. _cyclic-p-aux:

``cyclic-p-aux``
-----------------------------

.. code-block:: lisp

    (defun cyclic-p-aux (l seen)
    (if (consp l)
        (or (gethash l seen)
            (progn
            (setf (gethash l seen) t)
            (or (cyclic-p-aux (car l) seen)
                (cyclic-p-aux (cdr l) seen))))))

.. _safe-read-from-string-blacklist:

``safe-read-from-string-blacklist``
------------------------------------------

.. code-block:: lisp

    (defvar safe-read-from-string-blacklist
    '(#\# #\: #\|))

    (let ((rt (copy-readtable nil)))
    (defun safe-reader-error (stream closech)
        (declare (ignore stream closech))
        (error "safe-read-from-string failure"))

    (dolist (c safe-read-from-string-blacklist)
        (set-macro-character
        c #'safe-reader-error nil rt))

.. _safe-read-from-string:

``safe-read-from-string``
-----------------------------

.. code-block:: lisp

    (defun safe-read-from-string (s &optional fail)
        (if (stringp s)
        (let ((*readtable* rt) *read-eval*)
            (handler-bind
            ((error (lambda (condition)
                        (declare (ignore condition))
                        (return-from
                        safe-read-from-string fail))))
            (read-from-string s)))
        fail)))


.. _chapter05-src-code:

Chapter05
========================

.. _defunits%:

``defunits%``
-----------------------------

.. code-block:: lisp

    (defmacro! defunits% (quantity base-unit &rest units)
    `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
        `(* ,,g!val
            ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x)) ,(cadr x)))
                        (group units 2))))))

.. _defunits-chaining%:

``defunits-chaining%``
-----------------------------

.. code-block:: lisp

    (defun defunits-chaining% (u units)
    (let ((spec (find u units :key #'car)))
        (if (null spec)
        (error "Unknown unit ~a" u)
        (let ((chain (cadr spec)))
            (if (listp chain)
            (* (car chain)
                (defunits-chaining%
                (cadr chain)
                units))
            chain)))))

.. _defunits%%:

``defunits%%``
-----------------------------

.. code-block:: lisp

    (defmacro! defunits%% (quantity base-unit &rest units)
    `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
        `(* ,,g!val
            ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x))
                            ,(defunits-chaining%
                                (car x)
                                (cons `(,base-unit 1)
                                    (group units 2)))))
                        (group units 2))))))

.. _defunits-chaining:

``defunits-chaining``
-----------------------------

.. code-block:: lisp

    (defun defunits-chaining (u units prev)
    (if (member u prev)
        (error "~{ ~a~^ depends on~}"
        (cons u prev)))
    (let ((spec (find u units :key #'car)))
        (if (null spec)
        (error "Unknown unit ~a" u)
        (let ((chain (cadr spec)))
            (if (listp chain)
            (* (car chain)
                (defunits-chaining
                (cadr chain)
                units
                (cons u prev)))
            chain)))))

.. _defunits:

``defunits``
-----------------------------

.. code-block:: lisp

    (defmacro! defunits (quantity base-unit &rest units)
    `(defmacro ,(symb 'unit-of- quantity)
                (,g!val ,g!un)
        `(* ,,g!val
            ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x))
                            ,(defunits-chaining
                                (car x)
                                (cons
                                    `(,base-unit 1)
                                    (group units 2))
                                nil)))
                        (group units 2))))))

.. _units:

``units``
-----------------------------

.. code-block:: lisp

    (defunits distance m
    km 1000
    cm 1/100
    mm (1/10 cm)
    nm (1/1000 mm)

    yard 9144/10000 ; Defined in 1956
    foot (1/3 yard)
    inch (1/12 foot)
    mile (1760 yard)
    furlong (1/8 mile)

    fathom (2 yard) ; Defined in 1929
    nautical-mile 1852
    cable (1/10 nautical-mile)

    old-brit-nautical-mile ; Dropped in 1970
        (6080/3 yard)
    old-brit-cable
        (1/10 old-brit-nautical-mile)
    old-brit-fathom
        (1/100 old-brit-cable))


.. _tree-leaves%:

``tree-leaves%``
-----------------------------

.. code-block:: lisp

    (defun tree-leaves% (tree result)
    (if tree
        (if (listp tree)
        (cons
            (tree-leaves% (car tree)
                        result)
            (tree-leaves% (cdr tree)
                        result))
        result)))

.. _predicate-splitter:

``predicate-splitter``
-----------------------------

.. code-block:: lisp

    (defun predicate-splitter (orderp splitp)
    (lambda (a b)
        (let ((s (funcall splitp a)))
        (if (eq s (funcall splitp b))
            (funcall orderp a b)
            s))))

.. _tree-leaves%%:

``tree-leaves%%``
-----------------------------

.. code-block:: lisp

    (defun tree-leaves%% (tree test result)
    (if tree
        (if (listp tree)
        (cons
            (tree-leaves%% (car tree) test result)
            (tree-leaves%% (cdr tree) test result))
        (if (funcall test tree)
            (funcall result tree)
            tree))))

.. _tree-leaves:

``tree-leaves``
-----------------------------

.. code-block:: lisp

    (defmacro tree-leaves (tree test result)
    `(tree-leaves%%
        ,tree
        (lambda (x)
        (declare (ignorable x))
        ,test)
        (lambda (x)
        (declare (ignorable x))
        ,result)))

.. _nlet-tail:

``nlet-tail``
-----------------------------

.. code-block:: lisp

    (defmacro! nlet-tail (n letargs &rest body)
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

.. _cxr%:

``cxr%``
-----------------------------

.. code-block:: lisp

    (defmacro cxr% (x tree)
    (if (null x)
        tree
        `(,(cond
            ((eq 'a (cadr x)) 'car)
            ((eq 'd (cadr x)) 'cdr)
            (t (error "Non A/D symbol")))
        ,(if (= 1 (car x))
            `(cxr% ,(cddr x) ,tree)
            `(cxr% ,(cons (- (car x) 1) (cdr x))
                    ,tree)))))


.. _cxr:

``cxr``
-----------------------------

.. code-block:: lisp

    (defvar cxr-inline-thresh 10)

    (defmacro! cxr (x tree)
    (if (null x)
        tree
        (let ((op (cond
                    ((eq 'a (cadr x)) 'car)
                    ((eq 'd (cadr x)) 'cdr)
                    (t (error "Non A/D symbol")))))
        (if (and (integerp (car x))
                (<= 1 (car x) cxr-inline-thresh))
            (if (= 1 (car x))
            `(,op (cxr ,(cddr x) ,tree))
            `(,op (cxr ,(cons (- (car x) 1) (cdr x))
                        ,tree)))
            `(nlet-tail
                ,g!name ((,g!count ,(car x))
                        (,g!val (cxr ,(cddr x) ,tree)))
                (if (>= 0 ,g!count)
                ,g!val
                ;; Will be a tail:
                (,g!name (- ,g!count 1)
                        (,op ,g!val))))))))

.. _def-english-list-accessors:

``def-english-list-accessors``
---------------------------------

.. code-block:: lisp

    (defmacro def-english-list-accessors (start end)
    (if (not (<= 1 start end))
        (error "Bad start/end range"))
    `(progn
        ,@(loop for i from start to end collect
            `(defun
                ,(symb
                (map 'string
                        (lambda (c)
                        (if (alpha-char-p c)
                            (char-upcase c)
                            #\-))
                        (format nil "~:r" i)))
                (arg)
                (cxr (1 a ,(- i 1) d) arg)))))

.. _cxr-calculator:

``cxr-calculator``
-----------------------------

.. code-block:: lisp

    (defun cxr-calculator (n)
    (loop for i from 1 to n
            sum (expt 2 i)))

.. _cxr-symbol-p:

``cxr-symbol-p``
-----------------------------

.. code-block:: lisp

    (defun cxr-symbol-p (s)
    (if (symbolp s)
        (let ((chars (coerce
                    (symbol-name s)
                    'list)))
        (and
            (< 6 (length chars))
            (char= #\C (car chars))
            (char= #\R (car (last chars)))
            (null (remove-if
                    (lambda (c)
                    (or (char= c #\A)
                        (char= c #\D)))
                    (cdr (butlast chars))))))))

.. _cxr-symbol-to-cxr-list:

``cxr-symbol-to-cxr-list``
-----------------------------

.. code-block:: lisp

    (defun cxr-symbol-to-cxr-list (s)
    (labels ((collect (l)
                (if l
                (list*
                    1
                    (if (char= (car l) #\A)
                    'A
                    'D)
                    (collect (cdr l))))))
        (collect
        (cdr       ; chop off C
            (butlast ; chop off R
            (coerce
                (symbol-name s)
                'list))))))

.. _with-all-cxrs:

``with-all-cxrs``
-----------------------------

.. code-block:: lisp

    (defmacro with-all-cxrs (&rest forms)
    `(labels
        (,@(mapcar
            (lambda (s)
                `(,s (l)
                (cxr ,(cxr-symbol-to-cxr-list s)
                        l)))
            (remove-duplicates
                (remove-if-not
                #'cxr-symbol-p
                (flatten forms)))))
        ,@forms))

.. _dlambda:

``dlambda``
-----------------------------

.. code-block:: lisp

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


.. _chapter06-src-code:

Chapter06
===========================

.. _alambda:

``alambda``
-----------------------------

.. code-block:: lisp

    ;; Graham's alambda
    (defmacro alambda (parms &body body)
    `(labels ((self ,parms ,@body))
        #'self))

.. _aif:

``aif``
-----------------------------

.. code-block:: lisp

    ;; Graham's aif
    (defmacro aif (test then &optional else)
    `(let ((it ,test))
        (if it ,then ,else)))

.. _#`-reader:

``#`-reader``
-----------------------------

.. code-block:: lisp

    (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                    collect (symb 'a i))
        ,(funcall
            (get-macro-character #\`) stream nil)))

    (set-dispatch-macro-character
    #\# #\` #'|#`-reader|)

.. _alet%:

``alet%``
-----------------------------

.. code-block:: lisp

    (defmacro alet% (letargs &rest body)
    `(let ((this) ,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        this))

.. _alet:

``alet``
-----------------------------

.. code-block:: lisp

    (defmacro alet (letargs &rest body)
    `(let ((this) ,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        (lambda (&rest params)
        (apply this params))))

.. _alet-fsm:

``alet-fsm``
-----------------------------

.. code-block:: lisp

    (defmacro alet-fsm (&rest states)
    `(macrolet ((state (s)
                    `(setq this #',s)))
        (labels (,@states) #',(caar states))))

.. _ichain-before:

``ichain-beofre``
-----------------------------

.. code-block:: lisp

    (defmacro! ichain-before (&rest body)
    `(let ((,g!indir-env this))
        (setq this
        (lambda (&rest ,g!temp-args)
            ,@body
            (apply ,g!indir-env
                    ,g!temp-args)))))

.. _ichain-after:

``ichain-after``
-----------------------------

.. code-block:: lisp

    (defmacro! ichain-after (&rest body)
    `(let ((,g!indir-env this))
        (setq this
        (lambda (&rest ,g!temp-args)
            (prog1
            (apply ,g!indir-env
                    ,g!temp-args)
            ,@body)))))

.. _ichain-intercept%:

``ichain-intercept%``
-----------------------------

.. code-block:: lisp

    (defmacro! ichain-intercept% (&rest body)
    `(let ((,g!indir-env this))
        (setq this
        (lambda (&rest ,g!temp-args)
            (block intercept
            (prog1
                (apply ,g!indir-env
                        ,g!temp-args)
                ,@body))))))

.. _ichain-intercept:

``ichain-intercept``
-----------------------------

.. code-block:: lisp

    (defmacro! ichain-intercept (&rest body)
    `(let ((,g!indir-env this))
        (setq this
        (lambda (&rest ,g!temp-args)
            (block ,g!intercept
            (macrolet ((intercept (v)
                        `(return-from
                            ,',g!intercept
                            ,v)))
                (prog1
                (apply ,g!indir-env
                        ,g!temp-args)
                ,@body)))))))

.. _alet-hotpatch%:

``alet-hotpatch%``
-----------------------------

.. code-block:: lisp

    (defmacro alet-hotpatch% (letargs &rest body)
    `(let ((this) ,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        (lambda (&rest args)
        (if (eq (car args) ':hotpatch)
            (setq this (cadr args))
            (apply this args)))))

.. _alet-hotpatch:

``alet-hotpatch``
-----------------------------

.. code-block:: lisp

    (defmacro alet-hotpatch (letargs &rest body)
    `(let ((this) ,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        (dlambda
        (:hotpatch (closure)
            (setq this closure))
        (t (&rest args)
            (apply this args)))))

.. _let-hotpatch:

``let-hotpatch``
-----------------------------

.. code-block:: lisp

    (defmacro! let-hotpatch (letargs &rest body)
    `(let ((,g!this) ,@letargs)
        (setq ,g!this ,@(last body))
        ,@(butlast body)
        (dlambda
        (:hotpatch (closure)
            (setq ,g!this closure))
        (t (&rest args)
            (apply ,g!this args)))))

.. _let-binding-transform:

``let-binding-transform``
-----------------------------

.. code-block:: lisp

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

.. _sublet:

``sublet``
-----------------------------

.. code-block:: lisp

    (defmacro sublet (bindings% &rest body)
    (let ((bindings (let-binding-transform
                        bindings%)))
        (setq bindings
        (mapcar
            (lambda (x)
            (cons (gensym (symbol-name (car x))) x))
            bindings))
        `(let (,@(mapcar #'list
                        (mapcar #'car bindings)
                        (mapcar #'caddr bindings)))
        ,@(tree-leaves
            body
            #1=(member x bindings :key #'cadr)
            (caar #1#)))))

.. _sublet*:

``sublet*``
-----------------------------

.. code-block:: none

    (defmacro sublet* (bindings &rest body)
    `(sublet ,bindings
        ,@(mapcar #'macroexpand-1 body)))

.. _pandoriclet:

``pandoriclet``
-----------------------------

.. code-block:: none

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

.. _pandoriclet-get:

``pandoriclet-get``
-----------------------------

.. code-block:: none

    (defun pandoriclet-get (letargs)
    `(case sym
        ,@(mapcar #`((,(car a1)) ,(car a1))
                letargs)
        (t (error
            "Unknown pandoric get: ~a"
            sym))))

.. _pandoriclet-set:

``pandoriclet-set``
-----------------------------

.. code-block:: none

    (defun pandoriclet-set (letargs)
    `(case sym
        ,@(mapcar #`((,(car a1))
                    (setq ,(car a1) val))
                letargs)
        (t (error
            "Unknown pandoric set: ~a"
            sym val))))

.. _get-pandoric:

``get-pandoric``
-----------------------------

.. code-block:: lisp

    (declaim (inline get-pandoric))

    (defun get-pandoric (box sym)
    (funcall box :pandoric-get sym))

    (defsetf get-pandoric (box sym) (val)
    `(progn
        (funcall ,box :pandoric-set ,sym ,val)
        ,val))

.. _with-pandoric:

``with-pandoric``
-----------------------------

.. code-block:: none

    (defmacro! with-pandoric (syms o!box &rest body)
    `(symbol-macrolet
        (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                    syms))
        ,@body))

.. _pandoric-hotpatch:

``pandoric-hotpatch``
-----------------------------

.. code-block:: lisp

    (defun pandoric-hotpatch (box new)
    (with-pandoric (this) box
        (setq this new)))

.. _pandoric-recode:

``pandoric-recode``
-----------------------------

.. code-block:: lisp

    (defmacro pandoric-recode (vars box new)
    `(with-pandoric (this ,@vars) ,box
        (setq this ,new)))

.. _plambda:

``plambda``
-----------------------------

.. code-block:: lisp

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

.. _make-stats-counter:

``make-stats-counter``
-----------------------------

.. code-block:: lisp

    (defun make-stats-counter
        (&key (count 0)
                (sum 0)
                (sum-of-squares 0))
    (plambda (n) (sum count sum-of-squares)
        (incf sum-of-squares (expt n 2))
        (incf sum n)
        (incf count)))

.. _defpan:

``defpan``
-----------------------------

.. code-block:: lisp

    (defmacro defpan (name args &rest body)
    `(defun ,name (self)
        ,(if args
            `(with-pandoric ,args self
            ,@body)
            `(progn ,@body))))



    (defpan stats-counter-mean (sum count)
    (/ sum count))

    (defpan stats-counter-variance
            (sum-of-squares sum count)
    (if (< count 2)
        0
        (/ (- sum-of-squares
            (* sum
                (stats-counter-mean self)))
        (- count 1))))

    (defpan stats-counter-stddev ()
    (sqrt (stats-counter-variance self)))

.. _make-noisy-stats-counter:

``make-noisy-stats-counter``
-----------------------------

.. code-block:: lisp

    (defun make-noisy-stats-counter
        (&key (count 0)
                (sum 0)
                (sum-of-squares 0))
    (plambda (n) (sum count sum-of-squares)
        (incf sum-of-squares (expt n 2))
        (incf sum n)
        (incf count)

        (format t
        "~&MEAN=~a~%VAR=~a~%STDDEV=~a~%"
            (stats-counter-mean self)
            (stats-counter-variance self)
            (stats-counter-stddev self))))

.. _pandoric-eval:

``pandoric-eval``
-----------------------------

.. code-block:: lisp

    (defvar pandoric-eval-tunnel)

    (defmacro pandoric-eval (vars expr)
    `(let ((pandoric-eval-tunnel
            (plambda () ,vars t)))
        (eval `(with-pandoric
                ,',vars pandoric-eval-tunnel
                ,,expr))))


.. _chapter07-src-code:

Chapter07
==========================

.. _set-dispatch-macro-character:

``set-dispatch-macro-character``
------------------------------------

.. code-block:: none

    (set-dispatch-macro-character #\# #\f
    (lambda (stream sub-char numarg)
        (declare (ignore stream sub-char))
        (setq numarg (or numarg 3))
        (unless (<= numarg 3)
        (error "Bad value for #f: ~a" numarg))
        `(declare (optimize (speed ,numarg)
                            (safety ,(- 3 numarg))))))

.. _fast-progn:

``fast-progn``
-----------------------------

.. code-block:: none

    (defmacro fast-progn (&rest body)
    `(locally #f ,@body))

.. _safe-progn:

``safe-progn``
-----------------------------

.. code-block:: none

    (defmacro safe-progn (&rest body)
    `(locally #0f ,@body))

.. _fast-keywords-strip:

``fast-keywords-strip``
-----------------------------

.. code-block:: lisp

    (defun fast-keywords-strip (args)
    (if args
        (cond
        ((eq (car args) '&key)
            (fast-keywords-strip (cdr args)))
        ((consp (car args))
            (cons (caar args)
                #1=(fast-keywords-strip
                    (cdr args))))
        (t
            (cons (car args) #1#)))))

.. _defun-with-fast-keywords:

``defun-with-fast-keywords``
-----------------------------

.. code-block:: none

    (defmacro! defun-with-fast-keywords
            (name args &rest body)
    `(progn
        (defun ,name ,args ,@body)
        (defun ,g!fast-fun
                ,(fast-keywords-strip args)
                ,@body)
        (compile ',g!fast-fun)
        (define-compiler-macro ,name (&rest ,g!rest)
        (destructuring-bind ,args ,g!rest
            (list ',g!fast-fun
                ,@(fast-keywords-strip args))))))

.. _defun-keywords-test:

``defun-keywords-test``
-----------------------------

.. code-block:: lisp

    (defun
    slow-keywords-test (a b &key (c 0) (d 0))
    (+ a b c d))

    (compile 'slow-keywords-test)

    (defun-with-fast-keywords
    fast-keywords-test (a b &key (c 0) (d 0))
    (+ a b c d))

.. _keywords-benchmark:

``keywords-benchmark``
-----------------------------

.. code-block:: lisp

    (defun keywords-benchmark (n)
    (format t "Slow keys:~%")
    (time
        (loop for i from 1 to n do
        (slow-keywords-test 1 2 :d 3 :c n)))
    (format t "Fast keys:~%")
    (time
        (loop for i from 1 to n do
        (fast-keywords-test 1 2 :d 3 :c n))))

    (compile 'keywords-benchmark)

.. _fformat:

``fformat``
-----------------------------

.. code-block:: lisp

    (defun fformat (&rest all)
    (apply #'format all))

    (compile 'fformat)

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

.. _fformat-benchmark:

``fformat-benchmark``
-----------------------------

.. code-block:: lisp

    (defun fformat-benchmark (n)
    (format t "Format:~%")
    (time
        (loop for i from 1 to n do
        ( format nil "Hello ~a ~a~%" 'world n)))
    (format t "Fformat:~%")
    (time
        (loop for i from 1 to n do
        (fformat nil "Hello ~a ~a~%" 'world n))))

    (compile 'fformat-benchmark)

.. _dis:

``dis``
-----------------------------

.. code-block:: none

    (defmacro dis (args &rest body)
    `(disassemble
        (compile nil
        (lambda ,(mapcar (lambda (a)
                            (if (consp a)
                                (cadr a)
                                a))
                            args)
            (declare
            ,@(mapcar
                #`(type ,(car a1) ,(cadr a1))
                (remove-if-not #'consp args)))
            ,@body))))

.. _pointer-&:

``pointer-&``
-----------------------------

.. code-block:: lisp

    (defmacro! pointer-& (obj)
    `(lambda (&optional (,g!set ',g!temp))
        (if (eq ,g!set ',g!temp)
        ,obj
        (setf ,obj ,g!set))))

    (defun pointer-* (addr)
    (funcall addr))

    (defsetf pointer-* (addr) (val)
    `(funcall ,addr ,val))

    (defsetf pointer-& (addr) (val)
    `(setf (pointer-* ,addr) ,val))

.. _with-fast-stack:

``with-fast-stack``
-----------------------------

.. code-block:: none

    (defmacro! with-fast-stack
            ((sym &key (type 'fixnum) (size 1000)
                        (safe-zone 100))
                &rest body)
    `(let ((,g!index ,safe-zone)
            (,g!mem (make-array ,(+ size (* 2 safe-zone))
                                :element-type ',type)))
        (declare (type (simple-array ,type) ,g!mem)
                (type fixnum ,g!index))
        (macrolet
        ((,(symb 'fast-push- sym) (val)
                `(locally #f
                (setf (aref ,',g!mem ,',g!index) ,val)
                (incf ,',g!index)))
            (,(symb 'fast-pop- sym) ()
                `(locally #f
                (decf ,',g!index)
                (aref ,',g!mem ,',g!index)))
            (,(symb 'check-stack- sym) ()
                `(progn
                (if (<= ,',g!index ,,safe-zone)
                    (error "Stack underflow: ~a"
                            ',',sym))
                (if (<= ,,(- size safe-zone)
                        ,',g!index)
                    (error "Stack overflow: ~a"
                            ',',sym)))))
            ,@body)))

.. _make-tlist:

``make-tlist``
-----------------------------

.. code-block:: lisp

    (declaim (inline make-tlist tlist-left
                    tlist-right tlist-empty-p))

    (defun make-tlist () (cons nil nil))
    (defun tlist-left (tl) (caar tl))
    (defun tlist-right (tl) (cadr tl))
    (defun tlist-empty-p (tl) (null (car tl)))

.. _tlist-add:

``tlist-add``
-----------------------------

.. code-block:: lisp

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

.. _tlist-rem:

``tlist-rem``
-----------------------------

.. code-block:: none

    (declaim (inline tlist-rem-left))

    (defun tlist-rem-left (tl)
    (if (tlist-empty-p tl)
        (error "Remove from empty tlist")
        (let ((x (car tl)))
        (setf (car tl) (cdar tl))
        (if (tlist-empty-p tl)
            (setf (cdr tl) nil)) ;; For gc
        (car x))))

.. _tlist-update:

``tlist-update``
-----------------------------

.. code-block:: lisp

    (declaim (inline tlist-update))

    (defun tlist-update (tl)
    (setf (cdr tl) (last (car tl))))

.. _counting-cons:

``counting-cons``
-----------------------------

.. code-block:: lisp

    (defvar number-of-conses 0)

    (declaim (inline counting-cons))

    (defun counting-cons (a b)
    (incf number-of-conses)
    (cons a b))

.. _with-conses-counted:

``with-conses-counted``
-----------------------------

.. code-block:: lisp

    (defmacro! with-conses-counted (&rest body)
    `(let ((,g!orig number-of-conses))
        ,@body
        (- number-of-conses ,g!orig)))

.. _counting-push:

``counting-push``
-----------------------------

.. code-block:: lisp

    (defmacro counting-push (obj stack)
    `(setq ,stack (counting-cons ,obj ,stack)))

.. _with-cons-pool:

``with-cons-pool``
-----------------------------

.. code-block:: lisp

    (defmacro with-cons-pool (&rest body)
    `(let ((cons-pool)
            (cons-pool-count 0)
            (cons-pool-limit 100))
        (declare (ignorable cons-pool
                            cons-pool-count
                            cons-pool-limit))
        ,@body))

.. _cons-pool-cons:

``cons-pool-cons``
-----------------------------

.. code-block:: lisp

    (defmacro! cons-pool-cons (o!car o!cdr)
    `(if (= cons-pool-count 0)
        (counting-cons ,g!car ,g!cdr)
        (let ((,g!cell cons-pool))
        (decf cons-pool-count)
        (setf cons-pool (cdr cons-pool))
        (setf (car ,g!cell) ,g!car
                (cdr ,g!cell) ,g!cdr)
        ,g!cell)))

.. _cons-pool-free:

``cons-pool-free``
-----------------------------

.. code-block:: lisp

    (defmacro! cons-pool-free (o!cell)
    `(when (<= cons-pool-count
                (- cons-pool-limit 1))
        (incf cons-pool-count)
        (setf (car ,g!cell) nil)
        (push ,g!cell cons-pool)))

.. _make-cons-pool-stack:

``make-cons-pool-stack``
-----------------------------

.. code-block:: lisp

    (defmacro make-cons-pool-stack ()
    `(let (stack)
        (dlambda
        (:push (elem)
            (setf stack
                (cons-pool-cons elem stack)))
        (:pop ()
            (if (null stack)
            (error "Tried to pop an empty stack"))
            (let ((cell stack)
                (elem (car stack)))
            (setf stack (cdr stack))
            (cons-pool-free cell)
            elem)))))

.. _make-shared-cons-pool-stack:

``make-shared-cons-pool-stack``
-----------------------------------

.. code-block:: lisp

    (with-cons-pool
    (defun make-shared-cons-pool-stack ()
        (make-cons-pool-stack)))

.. _with-dynamic-cons-pools:

``with-dynamic-cons-pools``
-----------------------------

.. code-block:: lisp

    (defmacro with-dynamic-cons-pools (&rest body)
    `(locally (declare (special cons-pool
                                cons-pool-count
                                cons-pool-limit))
        ,@body))

.. _fill-cons-pool:

``fill-cons-pool``
-----------------------------

.. code-block:: lisp

    (defmacro fill-cons-pool ()
    `(let (tp)
        (loop for i from cons-pool-count
                    to cons-pool-limit
            do (push
                    (cons-pool-cons nil nil)
                    tp))
        (loop while tp
            do (cons-pool-free (pop tp)))))

.. _bad-3-sn:

``bad-3-sn``
-----------------------------

.. code-block:: lisp

    (defvar bad-3-sn
    '((0 1) (0 2) (1 2)))

.. _good-3-sn:

``good-3-sn``
-----------------------------

.. code-block:: lisp

    (defvar good-3-sn
    '((0 2) (0 1) (1 2)))

.. _interpret-sn:

``interpret-sn``
-----------------------------

.. code-block:: lisp

    (defvar tracing-interpret-sn nil)

    (defun interpret-sn (data sn)
    (let ((step 0) (swaps 0))
        (dolist (i sn)
        (if tracing-interpret-sn
            (format t "Step ~a: ~a~%" step data))
        (if (> #1=(nth (car i) data)
                #2=(nth (cadr i) data))
            (progn
            (rotatef #1# #2#)
            (incf swaps)))
        (incf step))
        (values swaps data)))

.. _all-sn-perms:

``all-sn-perms``
-----------------------------

.. code-block:: lisp

    (defun all-sn-perms (n)
    (let (perms curr)
        (funcall
        (alambda (left)
            (if left
            (loop for i from 0 to (1- (length left)) do
                (push (nth i left) curr)
                (self (append (subseq left 0 i)
                            (subseq left (1+ i))))
                (pop curr))
            (push curr perms)))
        (loop for i from 1 to n collect i))
        perms))

.. _average-swaps-calc:

``average-swaps-calc``
-----------------------------

.. code-block:: lisp

    (defun average-swaps-calc (n sn)
    (/ (loop for i in (all-sn-perms n) sum
        (interpret-sn (copy-list i) sn))
        (fact n)))

.. _build-batcher-sn:

``build-batcher-sn``
-----------------------------

.. code-block:: lisp

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

.. _prune-sn-for-median:

``prune-sn-for-median``
-----------------------------

.. code-block:: lisp

    (defun prune-sn-for-median (elems network)
    (let ((mid (floor elems 2)))
        (nreverse
        (if (evenp elems)
            (prune-sn-for-median-aux
            (reverse network)
            (list (1- mid) mid))
            (prune-sn-for-median-aux
            (reverse network)
            (list mid))))))

    (defun prune-sn-for-median-aux (network contam)
    (if network
        (if (intersection (car network) contam)
        (cons (car network)
                (prune-sn-for-median-aux
                (cdr network)
                (remove-duplicates
                    (append (car network) contam))))
        (prune-sn-for-median-aux
            (cdr network) contam))))

.. _prune-sn-for-median-calc:

``prune-sn-for-median-calc``
-------------------------------

.. code-block:: lisp

    (defun prune-sn-for-median-calc (n)
    (loop for i from 2 to n collect
        (let* ((sn (build-batcher-sn i))
            (snp (prune-sn-for-median i sn)))
        (list i
            (length sn)
            (length snp)))))

.. _paeth-9-median-sn:

``paeth-9-median-sn``
-----------------------------

.. code-block:: lisp

    (defvar paeth-9-median-sn
    '((0 3) (1 4) (2 5) (0 1) (0 2) (4 5) (3 5) (1 2)
        (3 4) (1 3) (1 6) (4 6) (2 6) (2 3) (4 7) (2 4)
        (3 7) (4 8) (3 8) (3 4)))

.. _paeth-25-median-sn:

``paeth-25-median-sn``
-----------------------------

.. code-block:: lisp

    (defvar paeth-25-median-sn
    '((0 1) (3 4) (2 4) (2 3) (6 7) (5 7) (5 6) (9 10)
        (8 10) (8 9) (12 13) (11 13) (11 12) (15 16)
        (14 16) (14 15) (18 19) (17 19) (17 18) (21 22)
        (20 22) (20 21) (23 24) (2 5) (3 6) (0 6) (0 3)
        (4 7) (1 7) (1 4) (11 14) (8 14) (8 11) (12 15)
        (9 15) (9 12) (13 16) (10 16) (10 13) (20 23)
        (17 23) (17 20) (21 24) (18 24) (18 21) (19 22)
        (8 17) (9 18) (0 18) (0 9) (10 19) (1 19) (1 10)
        (11 20) (2 20) (2 11) (12 21) (3 21) (3 12)
        (13 22) (4 22) (4 13) (14 23) (5 23) (5 14)
        (15 24) (6 24) (6 15) (7 16) (7 19) (13 21)
        (15 23) (7 13) (7 15) (1 9) (3 11) (5 17) (11 17)
        (9 17) (4 10) (6 12) (7 14) (4 6) (4 7) (12 14)
        (10 14) (6 7) (10 12) (6 10) (6 17) (12 17)
        (7 17) (7 10) (12 18) (7 12) (10 18) (12 20)
        (10 20) (10 12)))

.. _sn-to-lambda-form%:

``sn-to-lambda-form%``
-----------------------------

.. code-block:: none

    (defun sn-to-lambda-form% (sn)
    `(lambda (arr)
        #f
        (declare (type (simple-array fixnum) arr))
        ,@(mapcar
            #`(if (> #1=(aref arr ,(car a1))
                    #2=(aref arr ,(cadr a1)))
                (rotatef #1# #2#))
            sn)
        arr))

.. _sn-to-lambda-form:

``sn-to-lambda-form``
-----------------------------

.. code-block:: none

    (defun sn-to-lambda-form (sn)
    `(lambda (arr)
        #f
        (declare (type (simple-array fixnum) arr))
        ,@(mapcar
            #`(let ((a #1=(aref arr ,(car a1)))
                    (b #2=(aref arr ,(cadr a1))))
                (if (> a b)
                (setf #1# b
                        #2# a)))
            sn)
        arr))

.. _sortf:

``sortf``
-----------------------------

.. code-block:: none

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

.. _sort-benchmark-time:

``sort-benchmark-time``
-----------------------------

.. code-block:: none

    (defmacro sort-benchmark-time ()
    `(progn
        (setq sorter (compile nil sorter))
        (let ((arr (make-array
                    n :element-type 'fixnum)))
        (time
            (loop for i from 1 to iters do
            (loop for j from 0 to (1- n) do
                (setf (aref arr j) (random n)))
            (funcall sorter arr))))))

.. _do-sort-benchmark:

``do-sort-benchmark``
-----------------------------

.. code-block:: none

    (defun do-sort-benchmark (n iters)
    (let ((rs (make-random-state *random-state*)))
        (format t "CL sort:~%")
        (let ((sorter
                '(lambda (arr)
                    #f
                    (declare (type (simple-array fixnum)
                                arr))
                    (sort arr #'<))))
        (sort-benchmark-time))

        (setf *random-state* rs)
        (format t "sortf:~%")
        (let ((sorter
                `(lambda (arr)
                    #f
                    (declare (type (simple-array fixnum)
                                arr))
                    (sortf <
                    ,@(loop for i from 0 to (1- n)
                            collect `(aref arr ,i)))
                    arr)))
        (sort-benchmark-time))))

    (compile 'do-sort-benchmark)

.. _medianf-get-best-sn:

``medianf-get-best-sn``
-----------------------------

.. code-block:: none

    (defun medianf-get-best-sn (n)
    (case n
        ((0)  (error "Need more places for medianf"))
        ((9)  paeth-9-median-sn)
        ((25) paeth-25-median-sn)
        (t    (prune-sn-for-median n
                (build-batcher-sn n)))))

.. _medianf:

``medianf``
-----------------------------

.. code-block:: none

    (defmacro! medianf (&rest places)
    `(progn
        ,@(mapcar
            #`(let ((,g!a #1=,(nth (car a1) places))
                    (,g!b #2=,(nth (cadr a1) places)))
                (if (< ,g!b ,g!a)
                (setf #1# ,g!b
                        #2# ,g!a)))
            (medianf-get-best-sn (length places)))
        ,(nth (floor (1- (length places)) 2) ; lower
            places)))


.. _chapter08-src-code:

Chapter08
============================

.. code-block:: none

    (defvar forth-registers
            '(pstack rstack pc
            dict compiling dtable))

.. _forth-word:

``forth-word``
-----------------------------

.. code-block:: lisp

    (defstruct forth-word
    name prev immediate thread)

.. _forth-lookup:

``forth-lookup``
-----------------------------

.. code-block:: lisp

    (defun forth-lookup (w last)
    (if last
        (if (eql (forth-word-name last) w)
        last
        (forth-lookup
            w (forth-word-prev last)))))

.. _forth-inner-interpreter:

``forth-inner-interpreter``
------------------------------

.. code-block:: lisp

    (defmacro forth-inner-interpreter ()
    `(loop
        do (cond
            ((functionp (car pc))
                (funcall (car pc)))
            ((consp (car pc))
                (push (cdr pc) rstack)
                (setf pc (car pc)))
            ((null pc)
                (setf pc (pop rstack)))
            (t
                (push (car pc) pstack)
                (setf pc (cdr pc))))
        until (and (null pc) (null rstack))))

.. _prim-form:

``prim-form``
-----------------------------

.. code-block:: lisp

    ;; Prim-form: (name immediate . forms)
    (defvar forth-prim-forms nil)

    (defmacro def-forth-naked-prim (&rest code)
    `(push ',code forth-prim-forms))

    (defmacro def-forth-prim (&rest code)
    `(def-forth-naked-prim
        ,@code
        (setf pc (cdr pc))))

.. _prim-forms:

``prim-forms``
-----------------------------

.. code-block:: lisp

    (def-forth-prim nop nil)

    (def-forth-prim * nil
    (push (* (pop pstack) (pop pstack))
            pstack))

    (def-forth-prim drop nil
    (pop pstack))

    (def-forth-prim dup nil
    (push (car pstack) pstack))

    (def-forth-prim swap nil
    (rotatef (car pstack) (cadr pstack)))

    (def-forth-prim print nil
    (print (pop pstack)))

    (def-forth-prim >r nil
    (push (pop pstack) rstack))

    (def-forth-prim r> nil
    (push (pop rstack) pstack))

.. _go-forth:

``go-forth``
-----------------------------

.. code-block:: lisp

    (defmacro! go-forth (o!forth &rest words)
    `(dolist (w ',words)
        (funcall ,g!forth w)))

.. _forth-stdlib:

``forth-stdlib``
-----------------------------

.. code-block:: lisp

    (defvar forth-stdlib nil)

    (defmacro forth-stdlib-add (&rest all)
    `(setf forth-stdlib
            (nconc forth-stdlib
                    ',all)))

.. _new-forth:

``new-forth``
-----------------------------

.. code-block:: lisp

    (defmacro new-forth ()
    `(alet ,forth-registers
        (setq dtable (make-hash-table))
        (forth-install-prims)
        (dolist (v forth-stdlib)
        (funcall this v))
        (plambda (v) ,forth-registers
        (let ((word (forth-lookup v dict)))
            (if word
            (forth-handle-found)
            (forth-handle-not-found))))))

.. _forth-install-prims:

``forth-install-prims``
-----------------------------

.. code-block:: none

    ;; Prim-form: (name immediate . forms)
    (defmacro forth-install-prims ()
    `(progn
        ,@(mapcar
            #`(let ((thread (lambda ()
                            ,@(cddr a1))))
                (setf dict
                    (make-forth-word
                        :name ',(car a1)
                        :prev dict
                        :immediate ,(cadr a1)
                        :thread thread))
                (setf (gethash thread dtable)
                    ',(cddr a1)))
            forth-prim-forms)))

.. _forth-prims:

``forth-prims``
-----------------------------

.. code-block:: lisp

    (def-forth-prim [ t ; <- t means immediate
    (setf compiling nil))

    (def-forth-prim ] nil ; <- not immediate
    (setf compiling t))

.. _forth-compile-in:

``forth-compile-in``
-----------------------------

.. code-block:: lisp

    (defmacro forth-compile-in (v)
    `(setf (forth-word-thread dict)
            (nconc (forth-word-thread dict)
                    (list ,v))))

.. _forth-handle-found:

``forth-handle-found``
-----------------------------

.. code-block:: lisp

    (defmacro forth-handle-found ()
    `(if (and compiling
                (not (forth-word-immediate word)))
        (forth-compile-in (forth-word-thread word))
        (progn
        (setf pc (list (forth-word-thread word)))
        (forth-inner-interpreter))))

.. _forth-handle-not-found:

``forth-handle-not-found``
-----------------------------

.. code-block:: lisp

    (defmacro forth-handle-not-found ()
    `(cond
        ((and (consp v) (eq (car v) 'quote))
            (if compiling
            (forth-compile-in (cadr v))
            (push (cadr v) pstack)))
        ((and (consp v) (eq (car v) 'postpone))
            (let ((word (forth-lookup (cadr v) dict)))
            (if (not word)
                (error "Postpone failed: ~a" (cadr v)))
            (forth-compile-in (forth-word-thread word))))
        ((symbolp v)
            (error "Word ~a not found" v))
        (t
            (if compiling
            (forth-compile-in v)
            (push v pstack)))))

.. _forth-create-name-immediate:

``forth-create-name-immediate``
----------------------------------

.. code-block:: lisp

    (def-forth-prim create nil
    (setf dict (make-forth-word :prev dict)))

    (def-forth-prim name nil
    (setf (forth-word-name dict) (pop pstack)))

    (def-forth-prim immediate nil
    (setf (forth-word-immediate dict) t))

.. _forth-stdlib-add-create:

``forth-stdlib-add-create``
---------------------------------------

.. code-block:: lisp

    (forth-stdlib-add
    create
        ] create ] [
    '{ name)

.. _forth-stdlib-add-name-immediate:

``forth-stdlib-add-name-immediate``
-----------------------------------------

.. code-block:: lisp

    (forth-stdlib-add
    { (postpone [) [
    '} name immediate)

.. _forth-prim-@-!:

``forth-prim-@-!``
-----------------------------

.. code-block:: lisp

    (def-forth-prim @ nil
    (push (car (pop pstack))
            pstack))

    (def-forth-prim ! nil
    (let ((location (pop pstack)))
        (setf (car location) (pop pstack))))

.. _forth-unary-word-definer:

``forth-unary-word-definer``
--------------------------------

.. code-block:: none

    (defmacro forth-unary-word-definer (&rest words)
    `(progn
        ,@(mapcar
            #`(def-forth-prim ,a1 nil
                (push (,a1 (pop pstack))
                    pstack))
            words)))

.. _forth-binary-word-definer:

``forth-binary-word-definer``
---------------------------------

.. code-block:: none

    (defmacro! forth-binary-word-definer (&rest words)
    `(progn
        ,@(mapcar
            #`(def-forth-prim ,a1 nil
                (let ((,g!top (pop pstack)))
                (push (,a1 (pop pstack)
                            ,g!top)
                        pstack)))
            words)))


.. _forth-unary-word-definers:

``forth-unary-word-definers``
--------------------------------

.. code-block:: lisp

    (forth-unary-word-definer
    not car cdr cadr caddr cadddr
    oddp evenp)
    (forth-binary-word-definer
    eq equal + - / = < > <= >=
    max min and or)

.. _forth-prim-branch-if:

``forth-prim-branch-if``
-----------------------------

.. code-block:: lisp

    (def-forth-naked-prim branch-if nil
    (setf pc (if (pop pstack)
                (cadr pc)
                (cddr pc))))

.. _forth-stdlib-add-drop:

``forth-stdlib-add-drop``
-----------------------------

.. code-block:: lisp

    (forth-stdlib-add
    { r> drop } 'exit name)

.. _forth-prim-compile-here:

``forth-prim-compile-here``
-----------------------------

.. code-block:: lisp

    (def-forth-naked-prim compile nil
    (setf (forth-word-thread dict)
            (nconc (forth-word-thread dict)
                (list (cadr pc))))
    (setf pc (cddr pc)))

    (def-forth-prim here nil
    (push (last (forth-word-thread dict))
            pstack))

.. _forth-stdlib-add-compile:

``forth-stdlib-add-compile``
-----------------------------

.. code-block:: lisp

    (forth-stdlib-add
    { compile not
        compile branch-if
        compile nop
        here } 'if name immediate)

.. _forth-stdlib-add-here:

``forth-stdlib-add-here``
-----------------------------

.. code-block:: lisp

    (forth-stdlib-add
    { compile nop
        here swap ! } 'then name immediate)

.. _forth-stdlib-add-abs:

``forth-stdlib-add-abs``
-----------------------------

.. code-block:: lisp

    (forth-stdlib-add
    { 0 swap - } 'negate name
    { dup 0 < if negate then } 'abs name)

.. _forth-stdlib-add-swap:

``forth-stdlib-add-swap``
-----------------------------

.. code-block:: lisp

    (forth-stdlib-add
    { compile 't
        compile branch-if
        compile nop
        here swap
        compile nop
        here swap ! } 'else name immediate)

.. _forth-stdlib-add-mod2:

``forth-stdlib-add-mod2``
-----------------------------

.. code-block:: lisp

    (forth-stdlib-add
    { evenp if 0 else 1 then } 'mod2 name)

.. _forth-stdlib-add-again:

``forth-stdlib-add-again``
-----------------------------

.. code-block:: lisp

    (forth-stdlib-add
    { compile nop
        here } 'begin name immediate
    { compile 't
        compile branch-if
        compile nop
        here ! } 'again name immediate)

.. _get-forth-thread:

``get-forth-thread``
-----------------------------

.. code-block:: lisp

    (defun get-forth-thread (forth word)
    (with-pandoric (dict) forth
        (forth-word-thread
        (forth-lookup word dict))))

.. _print-forth-thread:

``print-forth-thread``
-----------------------------

.. code-block:: lisp

    (defun print-forth-thread (forth word)
    (let ((*print-circle* t))
        (print (get-forth-thread forth word))
        t))

.. _flubify-aux:

``flubify-aux``
-----------------------------

.. code-block:: lisp

    (defmacro flubify-aux ()
    `(alambda (c)
        (if c
        (cond
            ((gethash (car c) prim-ht)
            (assemble-flub
                `(funcall
                    ,(gethash (car c) prim-ht))
                (self (cdr c))))
            ((gethash (car c) thread-ht)
            (assemble-flub
                `(funcall #',(car (gethash (car c)
                                    thread-ht)))
                (self (cdr c))))
            ((eq (car c) branch-if)
            (assemble-flub
                `(if (pop pstack)
                    (go ,(gethash (cadr c) go-ht)))
                (self (cddr c))))
            ((consp (car c))
            (flubify forth (car c) prim-ht
                        thread-ht branch-if)
            (self c))
            (t
            (assemble-flub
                `(push ',(car c) pstack)
                (self (cdr c))))))))

.. _assemble-flub:

``assemble-flub``
-----------------------------

.. code-block:: lisp

    (defmacro assemble-flub (form rest)
    `(if (gethash c go-ht)
        (list* (gethash c go-ht)
                ,form
                ,rest)
        (list* ,form
                ,rest)))

.. _flubify:

``flubify``
-----------------------------

.. code-block:: lisp

    (defun flubify (forth thread prim-ht
                    thread-ht branch-if)
    (unless #1=(gethash thread thread-ht)
        (setf #1# (list (gensym)))
        (let ((go-ht (make-hash-table)))
        (funcall
            (alambda (c)
            (when c
                (cond
                ((eq (car c) branch-if)
                    (setf (gethash (cadr c) go-ht)
                    (gensym))
                    (self (cddr c)))
                ((consp (car c))
                    (flubify forth thread prim-ht
                            thread-ht branch-if)))
                (self (cdr c))))
            thread)
        (setf #1# (nconc #1# (funcall
                                (flubify-aux)
                                thread))))))

.. _compile-flubified:

``compile-flubified``
-----------------------------

.. code-block:: lisp

    (defun compile-flubified (thread thread-ht)
    `(labels (,@(let (collect)
                    (maphash
                    (lambda (k v)
                        (declare (ignore k))
                        (push
                        `(,(car v) ()
                            (tagbody ,@(cdr v)))
                        collect))
                    thread-ht)
                    (nreverse collect)))
        (funcall #',(car (gethash thread thread-ht)))))

.. _flubify-thread-shaker:

``flubify-thread-shaker``
-----------------------------

.. code-block:: lisp

    (defun flubify-thread-shaker
        (forth thread ht tmp-ht branch-if compile)
    (if (gethash thread tmp-ht)
        (return-from flubify-thread-shaker)
        (setf (gethash thread tmp-ht) t))
    (cond
        ((and (consp thread) (eq (car thread) branch-if))
        (if (cddr thread)
            (flubify-thread-shaker
            forth (cddr thread) ht
            tmp-ht branch-if compile)))
        ((and (consp thread) (eq (car thread) compile))
        (error "Can't convert compiling word to lisp"))
        ((consp thread)
        (flubify-thread-shaker
            forth (car thread) ht
            tmp-ht branch-if compile)
        (if (cdr thread)
            (flubify-thread-shaker
            forth (cdr thread) ht
            tmp-ht branch-if compile)))
        ((not (gethash thread ht))
        (if (functionp thread)
            (setf (gethash thread ht)
            (with-pandoric (dtable) forth
                (gethash thread dtable)))))))

.. _forth-to-lisp:

``forth-to-lisp``
-----------------------------

.. code-block:: lisp

    (defun forth-to-lisp (forth word)
    (let ((thread (get-forth-thread forth word))
            (shaker-ht (make-hash-table))
            (prim-ht (make-hash-table))
            (thread-ht (make-hash-table))
            (branch-if (get-forth-thread forth 'branch-if))
            (compile (get-forth-thread forth 'compile)))
        (flubify-thread-shaker
        forth thread shaker-ht
        (make-hash-table) branch-if compile)
        (maphash (lambda (k v)
                (declare (ignore v))
                (setf (gethash k prim-ht) (gensym)))
                shaker-ht)
        (flubify forth thread prim-ht thread-ht branch-if)
        `(let (pstack)
        (let (,@(let (collect)
                    (maphash
                    (lambda (k v)
                        (push `(,(gethash k prim-ht)
                                (lambda () ,@(butlast v)))
                            collect))
                    shaker-ht)
                    (nreverse collect)))
            ,(compile-flubified
                thread thread-ht)))))
