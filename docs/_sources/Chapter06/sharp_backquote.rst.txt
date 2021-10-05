.. _sharp_backquote:

==================================
6.2 Sharp-Backquote（ ``#``` )
==================================

:Author: Doug Hoyte
:Translator: Yuqi Liu <yuqi.lyle@outlook.com>

尽管大多数回指是由常规宏引入的，但 read 宏也有可能引入代码，隐形地创建绑定。当 read 宏这样做时，它们被称为读回指（read anaphora）。本节介绍了一个这样的 read 宏，虽然它本身很短，但它却是本书中最有用的宏之一，连我自己都感到惊讶。我已经尽可能快地引入了这个宏，以便它可以用于其余的代码。已经有几个宏使用了它


.. code-block::

  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                    collect (symb 'a i))
        ,(funcall
          (get-macro-character #\`) stream nil)))

    (set-dispatch-macro-character
      #\# #\` #'|#`-reader|)

反引号就是个 read 宏，将输入读成 lambda 结构。默认情况下，这个 lambda 结构只接收一个参数：``a1``。然后这个 read 宏递归地调用对应流的 ``read`` 函数。下面是一个停止求值(通过引用)的例子，这样就可以直接地观察着个回指读宏了：

.. code-block::


  * '#`((,a1))

  (LAMBDA (A1)
    `((,A1)))

该 read 宏抽象出一个通用宏模式。例如，如果有一个列表变量，并且要用 let 来绑定这个列表，将每个变量绑定到一个符号(假设为 ``empty`` )，就可以像这样使用 ``mapcar``：

.. code-block::

  * (mapcar (lambda (a)
              (list a ''empty))
      '(var-a var-b var-c))

  ((VAR-A 'EMPTY)
  (VAR-B 'EMPTY)
  (VAR-C 'EMPTY))

但特别是对复杂的列表结构，这样写就显得有点乱，所以 lisp 程序员喜欢用反引号将其引用提高一层：

.. code-block::

  * (mapcar (lambda (a)
              `(,a 'empty))
      '(var-a var-b var-c))

  ((VAR-A 'EMPTY)
  (VAR-B 'EMPTY)
  (VAR-C 'EMPTY))

新的 read 回指宏隐藏了 lambda 结构：

.. code-block::

  * (mapcar #`(,a1 'empty)
      '(var-a var-b var-c))

  ((VAR-A 'EMPTY)
  (VAR-B 'EMPTY)
  (VAR-C 'EMPTY))

上述代码中 ``a1`` 变量中的 ``1`` 的原因是，read 宏的使用这可以通过提供的 ``numarg`` 的数字来引入不同的回指数字：

.. code-block::

  * '#2`(,a1 ,a2)

  (LAMBDA (A1 A2)
    `(,A1 ,A2))

所以我们可以同时在多个表达式中 ``mapcar`` sharp-backquote（#`) 表达式：

.. code-block::

  * (let ((vars '(var-a var-b var-c)))
      (mapcar #2`(,a1 ',a2)
        vars
        (loop for v in vars
              collect (gensym
                        (symbol-name v)))))

  ((VAR-A '#:VAR-A1731)
  (VAR-B '#:VAR-B1732)
  (VAR-C '#:VAR-C1733))

另一种考虑 sharp-backquote 的方法是，它像 ``format`` 函数一样将插值（interpolation）作为字符串插值列出。就像 ``format`` 让我们使用带有接口（slot）的模板，接口用单独的参数值填充一样，sharp-backquote 让我们将列表插值的结构与想要拼接的值分开。由于前面描述的列表中函数位置的 lambda 结构和 ``lambda`` 宏展开成函数的 lambda 结构之间的语法二元性，还可以使用sharp-backquote 作为函数调用中的第一个元素：

.. code-block::

  * (#3`(((,a1)) ,@a2 (,a3))
        (gensym)
        '(a b c)
        'hello)

  (((#:G1734)) A B C (HELLO))

与 ``format`` 不同的是，sharp-quote 不用顺序定位，而是用回指绑定的数字。因此，顺序可以打乱，甚至可以在绑定中多次拼接：

.. code-block::

  * (#3`(((,@a2)) ,a3 (,a1 ,a1))
        (gensym)
        '(a b c)
        'hello)

  (((A B C)) HELLO (#:G1735 #:G1735))

思考：``gensym`` 生成的 ``#:G1735`` 的引用看起来是指向同一个符号，但是，当然，通过查看它们的打印名称，永远无法真正地分辨出 ``gensym``。这些符号相等（ ``eq`` ）吗？相等或不相等的原因是什么？