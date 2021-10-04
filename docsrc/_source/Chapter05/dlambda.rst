.. _dlambda:

==================================
5.7 Dlambda
==================================

:Author: Doug Hoyte
:Translator: Yuqi Liu <yuqi.lyle@outlook.com>

在讨论闭包时，我们提到了怎么将闭包当作对象使用，以及一般情况下，不确定范围和词法作用域怎么替代复杂的对象系统。但是，到目前为止，我们忽略了对象通常都有的一个特性：多方法。换句话说，虽然我们简单的计数器闭包示例只允许一个操作，即增量，但对象通常都能能够用不同的行为响应不同的消息。

尽管闭包可以被认为是个只有一个方法（ ``apply`` ）的对象，但可以根据传递给它的参数来设计个方法，使其具有不同的行为。例如，如果我们将第一个参数指定为表示所传递消息的符号，则可以基于第一个参数用简单的 ``case`` 语句提供多个行为。

为实现一个具有增加和减少方法的计数器，可能会这样写：

.. code-block::

  (let ((count 0))
    (lambda (msg)
      (case msg
        ((:inc)
          (incf count))
        ((:dec)
          (decf count)))))

注意，上述例子中使用了关键字符号，也就是冒号 ``:`` 开头的符号，通常计算这些符号用来指代消息。关键字很方便，因为不需要引用它们或从包中导出它们，而且很直观，因为它们就是设计来执行这个和其他类型的解构。通常在 ``lambda`` 或 ``defmacro`` 结构中，关键字在运行时不会被解构。但是由于我们正在实现一个消息传递系统，这个系统会在运行时解构，所以我们将关键字处理操作留在运行时执行。如前所述，符号的解构是个高效的操作（仅仅是指针比较）。计数器例子在编译时，可能会被缩减为以下机器码：

.. code-block::

  2FC:       MOV  EAX, [#x582701E4]  ; :INC
  302:       CMP  [EBP-12], EAX
  305:       JEQ  L3
  307:       MOV  EAX, [#x582701E8]  ; :DEC
  30D:       CMP  [EBP-12], EAX
  310:       JEQ  L2


但为了方便起见，我们要避免每创建个对象或类时都要编写一个对应的条件语句。这里就要用到宏了。我喜欢用的宏是 ``dlambda``，他会展开成 lambda 结构。这个展开包括一种方法，这个方法可以根据应用的参数执行许多不同的代码分支。这种运行时解构的类型就是 ``dlambda`` 名称的来源：它是 ``lambda`` 的解构或调度版本。

.. code-block::

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

``dlambda`` 的第一个参数是个关键词符号。根据使用的关键字符号，``dlambda`` 将执行相应的代码段。例如，我们最喜欢的闭包例子：简单的计数器，可以使用 ``dlambda``，根据第一个参数增加或减少计数。这就是所谓的 ``let over dlambda`` 模式：

.. code-block::

  * (setf (symbol-function 'count-test)
      (let ((count 0))
        (dlambda
          (:inc () (incf count))
          (:dec () (decf count)))))

  #<Interpreted Function>

既可以递增

.. code-block::

  * (count-test :inc)

  1

也可以递减

.. code-block::

  * (count-test :dec)

  0

闭包取决于传递的第一个参数。尽管在上面的 let over dlambda 中为空，关键字符号后面的列表实际上是 lambda 析构列表。每个调度，或者说每个关键字参数，都可以有自己特定的 lambda 解构列表，就像下面对计数器闭包的增强:

.. code-block::

  * (setf (symbol-function 'count-test)
      (let ((count 0))
        (dlambda
          (:reset () (setf count 0))
          (:inc (n) (incf count n))
          (:dec (n) (decf count n))
          (:bound (lo hi)
            (setf count
              (min hi
                  (max lo
                        count)))))))

  #<Interpreted Function>

现在，我们有几个不同的 lambda 解构列表可以使用，具体取决于第一个关键词参数， ``:reset`` 不需要参数，然后会将 ``count`` 置为 0：

.. code-block::

  * (count-test :reset)

  0

``:inc`` 和 ``:dec`` 都接受数字参数，``n``：

.. code-block::

  * (count-test :inc 100)

  100

``:bound`` 确保 ``count`` 的值时在边界值 ``lo`` 和 ``hi`` 之中。若 ``count`` 的值落在边界值之外，那么它会变成离该值较近的那个边界值：

.. code-block::

  * (count-test :bound -10 10)

  10

.. note::

  上面代码的结果之所以为 10 是因为上面的值已经将 ``count`` 设置为 100 了，加上了 ``:bond`` 后就变成 10 了

``dlambda`` 一个重要的属性是，它使用 lambda 进行所有的解构，因此保留了正常的错误检查和 COMMON LISP 环境中的调试（debugging）。例如，当只给 ``count-test`` 一个参数时，就会直接得到个和 lambda 程序类似的报错：

.. code-block::

  * (count-test :bond -10)

  ERROR: Wrong argument count, wanted 2 and got 1.

特别是当 ``dlambda`` 嵌入到词法环境中形成个闭包，``dlambda`` 可以让我们使用面向对象的方式编程，就像是创建个多方法的对象。 ``dlambda`` 经过适配，在不偏离 lambda 语法和用法的情况下，使该功能易于访问。 ``dlambda`` 仍然会展开成单个 lambda 表达式，因此，它的求值结果与对 ``lambda`` 求值完全相同：一个可以保存、应用的匿名函数，最重要的是，可以将这个 lambda 控件用作词法闭包。

但 ``dlambda`` 将这种同步与 ``lambda`` 更进一步。为了让 ``dlambda`` 尽可能平滑地从包含 ``lambda`` 宏的代码转换，``dlambda`` 可以不将关键字参数作为第一个符号传递的匿名函数调用。当我们通过正常的 ``lambda`` 接口使用闭包编写了大量的代码时，我们希望能够添加特殊情况的 ``dlambda`` 方法，而不改变其他代码调用接口的方式。

如果说最后可能的方法是给定符号 ``t`` 而不是关键字参数，在没有发现任何特殊情况的关键字参数方法适用时，所提供的方法将始终被调用。以下是个特意编造的例子：

.. code-block::

  * (setf (symbol-function 'dlambda-test)
      (dlambda
        (:something-special ()
          (format t "SPECIAL~%"))
        (t (&rest args)
          (format t "DEFAULT: ~a~%" args))))

  #<Interpreted Function>

有了这个定义，调用该函数的主要方法就是调用默认情况。默认情况用了 lambda 解构参数的 ``&rest`` 来接收所有可能的参数，我们可以通过提供更具体的 lambda 解构参数自由地缩小可接受的参数。

.. code-block::

  * (dlambda-test 1 2 3)
  DEFAULT: (1 2 3)
  NIL
  * (dlambda-test)
  DEFAULT: NIL
  NIL

然而，尽管这个匿名函数的行为很像用默认情况定义的常规 lambda 结构，但我们可以传递一个关键字参数来调用这个特殊方法。

.. code-block::

  * (dlambda-test :something-special)
  SPECIAL
  NIL

一个关键特性(后面的章节将会大量利用)是，默认方法和所有特殊方法当然都是在包含 ``dlambda`` 的词法上下文中调用的。由于 ``dlambda`` 与 ``lambda`` 表示法集成得非常紧密，这使得我们可以将多方法技术引入到创建和扩展词法闭包的领域。