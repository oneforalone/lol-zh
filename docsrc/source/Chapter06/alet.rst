.. _alet:

==================================
6.3 ``alet`` 和有限状态机
==================================

:Author: Doug Hoyte
:Translator: Yuqi Liu <yuqi.lyle@outlook.com>

对 ``lambda`` 和 ``if``，只有一个有用的回指配置，但却是最有趣的回指宏类型，该宏是以不可预见的方式展开。本节 —— 甚至本章的大部分内容 —— 都是基于这样一个宏：``alet``。有哪些额外的绑定对 let 结构主体中的结构有用呢？ ``let`` 的目的就是创建这样的绑定，这样就可以捕获给 let 的变量引入。但是， ``let`` 宏的增强可以完全访问提供给它的所有结构，甚至是用新绑定计算的表达式体。那么主体中最有用的部位是什么呢？在大多数情况下，主体中最有用的部分就是主体中的最后一个结构，因为该结构的结果将从 let 语句本身返回。我们已经看到，当返回一个引用 ``let`` 创建的绑定的 lambda 表达式时，结果是一个词法闭包 —— 一个通常存储并用于以后访问 let 语句中的变量的对象。因此，扩展我们的闭包对象模拟，``alet%`` 宏的行为与 ``let`` 特殊结构完全相似，除了 ``alet%`` 从主体中捕获符号 ``this`` 并将其绑定到该结构主体中的最后一个表达式 —— 作为闭包返回的表达式。

.. code-block::

  (defmacro alet% (letargs &rest body)
    `(let ((this) ,@letargs)
      (setq this ,@(last body))
      ,@(butlast body)
      this))

当在 lambda 结构中有初始化的代码，且不想重复初始化时，``alet%`` 就很有用。因为 ``this`` 绑定到要返回的 lambda 结构，所以我们可以在外围 let 返回它之前执行它。下面是一个闭包，它的构造展示了个简单的 ``alet%`` 例子，避免了重复它的重置和初始化代码：

.. code-block::

  * (alet% ((sum) (mul) (expt))
      (funcall this :reset)
      (dlambda
        (:reset ()
          (psetq sum 0
                mul 1
                expt 2))
        (t (n)
          (psetq sum (+ sum n)
                mul (* mul n)
                expt (expt expt n))
          (list sum mul expt))))
  #<Interpreted Function>

我们可以依次调用它来改变 ``sum``、``mul`` 和 ``expt`` 的值:

.. code-block::

  * (loop for i from 1 to 5 collect (funcall * 2))

  ((2 2 4)
  (4 4 16)
  (6 8 256)
  (8 16 65536)
  (10 32 4294967296))

现在也可以调用 ``:reset`` 方法来重置这个闭包。注意多亏了 ``alet%``，这里只需要在一个位置写重置的基本情况（将 ``sum`` 置为 0，``mul`` 置为 1， ``expt`` 置为 2）：

.. code-block::

  * (funcall ** :reset)

  NIL

现在，该闭包中的值都被重置了，我们从一开始就可以看到个新的序列了：

.. code-block::

  * (loop for i from 1 to 5 collect (funcall *** 0.5))

  ((0.5 0.5 1.4142135)
  (1.0 0.25 1.1892071)
  (1.5 0.125 1.0905077)
  (2.0 0.0625 1.0442737)
  (2.5 0.03125 1.0218971))

值得一提的是 ``alet%`` 改变了 let 主体中结构的计算顺序。如果你去看一下这个的展开的话，你就会发现主体中最后一个结构实际上是第一个执行的，随后该结果在其他结构执行前会绑定到词法变量 ``this`` 上。然而，只要最后一个参数是常量，这个重新排序不会产生差异。记住，lambda 表达式是个常量，因此特别适合用在 ``alet%`` 中。

与许多宏增强一样，因为有许多可用的自由度，对该宏的改进是违反直觉的。虽然有许多可能性，但本节将考虑其中一种具体的改进。可以让 ``alet%`` 不返回其主体的最后一个结构（预期是 lambda 结构)，而是返回一个在 let 结构词法作用域内查找另一个函数的函数，然后调用该函数。这有时又被称为间接调用（indirection），因为返回的是一个使用指针解引用查找函数的函数，然后使用该函数，而不是返回一个函数来执行某些操作。间接是个在编程语言中普遍存在的概念。它允许我们在运行时不间接地修改、编译时修复内容。Lisp 有比许多其他编程语言更简洁、更有效的方式使用间接方法。 ``alet`` 是 ``alet%`` 加入了间接调用的版本，可以返回的闭包函数正在被 alet 主体内部的代码访问或替换，或者，如果用 ``dlambda`` 的话（很快就会介绍），甚至可以在 alet 主体外部被替换。

现在可以用 ``alet`` 宏更改在调用闭包时执行的函数了，我们可以使用名为 alet over alambda 的模式创建一对相互引用的函数。只要所有的状态都变回原来的状态 —— 而不是相互转换 —— alet over alambda 就是指定无名状态机的一种便携的方法。

下面就是个典型的计数器闭包，接收参数 ``n``，当传递符号 ``invert`` 作为参数而不是数字时，它的方向可以在递增和递减之间按 ``n`` 切换：

.. code-block::

  * (alet ((acc 0))
      (alambda (n)
        (if (eq n 'invert)
          (setq this
                (lambda (n)
                  (if (eq n 'invert)
                    (setq this #'self)
                    (decf acc n))))
          (incf acc n))))

  #<Interpreted Function>

让我们把这个闭包保存起来，以便我们随时可以使用：

.. code-block::

  * (setf (symbol-function 'alet-test) *)

  #<Interpreted Function>

开始时，是增加的：

.. code-block::

  * (alet-test 10)

  10

但是，可以通过将符号  ``invert`` 传递给闭包来改变要调用内部 lambda 表达式的实际函数：

.. code-block::

  * (alet-test 'invert)

  #<Interpreted Function>

现在就变成递减了：

.. code-block::

  * (alert-test 3)

  7

最后，多亏了 ``alambda`` 的 ``self`` 绑定，我们可以用 ``invert`` 参数再次修改函数：

.. code-block::

  * (alert-test 'invert)

  #<Interpreted Function>

又回到了刚开始时的状态，递增：

.. code-block::

  * (alert-test 5)

  12

这个闭包被绑定到函数命名空间 ``alet-test`` 上了。但和常规的闭包略有不同。虽然这个闭包和常规闭包都是指向单个环境的指针，这个环境可以有任意数量的引用，但这个闭包使用间接方法来更改调用时运行的代码段。尽管可以插入任何一段代码，但只有 ``alet`` 的词法范围内的代码(即具有 ``this`` 回指符的代码)才能访问它的词法绑定。但是，仍然不能阻止我们插入个新的闭包，它有自己的词法绑定，可能还会因为在 ``alet`` 插入的间接环境中改变行为。本章剩下的大部分内容是使用 ``alet`` 创建的间接环境做些有用的事情。

一种常见的宏技术被非正式地称为将宏内部打开（turning a macro inside out）。当打开一个宏时，可以选择一个典型的结构，该结构使用与想要创建的宏类似的宏，并将其展开。然后使用该展开作为所需宏的模板。例如，我们希望有一种比前面介绍的 alet over alambda 计数器更通用的方法来创建具有多个状态的闭包。下面是上面由内而外展开的可逆计数器 alambda 用例:

.. code-block::

  * (macroexpand
    '(alambda (n)
        (if (eq n 'invert)
          (setq this
                (lambda (n)
                  (if (eq n 'invert)
                    (setq this #'self)
                    (decf acc n))))
          (incf acc n))))

  (LABELS ((SELF (N)
            (IF (EQ N 'INVERT)
              (SETQ THIS
                    (LAMBDA (N)
                      (IF (EQ N 'INVERT)
                        (SETQ THIS #'SELF)
                        (DECF ACC N))))
              (INCF ACC N))))
    #'SELF)

如果稍微重构上面的展开，利用标签来创建多个函数绑定的事实，将会得到以下结果：

.. code-block::

  (alet ((acc 0))
    (labels ((going-up (n)
              (if (eq n 'invert)
                (setq this #'going-down)
                (incf acc n)))
            (going-down (n)
              (if (eq n 'invert)
               (setq this #'going-up)
               (incf acc (- n)))))
    #'going-up))

通过这个例子，我们注意到 ``alambda`` 能使用 ``labels`` 这个特殊的结构让其所有绑定对它的函数主体都可用。还有就是，现在已经很完整的最终版宏的模版了。

.. code-block::

  (defmacro alet-fsm (&rest states)
    `(macrolet ((state (s)
                  `(setq this #',s)))
        (labels (,@states) #',(caar states))))

``alet-fsm`` 提供了一种便携的语法，该语法可以用来表达闭包存在的多种可能状态。就像是在 ``labels`` 上的宏包裹了一层薄薄的糖衣，并结合了代码遍历 ``macrolet`` 的变形，该变形可以假装有个 ``state`` 函数，用来改变闭包的当前状态，该函数通过 ``alet`` 提供的 ``this`` 回指来访问。下面是可逆计数器的更简洁的版本的例子：

.. code-block::

  (alet ((acc 0))
    (alet-fsm
      (going-up (n)
        (if (eq n 'invert)
          (state going-down)
          (incf acc n)))
      (going-down (n)
        (if (eq n 'invert)
          (state going-up)
          (decf acc n)))))

``alet-fsm`` 是之前没有见过的例子：回指注入（anaphor injection）。使用这种隐喻在很多方面违反了词法透明性，以至于它实际上在某种程度上是词法不可见的（lexically invisible）。``alet`` 不仅无形地绑定了 ``this``，而且 ``alet-fsm`` 宏对 ``this`` 的使用也是同样隐形的。 ``alet-fsm`` 将一个自由变量插入到词法上下文中，而在词汇上下文中根本看不到它。

这其中的格式问题是不确定的，当然，宏编程与格式无关。这关乎性能。有时，插入自由变量可以在两个宏之间创建共生关系 —— 它可以更好地以编程方式构造扩展，而不是两个独立的扩展。由于这种宏编程非常复杂，因此可以与 C 指针进行类比。就像学习 C 指针会产生可疑的风格建议一样，自由变量插入也是如此。

对于自由变量插入难以理解的原因，最合理的假设是它的故障安全行为。有了回指，如果用户代码没有使用绑定，那么代码很可能会继续工作，不管是否希望它这样做。它可能已经悄无声息地失败了，因此不安全。然而，当插入一个自由变量，并且没有捕获它的环境时，整个表达式就释放了。当这种情况发生时，需要在计算表达式之前决定要做什么。因为它有故障安全。

除了格式之外，自由变量插入有时正是两个相关宏来回通信时所需要的。插入和回指的操作其实是一样的，只是方向相反。因为在宏之间打开了一个新的沟通渠道，复杂性问题的扩展速度甚至更快。想象一下坐在一个满是易碎玻璃的房子里。你可以放心地向房子外面的人扔东西，即使这些东西不用费心去抓，但你最好确保你能抓住扔向你的东西。