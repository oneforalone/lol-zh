.. _chapter06:

***********************************
第六章：回指（Anaphoric）宏
***********************************


.. _6-1-more-phors:

6.1 More Phors?
====================

Paul Graham 的 OnLisp_ 中一些有趣的宏是回指宏（anaphoric macros）。回指宏是特意从
宏结构中捕捉变量的宏。多亏了它们的显示声明，这些特意捕获的变量为我们提供了控制宏展开
的窗口。通过这些窗口，可以用组合操作来控制宏展开。

OnLisp_ 中标准的回指宏的命名是根据 anaphor 和它的复数形式 anaphora 的字面意思。回指
是种捕获 U 语言的一个空闲单词，然后将这个单词用在之后的 U 语言中的方法。在编程术语中，
实现典型的回指意味着在代码中——或者即将编写的代码中——找到可以利用之前表达式结果的位置。
Graham 的回指和相关代码值得深究。特别是宏 **defanaph**，它支持一些有趣的自动回指
（automatic anaphor）编程类型。

经过一段时间的使用，发现 OnLisp_ 中最好用的回指宏就是 **alambda**。它也是个最简单和
最优雅的回指宏及特意捕捉变量的展示。

.. code-block:: lisp

    ;; Graham 's alambda
    (defmacro alambda (parms &body body)
      `(labels ((self ,parms ,@body))
          #'self))

通过 **alambda**，我们捕捉到 **self** 这个变量名，所以就可以在之后构建的匿名函数中引用这个
变量。换句话说，递归就像调用 **self** 一样简单。例如，下面的函数返回从 **n** 到 1 的一个列
表：

.. code-block:: lisp

    (alambda (n)
      (if (> n 0)
        (cons
          n
          (self (- n 1)))))

**alambda** 让代码更直观、更易读，并会改变我们的思维，即匿名函数是否应该像添加一个字母
那样容易调用自己。因为 **alambda** 对 **self** 绑定的显示定义，而且使用 **alambda** 的唯
一原因就是利用这个绑定，所以不会出现异常变量捕获的问题。

OnLisp* 中的另一个方便的回指宏是 **aif**，这个宏将测试子句的结果绑定到 **it** ，然后在
true 子句(辅助子句或结果子句)使用中使用。**aif** 用了一个 COMMON LISP 很重要的特性：
广义布尔值。在 COMMON LISP 中，任何非空值都是一个真布尔值，所以 COMMON LISP 程序员
通常会在真值中嵌入有趣的信息。保留了真值和假值的语言(尤其是Scheme)使用显式布尔值，
有时会强制抛出额外的信息来满足冗余的类型约束。Scheme 实际上添加了个组装程序（kludge），
让 **if**、**cond**、**and**、**or** 和 **do** 接受非布尔值。当然，COMMON LISP 的设计
才是对的 —— 一切都是布尔值。

.. code-block:: lisp

    ;; Graham 's aif
    (defmacro aif (test then &optional else)
      `(let ((it ,test))
        (if it ,then ,else)))

还必须指出，**aif** 和 **alambda**，就像所有的回指宏一样，违反了词法透明。用现在流行的话来
说，它们是不正常（unhygienic）的宏。也就是说，像本书中的许多宏一样，回指宏无形地引入了词法绑
定，因此不能在严格执行安全的宏系统中创建。即使是绝大多数 Scheme 系统，这个在安全方面试验最多的
平台，也有不安全的的 defmacro 风格的宏 —— 大概是因为就连Scheme的实现这也不太重视安全吧。
就像自行车上的辅助轮一样，在大多数情况下安全系统都是要丢掉的玩具，即使只掌握了一点技能。

是的，还有很多关于特定变量捕获的有趣的事可以做。还有很多 phors。本书和 Graham 的 OnLisp_
只描述了这种技术内在潜力的一小部分。更多不可思议的创造将会出现在回指宏的智能应用中。

在穿插的阅读宏，简短地介绍了下回指宏后，本章的剩余部分将介绍现代的、具体的回指宏应用，
一个与本书的中心主旨相关的：词汇闭包 —— let over lambda。本章的大部分内容将介绍一些用于
定制、调整和扩展闭包的有趣的回指宏。虽然这些主题在实际代码中的应用非常实用，但它们的
主要目的是一个讨论回指宏的属性和变化的平台。用宏来扩展闭包这个概念是当前的一个热点
研究课题。


.. _6-2-sharp-backquote:

6.2 Sharp-Backquote：#`
===========================

尽管大多数回指是由常规宏引入的，但 read 宏也有可能引入代码，隐形地创建绑定。当 read
宏这样做时，它们被称为读回指（read anaphora）。本节介绍了一个这样的 read 宏，虽然
它本身很短，但它却是本书中最有用的宏之一，连我自己都感到惊讶。我已经尽可能快地引入
了这个宏，以便它可以用于其余的代码。已经有几个宏使用了它

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

反引号就是个 read 宏，将输入读成 lambda 结构。默认情况下，这个 lambda 结构只接收一个
参数：**a1**。然后这个 read 宏递归地调用对应流的 **read** 函数。下面是一个停止求值(通过
引用)的例子，这样就可以直接地观察着个回指读宏了：

.. code-block:: none

    * '#`((,a1))

    (LAMBDA (A1)
      `((,A1)))

该 read 宏抽象出一个通用宏模式。例如，如果有一个列表变量，并且要用 let 来绑定这个列表，
将每个变量绑定到一个符号(假设为 **empty** )，就可以像这样使用 **mapcar**：

.. code-block:: none

    * (mapcar (lambda (a)
                (list a ''empty))
        '(var-a var-b var-c))

    ((VAR-A 'EMPTY)
    (VAR-B 'EMPTY)
    (VAR-C 'EMPTY))

但特别是对复杂的列表结构，这样写就显得有点乱，所以 lisp 程序员喜欢用反引号将其引用提高
一层：

.. code-block:: none

    * (mapcar (lambda (a)
                `(,a 'empty))
        '(var-a var-b var-c))

    ((VAR-A 'EMPTY)
    (VAR-B 'EMPTY)
    (VAR-C 'EMPTY))

新的 read 回指宏隐藏了 lambda 结构：

.. code-block:: none

    * (mapcar #`(,a1 'empty)
        '(var-a var-b var-c))

    ((VAR-A 'EMPTY)
    (VAR-B 'EMPTY)
    (VAR-C 'EMPTY))

上述代码中 **a1** 变量中的 **1** 的原因是，read 宏的使用这可以通过提供的 **numarg** 的数
字来引入不同的回指数字：

.. code-block:: none

    * '#2`(,a1 ,a2)

    (LAMBDA (A1 A2)
      `(,A1 ,A2))

所以我们可以同时在多个表达式中 **mapcar** sharp-backquote（#`) 表达式：

.. code-block:: none

    * (let ((vars '(var-a var-b var-c)))
        (mapcar #2`(,a1 ',a2)
          vars
          (loop for v in vars
                collect (gensym
                          (symbol-name v)))))

    ((VAR-A '#:VAR-A1731)
    (VAR-B '#:VAR-B1732)
    (VAR-C '#:VAR-C1733))

另一种考虑 sharp-backquote 的方法是，它像 **format** 函数一样将插值（interpolation）作
为字符串插值列出。就像 **format** 让我们使用带有接口（slot）的模板，接口用单独的参数值填充
一样，sharp-backquote 让我们将列表插值的结构与想要拼接的值分开。由于前面描述的列表中
函数位置的 lambda 结构和 **lambda** 宏展开成函数的 lambda 结构之间的语法二元性，还可以
使用sharp-backquote 作为函数调用中的第一个元素：

.. code-block:: none

    * (#3`(((,a1)) ,@a2 (,a3))
          (gensym)
          '(a b c)
          'hello)

    (((#:G1734)) A B C (HELLO))

与 **format** 不同的是，sharp-quote 不用顺序定位，而是用回指绑定的数字。因此，顺序可以
打乱，甚至可以在绑定中多次拼接：

.. code-block:: none

    * (#3`(((,@a2)) ,a3 (,a1 ,a1))
          (gensym)
          '(a b c)
          'hello)

    (((A B C)) HELLO (#:G1735 #:G1735))

思考：**gensym** 生成的 **#:G1735** 的引用看起来是指向同一个符号，但是，当然，通过查看
它们的打印名称，永远无法真正地分辨出 **gensym**。这些符号相等（ **eq** ）吗？相等或不相
等的原因是什么？


.. _6-3-alet-and-finite-state-machines:

6.3 **alet** 和有限状态机
==================================

对 **lambda** 和 **if**，只有一个有用的回指配置，但却是最有趣的回指宏类型，该宏是以
不可预见的方式展开。本节 —— 甚至本章的大部分内容 —— 都是基于这样一个宏：**alet**。
有哪些额外的绑定对 let 结构主体中的结构有用呢？ **let** 的目的就是创建这样的绑定，
这样就可以捕获给 let 的变量引入。但是， **let** 宏的增强可以完全访问提供给它的所有
结构，甚至是用新绑定计算的表达式体。那么主体中最有用的部位是什么呢？在大多数情况
下，主体中最有用的部分就是主体中的最后一个结构，因为该结构的结果将从 let 语句本身
返回。我们已经看到，当返回一个引用 **let** 创建的绑定的 lambda 表达式时，结果是一个
词法闭包 —— 一个通常存储并用于以后访问 let 语句中的变量的对象。因此，扩展我们的
闭包对象模拟，**alet%** 宏的行为与 **let** 特殊结构完全相似，除了 **alet%** 从主体中捕获
符号 **this** 并将其绑定到该结构主体中的最后一个表达式 —— 作为闭包返回的表达式。

.. code-block:: lisp

    (defmacro alet% (letargs &rest body)
      `(let ((this) ,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        this))

当在 lambda 结构中有初始化的代码，且不想重复初始化时，**alet%** 就很有用。因为
**this** 绑定到要返回的 lambda 结构，所以我们可以在外围 let 返回它之前执行它。
下面是一个闭包，它的构造展示了个简单的 **alet%** 例子，避免了重复它的重置和
初始化代码：

.. code-block:: none

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

我们可以依次调用它来改变 **sum**、**mul** 和 **expt** 的值:

.. code-block:: none

    * (loop for i from 1 to 5 collect (funcall * 2))

    ((2 2 4)
    (4 4 16)
    (6 8 256)
    (8 16 65536)
    (10 32 4294967296))

现在也可以调用 **:reset** 方法来重置这个闭包。注意多亏了 **alet%**，这里只需要在一个
位置写重置的基本情况（将 **sum** 置为 0，**mul** 置为 1， **expt** 置为 2）：

.. code-block:: none

    * (funcall ** :reset)

    NIL

现在，该闭包中的值都被重置了，我们从一开始就可以看到个新的序列了：

.. code-block:: none

    * (loop for i from 1 to 5 collect (funcall *** 0.5))

    ((0.5 0.5 1.4142135)
    (1.0 0.25 1.1892071)
    (1.5 0.125 1.0905077)
    (2.0 0.0625 1.0442737)
    (2.5 0.03125 1.0218971))

值得一提的是 **alet%** 改变了 let 主体中结构的计算顺序。如果你去看一下这个的展开的话，
你就会发现主体中最后一个结构实际上是第一个执行的，随后该结果在其他结构执行前会绑定
到词法变量 **this** 上。然而，只要最后一个参数是常量，这个重新排序不会产生差异。记住，
lambda 表达式是个常量，因此特别适合用在 **alet%** 中。

与许多宏增强一样，因为有许多可用的自由度，对该宏的改进是违反直觉的。虽然有许多
可能性，但本节将考虑其中一种具体的改进。可以让 **alet%** 不返回其主体的最后一个
结构（预期是 lambda 结构)，而是返回一个在 let 结构词法作用域内查找另一个函数的
函数，然后调用该函数。这有时又被称为间接调用（indirection），因为返回的是一个
使用指针解引用查找函数的函数，然后使用该函数，而不是返回一个函数来执行某些操作。
间接是个在编程语言中普遍存在的概念。它允许我们在运行时不间接地修改、编译时
修复内容。Lisp 有比许多其他编程语言更简洁、更有效的方式使用间接方法。 **alet**
是 **alet%** 加入了间接调用的版本，可以返回的闭包函数正在被 alet 主体内部的代码
访问或替换，或者，如果用 **dlambda** 的话（很快就会介绍），甚至可以在 alet 主体
外部被替换。

现在可以用 **alet** 宏更改在调用闭包时执行的函数了，我们可以使用名为 alet over
alambda 的模式创建一对相互引用的函数。只要所有的状态都变回原来的状态 ——
而不是相互转换 —— alet over alambda 就是指定无名状态机的一种便携的方法。

下面就是个典型的计数器闭包，接收参数 **n**，当传递符号 **invert** 作为参数而不是
数字时，它的方向可以在递增和递减之间按 **n** 切换：

.. code-block:: none

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

.. code-block:: none

    * (setf (symbol-function 'alet-test) *)

    #<Interpreted Function>

开始时，是增加的：

.. code-block:: none

    * (alet-test 10)

    10

但是，可以通过将符号  **invert** 传递给闭包来改变要调用内部 lambda 表达式的实际函数：

.. code-block:: none

    * (alet-test 'invert)

    #<Interpreted Function>

现在就变成递减了：

.. code-block:: none

    * (alert-test 3)

    7

最后，多亏了 **alambda** 的 **self** 绑定，我们可以用 **invert** 参数再次修改函数：

.. code-block:: none

    * (alert-test 'invert)

    #<Interpreted Function>


又回到了刚开始时的状态，递增：

.. code-block:: none

    * (alert-test 5)

    12

这个闭包被绑定到函数命名空间 **alet-test** 上了。但和常规的闭包略有不同。虽然这个
闭包和常规闭包都是指向单个环境的指针，这个环境可以有任意数量的引用，但这个闭包
使用间接方法来更改调用时运行的代码段。尽管可以插入任何一段代码，但只有 **alet**
的词法范围内的代码(即具有 **this** 回指符的代码)才能访问它的词法绑定。但是，仍然
不能阻止我们插入个新的闭包，它有自己的词法绑定，可能还会因为在 **alet** 插入的
间接环境中改变行为。本章剩下的大部分内容是使用 **alet** 创建的间接环境做些有用
的事情。

一种常见的宏技术被非正式地称为将宏内部打开（turning a macro inside out）。
当打开一个宏时，可以选择一个典型的结构，该结构使用与想要创建的宏类似的宏，
并将其展开。然后使用该展开作为所需宏的模板。例如，我们希望有一种比前面介绍
的 alet over alambda 计数器更通用的方法来创建具有多个状态的闭包。下面是上面
由内而外展开的可逆计数器 alambda 用例:

.. code-block:: none

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

.. code-block:: lisp

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

通过这个例子，我们注意到 **alambda** 能使用 **labels** 这个特殊的结构让其所有绑定
对它的函数主体都可用。还有就是，现在已经很完整的最终版宏的模版了。

.. code-block:: lisp

    (defmacro alet-fsm (&rest states)
      `(macrolet ((state (s)
                    `(setq this #',s)))
          (labels (,@states) #',(caar states))))

**alet-fsm** 提供了一种便携的语法，该语法可以用来表达闭包存在的多种可能状态。
就像是在 **labels** 上的宏包裹了一层薄薄的糖衣，并结合了代码遍历 **macrolet**
的变形，该变形可以假装有个 **state** 函数，用来改变闭包的当前状态，该函数通过
**alet** 提供的 **this** 回指来访问。下面是可逆计数器的更简洁的版本的例子：

.. code-block:: lisp

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

**alet-fsm** 是之前没有见过的例子：回指注入（anaphor injection）。使用这种
隐喻在很多方面违反了词法透明性，以至于它实际上在某种程度上是词法不可见的
（lexically invisible）。**alet** 不仅无形地绑定了 **this**，而且 **alet-fsm** 宏对
**this** 的使用也是同样隐形的。 **alet-fsm** 将一个自由变量插入到词法上下文中，
而在词汇上下文中根本看不到它。

这其中的格式问题是不确定的，当然，宏编程与格式无关。这关乎性能。有时，插入
自由变量可以在两个宏之间创建共生关系 —— 它可以更好地以编程方式构造扩展，
而不是两个独立的扩展。由于这种宏编程非常复杂，因此可以与 C 指针进行类比。
就像学习 C 指针会产生可疑的风格建议一样，自由变量插入也是如此。

对于自由变量插入难以理解的原因，最合理的假设是它的故障安全行为。有了回指，
如果用户代码没有使用绑定，那么代码很可能会继续工作，不管是否希望它这样做。
它可能已经悄无声息地失败了，因此不安全。然而，当插入一个自由变量，并且
没有捕获它的环境时，整个表达式就释放了。当这种情况发生时，需要在计算表达式
之前决定要做什么。因为它有故障安全。

除了格式之外，自由变量插入有时正是两个相关宏来回通信时所需要的。插入和
回指的操作其实是一样的，只是方向相反。因为在宏之间打开了一个新的沟通渠道，
复杂性问题的扩展速度甚至更快。想象一下坐在一个满是易碎玻璃的房子里。你可以
放心地向房子外面的人扔东西，即使这些东西不用费心去抓，但你最好确保你能
抓住扔向你的东西。


.. _6-4-indirection-chains:

6.4 间接链
==========================

有很多方法来使用 **alet** 的 **this** 回指。由于环境是通过虚拟闭包来访问的，该闭包将所有
调用转发给 **this** 所指向的真实闭包，所以可以随意的引用这个虚拟闭包，根据需要复制它。
这样的间接方法很有用，因为可以更改调用这个虚拟闭包时发生的事情，而不必更改对虚拟
闭包的引用。

.. code-block:: lisp

    (defmacro! ichain -before (&rest body)
      `(let ((,g!indir-env this))
        (setq this
          (lambda (&rest ,g!temp-args)
            ,@body
            (apply ,g!indir -env
                  ,g!temp-args)))))

**ichain-before** 会展开成 **alet** 结构。添加了一个新的代码体，以便在调用主闭包之前执行。
回到计数器例子，**ichain-before** 添加了个新的闭包，会在闭包 **acc** 变量增加前打印出它
的值：

.. code-block:: none

    * (alet ((acc 0))
        (ichain-before
          (format t "Changing from ~a~%" acc))
        (lambda (n)
          (incf acc n)))

    #<Interpreted Function>

和设想的一样：

.. code-block:: none

    * (funcall * 2)
    Changing from 0
    2
    * (funcall ** 2)
    Changing from 2
    4

不过，把 chain 放在 **ichain-before** 这个名字中是有原因的。让尽可能多的闭包来执行：

.. code-block:: none

    * (alet ((acc 0))
        (ichain-before
          (format t "A~%"))
        (ichain-before
          (format t "B~%"))
        (ichain-before
          (format t "C~%"))
        (lambda (n)
          (incf acc n)))

    #<Interpreted Function>

在链中每添加一个新链接都会将该链接添加到链的头部，导致访问链接的顺序与添加链接的顺序
相反：

.. code-block:: none

    * (funcall * 2)
    C
    B
    A
    2

在更改宏以避免通过添加新的周围代码来重新构造宏时，静态添加间接链有时很用的。但在动态
添加间接链时，最有趣的可能性就会出现。因为可以在运行时创建新的闭包，还可以通过回指访
问闭包的内部，所以可以重写函数在运行时的工作方式。下面是一个简单的例子，每个闭包调用
都会添加另一段代码，在运行时输出 “Hello world”：

.. code-block:: none

    * (alet ((acc 0))
        (lambda (n)
          (ichain-before
            (format t "Hello world~%"))
          (incf acc n)))

    #<Interpreted Function>

每次调用都会向间接链添加一个新的闭包：

.. code-block:: none

    * (loop for i from 1 to 4
        do
          (format t "~:r invocation:~%" i)
          (funcall * i))
    first invocation:
    second invocation:
    Hello world
    third invocation:
    Hello world
    Hello world
    fourth invocation:
    Hello world
    Hello world
    Hello world

**ichain-after** 宏与 **ichain-before** 宏相似，不同之处是 **ichain-after** 将闭包
添加到执行链的另一端：在主闭包被调用之后。**ichain-after** 用了 **prog1**， **prog1**
连续执行里面的代码结构，然后返回第一个结构的求值结果。

.. code-block:: lisp

    (defmacro! ichain -after (&rest body)
      `(let ((,g!indir-env this))
          (setq this
            (lambda (&rest ,g!temp-args)
              (prog1
                (apply ,g!indir -env
                      ,g!temp-args)
                ,@body)))))

**ichain-before** 和 **ichain-after** 可以组合在一起，让 before 结构在主闭包计算之前执
行，after 结构在主闭包计算之后执行:

.. code-block:: none

    * (alet ((acc 0))
        (ichain-before
          (format t "Changing from ~a~%" acc))
        (ichain-after
          (format t "Changed to ~a~%" acc))
        (lambda (n)
          (incf acc n)))

    #<Interpreted Function>
    * (funcall * 7)
    Changing from 0
    Changed to 7
    7

**ichain-before** 和 **ichain-after** 是将自由变量插入其展开的宏。这两个宏插入了
**this** 变量，所依赖的这个变量会被 **alet** 宏的展开捕获。这种类型的变量插入可能看起来格式
不好或容易出错，但实际上是一种常见的宏技术。事实上，几乎所有的宏都向展开中插入了变量。例如，除
了 **this**，宏 **ichain-before** 还会插入像 **let**、**setq** 和 **lambda** 这样的
符号，来拼接到宏展开的任何地方。这样的符号和预定义的符号（如 **setq** ）之间的区别在于，
**lambda** 总是指向一个易于理解的 ANSI 宏，而这样的符号可以指向不同的东西，这取决于它们的展
开环境。

在初始闭包表达式执行之前或之后对代码进行标记时，**ichain-before** 和 **ichain-after** 很
有用的，但这绝不是 **this** 回指唯一能做的。另一个常见的任务是在调用闭包之后检查闭包数据的
有效性。

.. code-block:: lisp

    (defmacro! ichain -intercept% (&rest body)
      `(let ((,g!indir-env this))
        (setq this
            (lambda (&rest ,g!temp-args)
              (block intercept
                (prog1
                  (apply ,g!indir -env
                        ,g!temp-args)
                  ,@body))))))

**ichain-intercept%** 是另一个用在 **alet** 中的宏。设想是，希望能够拦截闭包的调用，并验
证执行的操作没有导致闭包中的某种不一致状态。所以我们可以像这样在常规的计数器闭包中添加一个拦截：

.. code-block:: none

    * (alet ((acc 0))
        (ichain-intercept%
          (when (< acc 0)
            (format t "Acc went negative~%")
            (setq acc 0)
            (return-from intercept acc)))
        (lambda (n)
          (incf acc n)))

    #<Interpreted Function>

当计数器低于 0 时，**ichain-intercept%** 插入的代码将告警：

.. code-block:: none

    * (funcall * -8)
    Acc went negative
    0

计数器被重置为 0 ：

.. code-block:: none

    * (funcall ** 3)

    3

**ichain-intercept%** 最有趣的地方是，引入了 **intercept** 的块回指（block anaphor）。
可以用 **return-from** 来调用这个回指。代码块将从闭包调用中返回这个值，拦截原始值。

.. code-block:: lisp

    (defmacro! ichain -intercept (&rest body)
      `(let ((,g!indir-env this))
        (setq this
            (lambda (&rest ,g!temp-args)
              (block ,g!intercept
                (macrolet ((intercept (v)
                          `(return -from
                          ,',g!intercept
                          ,v)))
                  (prog1
                    (apply ,g!indir-env
                          ,g!temp-args)
                    ,@body )))))))

相反，**ichain-intercept** 创建了个本地宏，该宏允许 **ichain-intercept** 中的代码使用
**intercept** 展开成一个由 gensym 指定的 **return-from**。

.. code-block:: none

    * (alet ((acc 0))
        (ichain-intercept
          (when (< acc 0)
            (format t "Acc went negative~%")
          (setq acc 0)
          (intercept acc)))
        (lambda (n)
          (incf acc n)))

    #<Interpreted Function>

这和 **ichain-intercept%** 工作原理一样：

.. code-block:: none

    * (funcall * -8)
    Acc went negative
    0
    * (funcall ** 3)
    3

当然，将所有这些闭包透明地引入操作会影响运行时性能。幸运的是，现代 lisp 编译器擅长优化闭包。
如果应用程序可以忍受几个指针解引（通常是可以的），那么间接链可能是构建它的最佳方式。关于
间接链的另一种有趣的思考方式，请参阅第 :ref:`7-4-pointer-scope` 。还可以查看 CLOS 的
**before**、**after** 和 **around** 函数。


.. _6-5-hotpatching-closures:

6.5 热修复闭包
=============================

本节的目的有三个。首先，介绍 **alet** 中 **this** 回指的另一个有趣的用法。其次，讨论了
*let over dlambda* 。最后，介绍了一种很有用的宏技术，称为回指闭合（*anaphor closing*）。
要详细说明回指闭合，将不用 **alet** 宏，而是使用一个由内而外的展开。**alet-hotpatch%**
是 **alet** 的拓展，有个一个特殊的 lambda 结构。该 **lambda** 结构检查第一个参数是否为关
键字符号 **:hotpatch**，如果是，则用另一个参数替换间接闭包。

.. code-block:: none

    (defmacro alet-hotpatch% (letargs &rest body)
      `(let ((this) ,@letargs)
          (setq this ,@(last body))
          ,@(butlast body)
          (lambda (&rest args)
            (if (eq (car args) ':hotpatch)
              (setq this (cadr args))
              (apply this args)))))

在运行时更改另一个转发闭包中使用的闭包称为热补丁（ *hotpatching* ）。例如，这里我们创建
了一个热补丁闭包，并将其存储在 **hotpatch-test** 的 **symbol-function** 单元格中，以便
之后使用：

.. code-block:: none

    * (setf (symbol-function 'hotpatch-test)
        (alet-hotpatch% ((acc 0))
          (lambda (n)
            (incf acc n))))

    #<Interpreted Function>

现在可以这样使用:

.. code-block:: none

    * (hotpatch-test 3)

    3
    * (hotpatch-test 4)

    7

可以用 **:hotpatch** 和替换函数或闭包来替换 lambda 结构及其相关的环境:

.. code-block:: none

    * (hotpatch-test
        :hotpatch
        (let ((acc 0))
          (lambda (n)
            (incf acc (* 2 n)))))

    #<Interpreted Function>

现在闭包有了新的、热补丁的行为：

.. code-block:: none

    * (hotpatch-test 2)

    4
    * (hotpatch-test 5)

    14

注意计数器的值是怎么置为 0 的，因为我们还用计数器的累加器 **acc** 的新值热补丁了闭包的环境。
之前见过这种关键字符号的运行时解构吗？没错，实际上在 :ref:`5-7-dlambda` 中编写了个宏来完成这个操
作。**alet-hotpatch** 是 **alet-hotpatch%** 的 **dlambda** 版本。有时甚至在没有意识到
的情况下，在新的宏定义中会用到之前定义的宏，这就是宏组合（*macro combination* ）。使用精心设
计的宏可以完全理解扩展，尽管在许多方面可能违背词汇透明性，但不会出现组合问题，因为所有组件都能有
意义地组合在一起。

.. code-block:: lisp

    (defmacro! alet-hotpatch (letargs &rest body)
      `(let ((,g!this) ,@letargs)
        (setq ,g!this ,@(last body))
        ,@(butlast body)
        (dlambda
            (:hotpatch (closure)
              (setq ,g!this closure))
            (t (&rest args)
              (apply ,g!this args)))))

**alet-hotpatch** 创建了个可热补的闭包，但在概念上有一个小缺陷。因为使用
**alet-hotpatch** 的唯一真正原因是创建这种热补丁闭包，但可能忘了，它还将 **this** 回指引入
到所提供的作用域中。当忘了创建的的回指时，就有未知变量捕获问题的风险。为了避免这些问题，可以使用
一种回指闭合的技术。当要结束一个回指时，我们不需要改变回指宏的功能，只是限制他们组合的方式。

因为已经把 **alet** 展开从内到外看了一遍，我们可以在 **alet-hotpatch** 的定义中看到
**this** 回指的创建。同时也因为 **alet-hotpatch** 中用了 **this** 回指实现
**hotpatch** 代码，所以就可以关闭回指，这样 **this** 变量就不再被宏捕获了。通常该如何避免引
入预期之外的绑定？当然，可以用 gensyms 来命名绑定。

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

**let-hotpatch** 是将 **this** 回指闭合为一个更包含的版本的示例 —— 一个在只需要进行热补
时，更安全的版本。删掉了名字前面的 **a**，表示这个新宏不再在代码中引入回指。当然，如果出于某种
原因而不是因为热补而想引用 **this**，就应该保留这个回指。

当写了足够多类似的宏后，这种开启和关闭回指的技巧就变成了第二天性。就像在编写宏时，在插入
自由变量时不会想到该如何捕获它们，直到写的词法内容会展开，有时，在开发回指组合和自由变量
插入宏试验时，会放任回指不管。一旦找到了最适用的组合，就可以将宏合并在一起，用 gensyms
替换开发过程中使用的所有回指。像 **let-hotpatch** 一样，该技术可以用 **defmacro!** 将回指
的作用域从宏展开移到宏定义。我们没有从词法上引入回指，而是引入了另一种类型的回指 —— 这种
回指并不是在展开的整个词法作用域内起生效，而只在另一个有限的范围内生效。下节将进一步讲解
这个有效范围。


.. _6-6-sub-lexical-scope:

6.6 子词法作用域
=========================

在 :ref:`3-5-unwanted-capture` 中定义的 **defmacro!** 宏中用了 Graham 的 **flatten** 实用工具来查找代
码中的自动 gensyms。现在是时候承认本书撒的一个小谎了。在此之前，因为没有解释自由变量插入和回
指，我们假设 G-bang 符号名在 **defmacro!** 定义适用于宏定义的词法范围。实质上这是不对的 ——
**defmacro!** 在略微不同类型的作用域（叫做 *子词法作用域 sub-lexical scope* ）下提供了这
些绑定。

.. note::

  G-bang 指的是以 **g!** 开头的变量，**gensym** 是个宏，会自动生成个随机变量名，防止变量名
  突。

记住，作用域是变量的引用是有效的一个范围，而词法作用域是指该名称适用于如 **let** 等绑定
结构的代码。词法作用域和子词法作用域之间的重要区别是，词法作用域包括了 **let** 主体中
代码的所有宏展开。因此，将词法作用域描述为创建只能在绑定结构主体中的代码才能访问的
变量实际上是错误的 —— 宏可以插入变量引用。这些变量是从绑定构造的体外插入的。

通过限制不同访问词法变量的方法来实现真正的文本作用域，会产生子词法作用域。只有当表示
子词法作用域变量的符号出现在宏展开之前传给 lisp 的原始列表中时，对该变量的引用才有效。

因为 **defmacro!** 对给出的代码进行预处理，并在代码展开之前创建所有 G-bang 的列表，所以
G-bang 是子词法绑定。我们不能写一个在 **defmacro!** 中插入 G-bang 符号的的宏，因为
G-bang 的词法绑定从未创建过。下面是 G-bang 的经典用法：

.. code-block:: none

    * (defmacro! junk ()
        `(let ((,g!var))
          ,g!var))

    JUNK

两个 G-bang 变量在 **defmacro!** 的子词法作用域中，所以 **junk** 的展开不出意料是这样的:

.. code-block:: none

    * (macroexpand '(junk))

    (LET ()
      (LET ((#:VAR1663))
        #:VAR1663))
    T

然而，为了探索子词法作用域的概念，我们将定义一个插入 G-bang 的宏：

.. code-block:: none

    * (defmacro injector-for-g!var ()
        ''g!var)

    INJECTOR-FOR-G!VAR

现在可以编写 **junk2**。**junk2** 和 **junk** 基本一致，除了 **junk2** 中将 G-bang 替
换了展开为 G-bang 的宏：

.. code-block:: none

    * (defmacro! junk2 ()
        `(let ((,(injector-for-g!var)))
          ,(injector-for-g!var)))

    JUNK2

但是因为 G-bang 是子词法绑定的 —— 因此不考虑结构的宏展开 —— **defmacro!** 就不会将这些
符号转换成自动 gensym：

.. code-block:: none

    * (macroexpand '(junk2))

    (LET ()
      (LET ((G!VAR))
    G!VAR))
    T

虽然上面的代码仍然可以用，但当有些变量引用在此法作用于中存在，有些不存在时，子词法作用域
内的变量引用可能会破坏表达式：

.. code-block:: none

    * (defmacro! junk3 ()
      `(let ((,g!var))
          ,(injector-for-g!var)))

    JUNK3
    * (macroexpand '(junk3))

    (LET ()
      (LET ((#:VAR1672))
    G!VAR))
    T

子词法作用域在复杂宏中出现的频率惊人。还有 **defmacro!** ，在 :ref:`5-6-recursive-solutions` 中的
**with-all-cxrs** 宏的子词法绑定列表访问器函数中就用到了这个宏。子词法绑定的结果是，不能从宏
展开中引用这种绑定。有时这种访问限制很有用，有时不是。在 **with-all-cxrs** 中，子词法可能被
认为是不可取的。当访问器在 **with-all-cxrs** 的子词法作用域中时，没有问题：

.. code-block:: none

    * (with-all-cxrs
        (cadadadr nil))

    NIL

我们甚至可以编写扩展到这些访问器中的宏，只要宏定义在 **with-all-cxrs** 的子词法范围内:

.. code-block:: none

    * (with-all-cxrs
        (macrolet ((accessor (l)
                    `(cadadadr ,l)))
          (accessor nil)))

    NIL

但要注意，**with-all-cxrs** 是子词法绑定访问器函数的，所以不能定义宏来插入访问器：

.. code-block:: none

    * (macrolet ((accessor (l)
                  `(cadadadr ,l)))
        (with-all-cxrs
          (accessor nil)))

    This function is undefined: CADADADR

既然已经熟悉了回指，并且也见过这么多复杂宏的例子 —— 包括一些使用子词汇范围的宏 ——
我们可以讨论个有趣的理论宏：**sublet**。这个宏设计用来为代码创建子词法绑定，使用的
语法类似于通常的 let 结构语法。与许多 lisp 宏一样，对 **sublet** 的讨论先从一个实用程序
开始。

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

**let-binding-transform** 是个简单的实用工具，用于处理 let 结构绑定单个符号的情况。
在下面代码中，**a** 被归一化为 **(a)**：

.. code-block:: none

    * (let-binding-transform
        '(a (b) (c nil)))

    ((A) (B) (C NIL))

**sublet** 还需要用到 :ref:`5-3-implicit-contexts` 中的 **tree-leaves**。回想一下，
**tree-leaves** 宏有三个参数：一个任意的列表结构，一个用 **x** 变量来确定是否应该更改叶子
的表达式，以及另一个用不同的 **x** 来确定应该更改哪些有效叶子的表达式。


选择隐式化具有相同名称 **x** 的绑定是种有用的 *二元语法* （ *duality of syntax* ）。
当不用通用的方式在表达式中分解公共代码时，有时可以用其他方式使用语法对偶来获得这种简洁的优势。
**sublet** 的定义用到了 :ref:`4-5-cyclic-expressions` 中的自引用读宏。特别是对于像
访问器这样在编写过程中可以多次更改的东西，读宏允许有且只有一种结构来表示访问器。幸亏使用了
隐式的 **tree-leaves** 宏，很容易找到和理解代码重复，因为代码紧密地结合在一起。

.. code-block:: lisp

    (defmacro sublet (bindings% &rest body)
      (let ((bindings (let-binding-transform
                        bindings %)))
        (setq bindings
          (mapcar
            (lambda (x)
              (cons (gensym (symbol -name (car x))) x))
            bindings ))
        `(let (,@(mapcar #'list
                        (mapcar #'car bindings)
                        (mapcar #'caddr bindings)))
          ,@(tree-leaves
              body
              #1=(member x bindings :key #'cadr)
              (caar #1#)))))

**sublet** 接受表示let绑定的结构，并应用 **let-binding-transform**，在这个过程中生成新的
列表结构。然后，将gensym 附加到每个绑定，并使用与绑定名称相对应的打印名。**sublet** 展开
为 let 结构，通过 let 结构将这些 gensym 符号绑定到传递给绑定结构的值，然后用
**tree-leaves** 将代码中所有出现的绑定名称符号替换为对应的 gensym。**sublet** 不会展开任
何宏或解析主体中的任何特殊结构来查找这些绑定名称符号的出现，因为 **sublet** 会创建子词法绑
定。例如，如果所有 **a** 的引用都是子词法的，将用 gensym 替换它们:

.. code-block:: none

    * (macroexpand
        '(sublet ((a 0))
              (list a)))

    (LET ((#:A1657 0))
      (LIST #:A1657))
    T

但是，由于子词法作用域不涉及展开宏，因此不一定会解析 **quote** 这样的特殊结构，不是变量的
符号 **a** 也会被改掉：

.. code-block:: none

    * (macroexpand
      '(sublet ((a 0))
          (list 'a)))

    (LET ((#:A1658 0))
      (LIST '#:A1658))
    T

子词法作用域在列表结构被系统代码遍历程序解释为 lisp 代码之前生效。这是个重要的观测结果，
但其结果仍未被完全探索。**sublet** 对代码的解释不同于 COMMON LISP 的代码遍历程序。

这里，我们处于宏理解的众多边缘之一。在未扩展的子词法作用域和完全扩展的词法作用域之间有哪些
有趣的作用域类型？因为没有更好的名称，我们将这个无限大的范围称为 *超子词法作用域* （*super_
_sub-lexical scope*）。

.. code-block:: lisp

    (defmacro sublet*
      (bindings &rest body)
      `(sublet ,bindings
        ,@(mapcar #'macroexpand -1 body)))

超子词法作用域显然用到了 **sublet***。**sublet*** 宏中用了 **sublet**，但是用
**macroexpand-1** 函数的宏展开来修改主体中对应的结构。现在，对符号的引用必须出现在宏展开的第
一步之后，而不是出现在原始列表结构中。这种类型的超子词法作用域允许每个 let 结构主体中的宏从作用
域中插入或删除引用。如果宏没有做这两件事 —— 或者如果结构根本不是宏 —— 这种超子词法作用域的行为
就像子词法作用域：

.. code-block:: none

    * (macroexpand
      '(sublet* ((a 0))
          (list a)))

    (LET ((#:A1659 0))
      (LIST #:A1659))
    T

但我们可以定义另一个插入宏来测试这个超子词法作用域：

.. code-block:: none

    * (defmacro injector-for-a ()
        'a)

    INJECTOR-FOR-A

**sublet*** 将展开这个插入宏:

.. code-block:: none

    * (macroexpand-1
      '(sublet* ((a 0))
          (injector-for-a)))

    (SUBLET ((A 0))
      A)
    T

然后，**sublet** 将对其进行子词法解释，这意味着插入的变量 **a** 存在于 **sublet*** 提供的
超子词法作用域类型中：

.. code-block:: none

    * (macroexpand-1 *)

    (LET ((#:A1663 0))
      #:A1663)

但是表达式中的嵌套宏不会被 **macroexpand-1** 展开，所以 **sublet*** 不会把嵌套宏放到
**sublet** 的子词法作用域中：

.. code-block:: none

    * (macroexpand-1
      '(sublet* ((a 0))
          (list (injector-for-a))))

    (SUBLET ((A 0))
      (LIST (INJECTOR-FOR-A)))
    T

所以 **a** 不会被子词法捕获：

.. code-block:: none

    * (walker:macroexpand-all *)

    (LET ((#:A1666 0))
      (LIST A))

通过 **sublet** 和 **sublet***，可以用词法作用域或超词法作用域来控制在什么级别的宏展开中变
量 **a** 是有效的。如上所述，超子词法作用域实际上是一个无限类的范围，一个几乎完全未被智力探索的
范围。超子词法作用域的方法和遍历代码的方法（很多）一样多。这类作用域引出了另一类基本未被探索的
宏：这类宏改变 lisp 宏如何执行，何时展开，引用在哪里有效，特殊形式如何解释等。最终，就有了个可
编程宏（macro-programmable）的宏扩展器。


.. _6-7-pandoric-macros:

6.7 潘多拉宏
======================

潘多拉魔盒是个关于世界上第一个女人的希腊神话：潘多拉。潘朵拉，U 语言的符号，希腊语
翻译过来是全能。潘多拉，这个女人，在好奇心的诱惑下，打开了一个小盒子，无可挽回地释放
了人类所有的罪恶和罪恶。虽然本节中描述的宏非常强大，可能会教你一种永远不会忘记的编程
方法，但请放心，结果要比可怜的潘多拉好得多。现在开始，打开这个盒子。

首先，稍微绕过另一本著名的 lisp 书：克里斯蒂安·奎奈克的《Lisp in Small Pieces》。
Queinnec 是一位广受尊敬的 lisp 专家，对 lisp 知识做出了很大的贡献。Queinnec 的书的内容
是在 Scheme 编程语言中实现各种复杂的编译器和解释器。

..

  Lisp in Small Pieces: https://pages.lip6.fr/Christian.Queinnec/WWW/LiSP.html

《Lisp In Small Pieces》中有个简短但有趣的宏的讨论。由于 Scheme 宏规范的模糊性，它涉及
到描述不同的宏系统变化，但是为什么我们可能想要使用宏以及如何使用它们，有些有趣的注意
事项。如果你已经阅读并理解了 :ref:`chapter03`，那么 《Lisp in Small Pieces》章节中介绍
的大多数宏，对你来说，都属于微不足道的类别，除了我们现在要讨论的这个诱人的宏。

和许多编程书籍一样，《Lisp in Small Pieces》将我们带到了一个面向对象编程系统的实现。
通常这些实现用来概括 CLOS（ COMMON LISP  Object System）的一个子集。Queinnec
称他的子集为 MEROONET。Queinnec 指出，在为 MEROONET 类定义方法时，最好能够
直接引用所定义对象的字段，而不是使用访问器。把 Queinnec 的话翻译过来就是:
以 CLOS 中的 **with-slots** 宏为例；将它放到 MEROONET 环境中。对象的字段 ——
假设 **Point** 实例的字段 —— 是通过像 **Point-x** 或 **set-Point-y!** 这样的读和写函数
来处理的。在定义方法的上下文中，直接通过字段的名称(例如 **x** 或 **y** )来处理会更简单。

下面是 Queinnec 预想的接口（他称之为 **define-handy-method** ）定义新方法 **double**：

.. code-block:: lisp

    (define-handy-method (double (o Point))
      (set! x (* 2 x))
      (set! y (* 2 y))
      o)

这比 MEROONET 语法更让程序员高兴:

.. code-block:: lisp

    (define-method (double (o Point))
      (set-Point-x! o (* 2 (Point-x o)))
      (set-Point-y! o (* 2 (Point-y o)))
      o)

换句话说，如果可以使用宏来访问外部绑定（在本例中是对象槽），像是词法绑定一样，那就
太好了。虽然，不可否认的是这对缩写的目的很有用，但最重要的含义是它能够为现有的和未来
的宏提供二元（dualities）语法。

正如 Queinnec 所提出的， COMMON LISP  通过 **with-slots** 宏为 CLOS 实现了这个功能。
这是  COMMON LISP  实现其设计目的的一个例子：允许基于精炼的、标准化的宏系统进行抽象。
大多数语言被设计成易于实现，而  COMMON LISP  被设计成具有强大的编程功能。Queinnec
的结论是，语言的限制使得 Scheme 几乎不可能实现这一点，特别是在需要可移植性的地方。

由于缺乏关于语言及其实现的反射性信息，我们无法在 Scheme 中编写可移植的代码遍历程序，
因此我们不得不放弃编写 **define-handy-method**。

尽管  COMMON LISP  仍然可以使用大量合法的方法来实现宏系统，但它的设计目的是提供通用
的元编程工具，这些工具以标准和可移植的方式组合在一起。这两个先进  COMMON LISP  宏特性
允许我们实现像 CLOS 的 **with-slots** 一样的东西，它们是 *泛化变量（generalised
variables* 和 *符号宏（symbol macro*。本节就借此机会展示  COMMON LISP  特性的奇妙组合，
并将我们迄今为止见过所有关于回指宏的内容集合在一起，在这个过程中发现了一个有趣的宏类，称为
*pandoric* 宏。

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

**pandoriclet** 背后的思想是打开闭包，允许外部访问它们本来封闭的词法变量。与之前的一些宏
（如 **alet-hotpatch** ）一样，**pandoriclet** 编译一个间接环境，根据传递的参数选择不同的
运行时行为。

我们再次从 **alet** 由内而外的展开开始，记住这里引入了个叫 **this** 的回指词。
**pandoriclet** 与我们见过的其他宏类似。和所有的回指 **let** 变体一样，假设
**pandoriclet** 主体中的最后的结构将是 lambda 结构。就像 **alet-hotpatch** 一样，
**pandoriclet** 用 **dlambda** 宏来在调用 **pandoriclet** 返回的闭包时执行不同可能的代
码。**pandoriclet** 还用了上一节介绍的**let-binding-transform** 实用函数来处理已创建的
空绑定，如 **(let (a) ...)**。这个实用函数对 **pandoriclet** 是必需的，原因与需要
**sublet** 一样：这些宏遍历 **let** 中的绑定，而之前的宏盲目地将绑定拼接到另一个 **let**
中。

我们调用了两个没定义的创建列表的实用函数：**pandoriclet-get** 和 **pandoriclet-set**，
它们分别接受一个 **let** 绑定列表。注意，我们可以引用还不存在的函数，只要在宏展开之前定义
它们就可以，显然，在使用宏之前不能这样做。使用辅助函数来帮助定义宏是一个很好的习惯。
它不仅可以使定义更具可读性，还可以在测试宏的组件时提供帮助，并可以在将来的宏中证明是
有用的。这种抽象最好的部分是，当组合宏时，保持词法上下文可供实用程序使用。

因此，记住这个词法上下文，现在要写 **pandoriclet-get** 和 **pandoriclet-set** 。对于
**pandoriclet-get**，其中 **dlambda** 绑定了变量 **sym**，在这里列表将被拼接进去。在
**case** 结构中使用 **sym** ，将其与传递给 **pandoriclet** 的变量进行比较。如果找到这个变
量，则返回它所引用的绑定的当前值。如果没找到，则抛出异常。**pandoriclet-set** 差不多一样，除
了 **dlambda** 为它绑定了一个额外的变量：**val** 。**pandoriclet-set** 用 **setq**
将 **sym** 引用的绑定更改为 **val**。

.. code-block:: none

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
            sym val))))

**prandoriclet** 也有和回指 let 变体一样的接口，因此可以使用它来创建常见的 counter 闭包：

.. code-block:: none

    * (setf (symbol-function 'pantest)
        (pandoriclet ((acc 0))
          (lambda (n) (incf acc n))))

    #<Interpreted Function>

如预期般：

.. code-block:: none

    * (pantest 3)
    3
    * (pantest 5)
    8

同时，现在在创建闭包时可以直接访问 **acc** 的绑定：

.. code-block:: none

    * (pantest :pandoric-get 'acc)
    8

同样的也可以修改这个绑定的值：

.. code-block:: none

    * (pantest :pandoric-set 'acc 100)
    100
    * (pantest 3)
    103

甚至是 **this** 回指的值也能访问，因为我们特意将这个回指打开同时在宏展开时将 **this** 变量
添加到 **letargs** 绑定列表中：

.. code-block:: none

    * (pantest :pandoric-get 'this)
    #<Interpreted Function>

所以 **pandoriclet** 创建的这个闭包已经不再闭包了。这个闭包所使用的环境 —— 即使编译器
已经删除了所有的词法变量符号 —— 仍可以通过 **pandoriclet** 返回的匿名函数来访问。这是
怎么做到的呢？通过 pandoric 宏，将编译额外的代码，以提供从外部访问闭包的方法。但从这个
正在发生的低级角度看，并不能看到 pandoric 宏的威力。我们所做的是创建一个闭包间协议，
或消息传递系统，用于闭包之间的通信。

在继续讨论 pandoric 宏之前，首先需要指出一个 COMMON LISP 语法二元性的最重要的例子：
泛化变量（ *generalised variables* ）。这方面的细节很复杂，这里不会做详细的介绍。为此，
推荐去阅读 Graham 的  OnLisp* ，这是目前所知道的最好的解决方法。细节是微妙的，想法
很简单：访问一个泛化变量在语法上是双重的。只有一种 setter 结构：**setf**，**setf** 能够通过
使用访问变量时使用的相同语法设置所有类型的变量。

例如，通常是通过变量的变量名来访问其值，假设这个变量名为 **x**。可以用 **(setf x 5)** 来
设置 **x** 的值为 5。同样，要想访问个调用的 cons 的 car 单元，假设也为 **x**，可以使用
**(car x)**，也可以通过 **(setf (car x) 5)** 来设置其值。。这隐藏了个事实，机设置 cons 的
实际方法是使用 **rplaca** 函数。通过实现这种二义性语法，我们将需要记住的访问器和设置其的数量
减少了一半，更重要的是，为宏提供了的新方法。

.. code-block:: lisp

    (declaim (inline get-pandoric))

    (defun get-pandoric (box sym)
      (funcall box :pandoric -get sym))

    (defsetf get-pandoric (box sym) (val)
      `(progn
          (funcall ,box :pandoric -set ,sym ,val)
          ,val))

**get-pandoric** 函数是对内部闭包协议 getter 语法的封装。它被定义为内联，以消除这种封装所造
成的任何性能影响。

**defsetf** 是一个有趣的 COMMON LISP 宏，完全不像 **defmacro** 的拓展 **defmacro!**
隐式地绑定提供的结构的 gensyms。**defsetf** 非常适合定义泛化变量二元性的 setter 端，只要
getter 可以表示为一个函数或宏，对其所有参数精确计算。注意，虽然可以将 **get-pandoric** 定义
为宏，但这样做的唯一原因是为了内联。宏不是用来内联的，编译器是用来内联的。

回到 **pantest** 中的符号函数中存储的 pandoric 计数器，我们可以用这个新的 getter 函数来获
取 **pantest** 中 **acc** 当前绑定的值：

.. code-block:: none

    * (get-pandoric #'pantest 'acc)
    103

现在，多亏了泛型变量和 **defsetf**，可以用一个语法对偶来设置 **acc** 的值:

.. code-block:: none

    * (setf (get-pandoric #'pantest 'acc) -10)
    -10
    * (pantest 3)
    -7

通过函数关闭的环境 —— 该函数是在 *let over lambda* 中调用的 let —— 开始看起来像常规可访问
的通用变量，就像 cons 单元格或哈希表条目。闭包现在是比过去更一流的数据结构。以前对外部代码封闭
的绑定现在对我们开放，即使这些绑定被编译成高效的东西，或者它们的访问器符号早就被遗忘了。

但是，任何关于泛型变量的讨论，如果不提到它的近亲： *symbol macro* ，都是不完整的。像其名字所
提示的那样，**symbol-macrolet** 可以讲符号扩展成一般的 lisp 结构。因为它很直观以及更灵活的
使用形式，看起来像函数调用代表宏转换，没有大量使用 **symbol-macrolet** 的一个重要应用的关键
是：符号宏隐藏了泛型变量，这样宏的使用者认为他们正在访问常规词法变量。

符号宏的引入导致了 COMMON LISP 语言中最奇怪的组合之一：通常在设置个通过常规符号访问的变量时，
比如 **(setf x t)**， **setf** 将展开成 **setq** 结构，因为这就是设计 **setq** 最初目
的：设置词法变量和动态变量（通常由符号引用）。但是 **setq** 结构不能设置泛型变量，所以当引入符
号宏时，符号不仅可以表示词法/动态绑定，还可以表示任何泛化变量，有必要指出的是，通过 **setq**
结构设置由符号宏定义的符号会被转换回 **setf** 结构。奇怪的是，这确实是正确的做法，因为它允许宏
对宏的用户完全隐藏泛型变量的存在，即使他们用 **setq**。真正正确的解决办法是从语言中删除冗余的
**setq** 结构，支持的更通用的 **setf** ，但这不会发生，原因是明显的兼容性以及宏创建期间，
**setq** 也可以是个有用的安全快捷方式 —— **setf** 加上个检查符号是拼接的，而不是列表结构。
在用 **setq** 时，记住只有在其拼接安全属性有用；正如我们所看到的，多亏了
**symbol-macrolet**，符号可以引用任何泛型变量。

.. code-block:: none

    (defmacro! with-pandoric (syms o!box &rest body)
      `(symbol -macrolet
        (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                  syms ))
        ,@body))

**with-pandoric** 宏会展开成个 **symbol-macrolet**，**symbol-macrolet** 为
**syms** 中提供的每个符号定义了符号宏。每个符号宏将在符号宏的词法作用域中展开对其符号的引用，
用 **get-pandoric** 访问器/设置器 来访问宏的第二个参数的求值结果：**o!box** （保存在
**g!box** 中）。

因此 **with-pandoric** 让我们窥探到了闭包的闭变量绑定：

.. code-block:: none

    * (with-pandoric (acc) #'pantest
        (format t "Value of acc: ~a~%" acc))
    Value of acc: -7
    NIL

根据广义变量来形成 setting 和 getting 变量的语法对偶的设计，甚至可以假设它是个常规的词法变
量，然后通过 setq 设置它：

.. code-block:: none

    * (with-pandoric (acc) #'pantest
        (setq acc 5))
    5
    * (pantest 1)
    6

现在，我们已经研究了构成 pandemic 宏的大多数部分组成。首先，用于创建闭包的宏：
**pandoriclet**，这个宏捕获回指变量：**this**，**this** 变量引用了在调用闭包时使用的实际
函数。这个宏还会编译成一些特殊的代码，这些代码会拦截这个闭包的某些调用，然后访问或修改它的闭包词
法变量。其次，**get-pandoric** 和 **defsetf** 实现了访问和设置访问器的单一语法。最后，
**with-pandoric** 宏用 **symbol-macrolet** 来设置这些泛型变量，这些泛型变量看起来是新的
词法变量，其名称与闭合变量相同。这些变量引用了 **pandoriclet** 创建的原始环境，但是，这些环
境是不同的词法上下文。

作为示例，我们将这种打开闭包的功能与 :ref:`6-5-hotpatching-closures` 中的 **hotpatch**
宏进行了比较。回顾一下 **let-hotpatch** 及其同名的闭包 **let-hotpatch**，这两个宏使用间接
环境创建闭包，以便可以动态更改在调用闭包时调用的函数。这些宏的最大限制是，当对前一个匿名函数进行
热补丁时，会强制抛出所有在该函数上关闭的词法绑定。这种情况是不可避免的，因为在编写这些宏时，闭包
对我们关闭了。

对于 **let-hotpatch** 和 **let-hotpatch**，必须将特殊目的的代码编译到每个闭包中，这些闭包
能够将 **this** 回指的词法绑定设置为它的新值。但是由于现在可以打开由 **pandoriclet** 定义
的闭包并在外部运行这个 **setter** 代码，所以可以定义一个可以处理任何 pandoriclet 闭包的热补
丁函数 **pandoric-hotpatch**。

.. code-block:: lisp

    (defun pandoric-hotpatch (box new)
      (with-pandoric (this) box
        (setq this new)))

有时抽象在感觉很对，很难确切地说出为什么。也许是因为大多数编程都是不相关部分的不和谐
组合，当碰巧发现抽象完美地结合在一起的时，会感到很惊讶和愉快。**pandoric-hotpatch**
看起来和其工作原理完全一样：打开个 pandoric 接口，从闭包的词法范围中取变量 **this**，
然后使用 **setq** 将 **this** 设置为要热补丁的闭包 **new**。

甚至在我们意识到我们需要个 pandoric 闭包热补丁前使用 **pandoric-hotpatch**。还记得
本节中一直用的计数器闭包吗？它仍要绑定到 **pantest** 的符号函数。上次的结果是 6：

.. code-block:: none

    * (pantest 0)
    6

现在设置个新闭包 —— acc 有个新绑定，初始值为 100，之后就递减：

.. code-block:: none

    * (pandoric-hotpatch #'pantest
        (let ((acc 100))
          (lambda (n) (decf acc n))))
    #<Interpreted Function>

显然，热补丁成功了：

.. code-block:: none

    * (pantest 3)
    97

现在，counter 闭包中有个新值绑定到 **this** 上，用来执行计数。但这个 hotpatch
改变了 **acc** 变量绑定的 pandoric 值吗?

.. code-block:: none

    * (with-pandoric (acc) #'pantest
        acc)
    6

并没有。 **acc** 还是之前的值 6，因为这里只修改了 pandoric 环境中 **this** 的绑定因为
我们在这个混乱的环境中更改的唯一绑定是这个，然后将其变成了个有自己绑定的 **acc**
的新闭包。

.. code-block:: lisp

    (defmacro pandoric-recode (vars box new)
      `(with-pandoric (this ,@vars) ,box
        (setq this ,new)))

**pandoric-recode** 宏采用种略微不同的 hotpatch 方法。其保留了代码的原始词法环境，
同时还要在闭包被调用到外部代码和外部编译时，设法改变要执行的函数。听起来有点
难以置信？记住，在原来的 pandoric 环境中，**acc** 的值是 6，可以用
**pandoric-recode** 设置个新函数来使用这个原始值，哦，或者说，将计数器的值
减去 **n/2**:

.. code-block:: none

    * (pandoric-recode (acc) #'pantest
        (lambda (n)
          (decf acc (/ n 2))))
    #<Interpreted Function>

当然，就有了新的行为，会将 **acc** 减去 **(\* 1/2 2)**，从 6 变为 5:

.. code-block:: none

    * (pantest 2)
    5

那这和最初的 pandoric 绑定有关联吗？

.. code-block:: none

    * (with-pandoric (acc) #'pantest
        acc)
    5

对的，有关联。那 **pandorc-code** 是如何工作的呢？它在提供的 lambda 结构中关闭
了原始闭包打开的绑定。

.. code-block:: lisp

    (defmacro plambda (largs pargs &rest body)
      (let ((pargs (mapcar #'list pargs)))
        `(let (this self)
          (setq
            this (lambda ,largs ,@body)
            self (dlambda
                    (:pandoric-get (sym)
                      ,(pandoriclet-get pargs
                    (:pandoric-set (sym val)
                      ,(pandoriclet-set pargs))
                    (t (&rest args)
                      (apply this args)))))))

到目前为止，用来创建 pandoric 闭包的宏是 **pandoriclet**。**plambda** 是个由内到外
重写的 **pandoriclet**，增加了一些重要的特性。首先也是最重要的，**plambda** 不再
创建 pandoric 访问器使用的 let 环境。相反，**plambda** 接受一组符号，这些符号指向
的变量应该在调用者的词法环境中。**plambda** 可以在词法环境中导出任何变量，透明
地让其他词法作用域可以访问——甚至是在 **plambda** 结构之前或之后编写和编译的变量。

这是对 *let over lambda* 闭包系统的一个增量改进，该系统旨在最大化双语法。多亏了
pandoric 宏（其中最重要的是 **plambda** 和 **with-pandoric**），可以在需要时轻松
有效地超越词法作用域的界限。闭包不再关闭；我们可以轻松地打开闭包，就像将
lambda 结构重写为 lambda 结构一样。用 **plambda** 导出词法变量，然后用
**with-pandoric** 将它们作为完全等价的词汇变量导入。事实上，这些新变量是
等价的，它们根本就不是新变量。理解 pandoric 变量的一种更好的方法是，它们只是
原始词法作用域的扩展。以 **plambda** 的使用做个简单示例，有个 pandoric 计数器，
它从两个可能不同的词法环境导出变量：

.. code-block:: none

    * (setf (symbol-function 'pantest)
        (let ((a 0))
          (let ((b 1))
            (plambda (n) (a b)
              (incf a n)
              (setq b (* b n))))))
    #<Interpreted Function>

请注意，导出这些词法引用是多么容易。让闭包 pandoric 就像在 **lambda** 之前添加个
**p** 字符一样简单，或者是像在 **lambda** 参数后添加一个要导出的变量列表一样简单。
我们可以打开这个闭包 —— 或者是任何导出 **a** 和 **b** 的 pandoric 闭包 —— 像这样
使用 **with-pandoric**：

.. code-block:: none

    * (defun pantest-peek ()
        (with-pandoric (a b) #'pantest
          (format t "a=~a, b=~a~%" a b)))
    PANTEST-PEEK
    * (pantest-peek)
    a=0, b=1
    NIL

**plambda** 就是个例子，说明了如何分解宏展开的一般组件。还记得编写 **pandoriclet**
时决定将 getter 和 setter 代码的 case 创建语句移到 pandoriclet-get函数中吗？
**plambda** 用到了与之相同的函数。尽管这些宏将函数的结果拼接到相当不同的词法上下
文中，但由于两个宏都是用相同的变量命名约定和内部闭包协议编写的，所以代码是可重
用的。

因此，pandoric 宏打破了词法界限。它们允许在需要的时候打开闭包，同时也代表了各种
COMMON LISP 语言特性的美丽融合：回指宏、泛型变量和符号宏。但它们到底有什么好
的呢?

pandoric 的宏很重要，因为它们在不需要脱离更自然的 let-lambda 组合编程风格的情况下，
提供了 CLOS 等对象系统的主要优势。尤其是在不重新实力化已经创建了的对象实力的情
况下，就可以为闭包添加功能或方法。

.. code-block:: lisp

    (defun make-stats-counter
          (&key (count 0)
                (sum 0)
                (sum-of-squares 0))
      (plambda (n) (sum count sum-of-squares)
        (incf sum-of-squares (expt n 2))
        (incf sum n)
        (incf count)))


**make-stats-counter** 是个 lambda over let over dlambda，用来创建计数器，只不过
它维护了三条信息。除求和外，还保留平方和以及到目前为止处理的项目数。如果在
**make-stats-counter** 的定义中使用 **lambda** 而不是 **plambda**，那么大多数信息都
是不可访问的。这样就被卡住了，因为这些变量是关闭。

那么要怎么写 **pandoric** 方法？可以像上面演示的那样简单地使用 **with-pandoric**
访问变量，或者，既然是 lisp，那么就设计个更具体的接口。

.. code-block:: lisp

    (defmacro defpan (name args &rest body)
      `(defun ,name (self)
        ,(if args
          `(with-pandoric ,args self
            ,@body)
        `(progn ,@body))))

**defpan** 是 **defun** 和 **with-pandoric** 两个宏的组合。**defpan** 的主要目的是在
**defun** 编写函数和 **with-pandoric** 访问外部词法范围之间实现语法的二元性。尽管
**defpan** 的参数和lambda 结构的语言相同 —— 符号列表 —— 但 **defpan** 参数的含义
不同。这些 pandoric 函数不是创建了新的词法环境，而是扩展了它们所应用的 pandoric
闭包的词法环境。对于 **defun** 和常规的 lambda 结构，变量的名称（符号）不重要。但
在 pandoric 函数中，变量名称就是一切。此外，在 pandoric 函数中，参数的顺序并不重
要，可以随意地选择使用导出的词法变量数量。

**defpan** 还有个 **self** 的回指，可以执行一种叫做 *回指链（anophor chaining）* 的有用
技术。通过在 pandoric 函数之间隐式地传递 **self** 的值，就可以在整个函数调用链中维护
这个回指的值。与所有的链接结构一样，要确保这个链不会以无限循环结束。

.. code-block:: lisp

    (defpan stats-counter-mean (sum count)
      (/ sum count ))

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

本文给出了三种方法，它们可以用于 **make-stats-counter** 创建的闭包或任何其他导出
必要变量名的 pandoric 闭包。**stats-counter-mean** 只是返回传递给闭包的所有值的
平均值。**stats-counter-variance** 通过跟踪链中的链接来计算这些值的方差，而
**stats-counter-stddev** 通过跟踪另一个链接来计算标准差。注意，链中的每个链接
只需要传递一个回指 **self** 来引用闭包的完整词法上下文。可以看到，单个的 pandoric
函数只需要引用它们实际使用的变量，这些变量可以随意调整引用顺序。

所以 **plambda** 创建了另一个回指 —— **self**。**this** 指的是要调用的实际闭包，而
**self** 指的是调用这个闭包的间接环境。虽然听起来有点奇怪，但 **plambda** 内部的代码
可以用 **self** 来大规模访问它自己的词法环境，而不是直接访问它。到目前为止，这似乎
只对为在词法作用域内工作而编写的 **defpan** 方法有用。

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

**make-noise-stats-counter** 和 **make-stats-counter** 类似，不同之处是
**make-noisy-stats-counter** 用 **self** 回指来调用 **defpan** 函数
**stats-counter-mean**、**stats-counter-variance** 和 **stats-counter-stddev**。
**plambda** 和 **with-pandoric** 可以随意改写词汇范围。我们以这样一个例子结束本章。
词法作用域的一个局限性有时令人遗憾，即当 COMMON LISP 函数 **eval** 计算传递给
它的结构时，它会丢弃当前的词法环境。换句话说，**eval** 在空词法环境中计算结构。
在 COMMON LISP 中没有其他方法：**eval** 是一个函数。那么问题就来了:

.. code-block:: none

    * (let ((x 1))
        (eval
          '(+ x 1)))
    Error: The variable X is unbound.

有时，将词法环境扩展到 **eval** 显然是可取的。但是要小心。经常有人说，如果正在
使用 **eval**，那么可能正在做一些错误的事情。**eval** 的误用会导致程序速度变慢，
因为 **eval** 是非常昂贵的操作 —— 主要是因为它需要展开传递给它的结构中的宏。
假如在编程时突然发现需要 **eval**，问一下自己，为什么不能早点做想做的事情。
如果答案是不能，比如说因为刚刚读取了结构，那么恭喜，你找到了 **eval** 的一个
罕见的合法用法。其他任何答案都将直接导致可能一开始就应该使用的方法：使用宏。

.. code-block:: lisp

    (defvar pandoric-eval-tunnel)

    (defmacro pandoric-eval (vars expr)
      `(let ((pandoric-eval-tunnel
              (plambda () ,vars t)))
        (eval `(with-pandoric
                  ,',vars pandoric-eval-tunnel
                  ,,expr))))

但是假设你真的想要 **eval** 计算某样东西，只要你能使用那个讨厌的词法上下文。
**pandoric-eval** 宏是个用 **plambda** 和 **with-pandoric** 的有趣示例。
**pandoric-eval** 使用了 **pandoric-eval-tunnel** 的特殊变量，使
**pandoric** 闭包可以通过动态环境提供给 **eval** 函数。通过提供所有符号的列表
作为 **pandoric-eval** 的第一个参数，可以精确地选择要在动态环境中使用的词法
变量。这里我们将它应用到前面的例子中:

.. code-block:: none

    * (let ((x 1))
        (pandoric-eval (x)
          '(+ 1 x)))
    2

同时 **pandoric-eval** 计算的表达式会改变原有的词汇环境；**pandoric-eval**
是一个双向隧道:

.. code-block:: none

    * (let ((x 1))
        (pandoric-eval (x)
          '(incf x))
        x)
    2

这一节虽然很长，但仍然只触及了 **pandoric** 宏及其许多可能的变体的皮毛。
期待他们在未来的许多有趣的发展。

思考1：**pandoric-eval** 可以嵌套调用吗？也就是说，可以使用 **pandoric-eval**
来计算 **pandoric-eval** 的结构吗？为什么或为什么不？

思考2：虽然这里的 pandoric 宏的实现效率很高，但还可以改进。可以尝试改进
**pandoriclet-get** 和 **pandoriclet-set** ，以生成使用哈希表而不是 **case**
的代码，然后对这两个实现分别进行小量和大量的 pandoric 变量进行基准测试。
研究你最喜欢的 CLOS 实现，模拟调度是如何进行的，重新进行基准测试。

.. _OnLisp: http://www.paulgraham.com/onlisp.html