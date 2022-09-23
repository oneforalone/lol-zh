.. _chapter07:

********************
第七章：宏的效率
********************

.. _7-1-lisp-is-fast:

7.1 Lisp Is Fast
====================

::

  如果用指甲盖大小的瓷砖来铺地面的话，就不会有太多的浪费。 —— Paul Graham

有人觉得 lisp 很慢。对于一些早期的 lisp 实现来说，确实是这样的，但多年来这种看法
已经被证明是错误的。事实上，像 COMMON LISP 这样的现代Lisp 已经设计成用宏来
提高 Lisp 的速度。非常快。如果相信低级语言比 lisp 更高效的性能神话，那么本章的目
标可能会让你感到惊讶。本章旨在说明 lisp 可以比其他编程语言更快，而像 C 这样的低
级编程语言实际上在性能上比 lisp 更差，因为它们缺少宏。Lisp 能编写比其他语言更高效
的代码。特别是大型和复杂的程序，宏能构建比 Blub 语言更具绝对性能优势的代码。有时，
我们的语言设计成具有高效的实现，但更多时候，它们设计成为程序员提供最大的表达能力。
当选择效率时，lisp 很快，非常得快。

当其他语言提供小的、正方形的瓦片时，lisp 可以选择随意大小、形状的瓦片。在 C 语言中，
程序员总是使用一种与花哨的固定数字加法器功能直接相关的语言。除了过程和结构，在 C
语言中几乎不可能进行抽象。相反，lisp 根本不是围绕机器的能力和限制而设计的。

但可以肯定的是，其他语言能用更低效率、更方便的方式来编写。毕竟，Perl 让程序员使用
单个密集表达式进行精确匹配，但也为更快的代码提供了许多升级路径。那么，当说 lisp
不像其他语言那样，可以控制抽象的效率时，这意味着什么呢？现在你可能已经猜到了，
答案就是书中的主题：宏。

与其询问什么让程序运行得快，不如问问是什么让程序运行得慢。这是编程中最常被研究的
话题之一。根本原因可以大致分为三大类：糟糕的算法、差劲的数据结构和通用代码。

所有的语言实现都需要好算法。算法大概是对如何执行编程任务进行了充分研究的过程描述。
因为发明算法所需的投资远远大于实现算法，所以算法在整个计算机科学中无处不在。有人
已经知道了算法如何、为什么以及多快地工作；要使用算法，所要做的就是把它的伪代码转换
成系统能够理解的东西。因为 COMMON LISP 实现通常都是由聪明的人实现的，并且在过去
的几十年里不断改进，他们通常使用一些最好和最快的算法来完成大多数常见的任务。例如，
CMUCL 使用调优的堆排序实现对列表和向量的排序，使用 Mersenne Twister 19937 算法
及大周期来其生成随机数。

好的数据结构对优秀的编程语言来说也是必要的。数据结构很重要，忽略它们将导致任何语言
实现缓慢。数据结构的优化本质上归结为一个叫做局部性的概念。解释这个概念很容易 ——
访问最频繁的数据应该是访问速度最快的。数据结构和局部性是如此重要，以至于几乎所有
需要提高性能的计算级别上都能清楚地看到它们：大量的CPU寄存器、内存缓存、数据库和
缓存网络代理是其中的一些亮点。Lisp 提供了一组巨大的标准数据结构，它们也都实现得很好。
哈希表、链表（显然）、带填充指针的向量、带可互连符号的包，以及更多的都是特定、可用
且为 Common Lisp 程序员利用的都很好地实现了。

如果 lisp 提供了这么好的算法和数据结构，那 lisp 代码怎么可能比其他语言的慢呢？这个解释
是基于 lisp 最重要的设计决策：通用代码，一个与我们熟悉的二元性相似的概念。在编写 lisp
代码时，我们尽可能多地使用二元性。语言本身的结构鼓励我们这样做。lisp 程序通常比 Blub
程序短得多的部分原因是，任何给定的 lisp 代码段的用途都比相应的 Blub 代码段大得多，因此
可以更频繁地重用它。从 Blub 语言的角度来看，写得多得到得少可能会让人觉得不寻常，但这是
我们一直在讨论的 lisp 的重要设计决策 —— 语法的二元性。每个表达式附加的二元性越多，
程序似乎就越短。那么，这是否意味着为了达到或超过 C 语言的性能，需要使 lisp 程序与相应的
C 语言程序一样长且危险呢？不，Lisp 有宏。


.. _7-2-macros-make-lisp-fast:

7.2 宏让 Lisp 很快
==============================

本节展示了用三种类型的宏来协助创建高效程序的示例：常规宏、读取宏和这里介绍的
新类型宏 —— 编译宏。

宏可以用来控制算法、数据结构、类型检查、安全检查、代码或部分代码的优化级别等等。
我们可以在一个程序（甚至函数）中同时存在安全和通用的代码，也可以同时存在执行快
和危险的代码。简而言之，没有任何一种语言提供了像 lisp 这样的开放接口来控制编译器，
这都要归功于宏（不然还能是什么呢？）。大概浏览下 ANSI 标准会发现标准里看起来是说：
宏和声明（与编译器沟通的最直接方式）不能很好地协同工作: 宏结构不能展开成声明；
声明表达式必须以它们所引用结构的实际子表达式的形式出现。

ANSI 的意思是，下面的宏不会按预期工作:

.. code-block:: lisp

    (defmacro go-fast () ; Broken!
      '(declare (optimize (speed 3) (safety 0))))


我们不能把宏调用放在需要声明的地方。这个问题的另一种思考角度是，在检查声明之前，
系统的代码遍历程序不需要展开特殊结构主体中的宏。想要执行得快是件很常见的事情，
所以也许可以做得比上面有问题的 **go-fast** 宏更好。想要尽可能多地压缩含义时，
通常需要一个读宏。读宏也适合展开成声明，因为它们在代码遍历程序尝试遍历代码之前
就展开了。它们是以实际的子表达式读入的。

.. code-block:: lisp

    (set-dispatch-macro-character #\# #\f
      (lambda (stream sub-char numarg)
        (declare (ignore stream sub-char))
        (setq numarg (or numarg 3))
        (unless (<= numarg 3)
          (error "Bad value for #f: ~a" numarg))
        `(declare (optimize (speed ,numarg)
                            (safety ,(- 3 numarg))))))

**#f** 是个读宏，能控制 COMMON LISP 程序最重要的性能权衡：声明的速度和安全之间
的平衡。例如，**#f** 本身读取的是我们希望 **go-fast** 扩展的内容:

.. code-block:: none

    * '#f
    (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))

但是，我们可以改变这一点，并将一个小于 3 的数作为 reader number 参数来声明安全
高于速度。所有的调度读宏都可以接受这样一个数字参数，它作为第三个参数（通常称为
**numarg**）传递给 read 宏函数。下面是个体现我们重视安全而不是速度的例子，将
SPEED 的参数设为 0:

.. code-block:: none

    * '#0f
    (DECLARE (OPTIMIZE (SPEED 0) (SAFETY 3)))

也可以设置为 1 和 2，从而产生以下声明。这些不同的声明设置的优点非常依赖于编译器，
所以你几乎不会使用它们:

.. code-block:: none

    * '(#1f #2F)
    ((DECLARE (OPTIMIZE (SPEED 1) (SAFETY 2)))
    (DECLARE (OPTIMIZE (SPEED 2) (SAFETY 1))))

尽管宏不能直接扩展为声明，但我们仍然可以使用常规宏来控制声明。因为代码遍历程序在
展开宏之前不能遍历宏结构来搜索声明，所以无法判断该声明是编写结构的实际子表达式，
还是宏在展开时添加了声明。

.. code-block:: none

    (defmacro fast-progn (&rest body)
      `(locally #f ,@body))

    (defmacro safe-progn (&rest body)
      `(locally #0f ,@body))

**fast-progn** 和 **safe-progn** 是一些展开的结构中包含声明的简单例子。请注意，这里
使用的是 **locally** 的隐式 progn 而不是 **progn** 本身，因为 **progn** 中不能有声明。
这两个宏用了之前定义的 **#f** 读宏。我们可以使用这些结构作为 **progn** 的一个版本，其中
内部表达式对执行速度进行了优化(但很危险)，另一个版本确保内部表达式是安全的(可能很慢)：

.. code-block:: none

    * (macroexpand
        '(fast-progn
          (+ 1 2)))
    (LOCALLY
      (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))
    (+ 1 2)) T

我们还可以在宏参数中提供其他声明，因为它们的位置不是也不能在宏展开之前验证：

.. code-block:: none

    * (macroexpand
        '(fast-progn
          (declare (type fixnum a))
          (the fixnum (+ a 1))))
    (LOCALLY
      (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))
      (DECLARE (TYPE FIXNUM A))
      (THE FIXNUM (+ A 1)))
    T

在尝试宏扩展时，有时会想看看在将宏扩展嵌入不同的词法上下文时会发生什么。
将 :ref:`4-1-runtime-at-readtime` 中的读取时计算宏与 **\*** 变量
（保持最后三个REPL结果可用）结合起来，可以看到我们的代码的计算结果如预期
的那样:

.. code-block:: none

    * (let ((a 0))
        #.*)
    1

但是请注意，尽管上面的计算是正确的，但是声明有时只对编译后的代码进行充分
考虑。例如，由于上面的计算解释了代码，它可能会忽略安全声明，并继续将溢出
结果提升为大数（ *bignum* ）。来看看这里是否会发生这种情况:

.. code-block:: none

    * (let ((a most-positive-fixnum))
        #.**)
    536870912

确实会将溢出结果提升为大数，CMUCL忽略了解释代码的声明。我们想在 **\*\*\***
中继续玩我们的表达式，但由于不确定下次是否能得到它，就把它带回 * ，这样
就不会丢失表达式:

.. code-block:: none

    * ***
    (LOCALLY
      (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))
      (DECLARE (TYPE FIXNUM A))
      (THE FIXNUM (+ A 1)))

就是这样。所以现在有三次机会让它工作。试试编译它，看下会不会得到个 *fixnum* 的封装：

.. code-block:: none

    * (funcall
        (compile nil
          `(lambda ()
            (let ((a most-positive-fixnum))
    ,*))))
    ; Warning: This is not a (VALUES FIXNUM &REST T):
    ;   536870912
    536870912

Emm，到底发生了呢？我们不是告诉 lisp 不要检查吗？像常量折叠这样的编译时优化
让声明的推导更复杂。当 lisp 编译代码时，它能够在编译时执行加法，因为我们添加的
是常量，因此它知道结果也将是常量，所以就没必要在运行时计算它。当 lisp 这样做的
时候，它看到我们对一个 fixnum 的声明肯定是错误的。这个警告是用 lisp 的方式告诉
我们“你这个笨蛋，我无视你的声明，因为你不可信。”如果稍微改变一下表达式，让
lisp 不能折叠任何常量，最终可以看到 *fixnum* 封装的效果:

.. code-block:: none

    * (funcall
        (compile nil
    `(lambda (a)
    7.2. MACROS MAKE LISP FAST 215
            ,**))
        most-positive-fixnum)
    -536870912

声明的另一个重要属性是，它们可以像词法变量可以遮蔽其他词法变量一样遮蔽其他
声明。例如，我们可能希望编写个宏来执行安全检查，即便是被嵌入到声明为不安全
的代码中:

.. code-block:: lisp

    (defmacro error-checker ()
      `(safe-progn
        (declare (type integer var))
        do-whatever-other-error-checking))

再封装一层，我们可以用这些宏来添加错误检查代码，这些代码需要执行的比较快而不
是比较安全，通过嵌套这些宏的其他用法来实现：**fast-progn** ：

.. code-block:: lisp

    (defun wrapped-operation ()
      (safe-progn
        do-whatever-error-checking
        (fast-progn
          but-this-needs-to-go-fast)))

在高性能lisp代码中，使用围绕某些功能的快速实现的错误检查区域安全地验证参数是
一种常见模式。特别是对于数组遍历这样的迭代过程，可以通过在操作开始前进行类型
和边界检查等错误检查，然后在执行时尽可能地忽略它们，从而显著提高运行时性能。

COMMON LISP 首先是为了强大的编程能力而设计的；效率是个较远的次要问题。然而，
这些功能、功率和效率并不一定代表一种权衡。通过宏，我们可以应用 lisp 强大功能来
解决效率问题。除了常规宏和读取宏（它们本身已经提供了相当强大的功能）之外，
COMMON LISP还提供了编译宏。编译宏是与其他类型宏相同意义上的宏：它们是编程
的程序。大多数lisp教程都没有很好地描述编译器宏，这表明性能对于程序员来说是多么
重要（几乎从来没有）。然而，编译宏是某些效率问题的优雅解决方案，值得成为每个
lisp专业人员的工具包。

编译宏定义了 lisp 编译器将应用于（命名）函数调用的转换。这意味着可以使用 **defun**
创建的函数，并告诉 lisp 不要编译对该函数的调用，而是应该编译编译宏指示的一些代码。
为什么要将函数与编译宏结合使用，而不是一开始就用这个名字编写宏呢？第一个不太重
要的原因是，这让我们能够更多地控制何时吸收编译开销。特别的是，COMMON LISP
并没有指定何时或者多长时间扩展一个宏。在解释代码中，宏每次被调用时都有可能被展
开。在进行编译时优化时，我们希望在运行函数之前执行一个（可能很长且昂贵的）计算，
以减少函数本身必须执行的计算量。编译宏为我们提供了一种方法，当我们编译代码时，
只执行一次冗长的编译计算 —— 它本该是这样的。

但比只在正确的时间执行一次编译计算更重要的是，编译宏很有用，因为它们将语法的
二元性引入语言。编译宏允许我们为任何表示（命名）函数调用的代码结构添加双重含义。
除了常规意义外，编译器宏还添加了编译意义。强烈推荐确保编译后的含义实现与常规含义
任务相同，但可以随意改变它的执行方式（这是重点）。使用双重语法的好处是，可以改变
代码的效率，而不需要修改代码。我们可以使用一个现有的代码库 —— 一个可能使用了
大量函数调用的代码 —— 并通过引入双重语法来改变代码的编译方式。我们所要做的就是
找到代价很高的函数调用，然后实现编译器宏，将它们转换为代价低的展开。

哪种类型的函数调用开销高呢？作为第一个例子，回想一下 :ref:`4-6-reader-security` 中，
函数可以执行 lambda 析构，而且这是更通用的 defmacro 析构的子集。当函数接受关键字
参数时，我们将它们作为分组的关键字符号对及其对应的值进行传递。关键字参数非常有用，
但遗憾的是，使用关键字参数的函数比不使用关键字参数的函数调用开销更大。解构不是
免费的。编译器需要将代码编译到函数中，该函数扫描必要的可变长度参数列表，以正确
的顺序获取值(包括插入默认值)，然后实际执行函数。一般来说，lisp编译这些关键字参数
的代码非常快，所以我们几乎从不注意（或关心）这种低效率。然而，在某些情况下，
我们确实会关心这个问题，特别是当我们在性能关键的循环中调用这样的函数时。

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

**fast-keys-strip** 是个实用程序，它接受由常规参数和关键字参数组成的 lambda 解构
列表，并返回用于引用这些参数的符号列表。换句话说，当传递 **(a b c)** 或
**(a &key b (c 0))** 时，程序返回 **(a b c)** ，但是传给程序 **(a &optional b c)**
是不行的。

.. code-block:: lisp

    (defmacro! defun-with-fast-keywords
              (name args &rest body)
      `(progn
          (defun ,name ,args ,@body)
          (defun ,g!fast-fun
                ,(fast-keywords-strip args)
                ,@body)
          (compile ',g!fast-fun)
          (define-compiler-macro ,name (&rest ,g!rest)
            (destructuring -bind ,args ,g!rest
              (list ',g!fast -fun ,@(fast-keywords-strip args))))))

**defun-with-fast-keywords** 用法与 **defun** 相同。与 **defun** 类似，
**defaun-with-fast-keywords** 的第一个参数是命名函数的符号，第二个参数是
参数列表，其余的是定义要执行的函数的形式。然而，与 **defun** 不同的是，
**defun-with-fast-keywords** 结构只能给出常规参数和关键字参数（没有 optional，
rests 等）。练习：扩展 **fast-keywords-strip** 来处理所有的 lambda 解构列表。

**defun-with-fast-keywords** 的展开非常复杂。它展开成三种结构。第一种展开
对函数的定义和常规的 **defun** 函数一样。第二种展开将函数定义了一个名为
**g!fast-fun** 的函数。这个函数类似于第一个函数，除了对每个参数（是否关键字）
接受一个非关键字参数。接下来定义一个编译器宏来将对第一个函数的调用转换为对
第二个函数的调用。因此，我们不是让第一个函数执行关键字解构，而是利用调用函数
的格式的编译时知识，并使用解构绑定将关键字按正确的顺序放在一起。

.. code-block:: lisp

    (defun
      slow-keywords-test (a b &key (c 0) (d 0))
      (+ a b c d))

    (compile 'slow-keywords-test)

    (defun-with-fast-keywords
      fast-keywords-test (a b &key (c 0) (d 0))
      (+ a b c d))

现在我们有了一个（几乎）双重语法 **defun**。带有关键字参数的函数的常规定义类似于
**slow-keyword-test**。编译它是为了下面的基准测试。**fast-keywords-test** 与
**slow-keywords-test** 的写法相同，只是用的是 **defun-with-fast-keywords**
而不是 **defun**。事实证明，我们不需要编译这个函数，因为
**defun-with-fast-keywords** 展开为一个调用，只对其中一个需要它的定义进行编译
—— 自动的 gensym **g!fast-fun**。

.. code-block:: lisp

    (defun keywords-benchmark (n)
      (format t "Slow keys: ~%")
      (time
        (loop for i from 1 to n do
          (slow-keywords-test 1 2 :d 3 :c n)))
      (format t "Fast keys: ~%")
      (time
        (loop for i from 1 to n do
          (fast-keywords-test 1 2 :d 3 :c n))))

    (compile 'keywords-benchmark)

**keywords-benchamrk** 是个简单的函数，其中使用了 **time** 宏来告诉我们对这两个
函数进行等价的一系列调用需要多长时间。注意，我们还编译了 **keywords-benchmark**。
关于基准测试的更多内容将在 :ref:`7-7-writing-and-benchmarking-compilers` 中介绍。

.. code-block:: none

    * (keywords-benchmark 100000000)
    Slow keys:
    ; Evaluation took:
    ;   17.68 seconds of real time
    Fast keys:
    ; Evaluation took:
    ;   10.03 seconds of real time

调用这个函数1亿次足以让我们看到，即使两个函数都被编译了，使用
**defun-with-fast-keywords** 定义的函数运行速度也比它的编译宏快了 40% 左右。
还要注意的是，编译宏的性能并不依赖于关键字参数是在编译时已知的常量。注意，
我们传递了 **n**，一种不同的 lisp 结构，作为 **:c** 关键字的参数。因此，编译宏将
快速版本展开为与慢版本相同的版本，除了没有关键字的析构开销。

那么，为什么 COMMON LISP 不为每个接受关键字的函数都这样做，并总是避免
开销呢？编译宏只在编译时应用，但我们希望在运行时保留对参数进行解构的能力。
下面是关于编译宏的要点：编译宏是对函数调用的优化，而不是对函数本身的优化。
在关键字的情况下，编译宏允许我们消除对函数的编译调用的开销，同时仍然让
原始函数（及其关键字解构代码）在运行时可用。编译宏为我们提供了两种不同
操作的双重语法，这两种操作只能通过上下文来区分。另一种避免关键字开销的
方法，请参阅 Norvig’s PAIP (PAIP-P323)。

还有哪些函数调用可以从编译宏中受益？我们不仅可以减少析构开销，而且通常
还可以通过预处理常量参数来减少函数本身的开销。编译宏可以在编译时执行一
些准备工作，因此不必在运行时执行。其中最明显的例子是 **format** 函数。
想想 **format** （或者，在 C 语言中，**printf** ）是如何工作的。它是个在运行
时将控制字符串传递给它的函数。然后 **format** 处理控制字符串并将格式化后
的输出打印到流中（或将其作为字符串返回）。实际上，在使用 **format** 时，
使用控制字符串作为程序对格式字符串解释器进行函数调用。使用编译宏，可以
消除函数调用，预处理控制字符串，并将函数调用更改为与调用站点相连接的
专门代码，编译器可以在其中进行进一步优化。听起来很难，不是吗？我们
必须知道如何将格式控制字符串转换成等价的 lisp 代码。幸运的是，与许多
其他事情一样，COMMON LISP 已经考虑过这个问题。COMMON LISP 对
格式化的处理是正确的。这是它为创建格式化输出而指定的特定于领域的语言，
可以将自己宏编译为 lisp 代码。这是 lisp 哲学的一部分 —— 所有的东西都
应该编译成 lisp。将控制字符串编译为 lisp 的宏是 **formatter**。当把控制
字符串提供给 **formatter** 时，它将展开为执行所需格式化的 lambda 结构。
例如，下面是个简单控制字符串的展开：

.. code-block:: none

    * (macroexpand '(formatter "Hello ~a~%"))
    #'(LAMBDA (STREAM &OPTIONAL
                      (#:FORMAT-ARG-1783
                        (ERROR "Missing arg"))
                      &REST FORMAT::ARGS)
        (BLOCK NIL
          (WRITE-STRING "Hello " STREAM)
          (PRINC #:FORMAT-ARG-1783 STREAM)
          (TERPRI STREAM))
        FORMAT::ARGS)
    T

所以说 **formatter** 展开成了个 lambda 结构。将控制字符串编译成 lisp
结构代码，适合于求值或将宏嵌入到其他 lisp 代码中，在那里它将成为一个
编译函数或内联到调用站点的编译代码中。但是请注意，**formatter** 的展开
必须要接受一个流，不能像 **format** 那样可以接受 **nil**。这是因为
**formatter** 展开的函数（如 **write-string** 和 **terpri** ）需要流。
可以用 **with-output-to-string** 宏来解决这个问题。

.. code-block:: lisp

    (defun fformat (&rest all)
      (apply #'format all))

    (compile 'fformat)

    (define-compiler-macro fformat
                          (& whole form
                            stream fmt &rest args)
      (if (constantp fmt)
        (if stream
          `(funcall (formatter ,fmt)
            ,stream ,@args)
          (let ((g!stream (gensym "stream")))
            `(with-output-to-string (,g!stream)
              (funcall (formatter ,fmt)
                ,g!stream ,@args))))
        form ))

**fformat** 是个完全透明的 **format** 封装器。 **fformat** 的存在是为了
定义一个编译宏来进行格式化。我们需要一个新的函数名，因为在 COMMON
LISP 指定的函数上定义编译宏是不行的。我们的编译宏利用了 defmacro 的
解构特性：&whole。我们使用它将 **format** 绑定到宏调用的实际列表结构。
这样做是为了利用编译宏的一个特性：编译宏完全可以选择不展开。如果我们
返回 **form** ，lisp 会发现我们只是返回传递的 form（用 **eq** 检查），同时
lisp 也将要求编译宏不对 form 进一步展开 —— 即便是我们正用编译宏展开
成个函数的用法。在编译时，我们选择使用 form 的另一种含义。这是编译宏
和普通宏之间的根本区别。编译宏可以与函数共享精确的双重语法，但普通宏
不能。在 **fformat** 中，当它的控制字符串参数不是常量时，编译宏不展开为
更有效的含义。在 **fformat** 中，我们仍然希望对非字符串控制字符串（比如
返回字符串的函数调用）调用 **fformat** 来工作。换句话说，我们仍然希望
能够在运行时生成控制字符串。这样的调用显然不能对控制字符串使用编译时
优化。

.. code-block:: lisp

    (defun fformat-benchmark (n)
      (format t "Format:~%")
      (time
        (loop for i from 1 to n do
          (format nil "Hello ~a ~a~%" 'world n)))
      (format t "Fformat:~%")
      (time
        (loop for i from 1 to n do
          (fformat nil "Hello ~a ~a~%" 'world n))))
    (compile 'fformat -benchmark)

**format-benchmark** 与前面介绍的 **keywords-benchmark** 函数几乎相同。
它使用 **time** 来比较使用常规 **format** 和新的 **fformat** 执行大量格式操作
所需的时间。以下是 100 万次迭代的结果：

.. code-block:: none

    * (fformat-benchmark 1000000)
    Format:
    ; Evaluation took:
    ;   37.74 seconds of real time
    ;   [Run times include 4.08 seconds GC run time]
    ;   1,672,008,896 bytes consed.
    Fformat:
    ; Evaluation took:
    ; ; ;
    26.79 seconds of real time
    [Run times include 3.47 seconds GC run time]
    1,408,007,552 bytes consed.

大概提升了 30%。编译宏不仅减少了执行格式化所需的时间，而且还减少了开销
（这反过来又减少了垃圾回收的时间）。编译宏避免了在运行时解释格式字符串，
而是在函数被编译时只执行一次大部分的计算 —— 这是它本该做的。不幸的是，
基准测试常常模糊或删除重要的细节。虽然用 **fformat** 预编译格式字符串可以
消除解释开销，但这样做的代价是编译一个更大的程序。即使主存充足，较大的
代码也会因为指令缓存性能的降低而运行得更慢。

在本节中，我们讨论了使用常规宏、读取宏和专为这个任务设计的一种特殊类型
的宏 —— 编译宏来定制代码性能的方法。希望本节和本章的其余部分能说服你，
如果想编写真正有效的代码，就需要 COMMON LISP。因为宏，你需要
COMMON LISP。

练习1：下载 Edi Weitz 的 CL-PPCRE（在 :ref:`4-4-cl-ppcre` 中），
看看 **api.lisp** 怎么使用编译宏。访问Edi 的网站并下载一些他的 lisp 包，
这些包看起来很有趣。

练习2：当我们为 **fformat** 编写编译宏时，我们被迫显式地使用 **gensym**，
因为没有 **define-compiler-macro!** 宏。解决这个问题。
较难的练习：定义 **define-compiler-macro!** 这样就能使用了 **defmacro!**
的功能而不用调用 **gensym**。提示：跳出思维定势。


.. _7-3-getting-to-know-your-disassembler:

7.3 了解反汇编
=====================================

如果不检查处理器为不同的 lisp 结构执行的原始指令，就很难真正了解在 lisp 中那些代码
的开销昂贵。就像在编写宏时，查看它们的展开通常很有帮助，有时查看lisp 程序编译后
的展开（通常是汇编指令）也很有用。因为 lisp 编译器可以并且经常被认为是宏扩展器，
它们生成的机器码，从某种奇怪的意义上说，本身就是 lisp 代码。因为 lisp 与其说是一
种语言，不如说是一种创建语言的构建材料和结构，lisp 是用来定义和编译一种恰好与
处理器指令集相同的语言。

COMMON LISP 提供了一个名为 **disassemble** 的函数来查看已编译的展开。
**disassemble** 类似于 [USEFUL-LISP-ALGOS2](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.55.9152)
中描述的CMUCL宏扩展 **macroexpand-all** 。给 **disassembler** 函数或存在的
**symbol-function** 绑定，我们可以查看在调用函数时要执行的原始机器码指令。

问题是这些原始的机器代码指令看起来一点也不像 lisp。这些指令对于某些非常随意
的机器来说，通常是奇怪的、微小的步骤，而不是 lisp 舒服的嵌套括号。查看编译后
的 lisp 代码展开就像用放大镜阅读海报一样。可以看到喜欢的任何部分的细节，但
仅凭这一点来解释整体情况是困难的，甚至是不可能的。更糟糕的是，当查看这种
细节级别的代码时，有时不可能查看任何一段机器码并确定编译器为什么把它放在
那里。

不幸的是，没人知道超过 **compile** 函数的 lisp 的最好实现。毫无疑问，有很多宏
展开来完整这个代码，其中一些是板上钉钉的事，因此它可能可以标准化，但最好的
使用硬件资源（如 CPU 周期和内存）的方法仍然是（可能一直都是）个非常热门的
研究课题。比编译器设计的改进更难跟踪的是硬件的不断改进。最初有意义的优化可
能变得不相关甚至完全不正确。我们不需要找太多的例子来说明不断变化的世界是如
何影响效率假设的。

科学家们过去避免在需要良好表现的代码中使用浮点计算，而是选择基于机器字的定点
计算。这是因为计算机没有专门的浮点硬件，所以被迫使用处理器的整数指令来模拟它。
因为处理器并没有为此进行真正的优化，浮点运算总是比定点运算慢得多。然而，随着
时间的推移，硬件开始出现专门的浮点协同处理器，这些处理器被设计来以光速般的
速度执行这些浮点运算。几乎在一夜之间，科学家们从假设固定点运算总是比浮点运算
快得多，到不得不在做出决定之前对他们的硬件进行调查和基准测试。硬件的发展改变
了浮点数的性能现实。不久之后，计算机开始配备 2 个、4 个或更多的浮点协同处理器，
科学家们发现，如果他们能够让浮点指令的流水线充满，浮点运算通常可以比定点运算
表现得更好。许多出于性能原因而选择固定点的程序 —— 在大约 10 年的时间框架内
—— 从选择正确的实现到选择错误的实现。

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

在开发宏时，同样有用的是看 **macroexpand** 和 **macroexpand-all** 的输出，这有
助于查看 **disassembler** 的输出，不仅了解实现功能，而且确保给 lisp 所需的所有
信息来生成有效的展开。 **dis** 是个令在反汇编输出中检查部分 lisp 代码变得很容易的
宏。它的第一个参数是一个符号列表或一个类型和符号列表。想知道 **dis** 是怎么工作
的，直接展开。这里是 **dis** 展开为一个简单的二进制加法:

.. code-block:: none

    * (macroexpand
        '(dis (a b) (+ a b)))
    (DISASSEMBLE
      (COMPILE NIL
        (LAMBDA (A B)
          (DECLARE)
          (+ A B))))
    T

为什么其中会有个空的 **declare** 结构呢？它是一个占位符，**dis** 可以插入类型声明，
当像下面那样在参数中指定它们:

.. code-block:: none

    * (macroexpand
        '(dis ((fixnum a) (integer b))
    (+ a b)))
    (DISASSEMBLE
      (COMPILE NIL
        (LAMBDA (A B)
          (DECLARE (TYPE FIXNUM A)
                  (TYPE INTEGER B))
          (+ A B))))
    T

因为 **dis** 展开成一个（封装的）lambda 结构，所以它的工作方式与 lambda 非常相似。
只要你想的话，可以添加额外的声明，并且返回值很重要（因为 lambda 结构提供了一个
隐式的 progn）。加载了本书的代码后，试着在你的 lisp 环境中输入下面的代码：

.. code-block:: lisp

    (dis (a b)
      (+ a b))

机器码应该相当短，但这是因为调用了一个预编译函数 —— 这个函数足够的智能来提供
所有花哨的 lisp 数字特性，如类型感染、有理数简化等，从而隐藏了大部分的复杂性。
这被称为 *间接（indirection）* ，在反汇编器的输出中可能相当明显:

CALL #x1000148 ; GENERIC-+

用三个参数试试看：

.. code-block:: lisp

    (dis (a b c)
      (+ a b c))

练习：通用加法函数有多少层间接呢？ **(<= 0 N)** 中的参数 N 又有多少层呢?

现在尝试锁定其中一个变量的类型。将其与前面没有声明类型的示例进行比较：

.. code-block:: lisp

    (dis ((fixnum a) b)
      (+ a b))

某些 **OBJECT-NOT-FIXNUM-ERROR** 现在应该很明显了。Lisp 编译了一些
额外的代码来做这种类型检查，同时间接控制泛型的加法函数，因为 **b** 的
类型在编译时是未知的，因此可能需要 lisp 的所有花哨的数值行为，比如
感染。

这不是获得高效代码的方法。事实上，这段代码的效率甚至可能比前一段
代码略低。为了编写高效代码，需要用到一个称为 *内联（inlining）* 的
进程。对于一些特殊的操作，当有足够的类型信息时，lisp 编译器知道如何
避免间接或直接向正在编译的函数中添加机器代码来执行所需的操作。
下面的通用加法函数中不应该有间接：

.. code-block:: lisp

    (dis ((fixnum a) (fixnum b))
      (+ a b))

这种内联过程可能会导致比使用间接方法的机器代码更多的机器代码。
这是因为泛型加法函数中实现的一些（但不是全部）功能被复制到了
编译的函数中。虽然看起来更长，但在某些情况下，由于更少的间接，
该代码将执行效率更高。

但是这种混乱的机器码仍比 C 实现的效率低得多。在编译还是有各种
参数计数、类型和溢出检查，以至于与开销相比，这么多的额外开销
比实际添加的成本仍要低。如果在循环中使用这个函数，这种开销可能
就不能接受了。

对于像 C 这样的语言，可以在任何地方指定类型，而在任何地方都不
强制执行安全性，所以代码总是高效的，但也不安全，编写起来总是
很麻烦。在大多数动态 Blub 语言中，不需要指定类型，并在任何地方
都强制执行安全性，因此代码总是安全的，不烦人，但也不会高效。
对于大多数强大的静态 Blub 语言，可以在任何地方指定类型，并在
任何地方强制执行安全性，因此代码总是高效和安全的，但很烦人。
Lisp 给了你选择。因为 lisp 默认为安全模式，lisp 程序通常看起来比
C 程序慢一些，但几乎总是更安全。因为 lisp 为程序员提供了一个优秀
的类型声明系统和实现，并且有很优秀的编译器，所以 lisp 程序几乎总
是和动态 Blub 程序一样安全，而且通常要快得多。最重要的是，lisp
有宏，所以如果有什么烦人的东西，好吧，改变它！

让我们继续，并让 lisp 让我们的加法更高效。回想一下， **#f** 是高速、
低安全声明读宏的缩写。

.. code-block:: none

    (dis ((fixnum a) (fixnum b))
      #f
      (+ a b))

这次机器指令码应该比之前的短一点。类型检查和参数计数检查应该删除了。
但这仍然不是我们想要的单一指令、混乱、危险的 fixnum 加法。为了深入
了解正在发生的事情，我们应该检查编译器注释。注释是编译器所做的观察，
它本质上是说：“你看起来像是在尝试做一些高效的事情，而且你已经快完
成了，但我需要澄清一下你的意图。这里有个小窍门让你更清楚……”

编译注释是无价的信息来源。当试图创建高效的 lisp 代码时，应该仔细阅读
并考虑它们。Lisp 编译器使用类型推断系统来发现代码的复杂属性，即使是
程序员也可能没有考虑到这些属性。在上面的例子中，编译器应该会给我们
这样的提示:

.. code-block:: none

    ; Note: Doing signed word to integer coercion
    ;       (cost 20) to "<return value>".


Lisp 不会做任何愚蠢的事情，比如忽略 fixnum 溢出，除非明确要求它这样做。
因此，为了让 lisp 不小心给出了个可能不是很安全的函数，我们需要避免带符号
的单词 （fixnum）到整数 （bignum）的检查和强制。我们需要告诉 lisp，
溢出是可以接受的，是的，我们真的想安静地返回一个 fixnum：

.. code-block:: none

    (dis ((fixnum a) (fixnum b))
      #f
      (the fixnum (+ a b)))

现在已经燃起来了。这大致相当于一个 C 的 fixnum 加法函数：一些机器指令
将两个寄存器相加，然后将控制权返回给调用者。虽然反汇编程序可以为 lisp
效率的所有领域提供许多见解，但它会教你两项主要的技能。第一个技巧在
本节中主要介绍：如何使用声明来获得有效的数值行为，特别是在循环内部。
第二个问题是如何有效地使用数组/向量数据结构。这将在 :ref:`7-4-pointer-scope`
中讨论。

就像技术进步将浮点运算的效率现实从应该避免的东西变成了应该利用的
东西一样，lisp 编译器技术的进步 —— 结合 COMMON LISP 的正确类型
和安全声明系统 —— 正在改变我们对效率的看法。有了这些工具，以及软件
系统日益增长的复杂性需求，问题就从如何使 lisp 像低级语言一样高效变成
了如何使其他语言像 lisp 一样高效。当然，答案是在 lisp 中用宏实现它们。


.. _7-4-pointer-scope:

7.4 指针作用域
=======================

从一种语言中删除指针是否会降低该语言的能力？特别是，lisp 缺乏显式的指针作用域
是否妨碍我们有效地实现指针算法中指定的算法？事实证明不是这样的，在 lisp 中缺乏
对指针的直接支持在理论上和实践上都不构成挑战。在像 C 这样的语言中，任何可以用
指针实现的算法或数据结构都可以在 lisp 中实现，甚至更好。

但是，什么是指针作用域，我们为什么要使用它？指针作用域包括将计算机的内存(或
虚拟内存)作为一个大的、可索引的数组来处理，它可以从中加载和存储固定值。这听起来
危险吗？当然，因为它是许多复杂错误的根源，也是当今几种最大的软件安全问题的直接
原因。

.. code-block:: lisp

    (defmacro! pointer -& (obj)
      `(lambda (&optional (,g!set ',g!temp))
        (if (eq ,g!set ',g!temp)
          ,obj
          (setf ,obj ,g!set))))

    (defun pointer -* (addr)
      (funcall addr))

    (defsetf pointer -* (addr) (val)
      `(funcall ,addr ,val))

    (defsetf pointer -& (addr) (val)
      `(setf (pointer -* ,addr) ,val))

指针作用域实际上是指定间接访问的一种方法，也就是跨环境访问，而间接访问恰好也
与固定数值运算绑定。我们通常如何跨环境编程？我们使用 COMMON LISP 提供的词法
或动态作用域，这两种作用域的双重组合，或者由宏创建的新类型的作用域。
**pointer-&** 宏和 **pointer-*** 函数是为我们描绘指针作用域错觉的例子，表明当你
认为你需要一个指针时，你真正的需要可能是个闭包。我所听到的关于指针和闭包之间的
类比的第一个也是唯一的例子是 Oleg Kiselyov 在 **comp.lang.scheme** 新闻组上发表
的一篇文章 [pointer-as-closures](https://okmij.org/ftp/Scheme/pointer-as-closure.txt) 。
他建议使用闭包来模拟指针，并为 Scheme 提供了一个实现。


**pointer-&** 和 **pointer-*** 展示了一种通过闭包模拟指针间接指向的可能。当使用
**pointer-&** 宏时，它会展开成 lambda 结构，其中有一些智能，以确定您是否想要获取
或设置值，并相应地执行。 **pointer-&** 使用 *gensyms* 来做到这一点。而不是使用它们
作为绑定的名字以避免不必要的变量捕获在编译时， **pointer-&** 使用它们以确保没有
运行时的异常捕获，这里阻止将闭包的值设为个确定值，因为它与我们的实现冲突。例如，
我们可能已经为这个选择了 lisp 默认值 **nil**，通常这可以运行，除非我们将 **nil** 作为
参数传参。*gensym* 在运行时使用很方便，因为我们知道永远不会有另一个值 **eq** *gensym*。
这就是他们存在的理由。

**pointer-*** 及其 **defsetf** 是通过泛型变量访问这些间接值的框架。这里 **pointer-&**
中的 **defsetf** ，在 **pointer-&** 展开中知道如何设置嵌套的间接指向。一个简单的例子，
我们可以创建个闭包，通过在 let 环境中创建对绑定的引用来模拟 C 中常见的 *指向指针的指针
（pointer to a pointer）* 模式：

.. code-block:: none

    * (let ((x 0))
        (pointer-& (pointer-& x)))
    #<Interpreted Function>

将这个闭包保存起来，以便之后使用，方法是将它从 **\*** 特殊变量中转移过来（让我们保持
这些星号的清晰）：

.. code-block:: none

    * (defvar temp-pointer *)
    #<Interpreted Function>

现在可以解引用这个闭包了：

.. code-block:: none

    * (pointer-* temp-pointer)
    #<Interpreted Function>

看来又有另一个闭包了。我们只解引用了指针链的一个步骤。使用 **\*** 特殊变量来引用前面
的结果，让我们进一步解引用：

.. code-block:: none

    * (pointer-* *)
    0

**0** 是最开始指向的对象。我们也可以使用这种解引用语法 —— 当然这是闭包的错觉 ——
通过指针链来设置这个对象的值：

.. code-block:: none

    * (setf (pointer-* (pointer-* temp-pointer)) 5)
    5

当然，这改变了指向的原有的 let 环境，因此有了个新值 —— 5：

.. code-block:: none

    * (pointer-* (pointer-* temp-pointer))
    5

如果我们想的话，也可以添加另一层间接指向：

.. code-block:: none

    * (pointer-& temp-pointer)
    #<Interpreted Function>

现在需要三层解引用：

.. code-block:: none

    * (pointer-* (pointer-* (pointer-* *)))
    5

并且其自身也可以像通用变量那样访问：

.. code-block:: none

    * (setf (pointer-* (pointer-* (pointer-* **))) 9)
    9

即使它们可能处于不同的间接层，这个解引用链中的所有闭包仍然指向最初的 let 环境：

.. code-block:: none

    * (pointer-* (pointer-* temp-pointer))
    9

但这可能不是我们所说的指针作用域。因为大多数计算机处理器认为内存是一个很大的
固定数字数组，而且由于 C 是围绕现有处理器的功能设计的，所以 C 的指针作用域永久
性地与固定数字算法绑定在一起。在 C 语言中，当解除对指针的引用时，你总是知道发
生了什么：编译器在代码中编译到带有固定数字的内存索引，并检索或设置一个固定数
值。C 的指针作用域和上面的闭包解引用技术的最大区别在于，虽然 C 允许我们通过添
加或减去固定值来改变指针指向的位置，但由 **pointer-&** 编译并使用 **pointer-\***
访问的闭包是固定的。用于访问和设置它们的代码 —— 不管是什么 —— 都会在编译时
添加到间接环境中。即使在上面的简单示例中，我们至少使用了两种不同类型的闭包，
由于泛型变量的存在，这两种闭包都可以通过统一的解引用语法进行访问。我们最初
所指的 **x** 是一个词法变量，而我们所指的 **temp-pointer** *tunnel* 变量是动态变量。
正如 :ref:`6-7-pandoric-macros` 中，我们可以随意定制闭包，因此也可以随意定制
间接闭包。

所以闭包实际上比 C 风格的指针更灵活、更安全。当你认为你需要一个指针时，你可能
需要一个闭包。闭包不仅仅是个可以用作地址的固定数字，它是编译后用于在任何环境中
检索和设置任何类型数据的代码。尽管对于大多数任务来说，闭包是实现间接的最佳构造，
但有时我们希望利用处理器的固定数目寻址功能来实现非常高效的代码。C 可以做的，
COMMON LISP 做得更好。

在 lisp 中使用 C 风格的指针实际上非常简单，不需要偏离通常的 lisp 技术。只是提供
一个固定数值数组，使用数字索引数组 —— 就像 C 中那样。然后，用声明让 lisp 去掉
类型和安全检查，所以编译也和 C 一样。最后，用宏使整个过程方便和安全。

通常，为数组建立索引是一个复杂而缓慢的过程。编译器需要检查索引是否为数字，在
索引数组时，确保索引在数组的范围内。此外，不同类型的数组有不同的代码来访问元素。
加载了这本书的代码后，试着执行下面代码（ **dis** 详见 :ref:`7-3-getting-to-know-your-disassembler`）：

.. code-block:: lisp

    (dis (arr ind)
      (aref arr ind))

因为 **aref** 在不知道类型的情况下可以表示很多可能，所以编译器可能不会内联数组访问
代码。在上面的反汇编输出中，应该看到对类似 CMUCL 的 **data-vector-ref** 函数调用。

练习：获取 lisp 环境的源代码并检查这个函数。在 CMUCL 中，它位于 **array.lisp** 文件
中。还要检查该文件中的其他函数，包括数据向量集。如果 lisp 环境没有提供完整的源代码
，或者不能对所拥有的源代码做任何想做的事情，请尽快升级COMMON LISP 环境。

就像 COMMON LISP 在有足够的类型信息时可以内联函数 **+** 一样，它也可以内联 **aref** 。
试试下面的代码：

.. code-block:: lisp

    (dis (((simple-array fixnum) arr)
          (fixnum ind))
      (aref arr ind))

上述操作应该已经删除了对通用数组引用函数的间接访问。简单数组是一维数组，其中的元素
在内存中相邻，就像 C 风格的内存。在上面我们指定了固定数值作为数组元素，但是
COMMON LISP 环境可能还提供了不同大小、字节、无符号字节、浮点数、双浮点数等类型
的固定数值。虽然上面没有包含间接的，但是仍然有很多代码实现了在 lisp 编程时通常依赖
的类型和安全检查。然而，正如我们可以使用 :ref:`7-2-macros-make-lisp-fast` 中的
**#f** 读取宏 告诉 lisp 使算术更快，同样也可以用于数组引用：

.. code-block:: none

    (dis (((simple-array fixnum) arr)
      (fixnum ind))
    #f
    (aref arr ind))

与之前的 **aref** 不同，这段代码的性能将不会被类型和安全检查所控制。这是应该在性能
关键循环中使用的代码。请注意，因为我们已经从这段代码中删除了几乎所有的安全特性，
所以它与 C 语言中的同类代码一样危险。特别是，它可能会遇到缓冲区溢出问题。使用 C，
在任何地方都是这样编程的。使用 lisp，你可以安全地在任何地方编程，除了性能问题，
调优代码的 *hot-spots* ，使整个程序运行得更快。由于使用宏，这些 *hot-spots* 可以
任意小。比如说，不需要在快速/危险模式下编译整个函数。宏允许我们优化表达式中
细小的、特定的部分。高效代码可以透明地与安全代码和宏共存，这放弃了最不安全的
必要条件，以实现所需的性能。

因为如果你在本书中读到这里，你应该已经对宏的编写和声明有了很好的了解，关于
指针作用域没有更多需要说明的了。简而言之，C 提供了一种非常特定作用域的语言，
用于基于固定数量算法控制 CPU，但你可以使用宏编写更好的语言。高效的指针作用
域（我们现在可以承认这实际上意味着数组访问 —— 闭包示例除外）主要是了解宏
如何工作，声明如何工作，以及如何读取反汇编程序的问题。

.. code-block:: none

    (defmacro! with-fast-stack
              ((sym &key (type 'fixnum) (size 1000)
                          (safe -zone 100))
                &rest body)
      `(let ((,g!index ,safe-zone)
            (,g!mem (make-array ,(+ size (* 2 safe-zone))
                                :element-type ',type)))
        (declare (type (simple -array ,type) ,g!mem)
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
                  (if (<= ,,(- size safe -zone)
                          ,',g!index)
                    (error "Stack overflow: ~a"
            ,@body)))

高效访问数组的宏示例是 **with-fast-stack**。选择这个宏是为了讨论 *摊销（amortisation）*。
**with-fast-stack** 实现了个名为 **sym** 的堆栈数据结构。不同于 COMMON LISP **push**
和 **pop** 使用 cons 单元存储任何类型的栈的元素，**with-fast-stack** 中用简单的数组存储可
以用 **:type** 关键字来指定类型的固定类型。数组的大小也是固定的，但是这个大小可以通过 **:size**
关键字来设置。通过使用一些宏定义的局部宏来访问堆栈。如果堆栈名是 **input**，则宏绑定将是
**fast-push-input**、**fast-pop-input** 和 **check-stacks-input** 。用 **dis** 检
查编译后的展开：

.. code-block:: none

    * (dis ((fixnum a))
        (with-fast-stack (input :size 2000)
          (loop for i from 1 to 1000000 do
            (fast-push-input a))))

**fast-push-input** 操作编译成非常紧凑（且非常不安全）的机器代码:

.. code-block:: none

    ;;; [8] (FAST-PUSH-INPUT A)
    MOV     ECX, [EBP-20]
    MOV     EDX, [EBP-16]
    MOV     EAX, [EBP-12]
    MOV     [ECX+EDX+1], EAX
    MOV     EAX, [EBP-16]
    ADD     EAX, 4
    MOV     [EBP-16], EAX

但是循环像往常一样安全地编译，实现了错误检查和间接算术函数，即使是在
**with-fast-stack** 宏中。

.. code-block:: none

    ;;; [7] (LOOP FOR I FROM 1...)
    ...
    CALL    #x100001D0  ; #x100001D0: GENERIC-+
    ...
    CALL    #x10000468  ; #x10000468: GENERIC->

明显，这个循环不会像预期的那样快。它的性能将由循环开销决定，而不是堆栈
操作。如果需要高效，可以将 **i** 声明为固定值，并向循环中添加速度声明，就像
之前看到的那样。安全代码可以与高效代码共存。当然，刚刚反汇编的代码非常危险。
它从不检查堆栈的高度来缠看是否上溢或下溢出边界。这是为了效率而尽量避免的。
**with-fast-stack** 提供的解决方案是受到 *forth* 编程语言中 **stack** 一词的启发。
通过 **check-stacks-input** 本地宏，我们的代码可以验证堆栈是否在边界内，
否则会抛出异常。由于 *forth* 被设计为在最有限的硬件平台上性能也很好，因此 *forth*
分摊了执行边界检查的成本。与默认情况下 lisp 在每个操作之后执行不同，它只在每
N 个操作之后执行。在 *forth* 中，这个词通常只在对 REPL 中的结构求值之后才会被
调用（关于 *forth*，我们将在 :ref:`chapter08` 中介绍）。
因此，我们可以每 10 个操作检查一次边界，而不是每次操作都检查边界，也许可以减少 90%
的边界检查成本。当我们检查堆栈时，我们知道，最坏情况下，有 10 个超出边界的元素。或者
可能在代码中有一些方便的、非关键性能的地方可以使用 check 宏。

**with-fast-stack** 另一个特性是其创建的数组有安全区域。也就是说，如果你搞砸了，
它会在堆栈的任意一侧分配额外的内存作为紧急通道。这并不意味着跑到这些安全区域
是好主意（特别是下溢时），但比跑到未分配的内存要好。

正如前面提到的，刚刚反汇编的代码非常危险，它会将固定数值写入未分配的内存中。
永远不要这样做。

练习：试试这个，以下是我执行的结果:

.. code-block:: none

    * (compile nil
        '(lambda (a)
          (declare (type fixnum a))
          (with-fast-stack (input :size 2000)
            (loop for i from 1 to 1000000 do
              (fast-push-input a)))))
    #<Function>
    NIL
    NIL

危险的代码编译得很好。试试运行它:

.. code-block:: none

    * (funcall * 31337)
    NIL

好吧，这不是我们所担心的灾难。有什么不好的事情发生吗?

.. code-block:: none

    * (compile nil '(lambda () t))
    ; Compilation unit aborted.

Hm，这个结果不太好了。

.. code-block:: none

    * (gc)
    Help! 12 nested errors.
    KERNEL:*MAXIMUM-ERROR-DEPTH* exceeded.
    ** Closed the Terminal
    NIL

这个结果肯定不好。因为 lisp 是运行在 unix 上的进程，所以它也可能接收到信号，
指示在分配的虚拟内存之外编写了代码（称为 *段错误 seg-fault* ）。CMUCL 将
这些作为可恢复条件处理（尽管你应该总是重新加载 lisp 镜像）：

.. code-block:: none

    Error in function UNIX::SIGSEGV-HANDLER:
      Segmentation Violation at #x58AB5061.
      [Condition of type SIMPLE-ERROR]

在这些状态下，lisp 镜像称之为 _欺诈（hosed）_ 。那些有可能被像这样成为 “欺诈”
的项目都是即将发生的安全灾难。C 和 lisp 之间的区别是，C 几乎在所有地方都有
这种潜力，而 lisp 几乎没有。如果需要承担基于数组的指针作用域的风险，lisp 宏是
最不突出和最安全的方法。当然，如果不想承担这些风险 —— 坚持使用闭包。


.. _7-5-tlists-and-cons-pools:

7.5 Tlist 和 cons 池
=============================

本节是关于内存管理的，但可能并不是你所想象的那样。 我甚至都不想介绍它，因为我
害怕延续一个关于 lisp 的错误传言，即 consing 很慢的错误观念。 不好意思，
但这个传言是错的； consing 其实很快。 当然，最小化无限范围存储的算法通常是
理想的，但大多数算法可以通过 consing 更容易和直接地编写。 当要用到内存时，
不要害怕使用 cons。 实际上，有时可以在 lisp 中进行的出色优化是将算法调整为
可以用 cons 单元实现的形式，以便从经过调整的 lisp 垃圾收集器中受益。 就像
编写自己的哈希表实现可能是个坏主意一样，破解自己的内存分配例程可能同样愚蠢。
也就是说，本节解释了一些方法。 惊讶吧，我们用宏来进行内存管理。

在讲内存分配之前，我们先绕一下相关的弯路。 尽管 Common Lisp 是专业 lisp
程序员的首选，但很多好的 lisp 入门教科书都是关于 Scheme 的。 通常最受推崇的
是 Hal Abelson、Jerry Sussman 和 Julie Sussman 的计算机程序结构和解释（SICP）
。 SICP 几十年来一直被麻省理工学院的新生崇拜或忍受，它最初是在麻省理工学院首次引入的。
Scheme 对学术界的吸引力是深刻而普遍的。 大多数宏专家从 Scheme 开始他们的 lisp
体验——只有当他们准备好开始编写严肃的宏时，他们才会迁移到宏的黑客语言：Common Lisp。

但是，当迁移时，总是会携带一些东西。 你无法避免你的经历 —— 就是你的根源。 如果你
根源在于 Scheme 并且已经阅读过 SICP，那么你可能还记得队列（另请参阅 [USEFUL-LISP-ALGOS1-CHAPTER3]）。
对它们的另一种描述，我们在这里使用的描述，来自另一本优秀的 Scheme 书，
Schematics of Computation，被称为 tlist。 tlist 是一种以它的发明者命名
的数据结构，一个名叫 Warren Teitelman 的 Interlisp 黑客。 尽管 tlists
在 Schematics of Computation 中作为 Scheme 代码提供，但我们在这里将它们
作为 Common Lisp 的一个端口提供。

.. code-block:: lisp

    (declaim (inline make-tlist tlist-left
                    tlist-right tlist-empty-p))

    (defun make-tlist () (cons nil nil))
    (defun tlist-left (tl) (caar tl))
    (defun tlist-right (tl) (cadr tl))
    (defun tlist-empty-p (tl) (null (car tl)))

正如我们在构造函数 **make-tlist** 中看到的那样，**tlist** 只是个 **cons** 单元格。
但是，**tlist** 不像常规列表那样使用 **car** 作为元素，将 **cdr** 作为下一个 **cons**，
而是使用 **car** 指向实际列表中的第一个 **cons**，而 **cdr** 指向最后一个。 如果
**tlist** 的 **car** 为 **nil**，则认为该 **tlist** 为空。 与常规列表不同，空
**tlist** 是不同的（不相等 eq）。 对于 **tlist**，作为 **tlist** 的 **cons** 单元的
**car** 指向一个包含 **tlist** 左侧元素的 **cons** 单元。 **cdr** 指向右边的
**cons**。

函数 **tlist-left** 和 **tlist-right** 返回 tlist 的左右元素而不修改 tlist。 如果
tlist 为空，则这些函数返回 nil。 如果只使用这些功能，将无法在 tlist 中存储 nil。
幸运的是，可以在将 tlist 与 **tlist-empty-p** 谓词一起使用之前检查它是否为空，因此
可以存储 nil。

因为这样做很容易，我们决定告诉编译器所有这些函数都可以内联。 这将让 lisp 编译器为
tlist 函数生成更有效的展开。 在一些不太提供编译器控制的语言（如 C）中，使用原始宏
系统来确保像 tlist 实用程序这样的函数是内联的。 在 lisp 中，可以完全控制编译器，
不需要为此使用宏。 本章中的宏不仅仅是内联。

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

我们可以使用 **tlist-add-left** 函数将元素添加到 tlist 的左侧，使用
**tlist-add-right** 将元素添加到右侧。 因为维护了指向列表末尾的指针，所以将元素添加到
tlist 的任一端是相对于tlist 长度的恒定时间操作。 但是，一般来说，添加到 tlist 并不是一个恒定
的时间操作，因为 consing 有内存分配开销。 使用 cons 意味着添加 tlist 通常会导致垃圾收集的总
开销。

给定函数仅支持从 tlist 左侧删除项目。 因为我们只保留指向 tlist 的第一个和最后一个元素
的指针，所以找到倒数第二个元素的唯一方法是从 tlist 的左侧开始遍历整个列表。

.. code-block:: lisp

    (declaim (inline tlist-rem-left))

    (defun tlist-rem-left (tl)
      (if (tlist-empty-p tl)
        (error "Remove from empty tlist")
        (let ((x (car tl)))
          (setf (car tl) (cdar tl))
          (if (tlist-empty-p tl)
            (setf (cdr tl) nil)) ;; For gc
          (car x))))

tlist 是建立在 cons 单元之上的队列抽象，这个特别有用，因为它是一种透明的数据结构。 虽然
一些实现 tlist 功能的数据结构（如队列）只提供数据结构的有限接口，但 tlist 被直接指定为
cons 单元格。 Teitelman 没有发明一些 API 来满足每个人的需求，而是决定将 tlist 的规范
直接绑定到 lisp cons 单元格。 这个设计决策将 tlist 与其他队列实现区分开来。 使用透明
规范进行编程时，不是制作特殊的 API 函数来做事，代码就是 API。

.. code-block:: lisp

    (declaim (inline tlist-update))

    (defun tlist-update (tl)
      (setf (cdr tl) (last (car tl))))

如果要访问 tlist 的 car 并修改其内容，需要确保 tlist 保持一致。 假设在我们操作后，所需
的列表存储在 tlist 的 car 中，我们可以使用 **tlist-update** 来适当地设置 cdr。


因此，tlist 最主要的好处是尽可能地模拟常规的 lisp 列表，同时可以在恒定时间内将元素添加到
末尾的操作。 因为 tlist 像常规列表一样使用 cons，所以这两者的内存开销是一样的。

.. code-block:: lisp

    (defvar number-of-conses 0)

    (declaim (inline counting-cons))

    (defun counting-cons (a b)
      (incf number-of-conses)
      (cons a b))

Common Lisp 没有为监听或控制内存分配指定太多功能。 所以需要自己写一些。 首先，回顾 :ref:`3-5-unwanted-capture`，
我们不允许重新定义或重新绑定 Common Lisp 指定的函数。 我们不能直接拦截对 cons 的调
用，所以改为使用包装器。 **counting-cons** 与 cons 相同，只是每次调用它时都会增加
**number-of-conses**。

.. code-block:: lisp

    (defmacro! with-conses-counted (&rest body)
      `(let ((,g!orig number-of-conses))
        ,@body
        (- number-of-conses ,g!orig)))

**with-conses-counted** 是检查  **number-of-conses** 值的主要接口。 它的展开会记录它的
初始值，执行宏体中提供的操作，然后返回 **counting-cons** 被调用的次数。


将 cons 重命名为 **counting-cons** 策略的坏结果是，我们想要检查内存性能的任何例程都需要重写
以使用 **counting-cons**，就像在 **counting-push** 中一样。 这里我们可以看到，每次调用
**counting-push** 时，只调用了 **counting-cons** 一次：

.. code-block:: none

    * (let (stack)
        (with-conses-counted
          (loop for i from 1 to 100 do
            (counting-push nil stack)
            (pop stack))))
    100

上面的 **pop** 运算符从堆栈中删除元素以及用于存储该元素的 cons 单元格。这些 cons 单元格会
怎么样呢？它们会变成垃圾。通常 lisp 会随处吐出这些垃圾而没有人关心，因为 Common Lisp 环境
具有出色的回收程序，称为垃圾收集器，可以回收这些存储。然而，收集垃圾并不是免费的——垃圾的捡起、
运送到其他地方、再加工成适合使用的东西必须消耗一定的资源。如果我们可以在现场创建小型回收计划
会怎样？比如上面的循环调用了 **counting-cons** 100次，产生了100个需要回收的垃圾。但是，快速
浏览一下代码会发现堆栈上一次不会超过一个项目。如果我们回收了这个 cons 单元格，让它可以再次
用于 **count-push**，就不需要调用 **counting-cons** 来获取另一个 cons 单元格。这个概念被
称为 cons 池。除了减少垃圾收集器的压力之外，cons 池还可以帮助改善经常分配内存的数据结构的局部
性。

.. code-block:: lisp

    (defmacro counting -push (obj stack)
      `(setq ,stack (counting-cons ,obj ,stack)))


    (defmacro with-cons-pool (&rest body)
      `(let ((cons-pool)
            (cons-pool-count 0)
            (cons-pool-limit 100))
          (declare (ignorable cons-pool
                              cons-pool-count
                              cons-pool-limit))
          ,@body))

    (defmacro! cons-pool-cons (o!car o!cdr)
      `(if (= cons-pool-count 0)
          (counting-cons ,g!car ,g!cdr)
          (let ((,g!cell cons-pool))
            (decf cons-pool-count)
            (setf cons-pool (cdr cons-pool))
            (setf (car ,g!cell) ,g!car
                  (cdr ,g!cell) ,g!cdr)
            ,g!cell)))

**with-cons-pool** 是我们创建 cons 池的一种方式。 请注意，此宏扩展为 let 形式，为
**cons-pool**、**cons-pool-count** 和 **cons-pool-limit** 创建绑定。 这些变量用来保存
可回收的 cons 单元格。 因为无形地引入了变量，所以 **with-cons-pool** 是一个回指宏。
还要注意，因为 Common Lisp 为词法和动态变量提供了双重语法，所以这个宏的扩展创建的回指绑定
可能是动态的或词法的，这取决于在宏使用的地方是否将回指声明为特殊的。

.. code-block:: lisp

    (defmacro! cons-pool-free (o!cell)
      `(when (<= cons-pool-count
                (- cons-pool-limit 1))
        (incf cons-pool-count)
        (setf (car ,g!cell) nil)
        (push ,g!cell cons-pool)))

**cons-pool-cons** 展开为一些从 cons 池中分配 cons 单元的代码。 它假定自己在
**with-cons-pool** 的词法范围内，或者，如果回指被声明为特殊的，那么当前存在它们
的动态绑定。 **cons-pool-cons** 仅在其池为空时调用 **counting-cons**。 它永远
不会在池中保存超过 **cons-pool-limit** 的数量 。

如果确定不再需要一个 cons 单元，可以用 **cons-pool-free** 将其释放到 cons 池中。
完成后，代码必须保证不再访问它刚刚释放的 cons 单元格。 **cons-pool-free** 展开的
代码会将释放的 cons 单元推入 **cons-pool** 并增加 **cons-pool-count** 的值， 除非
**cons-pool-count** 大于 **cons-pool-limit**。 在这种情况下单元将保留垃圾收集来收集。
请注意，当确定不再需要它们时，不需要对 cons 单元进行 **cons-pool-free**，因为垃圾
收集器仍然能够确定何时不再需要它们。 如果知道一些 lisp 不知道的额外信息，释放它们
只是种效率优化。

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
                (cons -pool -free cell)
                elem )))))

所以 cons 池的设计由两个宏组成，一个创建回指，隐式地引入词汇或特殊绑定，另一个隐式
地消耗这些回指。 通常，另一个宏用于组合这些宏。 **make-cons-pool-stack** 就是这样
一个例子。 它创建了个类似于 Common Lisp 堆栈的数据结构，当然，实际上只是个使用
**push** 和 **pop** 宏更新的列表。 但是，我们的数据结构与 **push** 和 **pop** 不同，
因为它不是透明指定的。 这些堆栈的实现细节与它们的实际使用方式是分开的。 这很重要，
因为我们不想要求我们堆栈的用户使用他们自己的方法来推送和弹出数据，而是希望他们使用
我们的内存优化版本。 **make-cons-pool-stack** 使用 :ref:`5-7-dlambda` 中的
**dlambda**。 以下的示例中，我们创建了一个包含新堆栈数据结构的词法 cons 池，
然后推送和弹出一个 item 100 次：

.. code-block:: none

    * (with-cons-pool
        (let ((stack (make-cons-pool-stack)))
          (with-conses-counted
            (loop for i from 1 to 100 do
              (funcall stack :push nil)
              (funcall stack :pop)))))
    1

请注意，**counting-cons** —— 这是唯一使用的内存分配函数 —— 仅被调用一次。 曾经需要的一个
cons 单元被回收而不是被收集。 如果这个循环发生在编译的代码中，并且循环迭代了足够多的次数，
那么可以预期 cons pool 版本执行得更快，这仅仅是因为不会调用垃圾收集器。 通常更重要的是，
当垃圾收集器运行时，我们的循环不会有意外的执行暂停。 当然，我们几乎从来没有注意到这些停顿，
因为 lisp 足够聪明，不会立即进行完整的垃圾回收，而是使用一种称为增量回收的技术来摊销操作。
垃圾收集器还实现了一种称为分代收集的优化，其中最近分配的内存比旧内存更频繁地收集。 令人惊讶
的是，这竟然是一种引用计数[UNIFIED-THEORY-OF-GC]。

但是使用 cons 池，可以减少（或根本不） cons，从而减少（或消除）垃圾收集执行时间的不确定性。
大多数 lisp 系统还有一种方法可以暂时禁用垃圾收集器，这样就可以在不暂停的情况下执行某些操作，
而是在不关心此类暂停的某个时间点暂停更长的时间。 在 CMUCL 中，可以使用 **gc-on** 和
**gc-off** 函数。 另请参阅 signal.lisp 中的代码。 练习：禁用垃圾收集器，然后在循环中消耗一
堆垃圾。使用 unix **top** 程序来监控的内存使用情况。

.. code-block:: lisp

    (with-cons-pool
      (defun make-shared-cons-pool-stack ()
        (make-cons-pool-stack)))

虽然上面的栈实现需要在同一个词法上下文中使用 **with-cons-pool** 来表示想要共享一个 cons 池
的栈，但是由于这些宏的透明设计，我们可以按自己喜欢的想法将它们与闭包结合起来，用来指定这个本地变
量。**make-shared-cons-pool-stack** 的工作方式与 **make-cons-pool-stack** 相同，只是
它不需要用 **with-cons-pool** 包裹它们。 这些变量已经被捕获。 因此所有使用
**make-shared-cons-pool-stack** 创建的栈都将共享同一个 cons 池。

.. code-block:: lisp

    (defmacro with-dynamic-cons-pools (&rest body)
      `(locally (declare (special cons-pool
                                  cons-pool-count
                                  cons-pool-limit))
      ,@body))

由于词法变量和特殊变量之间语法的双重性，我们可以选择使用动态环境来保存 cons 池。
**with-dynamic-cons-pools** 宏使任何在其词法范围内的 cons 池引用都指向回指的动态绑定。
一种方法是使用 **with-dynamic-cons-pools** 包装所有使用 cons 池的代码，然后，当实际执行程
序时，为 cons 池创建动态绑定。 因为可以使用新的动态绑定来隐藏动态绑定，所以可以保留任何动态颗
粒的局部性。要创建动态绑定的话，只需将 **with-dynamic-cons-pools** 包裹在
**with-cons-pool** 周围。

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

特别是当试图减少垃圾收集执行时间的不确定性时，可能有必要确保 cons 池在其池中具有可用的单元格，
以便程序根本不会 cons（假设我们没有耗尽池）。 要做到这一点，最初只需简单地 cons 所需的单元格
—— 当 cons可以接受时 —— 然后使用 **fill-cons-pool** 将它们添加到池中，将 cons 池填充到其
**cons-pool-limit**。

内存是个非常复杂的话题，它的效率影响取决于硬件、lisp 解释器以及不可避免的技术进步。 除非是真的
知道自己在做什么，否则尝试改进系统的内存例程可能会带来更多麻烦而不值。 只要有系统，系统程序员就
一直在调整内存。他们肯定会这样做一段时间。 内存管理很难 —— 唯一可以肯定的是宏是用来做这个的最好
工具。


.. _7-6-sorting-networks:

7.6 排序
=======================

没有比 lisp 更好的工具来试验效率或实际实现高效程序了。 Lisp 是独一无二的，因为它不仅让我们能
够专注于智能算法和设计，还让我们能够利用最先进的机器代码编译器最大限度地利用这些算法和设计。 本
节从 lisp 的角度描述了已被广泛研究但仍远未穷尽的计算机科学的一个角落：排序。 大多数人考虑对已
解决的问题进行排序，因此可能会惊讶地发现仍有许多重要的未解决问题。

我们知道许多优秀的通用排序算法。像快速排序这样的算法是最常见的，因为它们可以有效地对大量数据进行
排序。但是，相反，如果我们希望对许多小批量数据进行排序，那么像快速排序这样的通用排序算法可能会过
大。本节是关于这个问题的解决方案，许多人几十年来一直痴迷于这个问题，但它仍然是研究的沃土。对我们
来说最重要的是，这个解决方案提供了个展示高级优化技术的机会，这些技术在 lisp 中很简单，但在大多
数其他语言中却是如此重要，以至于它们几乎不值得。在本节和下一节中，我们将重新实现 Graham 在
On Lisp 中描述的宏 **sortf**。Graham 的 **sortf** 旨在说明如何使用广义变量编写宏，而我们
的 **sortf** 旨在提高速度。在某些情况下，我们的 **sortf** 将比系统经过一定调整的排序功能实
现数量级的改进。

本节献给我的老师和朋友 Alan Paeth，他教会了我，在许多事情中，甚至连排序也很有趣。 我也非常感
谢 John Gamble 和他出色的 Perl 程序 Algorithm-Networksort[ALGORITHM-NETWORKSORT]。
该程序用于试验不同的算法并生成本节中出现的 ASCII 图表网络。

排序网络是一种算法，用于不经意地对特定固定大小的数据集进行排序。 也就是说，与大多数算法（如快速
排序）不同，排序网络的操作不依赖于它用于排序的特定数据集。 每一步都是在设计网络时决定的。 排序
网络是数据与索引对（pair）的简单列表。 这些对（pair）的每一个都对应于应该在比较交换操作中使用
的索引。 在按顺序执行所有这些比较交换操作后，元素将按排序顺序排列。

像快速排序这样非常适合大型数据集的算法对于某些类别的排序问题可能会产生无法接受的开销。 首先，快
速排序实现通常允许选择自定义比较运算符，以使排序代码更通用。 这意味着每次比较都需要对比较函数进
行函数调用，而不是作为内联机器代码实现。 其次，由于快速排序实现如此普遍，当我们知道我们的数据集
具有特别小的固定大小时，它们通常无法利用可以进行的优化。 第三，我们通常不想对数据集进行完全排
序，而是只对数据集进行排序，以确定某个元素（也许是中间元素）。 不查找完整排序的排序网络有时称为
选择网络。

为了阐明排序网络的概念，并说明该主题可能有多么微妙和违反直觉，我们考虑一些最简单的网络：对三个元
素进行排序的网络。 大多数程序员都知道，通过三个比较可以轻松地对三个元素进行排序，并且当恰好有三
个元素时，通常不会使用快速排序。 很容易说服自己，这些比较交换操作可以按任何顺序执行，结果都是一
样的。 但是，有些排序本质上比其他排序效率低，这并不是很明显。

.. code-block:: none

    (defvar bad-3-sn
      '((0 1) (0 2) (1 2)))

.. code-block:: none

    o--^--^-----o
      |  |
    o--v--|--^--o
          |  |
    o-----v--v--o

网络 **bad-3-sn** 可能是最明显的三元网络实现，但正如其名称所暗示的那样，它并不是最佳的。
ASCII 图表图片有助于可视化 **bad-3-sn** 中基于列表的网络描述所描述的算法。 该算法表示要比较
数据集索引 0 和 1 处的元素，如果它们无序，则将它们交换为正确的顺序。 对索引对 (0 2) 执行相同
的操作，最后对 (1 2) 执行相同的操作。在这个过程之后，元素将被排序。 如果我们将这个排序网络实现
为代码来对长度为 3 的数组进行排序，称之为 **a**，那么看起来可能是像这样：

.. code-block:: none

    (progn
      (if (> (aref a 0) (aref a 1))
        (rotatef (aref a 0) (aref a 1)))
      (if (> (aref a 0) (aref a 2))
        (rotatef (aref a 0) (aref a 2)))
      (if (> (aref a 1) (aref a 2))
        (rotatef (aref a 1) (aref a 2))))

**bad-3-sn** 结果是正确的，但与 **good-3-sn** 相比效率低下。通过交换前两个比较交换操作的顺
序，我们实现了更高效的网络。平均而言，该网络执行的交换操作比 **bad-3-sn** 少。描述这一点的最
好方法是使用条件概率，但因为这是一本关于 lisp 的书，而不是排序网络，所以我们会回避这一点。相
反，我们通过枚举所有排列然后测量当我们用两个网络解释它们时发生的交换次数来证明 **good-3-sn**
优于 **bad-3-sn**。现在这里有一个直观的解释：如果首先执行网络中的长链接，那么在第一次操作之
后，最小或最大元素中的至少一个将处于其正确的最终位置。因此，至少有一个后续的比较交换操作不会执行
交换。但是，如果先执行短链接，则这些元素可能都不在其最终位置，并且都需要将来交换。

.. code-block:: none

    (defvar good-3-sn
      '((0 2) (0 1) (1 2)))

.. code-block:: none

    o--^--^-----o
      |  |
    o--|--v--^--o
      |     |
    o--v-----v--o

.. code-block:: none

    (defvar tracing-interpret-sn nil)

    (defun interpret-sn (data sn)
      (let ((step 0) (swaps 0))
        (dolist (i sn)
          (if tracing -interpret -sn
            (format t "Step ~a: ~a~%" step data))
          (if (> #1=(nth (car i) data)
                #2=(nth (cadr i) data))
            (progn
              (rotatef #1# #2#)
              (incf swaps)))
          (incf step))
        (values swaps data)))

为了探索这种现象，我们实现了一个用于排序网络的解释器，**interpret-sn**。 此解释器将排序网络
**sn** 应用于由列表表示的数据集。 它将返回执行的交换次数作为第一个值，并将生成的排序数据集作为
第二个值。 注意这里用 **#=** 和 **##** 自引用读取宏来避免重新键入访问器表单。 如果我们想查看
分步排序过程，还要注意我们可以绑定到非空值的跟踪变量的使用。 首先，假设一个已经排序的数据集，
显然 **bad-3-sn** 和 **good-3-sn** 都不执行交换：

.. code-block:: none

    * (let ((tracing-interpret-sn t))
        (interpret-sn '(1 2 3) bad-3-sn))
    Step 0: (1 2 3)
    Step 1: (1 2 3)
    Step 2: (1 2 3)
    0
    (1 2 3)
    * (let ((tracing-interpret-sn t))
        (interpret-sn '(1 2 3) good-3-sn))
    Step 0: (1 2 3)
    Step 1: (1 2 3)
    Step 2: (1 2 3)
    0
    (1 2 3)

接下来，考虑每个元素都乱序的情况。 同样，两个排序网络执行相同的操作，执行必要的两次交换：

.. code-block:: none

    * (let ((tracing-interpret-sn t))
        (interpret-sn '(3 1 2) bad-3-sn))
    Step 0: (3 1 2)
    Step 1: (1 3 2)
    Step 2: (1 3 2)
    2
    (1 2 3)

    * (let ((tracing-interpret-sn t))
        (interpret-sn '(3 1 2) good-3-sn))
    Step 0: (3 1 2)
    Step 1: (2 1 3)
    Step 2: (1 2 3)
    2
    (1 2 3)

但是，在这种情况下，**bad-3-sn** 会导致最坏情况——交换三次：

.. code-block:: none

    * (let ((tracing-interpret-sn t))
        (interpret-sn '(3 2 1) bad-3-sn))
    Step 0: (3 2 1)
    Step 1: (2 3 1)
    Step 2: (1 3 2)
    3
    (1 2 3)

    * (let ((tracing-interpret-sn t))
        (interpret-sn '(3 2 1) good-3-sn))
    Step 0: (3 2 1)
    Step 1: (1 2 3)
    Step 2: (1 2 3)
    1
    (1 2 3)

在上面，**bad-3-sn** 执行了 3 次交换，而最优的 **good-3-sn** 只执行了一次。 不应该存在
**good-3-sn** 表现不佳的对称情况吗？ 事实证明，不，**good-3-sn** 真的更好。 如果你仍然不相
信这一点，自行查阅 **蒙蒂霍尔问题**，以了解这类问题可能有多么违反直觉。 因此，似乎合理的排序总
是尽快将元素交换到正确的位置，以便发生最少的交换。

为了量化 **good-3-sn** 比 **bad-3-sn** 具体要好多少，我们写了一个实用程序
**all-sn-perms**，它生成从 1 到 n 的数字的所有排列。 **all-sn-perms** 使用了很多 lisp
的特性，包括递归地构建连接的临时列表网络，以及使用 Graham 的照应 alambda 宏。 在这里，我们生
成数字 1 到 3 的所有 6 个（3 的阶乘）排列：

.. code-block:: none

    * (all-sn-perms 3)

    ((1 2 3) (2 1 3) (1 3 2)
    (3 1 2) (2 3 1) (3 2 1))

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

注意，由于 **all-sn-perms** 的编写方式，上述列表彼此共享结构，因此在使用它们来解释排序网络
（一种破坏性操作）时，我们应该始终确保对它们的副本进行排序，如 **average-swaps-calc**。 对
于可以以这种方式构造的结果的问题，像这样的共享结构通常是一种很好的编程技术，因为它可以减少数据结
构所需的总内存。

.. code-block:: lisp

    (defun average-swaps-calc (n sn)
      (/ (loop for i in (all-sn-perms n) sum
            (interpret-sn (copy-list i) sn))
        (fact n)))

使用 **interpret-sn** 排序网络解释器，我们可以用它为每个可能的排列记录的实际交换数和
**average-swaps-calc**。这个函数简单地遍历每个排列，将解释器应用于给定的排序网络，对发生的
交换求和，然后返回这个和除以可能的排列的数量。假设每一种排列都是等可能的，那么这个计算就代表了每
一种排序发生的平均交换次数。 下面，可以看到 **bad-3-sn** 平均每次排序发生了 1.5 次交换：

.. code-block:: none

    * (average-swaps-calc 3 bad-3-sn)
    3/2

平均而言，**good-3-sn** 只有 1.166 次交换：

.. code-block:: none

    * (average-swaps-calc 3 good-3-sn)
    7/6

目前为止，我们的排序网络只能对大小为 3 的数据集进行排序。 是否有生成任意大小的排序网络的算法？
有的，这些算法已经公布有一段时间了。 1968 年，Ken Batcher 将他的巧妙算法
[SN-APPLICATIONS] 描述为由 Donald Knuth 命名的合并交换排序或来自 [TAOCP-VOL3-P111] 的
算法 5.2.2M。 Batcher 的算法是 shell 排序和归并排序的一种组合，除了给定一个已知的输入大小，
它将进行的比较交换操作完全独立于数据本身确定——这正是我们对网络排序所需要的。因此，为了创建一个
排序网络，我们运行 Batcher 的算法并记录进行了哪些比较交换操作。 稍后我们可以将这些操作内联
到这个特定输入大小的函数中。 这个过程与循环展开并不完全不同，除非 lisp 允许我们更进一步。

.. code-block:: lisp

    (defun build -batcher -sn (n)
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

**build-batcher-sn** 是 Batcher 算法的 lisp 实现，直接转录自 Knuth 的描述。 由于 lisp
对按位整数运算的任意精度支持，此实现不会受到任何人为的大小限制，例如 32 或 64。可以使用
**build-batcher-sn** 轻松构建任意大小的高效排序网络 . 这是一个大小为 3的网络的构造——与上面
的 **good-3-sn** 相同：

.. code-block:: none

    * (build-batcher-sn 3)
    ((0 2) (0 1) (1 2))

下面是大小为 7 的网络结构：

.. code-block:: none

    * (build-batcher-sn 7)
    ((0 4) (1 5) (2 6) (0 2) (1 3) (4 6) (2 4)
    (3 5) (0 1) (2 3) (4 5) (1 4) (3 6) (1 2)
    (3 4) (5 6))

.. code-block:: none

    o--^--------^-----^-----------------o
      |        |     |
    o--|--^-----|--^--v--------^--^-----o
      |  |     |  |           |  |
    o--|--|--^--v--|--^-----^--|--v-----o
      |  |  |     |  |     |  |
    o--|--|--|-----v--|--^--v--|--^--^--o
      |  |  |        |  |     |  |  |
    o--v--|--|--^-----v--|--^--v--|--v--o
          |  |  |        |  |     |
    o-----v--|--|--------v--v-----|--^--o
            |  |                 |  |
    o--------v--v-----------------v--v--o

Batcher 的网络很好，但众所周知，对于大多数网络规模来说，它的效果并不理想。 虽然已经发现了许多
特定规模的更好的网络，但如何找到这些更好的网络，以及它们是否是最优的，这是个重要的未解决问题。
这一研究领域已经通过使用新的人工智能技术有效搜索排序网络问题的超指数空间的进化算法取得了重要进
展。 例如，目前已知的大小为 13 的最佳网络是由 Evolving Non-Determinism 算法 [END] 发现
的。

此处显示的排序网络的 ASCII 图表表示是由 John Gamble 出色的 Algorithm-Networksort Perl
程序创建的。注意，图表将一些可以并行执行的链接放在同一垂直列中。 这表明排序网络是至少在专用硬件
中可以从比较交换操作中的并行性中受益的算法。 发现如何创建良好的并行排序网络，以及我们可以使它们
如何并行，仍然很重要，也是未解决的问题。

.. code-block:: lisp

    (defun prune-sn-for-median (elems network)
      (let ((mid (floor elems 2)))
        (nreverse
          (if (evenp elems)
            (prune-sn-for-median-aux
              (reverse network)
              (list (1- mid) mid))
            (prune-sn-for-median-aux
              (reverse network) (list mid))))))

    (defun prune-sn-for-median-aux (network contam)
      (if network
        (if (intersection (car network) contam)
          (cons (car network)
                (prune-sn-for-median-aux
                  (cdr network)
                  (remove -duplicates
                    (append (car network) contam))))
          (prune-sn-for-median-aux
            (cdr network) contam))))

上面我们提到了通用排序函数的一个缺点是它们被硬编码为执行整个排序操作。 如果我们愿意，我们可以对
数据集进行排序，使其足以确定一个元素位于其最终位置。 通常，我们感兴趣的元素是中间元素或中值元
素。 函数 **prune-sn-for-median** 和 **prune-sn-for-median-aux** 采用了一种适度的、明
显的算法，我发现它可以消除许多不必要的比较交换操作，从而构建任意选择网络。

.. code-block:: none

    o--^--------^-----^-----------------o
      |        |     |
    o--|--^-----|--^--v--------^--------o
      |  |     |  |           |
    o--|--|--^--v--|--^-----^--|--------o
      |  |  |     |  |     |  |
    o--|--|--|-----v--|--^--v--|--^--^--o
      |  |  |        |  |     |  |  |
    o--v--|--|--^-----v--|--^--v--|--v--o
          |  |  |        |  |     |
    o-----v--|--|--------v--v-----|-----o
            |  |                 |
    o--------v--v-----------------v-----o

该算法从 Batcher 网络开始，然后向后工作，跟踪受污染的元素 - 不能删除任何现有链接的元素，因为
这样做会改变该元素的网络结果。 可以删除连接未受污染元素的任何链接，而不会改变受污染元素的结
果。 将受污染的元件连接到未受污染的链接的每个链接都会污染未受污染的元件。 当我们只污染中间元素
（或在输入大小相同的情况下污染两个中间元素）时，我们创建了一个中值选择网络。

显示了大小为 7 的算法输出，这是一个修改后的 Batcher 网络，其中两个链接被删除。 运行此网络后，
中值元素将位于正确位置，但不保证其他元素排序。 举个例子，这里我们只对列表进行排序，发现 4 是中
间元素：

.. code-block:: none

    * (interpret-sn
        '(4 2 3 7 6 1 5)
        (prune-sn-for-median
          7 (build-batcher-sn 7)))
    6
    (1 3 2 4 5 7 6)

.. code-block:: none

    (defun prune-sn-for-median-calc (n)
      (loop for i from 2 to n collect
        (let* ((sn (build -batcher -sn i))
              (snp (prune-sn-for-median i sn)))
          (list i
            (length sn)
            (length snp)))))

对于大小为 7 的网络，我们修改后的中值 Batcher 网络执行 12 次比较交换操作，而常规 Batcher 网
络执行 14 次操作。 **prune-sn-for-median-calc** 为我们提供了针对不同大小排序网络的此类网
络的数据。 它计算大小最大为 n 的 Batcher 网络，并将它们的大小与我们的算法创建的相关中值网络的
大小分组。

计算的网络大小最多为 49。 请注意，在最小尺寸下，保存的操作很少（如果有的话）。 但是对于稍微大一
点的数字，我们开始节省大约 20% 的比较交换操作。 当我们只关心中位数时，这些网络是不错的选择。
然而，最优中值排序网络的构建也是一个开放的研究领域。 本章开发的修改后的 Batcher 网络很不错，但
仍远未达到最佳状态。 Paeth[GRAPHICS-GEMS-P171-175]发现了目前已知的 9 和 25 尺寸（3x3
和 5x5 内核镜像尺寸）的最佳中值选择网络，并在此处介绍并包含在本书的代码中。以下是 Paeth 的中
值网络的长度：

.. code-block:: none

    * (length paeth-9-median-sn)
    20
    * (length paeth-25-median-sn)
    99

.. code-block:: none

    * (prune-sn-for-median-calc 49)
    ((2 1 1) (3 3 3) (4 5 5) (5 9 8) (6 12 12)
    (7 16 14) (8 19 17) (9 26 22) (10 31 29)
    (11 37 31) (12 41 35) (13 48 40) (14 53 47)
    (15 59 49) (16 63 53) (17 74 61) (18 82 72)
    (19 91 75) (20 97 81) (21 107 88) (22 114 98)
    (23 122 100) (24 127 105) (25 138 113) (26 146 124)
    (27 155 127) (28 161 133) (29 171 140) (30 178 150)
    (31 186 152) (32 191 157) (33 207 169) (34 219 185)
    (35 232 190) (36 241 199) (37 255 209) (38 265 223)
    (39 276 226) (40 283 233) (41 298 244) (42 309 259)
    (43 321 263) (44 329 271) (45 342 280) (46 351 293)
    (47 361 295) (48 367 301) (49 383 313))

.. code-block:: lisp

    (defvar paeth-9-median-sn
      '((0 3) (1 4) (2 5) (0 1) (0 2) (4 5) (3 5) (1 2)
        (3 4) (1 3) (1 6) (4 6) (2 6) (2 3) (4 7) (2 4)
        (3 7) (4 8) (3 8) (3 4)))

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

对于大小为 9 的网络，Batcher 的完整排序网络执行 26 次操作。 目前最著名的是弗洛伊德发现的，是
执行了 25 次操作。我们修剪后的 Batcher 网络的中值版本为 22，Paeth 的中值网络为 20。对于大小
为 25 的网络，Batcher：138，修剪：113，Paeth：99。所以我们的中值网络似乎与 Paeth 的网络相
差 10%，这是目前最知名的 这些大小的中值网络。 正如预期的那样，我们不能修剪 Paeth 网络的任何额
外操作：

.. code-block:: none

    * (length (prune-sn-for-median
                9 paeth-9-median-sn))
    20
    * (length (prune-sn-for-median
                25 paeth-25-median-sn))
    99

从理论上讲，这一切都非常有趣。 但在实践中，理论是相当无聊的。 我们开发了所有这些基于列表的排序
网络，其中一些使用 Batcher 算法执行完整排序，还有一些使用 Batcher 算法的污染优化来查找中
值。 然后，我们为这些网络开发了一个玩具解释器，与真正的分类程序相比，它无疑会表现得非常糟糕。
这与效率有什么关系？ 我们的实验只是产生了理论结果而不是有用的代码吗？ 在大多数语言中，这些实验
的结果——我们的排序网络——会以某种高级数据结构表示，并没有多大用处。 但是在 lisp 中，这些网络已
经是非常高效的排序程序。 我们只是还没有为它们编写编译器。

练习：调整剪枝算法（及其污染方法），使其产生四分位选择网络。 这些网络不仅确定了中位数，而且还确
定了有序元素的高半部分和低半部分的中值元素。


.. _7-7-writing-and-benchmarking-compilers:

7.7 编写基准测试
===========================

编译器对大多数程序员来说是一个可怕的概念，因为大多数语言都不适合编写编译器。下面是个类比：解析一
个复杂的日志文件对于只知道 C 或汇编的程序员来说可能是一个令人生畏的、容易出错的愿景，但由于
Perl 和正则表达式，这对我们多语言程序员来说不是问题。同样，如果我们不了解 lisp，那么设计一种功
能强大、富有表现力的编程语言，然后创建一个编译器将这种语言的程序转换为高效的机器代码将是项令人生
畏的任务。在编写编译器方面，Lisp 的优势不仅使它比其他语言好一点——实际上让表达式上了一个新台
阶。一般来说，这个优势是能与不能的区别。 Lisp 程序员在任何地方都使用编译器，且有时以非 lisp
程序员难以置信的方式和任务使用。有多少 C 程序员考虑过 :ref:`7-2-macros-make-lisp-fast` 中
描述（并克服）的 printf 函数的解释开销？有多少人会尝试为 printf 编写编译器？而这在 lisp 的
课程中却是标准。一切都应该编译成 lisp。

什么是编译器？如果你来自 Blub，答案可能隐藏在一大堆解释解析、语法定向翻译、上下文无关语法等的书
籍中。但别担心，这是 lisp，编译器很容易。它是如此简单，以至于如果你曾认真的做过 lisp 编程相关
的编程，那么你已经编写了它们，甚至可能没有意识到。编译器的另一个名称是“宏”。宏将程序从一种语言
编译成另一种语言。 Lisp 实际上就是编写这些编译器——其他一切都是次要的。在 lisp 中，编译器设计
唯一重要的方面是如何保持目标程序的正确性，同时为它找到有效的扩展。换句话说，这才是编译问题的本
质。到目前为止，我们已经看到了如何使用宏来创建完全适合手头任务的自定义语言，以及如何通过使用声明
来消除对偶性和安全检查来提高 lisp 代码的效率。有效的编译器编写只是将这两种技能结合起来。

.. code-block:: none

    (defun sn-to-lambda-form% (sn)
      `(lambda (arr)
        #f
        (declare (type (simple -array fixnum) arr))
        ,@(mapcar
            #`(if (> #1=(aref arr ,(car a1))
                    #2=(aref arr ,(cadr a1)))
                (rotatef #1# #2#))
            sn)
        arr))

当在 :ref:`7-2-macros-make-lisp-fast` 中创建编译器宏来处理格式时，格式化程序编译器扩展成什么？
这是一个 lambda 结构。 编译为 lambda 结构有时是有意义的，因为我们可以使用 compile 函数直接将
它们转换为机器代码。回到上一章的排序网络，**sn-to-lambda-form%** 是一个返回 lambda 结构的函数。
这种 lambda 结构将对基于列表的排序网络中的每个比较交换操作都有一个指令。 每条指令都会（不安全
地）索引到一个 **fixnum** 数组，比较并可能使用 **rotatef** 来交换元素。 **fixnum** 数组
将作为参数 (**arr**) 传递给由此 lambda 结构创建的函数。 这就是一个体面的机器代码编译器的
全部内容。与所有 lambda结构一样，由于 lambda 宏，我们能够计算它们以获取函数：

.. code-block:: none

    * (eval
        (sn-to-lambda-form%
          (build-batcher-sn 3)))
    #<Interpreted Function>

只需在它们上调用 **compile** 即可成为编译函数：

.. code-block:: none

    * (compile nil *)
    #<Function>

让我们看一下反汇编输出（编译后的展开）：

.. code-block:: none

    * (disassemble *)
    ...
    ;;; (> (AREF ARR 0) (AREF ARR 2))
    9E:       MOV
    A1:       MOV
    A4:       CMP
    A6:       JLE
    EAX, [EDX+1]
    ECX, [EDX+9]
    EAX, ECX
    L0
    ;;; (ROTATEF (AREF ARR 0) (AREF ARR 2))
    A8:       MOV
    AB:       MOV
    AE:       MOV
    B1:       MOV
    EAX, [EDX+9]
    ECX, [EDX+1]
    [EDX+1], EAX
    [EDX+9], ECX
    ;;; (> (AREF ARR 0) (AREF ARR 1))
    B4: L0:   MOV
    B7:       MOV
    BA:       CMP
    BC:       JLE
    EAX, [EDX+1]
    ECX, [EDX+5]
    EAX, ECX
    L1
    ;;; (ROTATEF (AREF ARR 0) (AREF ARR 1))
          BE:       MOV     EAX, [EDX+5]
    C1:       MOV
    C4:       MOV
    C7:       MOV
    ECX, [EDX+1]
    [EDX+1], EAX
    [EDX+5], ECX
    ;;; (> (AREF ARR 1) (AREF ARR 2))
    CA: L1:   MOV
    CD:       MOV
    D0:       CMP
    D2:       JLE
    EAX, [EDX+5]
    ECX, [EDX+9]
    EAX, ECX
    L2
    ;;; (ROTATEF (AREF ARR 1) (AREF ARR 2))
    D4:       MOV           EAX, [EDX+9]
    D7:       MOV           ECX, [EDX+5]
    DA:       MOV           [EDX+5], EAX
    DD:       MOV           [EDX+9], ECX
    E0: L2:   ...

上面的机器代码很快，但还可以更快。 Lisp 编译器很聪明——一些最聪明的编译器 —— 但它们总是可以更
聪明。 在关心性能的极少数情况下，检查编译的展开是至关重要的，因为很难知道你的 lisp 编译器有多
聪明。 在上面的汇编中，如果仔细看，就会发现它每次执行交换时实际上都在执行不必要的读取操作。 问
题是 **rotatef** 展开为冗余访问。 一个足够聪明的编译器可能会发现在寄存器中已经有这个值，并且
可以避免数组访问。 但是我的没有，所以我重新构建了代码，从而实现了更有效的扩展。

**sn-to-lambda-form** 是 **sn-to-lambda-form%** 的改进版本。 它为读入的变量创建临时绑
定，因此不会为交换操作重新执行数组读取指令。 下面是高级编译展开：

.. code-block:: none

    * (disassemble
        (compile nil
          (sn-to-lambda-form%
            (build-batcher-sn 3))))
    ...
    ;;; (LET ((A (AREF ARR 0)) (B (AREF ARR 2))) ...)

    2E:       MOV
    31:       MOV
    34:       CMP
    36:       JLE

    EAX, [EDX+1]
    ECX, [EDX+9]
    EAX, ECX
    L0

    ;;; (SETF (AREF ARR 0) B (AREF ARR 2) A)
          38:       MOV     [EDX+1], ECX
          3B:       MOV     [EDX+9], EAX
    ;;; (LET ((A (AREF ARR 0)) (B (AREF ARR 1))) ...)
    3E: L0:   MOV
    41:       MOV
    44:       CMP
    46:       JLE

    EAX, [EDX+1]
    ECX, [EDX+5]
    EAX, ECX
    L1

    ;;; (SETF (AREF ARR 0) B (AREF ARR 1) A)
          48:       MOV     [EDX+1], ECX
          4B:       MOV     [EDX+5], EAX
    ;;; (LET ((A (AREF ARR 1)) (B (AREF ARR 2))) ...)
    4E: L1:   MOV
    51:       MOV
    54:       CMP
    56:       JLE

    EAX, [EDX+5]
    ECX, [EDX+9]
    EAX, ECX
    L2

    ;;; (SETF (AREF ARR 1) B (AREF ARR 2) A)
          58:       MOV     [EDX+5], ECX
          5B:       MOV     [EDX+9], EAX
          5E: L2: ...

熟悉 lisp 编译器以便了解宏展开的效率对编写高效的 lisp 非常重要。 反汇编 lisp 系统的源代码，
使用像 **time** 宏这样的基准测试工具，以及大量的耐心，不幸的是，这是真正获得如何编写快速 lisp
代码的直觉的唯一方法。

如果你来自 Blub 语言，那么像 **sn-to-lambda-form** 宏那样展开为 lambda 结构可能是实现编
译器的最明显方式。源代码以 lambda 结构编译到反汇编循环感觉很像Blub的编辑、编译、反汇编循环。
把源代码放进去，然后把机器代码拿出来。 但是，这种方法可能更笨拙。 在 lisp 中，通常将编译器创建
为不可见的 —— 直接合并到其他 lisp 程序中。理想情况下，除非我们想让东西快速运行，否则永远不会调
用 **compile** 函数。 宏不应该一直编译到机器代码，而只是足以创建一个良好的展开，以便编译器在
运行时，将有足够的信息来使整个程序高效。

我们特别不想在运行时调用 **compile**。 编译是一项昂贵的操作，因为需要展开许多级别的宏来编译某
些东西。不要在运行时调用 **compile**，记住 lisp 已经在编译函数中编译了所有的 lambda 结
构。 由于这种在运行时构造已编译代码的闭包的能力，很容易确保在编译时完成尽可能多的计算，同时仍然
在运行时创建任意函数（闭包）。

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

**sortf** 是本书中我最喜欢的宏。 它不仅简洁、优雅，并且很好地展示了迄今为止描述的许多宏技术，
而且它还是一段有用的生产代码，能够执行极快的排序操作。 最棒的是，这个宏与 lisp 程序完美融合，
使用起来毫不费力。 我们不必费尽心思就能从这种先进的 lisp 优化中受益。 任何时候需要对小的、固定
大小的数据集进行排序，这个宏很容易合并，有时甚至比排序函数更容易。 **sortf** 不是展开为
lambda 结构，而是扩展为 tagbody 结构，因为 tagbody 是返回 nil 的标准 **progn**。 以下
是 **sortf** 的展开：

.. code-block:: none

    * (macroexpand
        '(sortf < a b c))

    (LET ()
      (TAGBODY
        (LET ((#:A1824 A) (#:B1823 C))
          (IF (< #:B1823 #:A1824)
            (SETF A #:B1823 C #:A1824)))
        (LET ((#:A1824 A) (#:B1823 B))
          (IF (< #:B1823 #:A1824)
            (SETF A #:B1823 B #:A1824)))
        (LET ((#:A1824 B) (#:B1823 C))
          (IF (< #:B1823 #:A1824)
          (SETF B #:B1823 C #:A1824)))))
      T

**sortf** 的接口设计来自于 On Lisp，但它是如此自然，以至于几乎每个 lisp 程序员都会这样实
现。第一个参数通常是表示比较运算符的符号 —— 通常类似于 **<**。 这通常表示一个函数，但正如
Graham 指出的那样，它也可以表示一个宏或特殊结构，因为它直接拼接到列表的函数位置。 我们甚至可以
传递一个 lambda 结构，因为它们也允许在列表的函数位置：

.. code-block:: none

    * (let ((a -3) (b 2))
        (sortf (lambda (a b) (< (abs a) (abs b)))
          a b)
        (list a b))
    (2 -3)

和 Graham 的宏一样，要排序的参数是广义变量。 这意味着可以用 **sortf** 对任何类型的变量进行排
序，不仅是那些由符号表示的变量，还包括任何可以 **setf** 的变量。 以下是个示例：

.. code-block:: none

    * (let ((a 2) (b '(4)) (c #(3 1)))
        (sortf < a (car b) (aref c 0) (aref c 1))
        (format t "a=~a b=~a c=~a~%" a b c))

    a=1 b=(2) c=#(3 4)
    NIL

虽然 Graham 的 **sortf** 和我们的 **sortf** 编译相同的源语言，但它们的展开却大有不同。
Graham 的宏可以说比我们的更正确，因为它只会执行一次访问这些位置的代码。使用 Graham 的
**sortf**，我们可以传入具有副作用的变量，并且只对它们进行一次计算，正如预期的那样。例如，
Graham 的 **sortf** 在给定位置时只会增加 **i** 一次 **(aref arr (incf i))**。Graham
的 **sortf** 的工作原理是将每个要排序的变量复制到临时绑定中，使用冒泡排序对这些临时绑定进行排
序，然后使用 **setf** 表达式将临时变量写回原来的位置，现在按排序顺序。相反，我们的
**sortf** 将在整个排序过程中多词计算每个位置格式，因此建议不要使用有副作用的位置。这种设计的另
一个结果是，如果追求效率，请确保访问器是高效的。特别是不要使用像 **caddr** 这样的长列表访问
器，因为它们最终会多次遍历列表。通过我们的实现，我们就地对参数进行排序，即没有任何临时绑定。代替
具有 (O (expt N 2)) 的 Big-O 复杂度的冒泡排序，我们使用 Batcher 更好的合并交换排序及其(O
(\* N (expt (log N) 2)))。有一些方法可以构建(O (\* N (log N))) 的排序网络，与快速排序相
同 - 但大多数对小型网络使用的操作比 Batcher 的要多。

你可能希望在调用 **sortf** 的地方添加个 **sharp-f** 快速声明，因为它自身不会添加它。 如果想
要真正快速的排序，请确保编译器知道要排序的所有广义变量的类型。 如果确实指定了类型，请始终确保将
所有通用变量声明为相同类型。这很重要，因为任何元素最终可能出现在任何地方。

但是我们怎么知道这个宏是否真的给我们任何优于排序函数的性能优势呢？ 我们需要对其进行基准测试。
基准测试已经讨论过很多了，因为，特别是对程序员来说，胡说八道的永恒爱好是如此令人愉快。 不幸的
是，几乎所有的基准测试结果都是无用的。甚至建议对本书中的基准测试结果持保留态度。 也就是说，在同
一台机器上运行在同一个 lisp 映像上运行的代码版本略有不同的精心设计、受控的实验对于理解和修复性
能瓶颈有时是无价的。 这种计量很有用，因为我们不仅可以判断哪些技术更有效，而且我们还可以判断它们
的效率有多高。 因为他们为我们编写代码，所以宏是设置这些实验的最佳工具。

.. code-block:: lisp

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

**sort-benchmark-time** 宏是我们实验中的一个组件。 它展开为假定 lambda 结构或函数绑定到
**sorter** 的代码，并且该函数将对大小为 **n** 的 **fixnum** 数组进行排序。 然后将它编译成
一个函数并使用它对随机生成的数组进行排序迭代。 **time** 宏用于收集有关排序过程所需时间。

**do-sort-benchmark** 是执行基准测试的实际接口。 给定数据集大小 **n** 和迭代数
**iters**，它将同时测试 Common Lisp 排序函数和我们的 **sortf** 宏。 其保留随机数生成器的
状态，并在执行排序测量之后但在运行 **sortf** 之前将其重置，以便要排序的随机数组相同。 运行时
编译 **do-sort-benchmark** 非常重要，这样我们的测试中可能会出现最少的噪声（noise）。

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

        (setf *random -state* rs)
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

运行时，**do-sort-benchmark** 不仅告诉我们 **sortf** 是高效的，而且就小型固定大小数据集的
性能而言，通用排序算法甚至与排序网络不在同一个级别。 我们还注意到 **sortf** 没有 cons，这反
过来会减少垃圾收集运行时间，从而提高性能。 以下是大小为 2、3、6、9、25、49 的数据集的结果：

.. code-block:: none

    * (do-sort-benchmark 2 1000000)
    CL sort:
    ; Evaluation took:
    ;   1.65 seconds of real time
    ;   8,000,064 bytes consed.
    sortf:
    ; Evaluation took:
    ;   0.36 seconds of real time
    ;   0 bytes consed.
    * (do-sort-benchmark 3 1000000)
    CL sort:
    ; Evaluation took:
    ;   3.65 seconds of real time
    ;   24,000,128 bytes consed.
    sortf:
    ; Evaluation took:
    ;   0.46 seconds of real time
    ;   0 bytes consed.
    * (do-sort-benchmark 6 1000000)
    CL sort:
    ; Evaluation took:
    ;   10.37 seconds of real time
    ;   124,186,832 bytes consed.
    sortf:
    ; Evaluation took:
    ;   0.8 seconds of real time
    ;   0 bytes consed.
    * (do-sort-benchmark 9 1000000)
    CL sort:
    ; Evaluation took:
    ;   19.45 seconds of real time
    ;   265,748,544 bytes consed.
    sortf:
    ; Evaluation took:
    ;   1.17 seconds of real time
    ;   0 bytes consed.
    * (do-sort-benchmark 25 1000000)
    CL sort:
    ; Evaluation took:
    ;   79.53 seconds of real time
    ;   1,279,755,832 bytes consed.
    sortf:
    ; Evaluation took:
    ;   3.41 seconds of real time
    ;   0 bytes consed.
    * (do-sort-benchmark 49 1000000)
    CL sort:
    ; Evaluation took:
    ;   183.16 seconds of real time
    ;   3,245,024,984 bytes consed.
    sortf:
    ; Evaluation took:
    ;   8.11 seconds of real time
    ;   0 bytes consed.

因此，对于某些任务，排序网络可以对我们的系统排序例程进行数量级或更好的改进。 这些测量并不是为了
让我们的排序实现看起来很糟糕（它实际上是一个出色的排序例程），而是为了展示一个现实的例子，说明使
用宏进行智能编程可以带来显著的效率提升。 Lisp 宏让我们可以轻松、便携地进行智能编程。 Blub 语
言在智能编程方面付出了巨大的努力，以至于 Blub程序员几乎总是满足于愚蠢地编程。 在 lisp 中，所
有内容都编译为 lisp，因此可以优化的内容永远不会有任何障碍。如果有什么东西慢得让人无法接受，那就
改变它并让它变得更快。 我们几乎从不需要东西快速运行，但是当我们这样做时，lisp 就是解决方案。

.. code-block:: none

    (defun medianf-get-best-sn (n)
      (case n
        ((0) (error "Need more places for medianf"))
        ((9)  paeth -9-median -sn)
        ((25) paeth -25-median -sn)
        (t    (prune-sn-for-median n
                (build -batcher -sn n)))))

    (defmacro! medianf (&rest places)
      `(progn
          ,@(mapcar
              #`(let ((,g!a #1=,(nth (car a1) places))
                      (,g!b #2=,(nth (cadr a1) places)))
                  (if (< ,g!b ,g!a)
                    (setf #1# ,g!b #2# ,g!a)))
              (medianf-get-best-sn (length places)))
          ,(nth (floor (1- (length places)) 2) ; lower
                places)))

另一个与 **sortf** 相似的宏是 **medianf**，它使用修剪的中值选择网络或 Paeth 手工制作的中值
网络对位置进行排序，以确保中值元素处于正确位置。 在网络大小均匀的情况下，下中位数和上中位数都将
在正确的位置。 与总是返回 **nil** 的 **sortf** 不同，**medianf** 将返回下中位数的值（与奇
数网络大小的上中位数相同）。

正如之前所说，**sortf** 和 **medianf** 对可以设置的任何类型的位置进行排序。 对于存储在寄存
器中的变量，这使 lisp 有机会生成甚至不访问内存的排序代码。 例如，这里是在三个固定位置上为
**medianf** 编译的展开：

.. code-block:: none

    * (dis ((fixnum a) (fixnum b) (fixnum c))
        #f
        (medianf a b c))
    ...
    ;;; (MEDIANF A B C)
          34:       MOV		EBX, EAX
          36:       CMP		EDX, EAX
          38:       JL 		L4
          3A: L0:   MOV		EBX, EAX
          3C:       CMP		ECX, EAX
          3E:       JL 		L3
          40: L1:   MOV		EAX, ECX
          42:       CMP		EDX, ECX
          44:       JNL		L2
          46:       MOV		ECX, EDX
          48:       MOV		EDX, EAX
          4A: L2:
    ...
          5B: L3:   MOV		EAX, ECX
          5D:       MOV		ECX, EBX
          5F:       JMP		L1
          61: L4:   MOV		EAX, EDX
          63:       MOV		EDX, EBX
          65:       JMP		L0

Lisp 比任何其他语言都更有潜力编写高效的代码，这一切都归功于宏。 因为它们非常擅长创建受控计量实
验，所以宏也是确定哪些技术产生更有效结果的解决方案。 编译器是编写程序的程序，而宏是最好的方法。
