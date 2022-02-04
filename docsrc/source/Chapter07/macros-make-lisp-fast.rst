.. _macros_make_lisp_fast:

==================================
7.2 宏让 Lisp 更快
==================================

:Author: Doug Hoyte
:Translator: Yuqi Liu <yuqi.lyle@outlook.com>

本节展示了用三种类型的宏来协助创建高效程序的示例：常规宏、读取宏和这里介绍的新类型宏 —— 编译宏。

宏可以用来控制算法、数据结构、类型检查、安全检查、代码或部分代码的优化级别等等。我们可以在一个程序（甚至函数）中同时存在安全和通用的代码，也可以同时存在执行快和危险的代码。简而言之，没有任何一种语言提供了像 lisp 这样的开放接口来控制编译器，这都要归功于宏（不然还能是什么呢？）。大概浏览下 ANSI 标准会发现标准里看起来是说：宏和声明（与编译器沟通的最直接方式）不能很好地协同工作::

  宏结构不能展开成声明；声明表达式必须以它们所引用结构的实际子表达式的形式出现。

ANSI 的意思是，下面的宏不会按预期工作:

.. code-block::

  (defmacro go-fast () ; Broken!
    '(declare (optimize (speed 3) (safety 0))))

我们不能把宏调用放在需要声明的地方。这个问题的另一种思考角度是，在检查声明之前，系统的代码遍历程序不需要展开特殊结构主体中的宏。想要执行得快是件很常见的事情，所以也许可以做得比上面有问题的 ``go-fast`` 宏更好。想要尽可能多地压缩含义时，通常需要一个读宏。读宏也适合展开成声明，因为它们在代码遍历程序尝试遍历代码之前就展开了。它们是以实际的子表达式读入的。

.. code-block::

  (set-dispatch-macro-character #\# #\f
    (lambda (stream sub-char numarg)
      (declare (ignore stream sub-char))
      (setq numarg (or numarg 3))
      (unless (<= numarg 3)
        (error "Bad value for #f: ~a" numarg))
      `(declare (optimize (speed ,numarg)
                          (safety ,(- 3 numarg))))))

``#f`` 是个读宏，能控制 COMMON LISP 程序最重要的性能权衡：声明的速度和安全之间的平衡。例如，``#f`` 本身读取的是我们希望 ``go-fast`` 扩展的内容:

.. code-block::

  * '#f
  (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))

但是，我们可以改变这一点，并将一个小于 3 的数作为 reader number 参数来声明安全高于速度。所有的调度读宏都可以接受这样一个数字参数，它作为第三个参数（通常称为 ``numarg``）传递给 read 宏函数。下面是个体现我们重视安全而不是速度的例子，将 SPEED 的参数设为 0:

.. code-block::

  * '#0f
  (DECLARE (OPTIMIZE (SPEED 0) (SAFETY 3)))

也可以设置为 1 和 2，从而产生以下声明。这些不同的声明设置的优点非常依赖于编译器，所以你几乎不会使用它们:

.. code-block::

  * '(#1f #2F)
  ((DECLARE (OPTIMIZE (SPEED 1) (SAFETY 2)))
  (DECLARE (OPTIMIZE (SPEED 2) (SAFETY 1))))

尽管宏不能直接扩展为声明，但我们仍然可以使用常规宏来控制声明。因为代码遍历程序在展开宏之前不能遍历宏结构来搜索声明，所以无法判断该声明是编写结构的实际子表达式，还是宏在展开时添加了声明。

.. code-block::

  (defmacro fast-progn (&rest body)
    `(locally #f ,@body))

  (defmacro safe-progn (&rest body)
    `(locally #0f ,@body))

``fast-progn`` 和 ``safe-progn`` 是一些展开的结构中包含声明的简单例子。请注意，这里使用的是 ``locally`` 的隐式 progn 而不是 ``progn`` 本身，因为 ``progn`` 中不能有声明。这两个宏用了之前定义的 ``#f`` 读宏。我们可以使用这些结构作为 ``progn`` 的一个版本，其中内部表达式对执行速度进行了优化(但很危险)，另一个版本确保内部表达式是安全的(可能很慢)：

.. code-block::

  * (macroexpand
      '(fast-progn
        (+ 1 2)))
  (LOCALLY
    (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))
  (+ 1 2)) T

我们还可以在宏参数中提供其他声明，因为它们的位置不是也不能在宏展开之前验证：

.. code-block::

  * (macroexpand
      '(fast-progn
        (declare (type fixnum a))
        (the fixnum (+ a 1))))
  (LOCALLY
    (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))
    (DECLARE (TYPE FIXNUM A))
    (THE FIXNUM (+ A 1)))
  T

在尝试宏扩展时，有时会想看看在将宏扩展嵌入不同的词法上下文时会发生什么。将 :doc:`../Chapter04/runtime` 中的读取时计算宏与 ``*`` 变量（保持最后三个REPL结果可用）结合起来，可以看到我们的代码的计算结果如预期的那样:

.. code-block::

  * (let ((a 0))
      #.*)
  1

但是请注意，尽管上面的计算是正确的，但是声明有时只对编译后的代码进行充分考虑。例如，由于上面的计算解释了代码，它可能会忽略安全声明，并继续将溢出结果提升为大数（ *bignum* ）。来看看这里是否会发生这种情况:

.. code-block::

  * (let ((a most-positive-fixnum))
      #.**)
  536870912

确实会将溢出结果提升为大数，CMUCL忽略了解释代码的声明。我们想在 ``***`` 中继续玩我们的表达式，但由于不确定下次是否能得到它，就把它带回 * ，这样就不会丢失表达式:

.. code-block::

  * ***
  (LOCALLY
    (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0)))
    (DECLARE (TYPE FIXNUM A))
    (THE FIXNUM (+ A 1)))

就是这样。所以现在有三次机会让它工作。试试编译它，看下会不会得到个 *fixnum* 的封装：

.. code-block::

  * (funcall
      (compile nil
        `(lambda ()
          (let ((a most-positive-fixnum))
  ,*))))
  ; Warning: This is not a (VALUES FIXNUM &REST T):
  ;   536870912
  536870912

Emm，到底发生了呢？我们不是告诉 lisp 不要检查吗？像常量折叠这样的编译时优化让声明的推导更复杂。当 lisp 编译代码时，它能够在编译时执行加法，因为我们添加的是常量，因此它知道结果也将是常量，所以就没必要在运行时计算它。当 lisp 这样做的时候，它看到我们对一个 fixnum 的声明肯定是错误的。这个警告是用 lisp 的方式告诉我们“你这个笨蛋，我无视你的声明，因为你不可信。”如果稍微改变一下表达式，让 lisp 不能折叠任何常量，最终可以看到 *fixnum* 封装的效果:

.. code-block::

  * (funcall
      (compile nil
  `(lambda (a)
  7.2. MACROS MAKE LISP FAST 215
          ,**))
      most-positive-fixnum)
  -536870912

声明的另一个重要属性是，它们可以像词法变量可以遮蔽其他词法变量一样遮蔽其他声明。例如，我们可能希望编写个宏来执行安全检查，即便是被嵌入到声明为不安全的代码中:

.. code-block::

  (defmacro error-checker ()
    `(safe-progn
      (declare (type integer var))
      do-whatever-other-error-checking))

再封装一层，我们可以用这些宏来添加错误检查代码，这些代码需要执行的比较快而不是比较安全，通过嵌套这些宏的其他用法来实现：``fast-progn`` ：

.. code-block::

  (defun wrapped-operation ()
    (safe-progn
      do-whatever-error-checking
      (fast-progn
        but-this-needs-to-go-fast)))

在高性能lisp代码中，使用围绕某些功能的快速实现的错误检查区域安全地验证参数是一种常见模式。特别是对于数组遍历这样的迭代过程，可以通过在操作开始前进行类型和边界检查等错误检查，然后在执行时尽可能地忽略它们，从而显著提高运行时性能。

COMMON LISP 首先是为了强大的编程能力而设计的；效率是个较远的次要问题。然而，这些功能、功率和效率并不一定代表一种权衡。通过宏，我们可以应用 lisp 强大功能来解决效率问题。除了常规宏和读取宏（它们本身已经提供了相当强大的功能）之外，COMMON LISP还提供了编译宏。编译宏是与其他类型宏相同意义上的宏：它们是编程的程序。大多数lisp教程都没有很好地描述编译器宏，这表明性能对于程序员来说是多么重要（几乎从来没有）。然而，编译宏是某些效率问题的优雅解决方案，值得成为每个lisp专业人员的工具包。

编译宏定义了 lisp 编译器将应用于（命名）函数调用的转换。这意味着可以使用 ``defun`` 创建的函数，并告诉 lisp 不要编译对该函数的调用，而是应该编译编译宏指示的一些代码。为什么要将函数与编译宏结合使用，而不是一开始就用这个名字编写宏呢？第一个不太重要的原因是，这让我们能够更多地控制何时吸收编译开销。特别的是，COMMON LISP并没有指定何时或者多长时间扩展一个宏。在解释代码中，宏每次被调用时都有可能被展开。在进行编译时优化时，我们希望在运行函数之前执行一个（可能很长且昂贵的）计算，以减少函数本身必须执行的计算量。编译宏为我们提供了一种方法，当我们编译代码时，只执行一次冗长的编译计算 —— 它本该是这样的。

但比只在正确的时间执行一次编译计算更重要的是，编译宏很有用，因为它们将语法的二元性引入语言。编译宏允许我们为任何表示（命名）函数调用的代码结构添加双重含义。除了常规意义外，编译器宏还添加了编译意义。强烈推荐确保编译后的含义实现与常规含义任务相同，但可以随意改变它的执行方式（这是重点）。使用双重语法的好处是，可以改变代码的效率，而不需要修改代码。我们可以使用一个现有的代码库 —— 一个可能使用了大量函数调用的代码 —— 并通过引入双重语法来改变代码的编译方式。我们所要做的就是找到代价很高的函数调用，然后实现编译器宏，将它们转换为代价低的展开。

哪种类型的函数调用开销高呢？作为第一个例子，回想一下 :doc:`../Chapter04/reader-security` 中，函数可以执行 lambda 析构，而且这是更通用的 defmacro 析构的子集。当函数接受关键字参数时，我们将它们作为分组的关键字符号对及其对应的值进行传递。关键字参数非常有用，但遗憾的是，使用关键字参数的函数比不使用关键字参数的函数调用开销更大。解构不是免费的。编译器需要将代码编译到函数中，该函数扫描必要的可变长度参数列表，以正确的顺序获取值(包括插入默认值)，然后实际执行函数。一般来说，lisp编译这些关键字参数的代码非常快，所以我们几乎从不注意（或关心）这种低效率。然而，在某些情况下，我们确实会关心这个问题，特别是当我们在性能关键的循环中调用这样的函数时。

.. code-block::

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

``fast-keys-strip`` 是个实用程序，它接受由常规参数和关键字参数组成的 lambda 解构列表，并返回用于引用这些参数的符号列表。换句话说，当传递 ``(a b c)`` 或 ``(a &key b (c 0))`` 时，程序返回 ``(a b c)`` ，但是传给程序 ``(a &optional b c)`` 是不行的。

.. code-block::

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

``defun-with-fast-keywords`` 用法与 ``defun`` 相同。与 ``defun`` 类似，``defaun-with-fast-keywords`` 的第一个参数是命名函数的符号，第二个参数是参数列表，其余的是定义要执行的函数的形式。然而，与 ``defun`` 不同的是，``defun-with-fast-keywords`` 结构只能给出常规参数和关键字参数（没有 optional，rests 等）。练习：扩展 ``fast-keywords-strip`` 来处理所有的 lambda 解构列表。

``defun-with-fast-keywords`` 的展开非常复杂。它展开成三种结构。第一种展开对函数的定义和常规的 ``defun`` 函数一样。第二种展开将函数定义了一个名为 ``g!fast-fun`` 的函数。这个函数类似于第一个函数，除了对每个参数（是否关键字）接受一个非关键字参数。接下来定义一个编译器宏来将对第一个函数的调用转换为对第二个函数的调用。因此，我们不是让第一个函数执行关键字解构，而是利用调用函数的格式的编译时知识，并使用解构绑定将关键字按正确的顺序放在一起。

.. code-block::

  (defun
    slow-keywords-test (a b &key (c 0) (d 0))
    (+ a b c d))

  (compile 'slow-keywords-test)

  (defun-with-fast-keywords
    fast-keywords-test (a b &key (c 0) (d 0))
    (+ a b c d))

现在我们有了一个（几乎）双重语法 ``defun``。带有关键字参数的函数的常规定义类似于 ``slow-keyword-test``。编译它是为了下面的基准测试。``fast-keywords-test`` 与 ``slow-keywords-test`` 的写法相同，只是用的是 ``defun-with-fast-keywords`` 而不是 ``defun``。事实证明，我们不需要编译这个函数，因为 ``defun-with-fast-keywords`` 展开为一个调用，只对其中一个需要它的定义进行编译 —— 自动的 gensym ``g!fast-fun``。

.. code-block::

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

``keywords-benchamrk`` 是个简单的函数，其中使用了 ``time`` 宏来告诉我们对这两个函数进行等价的一系列调用需要多长时间。注意，我们还编译了 ``keywords-benchmark``。关于基准测试的更多内容将在 :doc:`writing-benchmarking` 中介绍。

.. code-block::

  * (keywords-benchmark 100000000)
  Slow keys:
  ; Evaluation took:
  ;   17.68 seconds of real time
  Fast keys:
  ; Evaluation took:
  ;   10.03 seconds of real time

调用这个函数1亿次足以让我们看到，即使两个函数都被编译了，使用 ``defun-with-fast-keywords`` 定义的函数运行速度也比它的编译宏快了 40% 左右。还要注意的是，编译宏的性能并不依赖于关键字参数是在编译时已知的常量。注意，我们传递了 ``n``，一种不同的 lisp 结构，作为 ``:c`` 关键字的参数。因此，编译宏将快速版本展开为与慢版本相同的版本，除了没有关键字的析构开销。

那么，为什么 COMMON LISP 不为每个接受关键字的函数都这样做，并总是避免开销呢？编译宏只在编译时应用，但我们希望在运行时保留对参数进行解构的能力。下面是关于编译宏的要点：编译宏是对函数调用的优化，而不是对函数本身的优化。在关键字的情况下，编译宏允许我们消除对函数的编译调用的开销，同时仍然让原始函数（及其关键字解构代码）在运行时可用。编译宏为我们提供了两种不同操作的双重语法，这两种操作只能通过上下文来区分。另一种避免关键字开销的方法，请参阅 Norvig's PAIP (PAIP-P323)。

还有哪些函数调用可以从编译宏中受益？我们不仅可以减少析构开销，而且通常还可以通过预处理常量参数来减少函数本身的开销。编译宏可以在编译时执行一些准备工作，因此不必在运行时执行。其中最明显的例子是 ``format`` 函数。想想 ``format`` （或者，在 C 语言中，``printf`` ）是如何工作的。它是个在运行时将控制字符串传递给它的函数。然后 ``format`` 处理控制字符串并将格式化后的输出打印到流中（或将其作为字符串返回）。实际上，在使用 ``format`` 时，使用控制字符串作为程序对格式字符串解释器进行函数调用。使用编译宏，可以消除函数调用，预处理控制字符串，并将函数调用更改为与调用站点相连接的专门代码，编译器可以在其中进行进一步优化。听起来很难，不是吗？我们必须知道如何将格式控制字符串转换成等价的 lisp 代码。幸运的是，与许多其他事情一样，COMMON LISP 已经考虑过这个问题。COMMON LISP 对格式化的处理是正确的。这是它为创建格式化输出而指定的特定于领域的语言，可以将自己宏编译为 lisp 代码。这是 lisp 哲学的一部分 —— 所有的东西都应该编译成 lisp。将控制字符串编译为 lisp 的宏是 ``formatter``。当把控制字符串提供给 ``formatter`` 时，它将展开为执行所需格式化的 lambda 结构。例如，下面是个简单控制字符串的展开：

.. code-block::

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

所以说 ``formatter`` 展开成了个 lambda 结构。将控制字符串编译成 lisp 结构代码，适合于求值或将宏嵌入到其他 lisp 代码中，在那里它将成为一个编译函数或内联到调用站点的编译代码中。但是请注意，``formatter`` 的展开必须要接受一个流，不能像 ``format`` 那样可以接受 ``nil``。这是因为 ``formatter`` 展开的函数（如 ``write-string`` 和 ``terpri`` ）需要流。可以用 ``with-output-to-string`` 宏来解决这个问题。

.. code-block::

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

``fformat`` 是个完全透明的 ``format`` 封装器。 ``fformat`` 的存在是为了定义一个编译宏来进行格式化。我们需要一个新的函数名，因为在 COMMON LISP 指定的函数上定义编译宏是不行的。我们的编译宏利用了 defmacro 的解构特性：&whole。我们使用它将 ``format`` 绑定到宏调用的实际列表结构。这样做是为了利用编译宏的一个特性：编译宏完全可以选择不展开。如果我们返回 ``form`` ，lisp 会发现我们只是返回传递的 form（用 ``eq`` 检查），同时 lisp 也将要求编译宏不对 form 进一步展开 —— 即便是我们正用编译宏展开成个函数的用法。在编译时，我们选择使用 form 的另一种含义。这是编译宏和普通宏之间的根本区别。编译宏可以与函数共享精确的双重语法，但普通宏不能。在 ``fformat`` 中，当它的控制字符串参数不是常量时，编译宏不展开为更有效的含义。在 ``fformat`` 中，我们仍然希望对非字符串控制字符串（比如返回字符串的函数调用）调用 ``fformat`` 来工作。换句话说，我们仍然希望能够在运行时生成控制字符串。这样的调用显然不能对控制字符串使用编译时优化。

.. code-block::

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

``format-benchmark`` 与前面介绍的 ``keywords-benchmark`` 函数几乎相同。它使用 ``time`` 来比较使用常规 ``format`` 和新的 ``fformat`` 执行大量格式操作所需的时间。以下是 100 万次迭代的结果：

.. code-block::

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

大概提升了 30%。编译宏不仅减少了执行格式化所需的时间，而且还减少了开销（这反过来又减少了垃圾回收的时间）。编译宏避免了在运行时解释格式字符串，而是在函数被编译时只执行一次大部分的计算 —— 这是它本该做的。不幸的是，基准测试常常模糊或删除重要的细节。虽然用 ``fformat`` 预编译格式字符串可以消除解释开销，但这样做的代价是编译一个更大的程序。即使主存充足，较大的代码也会因为指令缓存性能的降低而运行得更慢。

在本节中，我们讨论了使用常规宏、读取宏和专为这个任务设计的一种特殊类型的宏 —— 编译宏来定制代码性能的方法。希望本节和本章的其余部分能说服你，如果想编写真正有效的代码，就需要 COMMON LISP。因为宏，你需要 COMMON LISP。

练习1：下载 Edi Weitz 的 CL-PPCRE（在 :doc:`../Chapter04/cl-ppcre` 中），看看 ``api.lisp`` 怎么使用编译宏。访问Edi 的网站并下载一些他的 lisp 包，这些包看起来很有趣。

练习2：当我们为 ``fformat`` 编写编译宏时，我们被迫显式地使用 ``gensym``，因为没有 ``define-compiler-macro!`` 宏。解决这个问题。

较难的练习：定义 ``define-compiler-macro!`` 这样就能使用了 ``defmacro!`` 的功能而不用调用 ``gensym``。提示：跳出思维定势。
