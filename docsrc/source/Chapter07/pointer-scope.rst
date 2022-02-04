.. _pointer_scope:

==================================
7.4 指针作用域
==================================

:Author: Doug Hoyte
:Translator: Yuqi Liu <yuqi.lyle@outlook.com>

从一种语言中删除指针是否会降低该语言的能力？特别是，lisp 缺乏显式的指针作用域是否妨碍我们有效地实现指针算法中指定的算法？事实证明不是这样的，在 lisp 中缺乏对指针的直接支持在理论上和实践上都不构成挑战。在像 C 这样的语言中，任何可以用指针实现的算法或数据结构都可以在 lisp 中实现，甚至更好。

但是，什么是指针作用域，我们为什么要使用它？指针作用域包括将计算机的内存(或虚拟内存)作为一个大的、可索引的数组来处理，它可以从中加载和存储固定值。这听起来危险吗？当然，因为它是许多复杂错误的根源，也是当今几种最大的软件安全问题的直接原因。

.. code-block::

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

指针作用域实际上是指定间接访问的一种方法，也就是跨环境访问，而间接访问恰好也与固定数值运算绑定。我们通常如何跨环境编程？我们使用 COMMON LISP 提供的词法或动态作用域，这两种作用域的双重组合，或者由宏创建的新类型的作用域。 ``pointer-&`` 宏和 ``pointer-*`` 函数是为我们描绘指针作用域错觉的例子，表明当你认为你需要一个指针时，你真正的需要可能是个闭包。我所听到的关于指针和闭包之间的类比的第一个也是唯一的例子是 Oleg Kiselyov 在 ``comp.lang.scheme`` 新闻组上发表的一篇文章 `pointer-as-closures <https://okmij.org/ftp/Scheme/pointer-as-closure.txt>`_ 。他建议使用闭包来模拟指针，并为 Scheme 提供了一个实现。

``pointer-&`` 和 ``pointer-*`` 展示了一种通过闭包模拟指针间接指向的可能。当使用 ``pointer-&`` 宏时，它会展开成 lambda 结构，其中有一些智能，以确定您是否想要获取或设置值，并相应地执行。 ``pointer-&`` 使用 *gensyms* 来做到这一点。而不是使用它们作为绑定的名字以避免不必要的变量捕获在编译时， ``pointer-&`` 使用它们以确保没有运行时的异常捕获，这里阻止将闭包的值设为个确定值，因为它与我们的实现冲突。例如，我们可能已经为这个选择了 lisp 默认值 ``nil``，通常这可以运行，除非我们将 ``nil`` 作为参数传参。 *gensym* 在运行时使用很方便，因为我们知道永远不会有另一个值 ``eq`` *gensym* 。这就是他们存在的理由。

``pointer-*`` 及其 ``defsetf`` 是通过泛型变量访问这些间接值的框架。这里 ``pointer-&`` 中的 ``defsetf`` ，在 ``pointer-&`` 展开中知道如何设置嵌套的间接指向。一个简单的例子，我们可以创建个闭包，通过在 let 环境中创建对绑定的引用来模拟 C 中常见的 *指向指针的指针（ pointer to a pointer )* 模式：

.. code-block::

  * (let ((x 0))
      (pointer-& (pointer-& x)))
  #<Interpreted Function>

将这个闭包保存起来，以便之后使用，方法是将它从 ``*`` 特殊变量中转移过来（让我们保持这些星号的清晰）：

.. code-block::

  * (defvar temp-pointer *)
  #<Interpreted Function>

现在可以解引用这个闭包了：

.. code-block::

  * (pointer-* temp-pointer)
  #<Interpreted Function>

看来又有另一个闭包了。我们只解引用了指针链的一个步骤。使用 ``*`` 特殊变量来引用前面的结果，让我们进一步解引用：

.. code-block::

  * (pointer-* *)
  0

``0`` 是最开始指向的对象。我们也可以使用这种解引用语法 —— 当然这是闭包的错觉 —— 通过指针链来设置这个对象的值：

.. code-block::

  * (setf (pointer-* (pointer-* temp-pointer)) 5)
  5

当然，这改变了指向的原有的 let 环境，因此有了个新值 —— 5：

.. code-block::

  * (pointer-* (pointer-* temp-pointer))
  5

如果我们想的话，也可以添加另一层间接指向：

.. code-block::

  * (pointer-& temp-pointer)
  #<Interpreted Function>

现在需要三层解引用：

.. code-block::

  * (pointer-* (pointer-* (pointer-* *)))
  5

并且其自身也可以像通用变量那样访问：

.. code-block::

  * (setf (pointer-* (pointer-* (pointer-* **))) 9)
  9

即使它们可能处于不同的间接层，这个解引用链中的所有闭包仍然指向最初的 let 环境：

.. code-block::

  * (pointer-* (pointer-* temp-pointer))
  9

但这可能不是我们所说的指针作用域。因为大多数计算机处理器认为内存是一个很大的固定数字数组，而且由于 C 是围绕现有处理器的功能设计的，所以 C 的指针作用域永久性地与固定数字算法绑定在一起。在 C 语言中，当解除对指针的引用时，你总是知道发生了什么：编译器在代码中编译到带有固定数字的内存索引，并检索或设置一个固定数值。C 的指针作用域和上面的闭包解引用技术的最大区别在于，虽然 C 允许我们通过添加或减去固定值来改变指针指向的位置，但由 ``pointer-&`` 编译并使用 ``pointer-*`` 访问的闭包是固定的。用于访问和设置它们的代码 —— 不管是什么 —— 都会在编译时添加到间接环境中。即使在上面的简单示例中，我们至少使用了两种不同类型的闭包，由于泛型变量的存在，这两种闭包都可以通过统一的解引用语法进行访问。我们最初所指的 ``x`` 是一个词法变量，而我们所指的 ``temp-pointer`` *tunnel* 变量是动态变量。正如 :doc:`../Chapter06/pandoric` 中，我们可以随意定制闭包，因此也可以随意定制间接闭包。

所以闭包实际上比 C 风格的指针更灵活、更安全。当你认为你需要一个指针时，你可能需要一个闭包。闭包不仅仅是个可以用作地址的固定数字，它是编译后用于在任何环境中检索和设置任何类型数据的代码。尽管对于大多数任务来说，闭包是实现间接的最佳构造，但有时我们希望利用处理器的固定数目寻址功能来实现非常高效的代码。C 可以做的，COMMON LISP 做得更好。

在 lisp 中使用 C 风格的指针实际上非常简单，不需要偏离通常的 lisp 技术。只是提供一个固定数值数组，使用数字索引数组 —— 就像 C 中那样。然后，用声明让 lisp 去掉类型和安全检查，所以编译也和 C 一样。最后，用宏使整个过程方便和安全。

通常，为数组建立索引是一个复杂而缓慢的过程。编译器需要检查索引是否为数字，在索引数组时，确保索引在数组的范围内。此外，不同类型的数组有不同的代码来访问元素。加载了这本书的代码后，试着执行下面代码（ ``dis`` 详见 :doc:`disassembler` ）：

.. code-block::

  (dis (arr ind)
    (aref arr ind))

因为 ``aref`` 在不知道类型的情况下可以表示很多可能，所以编译器可能不会内联数组访问代码。在上面的反汇编输出中，应该看到对类似 CMUCL 的 ``data-vector-ref`` 函数调用。练习：获取 lisp 环境的源代码并检查这个函数。在 CMUCL 中，它位于 ``array.lisp`` 文件中。还要检查该文件中的其他函数，包括数据向量集。如果 lisp 环境没有提供完整的源代码，或者不能对所拥有的源代码做任何想做的事情，请尽快升级COMMON LISP 环境。

就像 COMMON LISP 在有足够的类型信息时可以内联函数 ``+`` 一样，它也可以内联 ``aref`` 。试试下面的代码：

.. code-block::

  (dis (((simple-array fixnum) arr)
        (fixnum ind))
    (aref arr ind))

上述操作应该已经删除了对通用数组引用函数的间接访问。简单数组是一维数组，其中的元素在内存中相邻，就像 C 风格的内存。在上面我们指定了固定数值作为数组元素，但是 COMMON LISP 环境可能还提供了不同大小、字节、无符号字节、浮点数、双浮点数等类型的固定数值。虽然上面没有包含间接的，但是仍然有很多代码实现了在 lisp 编程时通常依赖的类型和安全检查。然而，正如我们可以使用 :doc:`macros-make-lisp-fast` 中的 ``#f`` 读取宏 告诉 lisp 使算术更快，同样也可以用于数组引用：

.. code-block::

  (dis (((simple-array fixnum) arr)
    (fixnum ind))
  #f
  (aref arr ind))

与之前的 ``aref`` 不同，这段代码的性能将不会被类型和安全检查所控制。这是应该在性能关键循环中使用的代码。请注意，因为我们已经从这段代码中删除了几乎所有的安全特性，所以它与 C 语言中的同类代码一样危险。特别是，它可能会遇到缓冲区溢出问题。使用 C，在任何地方都是这样编程的。使用 lisp，你可以安全地在任何地方编程，除了性能问题，调优代码的 *hot-spots* ，使整个程序运行得更快。由于使用宏，这些 *hot-spots* 可以任意小。比如说，不需要在快速/危险模式下编译整个函数。宏允许我们优化表达式中细小的、特定的部分。高效代码可以透明地与安全代码和宏共存，这放弃了最不安全的必要条件，以实现所需的性能。

因为如果你在本书中读到这里，你应该已经对宏的编写和声明有了很好的了解，关于指针作用域没有更多需要说明的了。简而言之，C 提供了一种非常特定作用域的语言，用于基于固定数量算法控制 CPU，但你可以使用宏编写更好的语言。高效的指针作用域（我们现在可以承认这实际上意味着数组访问 —— 闭包示例除外）主要是了解宏如何工作，声明如何工作，以及如何读取反汇编程序的问题。

.. code-block::

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

高效访问数组的宏示例是 ``with-fast-stack``。选择这个宏是为了讨论 *摊销（ amortisation）* 。``with-fast-stack`` 实现了个名为 ``sym`` 的堆栈数据结构。不同于 COMMON LISP ``push`` 和 ``pop`` 使用 cons 单元存储任何类型的栈的元素，``with-fast-stack`` 中用简单的数组存储可以用 ``:type`` 关键字来指定类型的固定类型。数组的大小也是固定的，但是这个大小可以通过 :size 关键字来设置。通过使用一些宏定义的局部宏来访问堆栈。如果堆栈名是 ``input``，则宏绑定将是 ``fast-push-input``、``fast-pop-input`` 和 ``check-stacks-input`` 。用 ``dis`` 检查编译后的展开：

.. code-block::

  * (dis ((fixnum a))
      (with-fast-stack (input :size 2000)
        (loop for i from 1 to 1000000 do
          (fast-push-input a))))

``fast-push-input`` 操作编译成非常紧凑（且非常不安全）的机器代码:

.. code-block::

  ;;; [8] (FAST-PUSH-INPUT A)
  MOV     ECX, [EBP-20]
  MOV     EDX, [EBP-16]
  MOV     EAX, [EBP-12]
  MOV     [ECX+EDX+1], EAX
  MOV     EAX, [EBP-16]
  ADD     EAX, 4
  MOV     [EBP-16], EAX

但是循环像往常一样安全地编译，实现了错误检查和间接算术函数，即使是在 ``with-fast-stack`` 宏中。

.. code-block::

  ;;; [7] (LOOP FOR I FROM 1...)
  ...
  CALL    #x100001D0  ; #x100001D0: GENERIC-+
  ...
  CALL    #x10000468  ; #x10000468: GENERIC->

明显，这个循环不会像预期的那样快。它的性能将由循环开销决定，而不是堆栈操作。如果需要高效，可以将 ``i`` 声明为固定值，并向循环中添加速度声明，就像之前看到的那样。安全代码可以与高效代码共存。当然，刚刚反汇编的代码非常危险。它从不检查堆栈的高度来缠看是否上溢或下溢出边界。这是为了效率而尽量避免的。 ``with-fast-stack`` 提供的解决方案是受到 *forth* 编程语言中 ``stack`` 一词的启发。通过 ``check-stacks-input`` 本地宏，我们的代码可以验证堆栈是否在边界内，否则会抛出异常。由于 *forth* 被设计为在最有限的硬件平台上性能也很好，因此 *forth* 分摊了执行边界检查的成本。与默认情况下 lisp 在每个操作之后执行不同，它只在每 N 个操作之后执行。在 *forth* 中，这个词通常只在对 REPL 中的结构求值之后才会被调用（关于 *forth*，我们将在 :doc:`../Chapter08/index` 中介绍）。因此，我们可以每 10 个操作检查一次边界，而不是每次操作都检查边界，也许可以减少 90% 的边界检查成本。当我们检查堆栈时，我们知道，最坏情况下，有 10 个超出边界的元素。或者可能在代码中有一些方便的、非关键性能的地方可以使用 check 宏。

``with-fast-stack`` 另一个特性是其创建的数组有安全区域。也就是说，如果你搞砸了，它会在堆栈的任意一侧分配额外的内存作为紧急通道。这并不意味着跑到这些安全区域是好主意（特别是下溢时），但比跑到未分配的内存要好。

正如前面提到的，刚刚反汇编的代码非常危险，它会将固定数值写入未分配的内存中。永远不要这样做。练习：试试这个，以下是我执行的结果:

.. code-block::

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

.. code-block::

  * (funcall * 31337)
  NIL

好吧，这不是我们所担心的灾难。有什么不好的事情发生吗?

.. code-block::

  * (compile nil '(lambda () t))
  ; Compilation unit aborted.

Hm，这个结果不太好了。

.. code-block::

  * (gc)
  Help! 12 nested errors.
  KERNEL:*MAXIMUM-ERROR-DEPTH* exceeded.
  ** Closed the Terminal
  NIL

这个结果肯定不好。因为 lisp 是运行在 unix 上的进程，所以它也可能接收到信号，指示在分配的虚拟内存之外编写了代码（称为 *段错误 seg-fault* ）。CMUCL 将这些作为可恢复条件处理（尽管你应该总是重新加载 lisp 镜像）：

.. code-block::

  Error in function UNIX::SIGSEGV-HANDLER:
    Segmentation Violation at #x58AB5061.
    [Condition of type SIMPLE-ERROR]

在这些状态下，lisp 镜像称之为 *欺诈（hosed）* 。那些有可能被像这样成为 "欺诈“ 的项目都是即将发生的安全灾难。C 和 lisp 之间的区别是，C 几乎在所有地方都有这种潜力，而 lisp 几乎没有。如果需要承担基于数组的指针作用域的风险，lisp 宏是最不突出和最安全的方法。当然，如果不想承担这些风险 —— 坚持使用闭包。
