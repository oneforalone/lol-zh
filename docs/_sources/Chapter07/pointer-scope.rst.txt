.. _pointer_scope:

==================================
7.4 指针作用域
==================================

:Author: Doug Hoyte
:Translator: Yuqi Liu <yuqi.lyle@outlook.com>

从一种语言中删除指针是否会降低该语言的能力?特别是，lisp缺乏显式的指针作用域是否妨碍我们有效地实现指针算法中指定的算法?事实证明不是这样的，在lisp中缺乏对指针的直接支持在理论上和实践上都不构成挑战。在像C这样的语言中，任何可以用指针实现的算法或数据结构都可以在lisp中实现，甚至更好。

但是，什么是指针作用域，我们为什么要使用它?指针作用域包括将计算机的内存(或虚拟内存)作为一个大的、可索引的数组来处理，它可以从中加载和存储固定值。这听起来危险吗?它应该是，因为它是许多复杂错误的根源，也是当今几种最大的软件安全问题的直接原因。

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

指针作用域实际上是指定间接访问的一种方法，也就是跨环境访问，而间接访问恰好也与固定数值运算绑定。我们通常如何跨环境编程?我们使用Common Lisp提供的词法或动态作用域，这两种作用域的双重组合，或者由宏创建的新类型的作用域。指针-&宏和指针-* 函数是为我们描绘指针作用域错觉的例子，表明当你认为你需要一个指针时，你可能真的需要一个闭包。我所听到的关于指针和闭包之间的类比的第一个也是唯一的一个例子是Oleg Kiselyov在comp.lang.scheme新闻组上发表的一篇文章[pointer-as-closures]。他建议使用闭包来模拟指针，并为Scheme提供了一个实现.

指针-&和指针-* 显示了一种通过闭包模拟指针间接指向的可能方法。当我们使用指针-&宏时，它会展开成一个lambda形式，其中有一些智能，以确定您是否想要获取或设置值，并相应地执行。指针-&使用gensyms来做到这一点。而不是使用它们作为绑定的名字以避免不必要的变量捕获在编译时,指针——使用它们,以确保不可能捕获运行时,我们禁止设置一个闭包的价值一定的价值,因为它与我们的实现冲突。例如，我们可能已经为这个选择了lisp默认值nil，这通常会工作，除非我们试图传递nil作为参数。gensym很方便在运行时使用，因为我们知道永远不会有另一个值eq的gensym。这就是他们存在的理由。

指针*及其defsetf是通过泛型变量访问这些间接值的框架。有指针-&的defsetf，以便指针-&的展开将知道如何设置嵌套的间接指向。作为一个简单的例子，我们可以创建一个闭包，通过在let环境中创建一个对绑定的引用来模拟C中常见的指针指向模式：

.. code-block::

  * (let ((x 0))
      (pointer-& (pointer-& x)))
  #<Interpreted Function>

让我们将这个闭包存储起来，以便以后使用，方法是将它从*特殊变量中转移过来(让我们保持这些星号的清晰)：

.. code-block::

  * (defvar temp-pointer *)
  #<Interpreted Function>

现在可以解引用这个闭包了：

.. code-block::

  * (pointer-* temp-pointer)
  #<Interpreted Function>

看来我们又有个了结了。我们只解引用了指针链的一个步骤。使用*特殊变量来引用前面的结果，让我们进一步取消引用：

.. code-block::

  * (pointer-* *)
  0

0是我们指向的原始对象。我们也可以使用这种解引用语法——这当然是闭包的错觉——通过指针链来设置这个对象的值:

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

即使它们可能处于不同的间接级别，这个解引用链中的所有闭包仍然指向最初的let环境:

.. code-block::

  * (pointer-* (pointer-* temp-pointer))
  9

但这可能不是我们所说的指针作用域。因为大多数计算机处理器认为内存是一个很大的固定数字数组，而且由于C是围绕现有处理器的功能设计的，所以C的指针作用域永久性地与固定数字算法绑定在一起。在C语言中，当你解除对指针的引用时，你总是知道发生了什么:编译器在代码中编译到带有fixnum的内存索引，并检索或设置一个fixnum值。C的指针作用域和上面的闭包解引用技术的最大区别在于，虽然C允许我们通过添加或减去固定值来改变指针指向的位置，但由指针-&编译并使用指针-* 访问的闭包是固定的。用于访问和设置它们的代码——不管|是什么，都会在编译时添加到间接环境中。即使在上面的简单示例中，我们至少使用了两种不同类型的闭包，由于泛型变量的存在，这两种闭包都可以通过统一的解引用语法进行访问。我们最初所指的x是一个词法变量，而我们所指的临时指针隧道变量是一个动态变量。正如我们在6.7节Pandoric宏中所看到的，我们可以以任何我们想要的方式定制闭包，从而间接定制闭包。

所以闭包实际上比C风格的指针更灵活，更不危险。当你认为你需要一个指针时，你可能需要一个闭包。闭包不仅仅是一个可以用作地址的固定数字，它是编译后用于在任何环境中检索和设置任何类型数据的代码。尽管对于大多数任务来说，闭包是实现间接的最佳构造，但有时我们希望利用处理器的固定数目寻址功能来实现非常高效的代码。C让我们做;Common Lisp让我们做得更好。

在lisp中使用c风格的指针实际上非常简单，不需要偏离我们通常的lisp技术。我们只是提供一个fixnum数组,使用数字索引数组索引|思考它就像c。然后,我们使用声明让lisp下降类型和安全检查,所以编译就像c。最后,我们使用宏使整个过程方便和安全。

通常，为数组建立索引是一个复杂而缓慢的过程。编译器需要检查您的索引是否为数字，您正在尝试索引一个数组，并且索引在数组的范围内。此外，不同类型的数组可以有不同的代码来访问元素。加载了这本书的代码后，试着评估以下形式(详见7.3节，了解反汇编器):

.. code-block::

  (dis (arr ind)
    (aref arr ind))

因为aref可以在不知道类型的情况下表示很多可能的东西，所以编译器可能不会内联数组访问代码。在上面的反汇编输出中，您应该看到对类似CMUCL的数据向量-ref的函数调用。练习:获取lisp环境的源代码并检查这个函数。在CMUCL中，它位于file array.lisp中。还要检查该文件中的其他函数，包括数据向量集。如果您的lisp环境没有提供完整的源代码，或者您不能对您所拥有的源代码做任何您想做的事情，请尽快升级您的Common lisp环境。

就像Common Lisp在有足够的类型信息时可以内联函数+一样，它也可以内联aref。试试下面的形式:

.. code-block::

  (dis (((simple-array fixnum) arr)
        (fixnum ind))
    (aref arr ind))

上述操作应该已经删除了对通用数组引用函数的间接访问。简单数组是一维数组，其中的元素在内存中相邻，就像c风格的内存。在上面我们指定了fixnum作为数组元素，但是您的Common Lisp环境可能还提供了不同大小、字节、无符号字节、浮点数、双浮点数等类型的fixnum。虽然上面没有包含间接的，但是它仍然有很多代码实现了我们在编程lisp时通常依赖的类型和安全检查。然而，正如我们可以使用第7.2节中的sharp-f read宏，宏使Lisp快速告诉Lisp使算术快速，同样也可以用于数组引用：

.. code-block::

  (dis (((simple-array fixnum) arr)
    (fixnum ind))
  #f
  (aref arr ind))

与我们之前的arefs不同，这段代码的性能将不会被类型和安全检查所控制。这是应该在性能关键循环中使用的代码。请注意，因为我们已经从这段代码中删除了几乎所有的安全特性，所以它与C语言中的同类代码一样危险。特别是，它可能会遇到缓冲区溢出问题。使用C，你在任何地方都是这样编程的。使用lisp，你可以安全地在任何地方编程，除了性能问题，调优代码的热点，使整个程序运行得更快。由于使用宏，这些热点可以任意小。例如，不需要在快速/危险模式下编译整个函数。宏允许我们优化表达式中狭窄的、特定的部分。快速代码可以透明地与安全代码和宏共存，这让我们放弃了最不安全的必要条件，以实现所需的性能。

因为如果您在本书中读到这里，您应该已经对宏的编写和声明有了很好的了解，关于指针作用域没有更多需要说明的了。简而言之，C提供了一种非常特定于领域的语言，用于基于固定数量算法控制CPU，但您可以使用宏编写更好的语言。高效的指针作用域(我们现在可以承认这实际上意味着数组访问——闭包示例除外)主要是了解宏如何工作，声明如何工作，以及如何读取反汇编程序的问题。

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

一个有效访问数组的宏示例是-fast-stack。选择这个宏观模型是为了提供一个讨论摊销的机会。With-fast-stack实现堆栈数据结构被信谊。与Common Lisp和流行推栈使用缺点细胞存储任何类型的栈的元素,这些栈使用一个简单的数组存储元素的一个固定的类型可以指定的:输入关键字。数组的大小也是固定的，但是这个大小可以通过:size关键字来选择。通过使用一些宏定义的局部宏来访问堆栈。如果您的堆栈名是input，则宏绑定将是fast-push-input、fast-pop-input和check-stacks-input。使用dis检查编译后的扩展:

.. code-block::

  * (dis ((fixnum a))
      (with-fast-stack (input :size 2000)
        (loop for i from 1 to 1000000 do
          (fast-push-input a))))

快速推入操作编译成非常严格(且非常不安全)的机器代码:

.. code-block::

  ;;; [8] (FAST-PUSH-INPUT A)
  MOV     ECX, [EBP-20]
  MOV     EDX, [EBP-16]
  MOV     EAX, [EBP-12]
  MOV     [ECX+EDX+1], EAX
  MOV     EAX, [EBP-16]
  ADD     EAX, 4
  MOV     [EBP-16], EAX

但是循环像往常一样安全地编译，实现了错误检查和间接算术函数，即使它是在with-fast-stack宏中。

.. code-block::

  ;;; [7] (LOOP FOR I FROM 1...)
  ...
  CALL    #x100001D0  ; #x100001D0: GENERIC-+
  ...
  CALL    #x10000468  ; #x10000468: GENERIC->

很明显，这个循环不会像它可以运行的那样快。它的性能将由循环开销决定，而不是堆栈操作。如果我们需要速度，我们可以将i声明为一个固定值，并向循环中添加速度声明，就像我们之前看到的那样。安全代码可以与快速代码共存。当然，我们刚刚拆解的代码非常危险。它从不检查堆栈的高度，看我们是否溢出或溢出超过我们的边界。这是我们为了效率而尽量避免的。with-fast-stack所提供的解决方案是受到第四种编程语言中“栈”一词的启发。通过使用check-stacks-input本地宏，我们的代码可以验证堆栈是否在边界内，否则会抛出一个错误。由于forth被设计为在最有限的硬件平台上表现良好，因此forth分摊了执行边界检查的成本。与默认情况下lisp在每个操作之后执行不同，它只在每个N个操作之后执行。在forth中，这个词通常只在对REPL中的表单求值之后才会被调用(关于forth，我们将在第8章，Lisp Moving forth Moving Lisp中有更多的介绍)。因此，我们可以每10个操作检查一次边界，而不是每次操作都检查边界，也许可以减少90%的边界检查成本。当我们检查堆栈时，我们知道，最坏情况下，它将有10个超出边界的元素。或者可能在代码中有一些方便的、非关键性能的地方可以使用check宏。

with-fast-stack的另一个特性是它创建的数组有安全区域。也就是说，如果你搞砸了，它会在堆栈的任意一侧分配额外的内存作为逃跑通道。这并不意味着跑到这些安全区域是一个好主意(特别是下流区域)，但它比跑到你没有分配的内存要好。

正如前面提到的，我们刚刚组装的代码是非常危险的，它会将补丁写入没有分配给它的内存中。永远不要这样做。练习:这样做。以下是发生在我身上的事情:

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

危险的代码编译得很好。让我们试着运行它:

.. code-block::

  * (funcall * 31337)
  NIL

这不是我们所担心的灾难。有什么不好的事情发生吗?

.. code-block::

  * (compile nil '(lambda () t))
  ; Compilation unit aborted.

听起来不太好。

.. code-block::

  * (gc)
  Help! 12 nested errors.
  KERNEL:*MAXIMUM-ERROR-DEPTH* exceeded.
  ** Closed the Terminal
  NIL

这听起来肯定不好。因为lisp是运行在unix上的进程，所以它也可能接收到信号，指示您已经在分配的虚拟内存之外编写了代码(称为段错误)。CMUCL将这些作为可恢复条件处理(尽管你应该总是重新加载你的lisp图像):

.. code-block::

  Error in function UNIX::SIGSEGV-HANDLER:
    Segmentation Violation at #x58AB5061.
    [Condition of type SIMPLE-ERROR]

在这些状态下，lisp图像被称为软管。那些有可能被像这样冲洗掉的项目都是即将发生的安全灾难。C和lisp之间的区别是，C几乎在所有地方都有这种潜力，而lisp几乎没有。如果我们需要承担基于数组的指针作用域的风险，lisp宏是最不突出和最安全的方法。当然，您几乎不想承担这些风险——坚持使用闭包。
