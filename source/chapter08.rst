.. _chapter08:

***************************
第八章：Lisp 和 Forth
***************************


.. _8-1-weired-by-design:

8.1 奇怪的设计
=======================

本章是迄今为止我们在本书中看到的许多宏技术的高潮。 使用已开发的宏抽象，我们创建
了我最喜欢的编程语言之一的实现：forth。 虽然这个实现体现了 forth 的大部分重要思想，
但它非常不同并且很混乱。 尽管本章中的代码有一些有趣的用途，但主要目的是向 lisp 读者
教授 forth 元编程的概念和基础知识，并成为讨论本书中心主题的平台——用宏创建和使用
二元性语法。

Forth，比除了 lisp 外所有语言都拥有丰富而迷人的历史，我很感激发现它。 出于这个
原因，以及其他一切，这一章是献给我的父亲 Brian Hoyte
的，他向我介绍了 forth 和计算机编程。 本章的部分灵感来自 [THREADING-LISP]
和 Henry Baker [LINEAR-LISP][LINEAR-LISP-AND-FORTH] 的研究。

Forth 是第一个在没有强力的政府、学术或企业赞助的情况下创建和开发的编程语言——或者
至少是第一个成功的此类语言。 1968 年左右，Chuck Moore 独立发明了forth，而不是
出于大型组织的需求，以解决他自己在天文学、硬件设计等方面的计算需求。 从那时起，f
orth 已被热情的草根用户社区分发、实现和改进[EVOLUTION-FORTH-HOPL2]。 与麻省
理工学院（以及后来的 DARPA）对早期 lisp 和 Common Lisp、IBM 的 FORTRAN 和
AT&T 的 unix 语言 C 的赞助形成对比。

由于这些原因，并且因计算机软件和硬件角色的普遍不同的哲学，所以 forth 是不同的。
甚至比 lisp 还要多，forth 看起来很奇怪。 但是和 lisp 一样，forth 看起来很
奇怪是有原因的：它的设计考虑了更多而不是格式。 Forth 在设计上很奇怪，而且这种
设计与宏有关。

如今，forth 常见于所谓的嵌入式平台——资源严重受限的计算机。 可以在几乎所有曾经
创建的可编程计算机系统上完全实现该语言，这证明了该语言的设计。 Forth 被设计成
尽可能容易实现和试验。 事实上，创建 forth 的克隆是如此的微不足道，以至于发明
一两种基于堆栈的 forth 语言对于对编程语言设计感兴趣的程序员来说几乎是一种仪式。
PostScript 和 Joy 这些基于堆栈的语言，它们可以追溯到 forth 并做出了有趣的贡献。

重要的 forth 实现决策通常基于正在实现 forth 的计算机的确切资源。 Forth 程序员
设计了一组抽象寄存器，这些寄存器要么映射到真实寄存器，要么映射到内存位置，或者可能
以完全不同的方式实现。 但是，如果我们在 lisp 上实现 forth，一个潜力无限且限制很少
的环境，该怎么做呢？ 不是简单地将 forth 抽象寄存器的任意映射强加到 lisp 代码中，
而是尝试退后一步。 如果 Chuck 有一个 lisp 机器会是什么样子？ 与其适应任意机器的
功能，无论是真实的还是虚拟的，我们探索了一组最小的抽象寄存器，在 lisp 上实现时针
对简单性和功能进行了优化。

但实际上，寻找一组最优的抽象概念是 Chuck 在创建 forth 时所做的，应用了他在很多
架构上的数十种不同的 forth 实现经验。 这就是为什么 forth 如此伟大。 与 lisp
一样，forth 代表了语言设计空间中的高局部最大值，并且与 lisp 一样，forth 与其
说是门编程语言或标准，不如说是种构建材料和智慧的集合，关于什么有效，什么无效。

.. code-block:: lisp

    (defvar forth-registers
      '(pstack rstack pc
        dict compiling dtable))

**forth-registers** 表示 forth 机器的抽象寄存器的符号列表。 当然 lisp 不考虑
寄存器和固定数字，而是变量和符号。 在这里开始我们开发 forth 环境看起来可能很奇怪，
只有一个变量名列表，但实际上这始终是实现 forth 系统的第一步。 创造 forth 是个
巧妙的引导过程，在美丽和聪明方面只有 lisp 才能超越。 以下是对该过程的适度描述。

forth 的特征之一是它直接访问程序使用的堆栈数据结构，既可以将参数传递给子例程，
也可以跟踪这些子例程中的执行路径。 Forth 特别有趣，因为与大多数编程语言不同，
它将堆栈数据结构的这两种用途分为两个堆栈，这两个堆栈你设置可以玩弄它。 在典型的
C 实现中，函数调用的参数及其所谓的返回地址存储在单个、可变大小的堆栈帧中，用于
每个函数调用。 forth，它们是两个不同的堆栈，称为参数堆栈和返回堆栈，分别表示为
我们的抽象寄存器 **pstack** 和 **rstack**。 我们使用 Common Lisp 的 push 和
pop 宏，这意味着这些堆栈是用 cons 单元链表实现的，而不是大多数使用的数组数据结构。

抽象寄存器 pc 是程序计数器的缩写，一个指向当前正在执行的代码的指针。 我们将很快
解释什么是代码以及如何指向它，以及我们的抽象寄存器编译和 dtable。

.. code-block:: lisp

    (defstruct forth-word
      name prev immediate thread)

另一个构建 forth 模块是其字典概念。  forth 字典是 forth words 的单链表，类似
于lisp函数。 words 用 lisp 结构表示。 结构是基于槽的有效数据结构，通常用向量
实现。 word 槽是用于在字典中查找单词的符号。 请注意，forth 字典不是按字母顺序
存储的，而是按时间顺序存储的。 当添加新单词时，会将新的 words 追加到字典的末尾，
以便在遍历字典时首先检查最新定义的 words。 字典的最后一个元素总是存储在抽象寄存器
字典中。 为了遍历字典，从 **dict** 开始并跟随单词结构的 prev 指针，prev 指针指向
先前定义的 word，如果是最后一个 word 的话，prev 指针指向 nil。

给定 **w**，一个要查找的单词，最后是要检索的字典，根据单词 **w** 是否在字典中找到，
**forth-lookup** 将返回个 forth 单词结构或 nil。 使用比较函数 **eql** 代替 **eq**
是因为 —— 与 lisp 不同 —— forth 允许用数字和其他非符号命名单词。

.. code-block:: lisp

    (defun forth-lookup (w last)
      (if last
        (if (eql (forth-word-name last) w)
          last
          (forth -lookup
            w (forth-word-prev last)))))

forth 词的 immediate 槽是个标志，指示该单词是否是立即的。即时性是 forth 元编程
概念，我们将很快深入探讨。现在这里是一个与其 lisp 对应物的粗略类比：立即字就像 lisp
宏，因为它们是在编译时而不是运行时执行的 forth 函数。什么？只有 lisp 应该有宏。
虽然 Common Lisp 宏系统确实比任何其他宏系统（包括最佳的 forth）强大得多，但 forth
的扩展能力几乎超过了所有其他语言。与 lisp 一样，这种能力是一种设计理念的结果：如果
它对语言实现者来说足够好，那么对于应用程序程序员来说也足够好。像 lisp 一样，forth
并没有真正认识到原始的概念。相反，它提供了一组元基元，可以将它们组合起来以构建你（程序员）
想要的语言。与 lisp 一样，与大多数 Blub 语言不同，通过使用宏以新颖的方式扩展语言不仅
是可能的，而且是鼓励的。像 lisp 一样，forth 与格式无关，而与强大有关。


.. _8-2-cons-threaded-code:

8.2 cons 线程代码
=========================

在上一节中，我们专注于抽象寄存器。 这些寄存器是个重点，这就是为什么 forth 哲学认为
它们如此基础，但这些寄存器实际上只是个更普遍的概念的组成部分：抽象机器。 不同的
forth 系统最显着的特性可能是它们对线程代码的实现。 线程代码的含义与抢占式调度共享
内存进程的传统含义非常不同。 Forth 线程与并发无关。 这是讨论代码编译和元编程的框架。

虽然 lisp 提供了对符号的树数据结构的访问权限，但在汇编到内存之前，程序是从这些符号
编译而来的，而 forth 不提供符号操作。 相反，forth 提供了将代码组装到内存中的过程
的访问权限。 虽然对于外人来说，最明显的特征是它的堆栈和后缀符号，但实际上是线程决定
了它的本质。 Forth 是关于堆栈的，就像 lisp 关于列表一样。 它们恰好是用于解决元编程
问题的最适用的数据结构——forth 和 lisp 的真正意义所在。

经典的线程风格被称为间接线程代码，但大多数现代 forth 都是用直接线程代码实现的。不同
之处在于间接级别。这种间接的低级效率影响取决于底层处理器，这里不会详细介绍。有很多关于
forth 线程[STARTING-FORTH][MOVING-FORTH] 的好教程。在内存中，这些线程样式都由
相邻的单元组成，这些单元是代表指针的固定机器字。一小段称为内部解释器的紧凑机器代码通常
是为正在使用的处理器量身定制的，因为它的重要工作是：跟随这些forth线程的指针，并在执行
过程中解释它们的含义。遇到单元时的默认行为是将当前程序计数器位置推入返回堆栈，然后将
程序计数器指向单元中包含的任何内容。当内部解释器到达线程的末尾时，会弹出返回堆栈并在
这个位置恢复执行——它停止的地方。

可以想象，这种类型的程序存储使得程序非常小。 编译后的 forth 单词只是一个连续的固定数组，
其中大部分表示指向其他单词的指针。 这一直是forth的优势之一。 由于程序线程进入内存的
透明性，forth 允许对编程权衡进行精细控制，包括最重要的权衡之一：执行速度与程序大小。
线程代码让我们尽可能优化抽象问题，从而产生极快的小程序。 但正如 lisp 宏不仅仅是效率一样，
forth 线程也是如此。 与 lisp 程序员一样，forth 程序员倾向于将自己视为实现者，而不仅仅
是用户。 Forth 和 lisp 都是关于控制的——制定自己的规则。

forth 程技术至少有两种其他常见类型：令牌（token）线程代码和子程序线程代码。在权衡速度
与大小时，这些代表了相反的方向。有时这些线程技术与间接和直接线程代码同时存在。令牌线程涉及
通过使用比指针更小的固定数字来表示线程中的单词，从而添加另一层间接性。另一端是子程序线程。
这种类型的线程代码正在变得流行，最好的现代 forth 编译器部分使用子程序线程。代替指向内部
解释器要遵循的字的连续指针，子程序线程代码存储内联机器指令以调用这些指针。在子程序线程代码中，
内部解释器消失了——它实际上是由硬件（或虚拟机）实现的。子程序线程代码通常被认为是个不透明的块，
只有特殊的、不可编程的编译器才能操作。特别是当对代码进行各种优化时，这些不透明的块开始看起来
不像统一的、基于单元的线程。几乎所有非 forth 编译器都只编译为子程序线程代码，并且不要想象你
会想要做任何其他事情，导致这个特殊的定义：

..

  A Flub is a language that only considers subroutine threaded code or a
  language implementation that only provides subroutine threaded code.

例如，C 是个 Flub，因为它只为程序员提供创建函数的方法——子程序线程代码的不透明块。 当然，
我们可以用 C 实现一个内部解释器来处理间接线程代码并使用这个程序引导一种基于堆栈的语言，
但是我们不再用 C 编程。几乎所有的 Blub 语言都是 Flubs。 如刚刚所描述的，作为抽象机器的
forth 不是 Flub。 正如我们将看到的，forth 为程序员/实现者提供了对其程序如何编译的大量
控制权。

lisp 是 Flub 吗？ 有趣的是，lisp 可能是第一个非 Flub 编程语言，但大部分都变成了 Flub。
尽管标准没有严格要求，但大多数 Common Lisp 编译器仅将函数编译为不透明的机器代码块，因此是
Flubs。 但是在很早期的 lisp 版本中，函数被存储为列表——这是种奇怪的代码线程，与 forth
线程并不完全不同。 虽然这确实允许一些非常聪明的运行时技巧，包括赋予循环代码意义，但它的效率低
得令人绝望。 与 forth 的许多类型的线程不同——几乎在所有架构上都有效地实现了——lisp 函数的
这种内部表示是不能容忍的，并且lisp 被更改为允许（非常）高效的代码。 结果，对于元程序员来说，
Common Lisp 的大多数实现都是 Flubs。

但是，无法添加到语言中的功能与我们可以通过宏添加的功能之间存在差异。使用宏，我们可以随意扩展
语言，并且它仍然是 lisp。 Common Lisp 缺少线程代码，就像它缺少延续和一流的宏一样：它们被
故意从语言中省略，并留给宏编写者根据需要实现。本章及其代码最重要的结果之一是代表，即使它们是
Flub，lisp 语言也可以通过宏转换为非 Flub 语言。 Non-Blub 意味着非 Flub，或者换句话说，
如果你不能将一种语言变成非 Flub，那么它必须是 Blub。然而，反过来却不一定成立。像这样的非
Flub 语言仍然是 Blubs，将它们变成目前已知的非 Blubs 的最直接方法是用它们实现 lisp 环境
—— 然后你就在编程 lisp。

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

我们的 forth 不是使用连续的内存单元来表示间接或直接线程代码的线程，而是利用 lisp 的动态类型
和 cons 单元列表结构。 我们称之为 cons 线程代码。 宏 **forth-inner-interpreter** 展开为
能够遵循这些 cons 单元链表线程的代码。 在这里开始为我们的 forth 环境编写逻辑可能看起来很奇
怪 —— 使用一个旨在展开为一些迄今为止未知的表达式的宏 —— 但这实际上是种理想的 lisp 编程模式。
因为宏让我们可以在任何我们想要的地方开始编程，为什么不从程序中真正有趣的驱动位开始呢？ 这些
是对程序最终设计影响最大的部分。

**forth-inner-interpreter** 的定义本身就是对我们所说的 cons 线程代码的简明定义。 每个
cons 单元的 car 指向一个函数、另一个 cons 单元或其他一些 lisp 原子（atom）。 函数在遇到
时执行。 请注意，函数本身会更新 pc 寄存器。 如果在线程中发现另一个 cons 单元格，则假定它
指示子例程调用——单词调用。 我们的内部解释器会将 pc 恢复位置推送到返回堆栈，然后跳转到这个
新线程。 如果遇到其他的 lisp atom，它会被简单地推入参数堆栈，并在线程的下一个单元格处继续
执行。 一旦内部解释器到达其线程的末尾并且在其返回堆栈上没有其他线程可以返回，它将返回。

.. code-block:: lisp

    ;; Prim-form: (name immediate . forms)
    (defvar forth-prim-forms nil)

    (defmacro def-forth-naked-prim (&rest code)
      `(push ',code forth-prim-forms))

    (defmacro def-forth-prim (&rest code)
      `(def-forth-naked-prim
        ,@code
        (setf pc (cdr pc))))

当然函数不能更新 **pc** 变量，除非它们被定义在它的词法范围内，所以我们使用了另一种宏技术：
创建一个类似的接口，而不是使用 **defun**，它做一些完全不同的事情。 **def-forth-naked-prim**
感觉类似于创建 **defun** 定义的函数，只是展开的代码将用户提供的结构推送到存储在 forth 基本结构
中的列表中。 最终的宏将使用这些结构在其词法范围内定义 forth 原语。 因为这些结构总是会展开到
这个环境中，所以可以自由地编写代码来调用所有的 forth 抽象寄存器，如 **pc** 、 **pstack** 等。

.. code-block:: lisp

    (def-forth-prim nop nil)

    (def-forth-prim * nil
      (push (* (pop pstack) (pop pstack))
            pstack ))

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

使用 **def-forth-naked-prim** 定义的元语不会将 pc 变量更新到线程中的下一个 cons 单元。
对于大多数原语，应该使用 **def-forth-prim** 以便执行通常的更新。 这两个宏都期望第一个参数是
用于引用原语的符号，第二个参数是指示原语是否立即的布尔值。 其余参数是在执行原语时要执行的
lisp 结构。

八个简单的原语——没有一个是赤裸裸的或直接的——目前所呈现。 **nop** 是个什么都不做的伪指令（“无操
作”）。 ***** 原语是乘法运算符：它从参数堆栈中弹出顶部的两个值，将它们相乘，然后将结果压入栈。
**dup** 是“duplicate”的缩写，它将参数堆栈上的顶部值再次推送到参数堆栈上，留下两个重复值。
**swap** 将使用一个非常有用的 Common Lisp 宏来交换顶部的两个参数堆栈元素：**rotatef**。 并非巧
合 forth 也具有（基于堆栈的）旋转机制。 **print** 弹出参数堆栈并打印它。 **>r** 将一个值从参数
堆栈传输到返回堆栈，**r>** 则相反。

名称 * 是否违反了第 3.5 节中的重要变量捕获规则，禁止我们重新绑定 Common Lisp 定义的函数呢？
不，因为实际上并没有使用这个符号来绑定任何函数——它只是 forth 基本格式中的一个列表中的第一个
元素。 我们没有做错任何事。 符号独立于它们有时用来表示的函数或宏。 可以在任何地方使用任何符号，
只要不违反重要的变量捕获规则。 这仅在编写 lisp 时起作用； 我们正在编写 forth。


.. _8-3-duality-of-syntax-defined:

8.3 语法二元性
==================================

如果忘了本书中的其他内容，那么请记住本节的信息。 在这里，我们最终定义并解释了一个我们一直触及
的概念：语法的二元性。 本节假定你至少阅读了三个介绍性章节、:ref:`chapter06` 和前面的 forth 章
节。


对于大多数 lisp 程序员来说，lisp 编程比 Blub 编程更有效率，最终看来也更自然，这一点在经验上
是显而易见的，但要回答为什么会出现这种情况很难。 虽然 lisp 确实从宏中获得了惊人的表达能力——
而我们在本书和其他地方看到了许多有趣的东西——到目前为止，所有的解释都无法令人满意。 宏的真正优势
是什么？ 部分解释当然包括简洁，使程序简短。 以下是宏的定义：

..

  Let L be a programming language, F a feature in that programming language,
  and A an arbitrary program in L. F provides a brevity feature if A is shorter
  than it would be in a version of L without F.

简洁特性为简洁理论提供了基础和合理性：

..

  The effort required to construct a program is inversely proportional to the
  amount of brevity features available in the programming language used.

简洁理论基于这样一种思想，即如果编程抽象使程序的表达非常简短，那么编写它们就会变得更容易，因为需
要编写的代码更少。我们的 CL-PPCRE 读取宏是简洁功能的示例：将很长的 CL-PPCRE 函数名称缩短为简
洁的 Perl 格式表达式，每次使用它们时都可以节省敲打键盘的次数。简洁理论非常适用于编写小程序，当
我们开始时，我们就知道要去哪里。不幸的是，大多数程序都不是这样的。大多数程序——至少是有趣的程序
——是通过一系列交互式编写测试周期迭代创建的，这些周期考虑了沿途每一步的反馈。抽象可能很简短，但
如果总是不得不将它们更改为不同（也许同样简短）的抽象，可能不会节省太多精力。与其考虑最终程序的长
度，也许应该考虑到达那里所需的过程长度。

在所有语言中，程序最终看起来都与它们开始时的方式不同。 大多数程序都是从一个简单的草图开始的，随
着作者对问题的了解越来越多，这个草图会被填写并详细说明。 在我们回到简洁和二元性之前，本章将引导
我们开发一个简单的程序来激发讨论：forth 环境。

嗯，我们讲到哪里了？ 啊，是的，我们已经讨论了很多关于抽象寄存器、抽象机器和线程代码的内容，以及
定义了一个名为 **forward-lookup** 的单词查找实用程序、一个用于我们的 cons 线程代码的内部解
释器，以及一个用于收集表示原语的列表的系统在我们的 forth 系统中。 但是在 lisp 上会发生什么
呢？那么，对于任何混合了行为和状态的抽象来说，最自然的结构是什么？ 当然是闭包了——我们的老朋友
**let** 和 **lambda**。有了这个想法可能会给出以下宏：

.. code-block:: lisp

    (defmacro new-forth ()
      `(let ,forth-registers
        (forth-install-prims)
        (lambda (v)
          (let ((word (forth-lookup v dict)))
            (if word
              (forth-handle-found)
              (forth-handle-not-found))))))

forth 抽象寄存器列表，**forth-registers**，直接拼接到展开中，最初将所有抽象寄存器绑定到
nil。注意，这个宏的功能上留下了很多漏洞。 我们发现不得不定义一个接受原始表单的宏
**forward-install-prims**，以及宏 **forward-handle-found** 和
**forward-handle-not-found**。 但从这张草图中学到的最重要的一点是，没错，就是这种封闭设计
看起来可行。 这个想法是通过遵循默认的 lisp 设计而产生的，它需要成为一个闭包，对于我们想要给它
的每个单词都调用一次。 我们的草图概述了以下用例的实现。 在这里，我们假定创建一个新的forth环
境：

.. code-block:: lisp

    (defvar my-forth (new-forth))

以下是 forth 中对 3 求平方后打印其结果的代码：

.. code-block:: none

    3 dup * print

我们可以在 forth 环境中执行它，如下所示：

.. code-block:: lisp

    (progn
      (funcall my-forth 3)
      (funcall my-forth 'dup)
      (funcall my-forth '*)
      (funcall my-forth 'print))

.. code-block:: lisp

    (defmacro! go-forth (o!forth &rest words)
      `(dolist (w ',words)
        (funcall ,g!forth w)))

虽然这是个笨拙的接口，但我们是在写 lisp 程序，所以我们知道总是可以创建一个宏来隐藏这些细节，而
这正是 **go-forth** 宏所做的。 注意，**go-forth** 使用了 **defmacro!** 的自动
**once-only** 功能，因为 **go-forth** 的第一个参数是在用 **dolist** 定义的循环内，并且可
能不会像宏的用户预期的那样被精确地计算一次。 有了 **go-forth**，将代码输入到 forth 环境变得
更加干净：

.. code-block:: lisp

    (go-forth my-forth
      3 dup * print)

在这一点上，我们可能会想到，在创建新的forth环境时，我们最终会想要执行一些forth引导代码。 所以
需要能够在创建闭包时调用它。 这可能需要更改程序的 let over lambda 设计，或者可能在
**new-forth** 宏周围创建某种包装函数，该函数使用 **new-forth** 宏，加载到标准库中，然后返
回结果。

.. code-block:: lisp

    (defvar forth-stdlib nil)
    (defmacro forth-stdlib-add (&rest all)
      `(setf forth-stdlib
            (nconc forth-stdlib
                    ',all)))

由于 forth 代码只是符号和其他原子的列表，我们的标准库提供了需要的所有引导（除了一些更多的原
语）可以存储在一个列表中。 变量 **forth-stdlib** 保存了这个 forth 代码列表，当新的 forths
被创建并且 **forth-stdlib-add** 宏展开为 lisp 代码时，它将把新的 forth 代码追加到
**forth-stdlib** 列表中。

适配 **new-forth** 以支持加载此标准库的最简单方法是什么？ 还记得在 :ref:`6-3-alet-and-finite-state-machines`
中写的 **alet** 宏吗？ 这个宏的目的是使用 Common Lisp 的 let 创建对偶语法，同时将回指变量
**this** 绑定在提供的代码周围。这将指向从 alet 返回的结果—— forth 闭包。

所以改变我们的草图比预期的更容易。 所要做的就是将草图中的第一个 **let** 关键词改为
**alet**，然后添加一些代码以将标准环境加载到 forth 闭包中。 不必调整其他的代码，因为
**alet** 的语法是故意与 **let** 保持一致的。 下面是下一次迭代的样子：

.. code-block:: lisp

    (defmacro new-forth ()
      `(alet ,forth-registers
        (forth-install-prims)
        (dolist (v forth-stdlib)
          (funcall this v))
        (lambda (v)
          (let ((word (forth-lookup v dict)))
            (if word
              (forth-handle-found)
              (forth-handle-not-found))))))

记住，**alet** 使用闭包引入了一个间接层，因此让 forth 环境效率稍低。 然而，正如我们不知道这
种效率负担是否会太大，我们也不知道最终我们会不会需要这种间接性。 要消除间接性，就使用
**alet** 之前定义的 **alet%** 宏。

也许现在，或者以后当我们尝试构建和调试 forth 环境时，我们可能会想到能够从 forth 环境之外访问
forth 抽象寄存器也是有用的。不幸的是，这些变量被一个 let over lambda 封闭。我们将不得不再次
更改程序以使其可访问。当然，有很多方法可以做到这一点。可以在 forth 环境中嵌入并返回多个闭包，
其中一些可以保存和访问抽象寄存器，或者可以重新考虑完全放弃 lambda 策略。但在这样做之前，是否有
任何二元性可以帮助我们？还记得 :ref:`6-7-pandoric-macros` 中的 plambda 吗？它的目的是使用 lambda 创建一种
二元语法，但它创建的闭包实际上对外部环境开放。更改我们的草图以支持这一点很简单，只需在我们作为闭
包返回的 lambda 添加个前缀字符 p 并添加要导出的变量列表。我们的列表可以方便地在 forth 寄存器
中提供给我们。草图变成：

.. code-block:: lisp

    (defmacro new-forth ()
      `(alet ,forth-registers
        (forth-install-prims)
        (dolist (v forth-stdlib)
          (funcall this v))
        (plambda (v) ,forth-registers
            (let ((word (forth-lookup v dict)))
              (if word
                (forth-handle-found)
                (forth-handle-not-found))))))

随着 forth 闭包的打开，我们可以使用以下用例。 这会将五个项推入个 forth 堆栈中：

.. code-block:: lisp

    * (go-forth my-forth
        1 2.0 "three" 'four '(f i v e))

    NIL

然后我们可以随意打开 **my-forth** 来检查其参数堆栈：

.. code-block:: lisp

    * (with-pandoric (pstack) my-forth
        pstack)
    ((F I V E) FOUR "three" 2.0 1)

这是为达到我们的新版本宏的最终版本而执行的过程。 最终定义与上一个草图相同，只是它还将 dtable
抽象寄存器设置为指向哈希表（很快就会解释）。

.. code-block:: lisp

    (defmacro new-forth ()
      `(alet ,forth-registers
        (setq dtable (make-hash-table))
        (forth -install -prims)
        (dolist (v forth -stdlib)
          (funcall this v))
        (plambda (v) ,forth -registers
          (let ((word (forth-lookup v dict)))
            (if word
              (forth-handle-found)
              (forth-handle-not-found))))))

编程，至少是有趣的编程，不是写程序，而是改变它们。 就生产力而言，简洁仅能带我们到此为止。 我们
可以将 lambda 重命名为 fn，但是这个简洁的特性并没有节省太多，只是少打几个字符。 然而，真正省
力的是有许多类似于 lambda 的抽象，我们可以使用它们来更改代码的含义，而无需过多地修改代码本
身。 语法的二元性为我们节省了精力。

就像给你的特殊变量名加上星号一样，如果你改变了关于变量应该是特殊的还是词法的想法，就会强迫你添加
或删除星号，不必要地分离语法和避免对偶可能会在编程过程中导致很多毫无意义的工作。另一个例子：
**#'** 你的 lambda 结构是个坏主意，因为这意味着当你决定一个函数真的需要 alambda 或者当你决
定在列表的函数位置使用 lambda 结构时，需要做很多的修改。广义变量还提供了一个非常重要的对偶性：
在编写宏时，可以将相同的结构拼接成展开，用于访问和修改变量。 Common Lisp 对空列表和 false 布
尔值的双重含义是又一个例子——除了语法的双重性之外，没有真正的理由这两者应该相同。对偶性也是本书
提倡闭包而不是其他 CLOS 特性（如 **defclass** 和 **defmethod**）的原因。与修改使用类和对
象的程序相比，修改使用闭包的程序时阻力通常更小，因为我们有很多很好的闭包语法二元性，而且构建闭包
的编程宏更加统一。考虑到这些和其他例子，终于可以对语法二元性的含义给出一个清晰的定义：

..

  Let L be a programming language, F a feature in that programming language,
  and A and B arbitrary programs in L. F provides a duality of syntax feature
  if the modifications required to change A into B become fewer than in a
  version of L without F.

这就有了二元性理论：

..

  The effort required to construct a program is inversely proportional to the
  amount of dual syntax available in the programming language used.

虽然语法二元性的概念及其好处的影响都非常清楚，但如何实际设计好的二元性却远没有那么清楚。 某种语
言中最有用的二元性是什么？ 我们如何判断两种不同语言中的哪一种会为某些给定问题提供更好的语法二元
性？

因为使用 lisp，我们完全控制了编程语言，所以我们可以根据需要使用或多或少的双重语法来设计我们的语
言。在我看来，遵循这种思路是当今编程语言研究最富有成果的领域。 使用 lisp 宏，我们可以使所有不
同的程序彼此相似到何种程度，从而使将它们更改为新程序变得容易得多？

在简洁性和对偶性的定义中，特征 F 是否有效取决于正在编写或更改的程序。 有时，提供简洁性或二元性
的功能在某些情况下实际上会增加所需的工作量。 最好的方法可能是提供尽可能多的有用的简洁性和二元性
功能，同时删除那些最终会带来更多麻烦的功能。


.. _8-4-going-forth:

8.4 开始构建 forth
=========================

在本节中，我们将通过填补上一节中 **new-forth** 宏中留下的漏洞来真正开始工作。 在验证了
forth 线程机制有效之后，我们引导了一个 forth 编程环境，并在此过程中解释了forth 即时性是什么
以及它与 lisp 宏的关系。

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

在 **new-forth** 的定义中，我们在宏中留下了个漏洞，将由 **forth-install-prim** 来填补。
我们想使用命名抽象而不丢掉词法环境，所以它必须是一个宏。 该宏的目的是在创建新的forth实例时编译
原语并将其安装到 forth 字典中。 **forth-install-prims** 展开为 **progn** 格式，每个子结
构都是将原始单词附加到 dict 链表上的指令，将提供的代码包装在 lambda 中，并设置单词的名称和立
即槽。 此外，由 lambda 为每个单词创建的函数，称为 thread，被添加到我们的 dtable 哈希表中
（很快就会解释）。 因为所有这些函数都将在最初的 **new-forth** 宏的范围内创建，所以它们可以完
全访问由我们的抽象寄存器指定的 forth 环境。 注意，线程绑定不会从任何用户提供的代码中捕获线程，
因此不需要使用 **gensym** 来命名它。

我们已经说过，forth 提供了一个与 lisp 不完全不同的元编程系统，并且该系统基于一个称为即时性的
概念。在传统的 forth 中，有一个称为 **state** 的变量，它要么为零，要么非零。 如果它为零，则认为
forth 处于常规解释（执行）状态。 如果在这种状态下给定一个单词，该单词将被查找并执行。 但是，如
果变量  **state** 不为零，则称 forth 变量处于编译状态。 如果我们在这种状态下表达一个单词，被表达的单词的地址将附
加到正在编译的当前线程——通常是字典中最近创建过的单词。 然而，有一个例外，这是关于即时性的重要一
点。 如果我们处于编译状态并且我们得到一个立即的单词，则该单词将被执行而不是编译。 因此，与 lisp 一样，forth
允许我们在编译时执行任意的 forth 代码。

.. note::
    译注：原文为 **base** ，但是通过查找 forth 语言的语法说明，推测作者应是笔误。参见： http://lars.nocrew.org/forth2012/core/STATE.html, https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/How-does-that-work_003f.html#How-does-that-work_003f

.. code-block:: lisp

    (def-forth-prim [ t ; <- t means immediate
      (setf compiling nil))
    (def-forth-prim ] nil ; <- not immediate
      (setf compiling t))

因为我们在 lisp 上构建了 forth 抽象机，所以我们忍受 fixnum 值到真假（布尔值）的任意映射。
在 lisp 中，有一个动态类型系统，可以享受所有值到真假的任意映射。代替 forth 变量状态，我们的
forth系统使用编译抽象寄存器将编译状态存储为 lisp 通用布尔值。用于控制编译状态的传统forth词
是 **[** 和 **]**，即开闭方括号。**[** 为退出编译模式，因此必须是一个立即字。 **]** 回到编
译模式，因此仅在处于解释模式时才执行，并且不必立即执行。这种符号的选择现在可能看起来很奇怪，但在
高级 forth 代码中会变得更加清晰。这些方括号可以指定要在编译 forth 线程的过程中执行的代码块。
在某种意义上，这些括号就像 lisp 的反引号和反引号操作符。以下是这些词通常在 forth 代码中的使用
方式：

.. code-block:: none

    ... compiled words ...
    [ interpret these words ]
    ... more compiled words ...

与大部分的 forth 一样，这些词是透明地指定的，这允许我们以特别的方式使用它们。 例如，这些词的平
衡与lisp 括号不同。 如果我们想要的话，可以在相反的方向使用它们：

.. code-block:: none

    ... interpret these words ...
    ] compile these words [
    ... more interpreted words ...

我们甚至有嵌套的界面了，但这并不是真正的嵌套，因为只有一个布尔状态：正在编译或未编译。

.. code-block:: none

    ... compiled words ...
    [ interpret these words
      ] compile these words [
      interpret these words
    ]
    ... more compiled words ...

.. code-block:: lisp

    (defmacro forth-compile-in (v)
      `(setf (forth-word-thread dict)
            (nconc (forth-word-thread dict)
                    (list ,v))))

我们的 forth 使用 **forth-compile-in** 宏作为缩写宏。 这个宏将 forth 单词编译到当前的线
程中，即创建的最后一个单词的线程。 因为我们的线程由 cons 单元表示，所以可以使用 lisp 函数
**nconc** 简单地将指向目标单词线程的指针追加到当前的线程上。

.. code-block:: lisp

    (defmacro forth-handle-found ()
      `(if (and compiling
                (not (forth-word-immediate word)))
        (forth-compile-in (forth-word-thread word))
        (progn
            (setf pc (list (forth-word-thread word)))
            (forth-inner-interpreter))))

**new-forth** 宏中留下的另一个漏洞是，如果它能够在字典中查找提供的单词，它应该做什么。 这个漏
洞由 **forth-handle-found** 修复。 该宏实现了上述的即时性。 如果正在编译并且查找的单词不是
立即的，我们将它编译到当前的线程中。 否则，将程序计数器 pc 设置为指向查找单词的线程并运行内部解
释器来执行该单词。回想一下，这个宏将被展开成一个词法环境，其中词绑定到查找的 forth 词上。

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

**new-forth** 中的最后一个漏洞是如果在其字典中没有找到单词时，它应该做什么。
**forth-handle-not-found** 修复了这个漏洞并实现了一些特殊情况。回想一下，
**forth-handle-not-found** 将展开为包含绑定 **v** 的词法环境，该绑定 **v** 引用传递给
forth 的值。我们还知道，如果调用此代码，**v** 将不会引用字典中的任何单词。如果 **v** 是一个
符号，则 **forth-handle-not-found** 将抛出异常。如果该值不是符号，则操作是将 **v** 压入参
数堆栈，或者，如果正在编译，则将其编译到当前线程中。但是，检查了两种特殊情况。如果 **v** 是带
有第一个元素引号的列表，我们将引用的值压入参数堆栈。这样我们就可以将符号推送到参数堆栈上，而不会
将它们解释为单词。第二种特殊情况是如果 **v** 是第一个元素 postpone 的列表。postpone 是个
ANSI Forth 词，它结合并澄清了几个传统的 forth 词。postpone 用于始终编译一个单词，即使该单
词是立即的。因此，如果我们处于编译模式，一个 postpone 的立即字将被编译到当前的线程中，即使它是
立即的。下面是一个postpone  **[** 字的例子：

.. code-block:: none

    ... compiling ...
    (postpone [)
    ... still compiling ...

在 **new-forth** 宏中填补了所有漏洞之后，现在可以使用 **new-forth** 宏创建新的 forth 实
例。之前我们用 **defvar** 创建了一个名为 **my-forth** 的特殊变量。 即使我们没有，我们也可
以隐含地声明它是特殊的，同时使用 top-level 的  **setq** 为它分配一个值：

.. code-block:: none

    * (setq my-forth (new-forth))
    #<Interpreted Function>

现在可以用 **go-forth** 宏来调用 forth 了：

.. code-block:: lisp

    * (go-forth my-forth
        2 3 * print)
    6
    NIL

但到目前为止，我们只定义了单词 **dup**、***** 和 **print** 。 为了做些有用的事情，我们需要
更多的原语。 与 lisp 一样，生产质量的 forth 实现具有为方便程序员而定义的大量单词。 经过几十年
的使用，许多常见的编程模式已经被识别出来，抽象成单词，然后被引入到常见的方言中。 像 lisp 一
样，能够扩展定义为语言一部分的语言已经导致了许多有价值的实验。 因为我们正在研究的正是这种理念和
过程，所以我们不会定义很多经验丰富的 Forth 程序员所依赖的词语。 相反，我们的目标是解释 forth
的元编程系统所需的最小原语集，以便可以将其与 lisp 宏进行比较。

.. code-block:: lisp

    (def-forth-prim create nil
      (setf dict (make-forth-word :prev dict)))
    (def-forth-prim name nil
      (setf (forth-word-name dict) (pop pstack)))
    (def-forth-prim immediate nil
      (setf (forth-word-immediate dict) t))

这里定义了另外三个原语，它们都不是直接的或裸露的：**create**、**name** 和 **immediate**。
**create** 原语将一个无名词附加到字典中。**name** 从参数堆栈中弹出一个值，并将字典中最后一个
单词的名称设置为该值。 **immediate** 简单地将定义的最后一个单词设置为立即单词。 默认情况下，
单词不是立即的。

回想一下，我们可以在 **my-forth** 环境中使用 **go-forth** 宏执行代码。 下面，我们将数字
3 平方并打印结果：

.. code-block:: lisp

    * (go-forth my-forth
        3 dup * print)
    9

是否已经有足够的 forth 来开始用 forth 词本身来引导？ 虽然还没有真正定义单词，但由于线程代码的
透明规范，可以开始使用 forth 编写 forth 单词。 例如，下面我们将使用 **create** 将一个新的
空词追加到字典中：

.. code-block:: lisp

    * (go-forth my-forth
        create)
    NIL

现在使用 **]** 开始编译，在线程中添加单词 **dup** 和 *****，然后使用 **[** 退出编译模式：

.. code-block:: lisp

    * (go-forth my-forth
        ] dup * [)
    NIL

现在我们的字典中有一个新词——一个具有完整的 forth 线程的词，当我们的内部解释器执行该线程时，它
将对堆栈顶部的数字进行平方。 但是这个词不是很有用，除非有办法访问它。 可以给这个词一个名字。 给
定的名字将是用来访问新线程的值：

.. code-block:: lisp

    * (go-forth my-forth
        'square name)
    NIL

注意传给 forth 的第一个值是如何被引用的。 回想一下，我们决定这种行为应该导致将符号
**square** 推入参数堆栈。然后这个符号被单词名称所消耗。 现在我们的单词被命名了，可以像使用任
何其他单词一样使用符号 **square** 来计算它：

.. code-block:: lisp

    * (go-forth my-forth
        3 square print)
    9 NIL

所以创建新词的通用技术是以下格式：

.. code-block:: none

    create
    ] ... compiled words ... [
    'whatever name

.. code-block:: lisp

    (forth-stdlib-add
      create
        ] create ] [
      '{ name)

但是我们可以使用一些 forth 元编程来改进这个接口。 新的 forth 词 **{** 的定义被添加到标准库
中。 它的线程由两个指针组成，第一个指向单词 **create** ，第二个指向单词 **]** 。 所以当这个
词的线程被执行时，它会在字典中追加一个新词，并让我们进入编译模式。 Forth 通常为此使用 **:**
词，但这与 lisp 中 **:** 的使用冲突，因此我们选择使用 **{** 来开始词定义。

.. code-block:: lisp

    (forth-stdlib-add
      { (postpone [) [
      '} name immediate)

类似地，我们在标准库中添加了一个补充词 **}** （替换了传统 forth 的 **:** ）。 实际上没有理由
定义这个词——它唯一的作用就是让我们脱离编译状态。 我们已经有了 **[** 来为我们做这件事。 尽管如
此，定义 **{** 还是有用的，因为它通过创建一对单词 { 和 } 为我们提供了正常的平衡括号，这使得定
义新单词变得直观。

我们现在可以创建个 forth 来利用这些新的标准库特性（丢弃我们之前对单词 **square** 的定义）：

.. code-block:: none

    * (setq my-forth (new-forth))
    #<Interpreted Function>

以下是使用定义词 **{** 和 **}** 的新词时的结构：

.. code-block:: lisp

    * (go-forth my-forth
        { dup * } 'square name)
    NIL
    * (go-forth my-forth
        5 square print)
    25

并且新线程可以像引用原语一样容易地引用自定义创建的单词。 以下是如何将单词 **quartic** 定义为
带有两个指向 **square** 单词指针的线程：

.. code-block:: lisp

    * (go-forth my-forth
        { square square } 'quartic name)
    NIL

**(Expt 1/2 4)** 的结果是 **1/16**:

.. code-block:: lisp

    * (go-forth my-forth
        1/2 quartic print)
    1/16
    NIL

因为非符号被直接编译到forth线程中，并且内部解释器将非函数视为数据项以在遇到时压入堆栈，我们可以
将数字包含在单词定义中：

.. code-block:: lisp

    * (go-forth my-forth
        { 3 } 'three name
        three three * print)
    9
    NIL

回想一下，我们使用 **eql** 函数查找传递给 forth 的所有元素，以查看它们之前是否在字典中被命名
过。 这样做的结果是可以使用任何 lisp 对象来命名一个单词。 在这里，我们使用数字：

.. code-block:: lisp

    * (go-forth my-forth
        { 4.0 } '4 name
        4 4 * print)
    16.0
    NIL

Forth 是学习如何使用指针作用域的优秀语言。 Forth 定义了两个简单的运算符，用于从内存中读取和写
入值：**@** (读取) 和 **!** （存储）。 因为我们的 forth 字存储在 cons 单元中而不是内存字
中，所以使用 fetch 取消引用指针是通过获取指针的 car 来实现的。 用 store 设置它是通过使用
**setf** 设置它的 car 来实现的。 Fetch 将从参数堆栈中弹出一个值，假设它是一个 cons 单元，
获取它的 car，然后将其压入堆栈。 Store 将从参数堆栈中弹出一个值，假设它是一个 cons 单元格，
从堆栈中弹出另一个值，并将其存储到第一个值的 car 中。 例如，以下是如何创建和打印循环列表：

.. code-block:: lisp

    * (let ((*print-circle* t))
        (go-forth my-forth
          '(nil) dup dup ! print))
    #1=(#1#)
    NIL

所以现在我们正在使用线程代码进行编程。 还是说我们真的这样吗？ 我们离开过 lisp 吗？ 两种语言之
间的区别是如此模糊，以至于几乎无法辨别。 本章的其余部分在进一步解释元编程时试图使这种区别更模
糊。


.. _8-5-going-forther:

8.5 forth 的拓展
=======================

Common Lisp 有很多我们希望能够包含在的 forth 线程中的函数。
**forth-unary-word-definer** 展开为与传递给其宏体的元素一样多的 **def-forth-prim** 结
构。 假定元素是表示函数或宏的符号，但它们也可以是lambda 结构。 由 lambda 形式命名的原语的唯
一限制是，要调用此类原语，需要将相同的 (eq) lambda 结构传递给 forth 环境。 下面传递一个符号
—— **not** 时的展开：

.. code-block:: none

    (defmacro forth-unary-word-definer (&rest words)
      `(progn
          ,@(mapcar
              #`(def-forth-prim ,a1 nil
                  (push (,a1 (pop pstack))
                        pstack ))
              words )))

.. code-block:: none

    * (macroexpand
        '(forth-unary-word-definer
          not))
    (PROGN
      (DEF-FORTH-PRIM NOT NIL
        (PUSH (NOT (POP PSTACK))
              PSTACK)))
    T

.. code-block:: none

    (defmacro! forth-binary-word-definer (&rest words)
      `(progn
          ,@(mapcar
              #`(def-forth-prim ,a1 nil
                  (let ((,g!top (pop pstack)))
                    (push (,a1 (pop pstack)
                              ,g!top)
                          pstack )))
              words)))

我们可以使用任何接受一个参数的 Common Lisp 函数，然后 **forth-unary-word-definer** 会将
其定义为 forth 原语，将该函数应用于 forth 参数堆栈的顶部元素。

.. code-block:: lisp

    (forth-unary-word-definer
      not car cdr cadr caddr cadddr oddp evenp)
    (forth-binary-word-definer
      eq equal + - / = < > <= >= max min and or)

这个插件的想法是 **forth-binary-word-definer** ，它做同样的事情，只不过是接受两个值的运算
符。 通过创建临时 **let** 绑定来保存参数堆栈的顶部元素，启用了将倒数第二个元素视为二进制函数
（如 **-** 和 **/** ）的第一个参数的 forth 约定。 以下是单词 **-** 的展开：

.. code-block:: none

    * (macroexpand
        '(forth-binary-word-definer
          -))
    (LET ()
      (PROGN
        (DEF-FORTH-PRIM - NIL
          (LET ((#:TOP1767 (POP PSTACK)))
      T

练习：当使用 **four-binary-word-definer** 时，怎么把宏当作 and 和 or 来对待，如果它们都
是同级别的值？

难一点的练习：为什么需要使用 **gensym** ( **g!top** ) 来避免在
**forth-binary-word-definer** 中捕获不需要的变量？ 提示：我们已经在本节中讨论过它。

所以这些宏让我们可以将各种 lisp 函数添加到 forth 原始环境中，以便在 forth 原始环境中使用它
们。 下面是个使用一个参数原语 **cadr** 的示例：

.. code-block:: none

    * (go-forth my-forth
        '(a (b) c) cadr print)
    (B) NIL

以及两个参数的 **<**:

.. code-block:: none

    * (go-forth my-forth
        2 3 < print)
    T
    NIL

到目前为止，我们的 forth 线程都是有向无环图，即它们由不会指向自身的 cons 单元结构组成（不是自
引用的），并且最终终止于我们的原语，即树的叶子。 例如，我们可以使用 **pandoric** 宏来获取我们
在上一节中定义 **quartic** 词时创建的线程：

.. code-block:: none

    * (with-pandoric (dict) my-forth
        (forth-word-thread
          (forth-lookup 'quartic dict)))
    ((#<Interpreted Function>   ;; square->|->dup
      #<Interpreted Function>)  ;;         |->*
    (#<Interpreted Function>   ;; square->|->dup
      #<Interpreted Function>)) ;;         |->*

上面代码中的注释只是从我们用 lisp 打印结构的角度来展示的。 从代码或注释中我们看不到的是，这个
线程结构实际上是共享的。 要确认这一点，需要使用 **eq**：

.. code-block:: lisp

    * (eq (car *) (cadr *))
    T

或者在 **\*print-circle\*** 中看起来是这样的：

.. code-block:: none

    * (let ((*print-circle* t))
        (print **)
    t)
    (#1=(#<Interpreted Function>  ;; square->|->dup
        #<Interpreted Function>) ;;         |->*
    #1#)                         ;; --------|
    T

线程代码可以带来惊人的内存和大小优势。 整个 forth 系统都是编译后的代码，这些代码像这样串连在一
起——从网络驱动程序到最高级别的用户程序。 更重要的是，请注意，可以干净地从 **quartic** 提取线
程，而无需使用大量无关的其他线程。 例如，我们的语言中有更多的原语，如 **+** 和 **cadddr**，
但它们根本没有出现在上面的线程中。这几乎就像有一个标记清除垃圾收集算法，它只提取执行给定单词所需
的线程。 在 lisp 中，这个过程称为 tree shaking，通常不是很有效。 然而，在 forth 中，效果惊
人。

不幸的是，从 **my-forth** 中随意提取的 **quartic** 线程对我们来说并没有那么有用。 它仍然永
久驻留在 **my-forth** 闭包中。 也就是说，表示 **dup** 和 ***** 原语的 lambda 表达式已经
引用了我们的宏 **new-forth** 的展开捕获的 forth 抽象寄存器。 我们能否将这段代码拉回到
lisp 宏表面以便将其嵌入到新程序中？ 我们将很快回到这一点，但首先会更深入地讨论元编程。

在所有语言的某个级别——通常是对程序员隐藏的级别|代码必须能够引用自身——这种必要性最有说服力的例子
是观察到代码需要能够以某种方式引用自己，以便实现循环、递归和条件表达式，如 **if** 语句。 Flub
语言和非 Flub 语言的区别在于 Flub 阻止直接自定义插入自引用的方式和位置。 但是，正如我们现在所
做的那样，lisp 的非 Blub 状态意味着我们可以使其成为非 Flub。

当前状态下的 forth 系统（它不能插入自引用）几乎是一个纯 Flub。 与纯函数式语言如何故意定义一种
没有副作用和非静态映射的语言类似，纯 Flub 语言被定义为没有像循环和递归这样的自引用代码结构。
这样做的结果是解释纯 Flub 线程将始终终止。 我们的 forth 环境不完全是纯的，因为我们可以——也将
——违反这一点，但在某种意义上说是纯的，如果仅按照目前描述的方式使用将导致纯 Flub 线程。 Pure
Flub 不是很有用，所以让我们破坏 forth 环境的 Flub 纯度。 与其朝着 Flub 方向前进——像
Common Lisp 这样的 Flub 语言，代码线程是不透明且不可访问的——朝着 forth 方向前进，并使代码
宏的这个属性可定制。

.. code-block:: lisp

    (def-forth-naked-prim branch-if nil
      (setf pc (if (pop pstack)
                (cadr pc)
                (cddr pc))))

**branch-if** 原语是迄今为止提出的第一个裸原语。 回想一下，裸原语是不会自动更新程序计数器抽象
寄存器 (pc) 的原语。 相反，他们必须自己更新它。 **branch-if** 将弹出参数堆栈的值。 如果该值
非空，则将 pc 设置为正在解释的线程中下一个单元格的内容。 如果值为 nil，则 pc 像往常一样恢复，
只是它跳过正在解释的线程中的下一个单元格。

例如，以下创建了一个 forth 环境，因此我们可以利用新的 **branch-if** 原语，并定义两个词：
**double** 和 **if-then-double** 。

.. code-block:: none

    * (go-forth (setq my-forth (new-forth))
        { 2 * } 'double name
        { branch-if double "Not doubling" print }
            'if-then-double name)
    NIL

**double** 只是将参数堆栈的顶部元素乘以 2，使其翻倍。**if-then-double** 需要参数堆栈上的
两项。 顶部元素被调用，并且仅当顶部元素为非空时，顶部元素的第二个元素才会加倍。 注意，因为在
**branch-if** 之后线程中的下一个值是指向另一个线程 (double) 的指针，所以执行控制权转移到另
一个线程，而不会将恢复位置推入返回堆栈。在 lisp 中，这称为尾调用。 因此，如果将 nil 传递给
**if-then-double** ，那么分支执行执行时，不会发生加倍，并且会打印字符串：

.. code-block:: none

    * (go-forth my-forth
        4 'nil if-then-double print)
    "Not doubling"
    4
    NIL

但是如果该值不为空，则不执行后面语句，执行加倍，且不打印字符串：

.. code-block:: none

    * (go-forth my-forth
        4 't if-then-double print)
    8
    NIL

.. code-block:: lisp

    (forth-stdlib-add
      { r> drop } 'exit name)

不过，有一种更简单的方法可以从单词中退出，这是通过一个名为 **exit** 的新单词来实现的。
forth 的一个有趣属性是，被调用的单词可以决定它是否是尾调用。 **exit** 是个普通的 forth 字，
所以像往常一样被调用：forth 把当前线程位置推到返回堆栈上，然后将程序计数器设置为指向
**exit** 字的开头。 当调用 **exit** 时，因为它可以使用原语 **r>** 和 **>r** 直接访问返回
堆栈，所以可以通过简单地从返回堆栈中删除恢复位置并将其丢弃来使调用字永远无法获得执行控制权的存
在。 下面是个使用 **exit** 的示例：

.. code-block:: none

    * (go-forth my-forth
        { "hello" print
          exit
          ;; Never gets here
          "world" print } 'exit-test name
    exit-test)
    "hello"
    NIL

.. code-block:: lisp

    (def-forth-naked-prim compile nil
      (setf (forth-word-thread dict)
            (nconc (forth-word-thread dict)
                    (list (cadr pc))))
      (setf pc (cddr pc)))

    (def-forth-prim here nil
      (push (last (forth-word-thread dict))
            pstack ))

因此，**branch-if** 实现了跳转或  **goto** 指令，可能跳转到存储在当前正在执行的线程的后续单
元格中的值。从当前正在执行的线程中获取值是 forth 常见模式，并且需要裸原语。另一个原语
**compile** 也使用这种模式。**compile** 是一个裸原语，它将获取当前正在执行的线程中下一个单
元格的值，然后将该值编译到添加到字典中的最后一个单词的线程中——通常是当前正在编译的单词。
**here** 是个简单的原语，它将正在编译的线程的最后一个 cons 单元推入参数堆栈。这里的
**here** 与这里的常规 forth 中的 **here** 词略有不同。forth 中，**here** 通常推送将被编译
的下一个位置，而不是最后编译的位置。这是因为，在传统的 forth 中，此时要编译的内存位置是已知的
——它将是下一个相邻的内存单元。使用 cons 线程代码我们无法知道这一点，因为我们还没有占用该内存。

有了 **compile** 和 **here**，现在可以开始编写 forth 宏了。记住，当 forth 单词是立即的
时，在编译时它将被执行而不是编译到定义的最后一个单词的线程中。 类似于如何编写宏来适配和扩展
lisp，可以使用直接词来适配和扩展。在 lisp 中，用于元编程的基本数据结构是列表。 在 forth 中，
基本数据结构是堆栈。

你可能已经注意到我们的 forth 环境甚至没有提供 **if** 语句。 我们有个条件分支原语，称为
**branch-if**，但到目前为止，这仅对对其他单词进行尾调用有用。 回想一下，forth 单词是由线程表
示的，我们可以将任何线程的值放入由 **branch-if** 跳转到的单元格中。 如果我们输入一个导致当前
正在编译的线程的一部分的值怎么办？ 从某种意义上说，我们会对当前 forth 单词的另一部分进行尾
声。 好吧，**if** 语句就是这样的 —— 对 **if** 语句末尾的条件尾调用，仅在条件为空时才执行。

.. code-block:: lisp

    (forth-stdlib-add
      { compile not
        compile branch -if
        compile nop
        here } 'if name immediate)

因为我们现在完全在 forth 中编程，所以不需要添加新的原语。要添加 **if** 语句，只需使用
**forth-stdlib-add** 宏将一些 forth 代码附加到标准库中。注意，**if** 被定义为直接词，这意
味着它只能在编译时使用。但由于它是立即的，它将被执行，而不是编译。当遇到立即字时，不会自动将任何
内容编译到目标线程中。所以 **if** 本身用三个词编译到目标线程： **not** 、 **branch-if**
和 **nop** 。然后它执行此处的单词，将最后编译的单词（ **nop** ）的地址留在堆栈上。把
**nop** 留在堆栈上？一个词在编译时将 **nop** 留在堆栈上是一件很奇怪的事情。它放在什么堆栈
上？从技术上讲，编译时使用的堆栈称为控制堆栈。在大多数情况下，控制堆栈是一个并且与参数堆栈相同。
由于可以实现的方式多种多样，因此区分是必要的。有时，特别是在交叉编译环境中，控制堆栈与最终的参数
堆栈完全分开。但是在这里 —— 与大多数交互式 forth 环境一样 —— 我们使用参数堆栈作为控制堆栈。

因此，如果压入与编译 **nop** 的位置相对应的值。 这有什么用？**nop** 本身并不是很重要，重要的
是它前面的内容。 在 nop 之前的单元格中，编译了个 **branch-if** 指令。无论将 **nop** 的值更
改为什么，如果 **if** 条件结果为空，则内部解释器将跳转到分支的位置。

.. code-block:: lisp

    (forth-stdlib-add
      { compile nop
        here swap ! } 'then name immediate)

但是为什么要放了个 **nop** 而不是内存地址呢？ 这是因为还不知道内存地址。 需要等待程序员执行另
一个立即字 —— **then**—— 这将消耗控制堆栈上的值。 然后将编译一个 **nop** 本身并将这个
**nop** 的位置写在 **if** 编译的 **nop** 上。因此，如果条件为空，则将跳过 **if** 和
**then** 之间的所有单词。

.. code-block:: lisp

    (forth-stdlib-add
      { 0 swap - } 'negate name
      { dup 0 < if negate then } 'abs name)

**abs** 是个使用 **if** 和 **then** 来计算堆栈顶部的绝对值的词。 它只是检查该值是否低于
0，如果是，它调用另一个词 **negate** 将负值转换为其绝对值。

在此编译过程中使用控制堆栈的最重要原因是，通过使用堆栈，可以拥有像 **if** 语句嵌套这样的控制结
构。 也就是说，可以将 **if** 语句包含在其他 **if** 语句中，只要确保所有 **if** 词与
**then** 相匹配。

.. code-block:: lisp

    (forth-stdlib-add
      { compile 't
        compile branch -if compile nop
        here swap
        compile nop
        here swap ! } 'else name immediate)

因为 forth 语言是种非 Flub 语言，所以如何创建这些线程并将其与 **if** 语句之类的控制结构一起
线程化是透明地指定并开放给我们以适配和扩展的。 大多数语言都有个与 **if** 语句关联的
**else** 子句； 也许我们也应该添加一个。 另一个直接的词 **else** 被添加到标准库中。
**else** 编译成一个无条件分支，然后终止，因此如果我们采用真（true）（二级或后续）分支，我们将
跳过错误（false）（三级或交替）分支。 然后 **else** 使用 **if** 留在堆栈上的值将这个
**nop** 替换为 **else** 子句开始的位置。 然后将自己的 **nop** 的位置留在堆栈上以供使用。
因为无论控制堆栈上的位置是由 **if** 还是由 **else** 留下的，我们想要 **then** 执行的行为都
是相同的，所以即使没有 **else** 子句，**then** 仍然有效。

.. code-block:: lisp

    (forth-stdlib-add
      { evenp if 0 else 1 then } 'mod2 name)

单词 **mod2** 使用 **if**、**else** 和 **then** 将整数减少到其模 2 的自然余数。如果堆栈
顶部是偶数，它会压入 0，如果堆栈顶部是奇数，则压入 1。

.. code-block:: lisp

    (forth-stdlib-add
      { compile nop
        here } 'begin name immediate
      { compile 't
        compile branch -if
        compile nop
        here ! } 'again name immediate)

因为我们的条件对正在编译的线程的其他部分执行尾调用，所以没有理由不用完全相同的技术来创建像循环这
样的迭代结构。最基本的 forth 循环由 **begin** 和 **again** 立即字定义。 这两个词提供了一个
简单的无限循环，实现起来与 **if** 和 **then** 非常相似，只是在看到这两个词之间保存在控制堆栈
上的地址对应于应该编译成分支语句的地址——而不是编译位置的一个地址。 下面是个简单的循环，它从堆栈
上提供的数字倒数到 1，然后从单词中退出：

.. code-block:: none

    * (go-forth my-forth
        { begin
            dup 1 < if drop exit then
            dup print
            1-
          again } 'countdown name

        5 countdown)
    5
    4
    3
    2
    1
    NIL

注意，在上面的示例中，**if** 和 **then** 构造嵌套在 **begin-again** 循环内。 多亏了
forth 的控制堆栈，嵌套任何对应堆栈的控制结构是完全可以接受的。 为了对应堆栈，控制结构应避免弹
出未推送的值，并应避免在完成后留下任何额外的值。但是就像在构建 lisp 宏时经常选择违反引用透明性
一样，在 forth 我们经常选择在编译时不对应堆栈。 下面的示例与前面的示例相同，只是不使用单词
**exit** 来退出循环。 相反，我们使用 **[** 和 **]** 词切换到编译模式，并交换 **if** 和
**begin**  放置在那里的指针，以便匹配对应的 **then** 和 **again** 无序的词：

.. code-block:: none

    * (go-forth my-forth
        { begin
          dup 1 >= if
                  dup print
                  1-
                  [ swap ] again
                then
          drop
        } 'countdown-for-teh-hax0rz name

        5 countdown-for-teh-hax0rz)

    5
    4
    3
    2
    1
    NIL

上面代码将重新编译，也就是回到开始的代码，只在 **if** 语句中执行。 很少有其他语言有这种方式访
问编译器 —— 准确地说，只有非 Flub 语言。 由于这种自由，forth 程序员有时甚至比 lisp 程序员更
习惯于宏组合。 尽管本书中的 lisp 代码经常使用宏组合技术，但大多数现有的 lisp 代码并没有充分利
用这些技术以及它们可以启用的杠杆作用。 然而，正如本书试图说明的那样，lisp 非常适合宏组合。 这
种组合技术是我认为在未来十年左右的语言研究中将在程序员生产力方面取得最大胜利的地方。


.. _8-6-going-lisp:

8.6 在 forth 中写 lisp 代码
=============================================

到目前为止，本章已经定义了一个极简的 forth 环境，并从 lispy 的角度展示了一些最重要的 forth
元编程概念。希望它已经表明，当拥有正确的工具（Common Lisp）时，设计和实现 forth 语言所需的努
力是多么的少。 我们可以编写 forth 程序来编写 forth 程序——但我们已经知道了。 这就是一切。 此
外，由于 lisp 的宏系统，我们可以编写 lisp 程序来编写 forth 程序。 但是我们可以编写 forth
程序来编写 lisp 程序吗？

.. code-block:: lisp

    (defun get-forth-thread (forth word)
      (with-pandoric (dict) forth
        (forth-word-thread
          (forth-lookup word dict))))

    (defun print-forth-thread (forth word)
      (let ((*print-circle* t))
        (print (get-forth-thread forth word))
        t))

回想一下，我们的 forth 线程是连接在一起的 cons 单元，这些树的叶子要么是函数（代表原语），要么
是原子（代表要压入到参数堆栈的数据）。 因为我们决定让 forth 抽象寄存器可以通过
**pandoric** 宏访问，所以编写实用程序来获取和打印 forth 线程很容易。
**get-forth-thread** 魔性（pandorically）地打开传递给它的 forth 闭包，然后检索并返回
word 中给出的单词的线程。 **print-forth-thread** 打印这个结果线程，其中
**\*print-circle\*** 绑定到 **t** 以防它包含循环。

为了解释说明，假设已经定义了两个 forth 词：**square** 和 **square3**：

.. code-block:: none

    * (go-forth my-forth
        { dup * } 'square name
        { 3 square print } 'square3 name)
    NIL

在编译的 forth 线程中，所有符号和其他单词信息已被删除。 我们所拥有的只是从 forth 的
**my-forth** 的字典中
提取的一个列表结构：

.. code-block:: none

    * (print-forth-thread my-forth 'square3)
    (3
    (#<Interpreted Function>
      #<Interpreted Function>)
    #<Interpreted Function>)
    T

上面的代码没有循环，因此是一个纯 Flub 程序。 如前所述，几乎所有有趣的程序都包含循环。 要创建条
件和循环，我们可以使用 forth 原语 **branch-if**，它可以更改 pc 抽象寄存器以指向由正在执行的
forth 线程中的后续单元格中的值指示的某个位置。 我们还能够使用 **>r** 和 **r>** 直接访问返回
堆栈来实现尾调用。 与大多数其他语言不同，我们可以直接自定义哪些调用是尾调用——甚至从被调用的单词
内部。

但似乎缺少个对 lisp 至关重要的结构：递归。 一个词可以调用自身吗？ 我们看到了如何使用
**branch-if** 跳回到一个词的起始位置 —— 尾递归。 然而，我们真正想做的是通过通用的线程机制让
一个词自己调用。 为此，它必须将其线程位置的开头存储为线程中的一个单元格，以便当前位置存储在返回
堆栈中，然后它必须将 pc 设置为单词的开头。 然而，到目前为止，还没有一个单词能够使用完全递归，因
为在完成编译之前不会命名单词 —— 当搜索字典试图编译递归调用时，我们无法访问它。 幸运的是，可以使
用一个简单的技巧来解决这个问题。 在编译递归调用之前，我们可以简单地退出编译模式并命名正在编译的
单词。 以下是计算阶乘的完全递归版本的示范定义：

.. code-block:: none

    * (go-forth (setq my-forth (new-forth))
        { [ 'fact name ]
          dup 1 -
          dup 1 > if fact then
          * })
    NIL

好了，**(fact 5)** 的结果是 120：

.. code-block:: lisp

    * (go-forth my-forth
        5 fact print)
    120
    NIL

练习：有些 forth 实现使用一个单词 **recurse**，它只是查找当前正在编译的单词的线程并将其插入
到正在编译的线程中。这称为匿名递归。 写一个立即字来代替上述实现命名递归的技巧。

**fact** 的线程比上面的 **square3** 更复杂。 它包含自引用代码：

.. code-block:: none

    * (print-forth-thread my-forth 'fact)
    #1=(#2=#<Interpreted Function>
        1 #<Interpreted Function> #2# 1
        #<Interpreted Function>
        #<Interpreted Function>
        #<Interpreted Function>
        #4=(#<Interpreted Function>
            #<Interpreted Function>)
        #1# . #4#)
    T

在上面， **#2#** 指向的是 **dup** 原语并被编译了两次。 **#1#** 指向的是 **fact** 线程本
身，实现了递归。

这些结构看起来很像用来编写 lisp 程序的 lisp 列表结构，不是吗？ 因为我们了解将执行这些线程的抽
象机器，所以可以在简要说明一些限制的情况下将这些线程编译回 lisp 列表结构，该结构可以通过宏插入
到表达式中并使用我们的 lisp 编译器进行编译。 这个过程被称为模糊代码，因为我们将编译的程序从统
一的、可编程的数据结构（线程）转换为不透明、不可访问的代码块（编译的 Common Lisp 函数）。

当然，我们可以计算或插入宏的 forth 线程和 lisp 列表结构之间存在重大差异。 首先，forth 原语是
指向函数的简单指针（此处显示为 **#<Interpreted Function>** ），但需要创建这些函数的 lisp
列表结构。 现在终于到了解释创建的 **dtable** 抽象寄存器的时候了。 **dtable** 是一个哈希表，
它提供了从这些函数到创建它们的列表结构的映射，在创建 forth 时填充。

forth 线程和 lisp 程序之间的一个很大区别是 forth 线程假设它们可以使用返回堆栈——这个概念在
像 Common Lisp 这样的 Flub 中并不存在。 我们希望消除对内部解释器代码的需求，而是让 lisp 编
译器使用常规的 lisp 控制结构（如函数调用和 **tagbody** / **go** 结构）来处理这个问题。

本章中其余代码的呈现方式与本书中其他大部分代码的不同之处在于，它的实现没有详细描述，而是从高层次
的角度描述的。这是因为实现的机制比较复杂和混乱的，老实说，并不那么有趣。 我只想说我怀疑大多数
lisp 程序员会以类似的方式实现它。

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

**flubify-aux** 是个宏，它展开为一个函数，该函数采用 forth 线程并将其转换为一段 lisp 代
码，利用每个非原始单词都被 **tagbody** 包围的事实，因此 **gensyms** 可以用作 **goto** 的
标签。

.. code-block:: lisp

    (defmacro assemble-flub (form rest)
      `(if (gethash c go-ht)
          (list* (gethash c go-ht)
                  ,form
                  ,rest)
          (list*  ,form
                  ,rest)))

**assemble-flub** 在 **flubify-aux** 中被大量使用作为缩写，它检查哈希表 **go-ht** 以查
看在之前的传递中是否找到任何引用当前正在编译的位置的 **go** s。 如果是，它会将之前为其选择的
**gensym** 标签添加到 **tagbody** 结构中。

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
                          (flubify-aux) thread ))))))

**flubify** 是使用 **flubify-aux** 的函数。 第一遍执行时，检查分支 **if** 指令并建立
**go-ht** 哈希表。 它还递归地清除所有连接到当前线程的线程。 事实上，**flubify** 实际上可以
是双重递归的——只是在展开 **flubify-aux** 的使用之前看不到它。 你看不到它，但 lisp 可以。
如果参照透明度是一块透明的玻璃板，那么我们在这里看到的是一座镜子屋。

.. code-block:: lisp

    (defun compile-flubified (thread thread -ht)
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

**compile-flubified** 采用由 **flubify** 构建的哈希表并将其转换为一个结构，该结构使用标签
将这些 flubbed 线程中的每一个绑定到由 **gensym** 在函数命名空间中命名的函数中。 在这个范围
内，它的展开然后调用原始线程（它也被 flubbed）。

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
            forth (car thread) ht tmp-ht branch-if compile)
          (if (cdr thread)
            (flubify-thread-shaker
              forth (cdr thread) ht
              tmp-ht branch-if compile)))
        ((not (gethash thread ht))
          (if (functionp thread)
            (setf (gethash thread ht)
              (with-pandoric (dtable) forth
                (gethash thread dtable)))))))

**flubify-thread-shaker** 是实际遍历 forth 线程的函数。 它递归地 shake 所有连接的
forth 线程。这意味着它只隔离了使用 **get-forth-thread** 实用程序执行给定线程所需的相关
forth 线程结构，然后将每个函数转换为相应的 lisp 代码，跳过 **if** 分支并在看到编译时出错。

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
              shaker -ht)
      (flubify forth thread prim-ht thread-ht branch-if)
      `(let (pstack)
        (let (,@(let (collect)
                    (maphash
                      (lambda (k v)
                        (push `(,(gethash k prim-ht)
                                (lambda () ,@(butlast v)))
                                collect ))
                        shaker-ht)
                      (nreverse collect)))
            ,(compile-flubified
                thread thread-ht)))))

**forth-to-lisp** 是本章前面的宏和函数所促进的终极功能。 它采用 **new-forth** 创建的
forth 环境，查找作为word传递的符号所指示的线程，然后返回相应的lisp代码来执行该线程。 它首先
shake 线程（递归地 shake 所有连接的线程），然后应用 Flubification 过程。 最后，它包装了少
量的 lisp 代码，这些代码用常规的 lisp 控制结构实现了内部解释器。

为了说明，回想之前定义的 forth 个单词 **square** 和 **square**。 同样，下面是它们在
**my-forth** 环境中的定义方式：

.. code-block:: none

    * (go-forth my-forth
        { dup * } 'square name
        { 3 square print } 'square3 name)
    NIL

下面我们将 **square3** 转换成 lisp 代码：

.. code-block:: none

    * (forth-to-lisp my-forth 'square3)

    (LET (PSTACK)
      (LET ((#:G1814 (LAMBDA () ; dup
                      (PUSH (CAR PSTACK) PSTACK)))
            (#:G1815 (LAMBDA () ; *
                      (PUSH (* (POP PSTACK)
                                (POP PSTACK))
                            PSTACK)))
            (#:G1816 (LAMBDA () ; print
                      (PRINT (POP PSTACK)))))
        (LABELS ((#:G1817 () ; square3
                  (TAGBODY
                    (PUSH '3 PSTACK)
                    (FUNCALL #'#:G1818)
                    (FUNCALL #:G1816)))
                (#:G1818 () ; square
                  (TAGBODY
                    (FUNCALL #:G1814)
                    (FUNCALL #:G1815))))
          (FUNCALL #'#:G1817))))

果然，上面是可执行的lisp代码。 如果我们想，可以使用宏在某个地方编译它。 或者我们可以 **eval**
它：

.. code-block:: none

    * (eval *)
    9
    NIL

为了展示一个带有分支和递归的 forth 线程是如何被混淆的，下面是来自 forth 单词 **fact** 编译
成 lisp 代码的一部分：

.. code-block:: none

    * (forth-to-lisp my-forth 'fact)
        ...
        (LABELS ((#:G1803 ()							; fact
                  (TAGBODY
                    (FUNCALL #:G1797)		; dup
                    (PUSH '1 PSTACK)
                    (FUNCALL #:G1798)		; -
                    (FUNCALL #:G1797)		; dup
                    (PUSH '1 PSTACK)
                    (FUNCALL #:G1799)		; >
                    (FUNCALL #:G1800)		; not
                    (IF (POP PSTACK) (GO #:G1804))
                    (FUNCALL #'#:G1803)  ; fact
                    #:G1804
                    (FUNCALL #:G1801)    ; nop
                    (FUNCALL #:G1802)))) ; *
          (FUNCALL #'#:G1803)) ; fact
        ...

所以我们用forth编写了这个程序，但它现在是lisp。 我们使用了 forth 直接词 **if** 和
**then** 来编译一个控制递归是否发生的条件控制结构。 代替返回堆栈，lisp 将使用其通用函数调用基
础结构为我们实现此递归。

当使用 eval 进行测试时，请记住单词 **fact** 假定堆栈上有一个值，但我们从一个新堆栈开始。
为了测试这个词，我们应该创建一个将值添加到堆栈的包装（wrapper）词。 例如：

.. code-block:: none

    * (go-forth my-forth
        { 5 fact print } 'fact5 name)
    NIL

然后这么执行：

.. code-block:: none

    * (eval (forth-to-lisp my-forth 'fact5))
    120
    NIL

如前所述，由于 lisp 和forth 之间的差异，我们的 **forth-to-lisp** 编译器有一定的局限性。
例如，我们不再提供对返回堆栈的访问，因此任何使用 **r>** 或 **>r** 的单词都被禁止。 这包括
退出，因此之前的单词倒计时将不起作用。 但是，因为它不使用退出，*countdown-for-teh-hax0rz*
可以正常工作。 因为 lisp 程序无法访问它们的返回堆栈，所以并非所有的控制结构都可以用像 Common
Lisp 这样的 Flub 语言来实现。 练习：添加 **exit** 作为一个特殊情况的词，它使用lisp 块从一
个词返回。

另一个限制是 lisp 代码无法编译非 Flub 代码，因此我们无法翻译出 forth 中用来编译的单词，
例如 **if**、**then**、**begin** 和 **again**。 当然，请注意，用这些词创建的 forth 线程
仍然可以用于编译词，如上与事实一样。 最后一个限制是，在 forth 中，**branch-if** 可以跳转到
任何线程，即使它是在与当前执行的单词不同的单词中创建的。 在 lisp 中，只能转到 **tagbody** 中
的其他位置。 Forth 允许非本地分支，但一般的非本地分支不能在像 Common Lisp 这样的 Flubs 中
完成。

等一等。 当我们之前在 forth 环境中进行编程时，我们不是只避免了所有这些 Flub 限制吗？是的。 宏
允许我们将程序与 lisp 相互转换。 多亏了宏，任何东西都可以用 lisp 编程。
