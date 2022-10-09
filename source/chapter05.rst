.. _chapter05:

********************************
第五章：Programs that program :sup:`（1）` 
********************************
   
.. note:: （1）
  可编程的程序


.. _5-1-lisp-is-not-funcitonal:

5.1 Lisp 并非函数式编程 :sup:`（2）` 
==============================
   
.. note:: （2）
  Lisp Is Not Functional
  
对 Lisp 常见的一种偏见是将其叫做函数式编程语言。但 lisp 不是函数式语言。事实上，
lisp 是有史以来最不函数式的语言。这个误解的根源很有趣，同时也是个很好例子，
说明了一个小的用词不当可以产生持久的影响，并在用词不当的原因变得无关紧要之后
很长时间内导致混淆。什么是函数式编程语言呢？唯一有意义的定义是:

..

  函数式编程语言是由函数构成的语言

那么，什么是函数呢？函数是数学中的一个概念，已经存在了几个世纪:

..

  函数是一个静态的，定义好的从输入到输出的映射关系

在 lisp 中，我们使用 **defun** 来定义新函数。例如，下面的函数使用求和将所有数字
的集合映射到一个新集合，这个新集合也包括所有数字：

.. code-block:: none
    :linenos:

    (defun adder (x)
      (+ x 1))

显然，我们可以将这段代码应用于任意数字，获得映射后得到的返回值，但 **adder**
真的是个函数吗？好吧，令人困惑的是， lisp 认为它是 :sup:`【1】` ：
  
.. hint:: 【1】 
  如果你之前没有在 Common Lisp 中见过 **describe** 函数的话，现在可以尝试一下。
  描述一个函数、一个特殊形式、一个宏、一个变量、一个符号和一个闭包。
   
.. code-block:: none
    :linenos:

    * (describe #'adder)
    #<Interpreted Function> is a function.

但是把 **adder** 称为函数是 lisp 历史中最根深蒂固的误解之一。 **defun** 和 lambda
表达式实际上创建的是个 *进程* :sup:`（3.1）` （ procedures ），或者更确切的说，一个 *可被调用的实例*  :sup:`（3.2）` （ AMOP ）。
这有什么区别呢？过程不一定与值的映射有任何关系，而是可执行的代码片段（函数调用了的），
可能是保存的环境变量。当 lisp 程序员以某种称作 *函数式风格* :sup:`（3.3）` 编写代码时，之后生成过程组合一起假装是，并可能被认为是数学风格的函数映射。
   
.. note:: （3）
  进程  procedures；可被调用的实例 funcallable instances；函数式风格 functional style
  
Lisp 之所以经常被误称为函数式编程语言是有历史原因的。信不信由你，曾有段时间大部分
语言甚至都不支持进程这一概念，而现代程序员认为在所有的语言中过程是理所当然的。早期
的一些语言在可复用的代码中并没有提供给局部变量合适的抽象，程序员们只能通过手动分配
寄存器和操作堆栈来实现这种操作。 Lisp ，至始至终就有进程，似乎比那些语言更具备函数式的特性。

接下来，进程抽象得到应有的重视，并被纳入几乎所有编程语言，随后大家就开始慢慢地
遇到障碍，因为他们所实现的进程的有限性质。然后程序员们开始意识到，他们可以从其
他的进程中返回进程、把它们嵌入到新的环境中、聚合到一个数据结构中，或者更通用的
是，将其视为任意旧的常规的值。一个口号出现了，为这个下一步的巨大抽象的推进动员程序员： 一个没有类的社会，头等的就是进程 :sup:`（4）` 。与那些将进程降
级到次等二级类的语言相比，lisp ，这种一直拥有这些以进程为头等的语言，似乎更加是函数式
的。
   
.. note:: （4）
  a society without classes , first-class procedures 
  
最后，大部分语言为了支持某些糟糕的 _Blub_ 语法（如中缀赋值），在表达式和语句之间
做了无意义的区分。在 lisp 中，所有内容都会有所返回 :sup:`【2】` ，同时对代码的嵌套和组合也没有任
何（语法方面）限制。这是个很简单的问题，答案也显而易见：在一门语言中，对新手友
好的语法和真正的灵活性哪个更重要？中缀语法的语言在许多方面降低了其抽象的可能性。
幸运的是，大多数现代语言决定给予用户足够的信任，允许他们根据自己的需要来组合表
达式。与那些做出这些愚笨的语法决定的语言相比，lisp 似乎更具函数式的风范。
  
.. hint:: 【2】 
  除了 (values) 什么都不返回。但即使这也被强制为 nil ，因此可以在表达式中使用。
   
在熟悉了这个普遍存在但却被误导的术语之后，程序员们开始意识到，在函数式编程语言
和非函数式编程语言之间的争论中使用的函数概念不仅令人困惑，同时也是本末倒置。
为了纠正这个错误，程序员和学者们都回到黑板前，回到了函数的数学定义：从输入到输出
的映射。如果 lisp 在任何方面是函数式语言，那么它仅仅与Perl 和 Javascript 等现代语言一样。

显然， lisp 进程不是函数。 Lisp 进程可以返回非静态值，也就是说，可以使用相同的参数
多次调用，每次接收不同的返回值。就像我们前面章节的例子一样， lisp 进程可以通过封装
变量来存储状态。 **rplaca** 这样的进程可以改变内存或其他地方（如寄存器）中的值。
**terpri** 和 **format** 这样的 lisp 进程会产生指向终端或文件的输出 :sup:`【3】` （在 **terpri** 情况
下是换行）。 **yes-or-no-p** 这样的 lisp 进程会从终端中读取输入，然后根据输入返回
对应的值。难道这些进程都是静态的、定义好的映射吗？
  
.. hint:: 【3】 
  **Terpri** 和 **rplaca** 这两个操作符都合理地阐述了 COMMON LISP 操作符糟糕命名的区别
   
因为 lisp 进程不是数学函数，所以 lisp 也就不是函数式编程语言。事实上，一个强有力
的论点是 lisp 比大多数其他语言更不像函数式。在大部分语言中，看起来像进程调用的表
达式都是被语言的语法强制为进程调用。而在 lisp 中，我们有宏。正如我们所看到的，
宏可以隐式地将某些形式的含义从函数调用改变为任意的 lisp 表达式，这种技术能够在许多方面
违反了引用透明性，这在其他语言中是不可能的。

在考虑到大多数语言实际上根本不是函数式的之后，一些语言设计者决定弄清楚在真正的函数式
语言中编程是什么样子的。不出你所料，编程函数式语言大多是令人讨厌和不切实际的。
几乎没有现实世界的问题可以有效地表示为从输入值到输出值的静态、明确定义的映射。也就
是说，函数式编程并非没有优点，许多语言都被设计为利用函数式编程风格。这意味着找
到一种方便的方法，将程序的函数式部分与(实际上有趣的)非函数式部分隔离开来。 **Haskell**
和 **Ocaml** 这样的语言就是使用这种隔离作为进行积极优化假设的方法。

但这是 lisp。我们非常不函数式，并以此为豪。在某种程度上而言，这种副作用隔离是有用的，lisp
程序员可以并且确实能使用宏来实现它。函数式编程背后真正的目的是将应该发生什么的
函数描述与实际发生的机制分离开来。Lisp 肯定不是函数式，但由于宏的存在，没有比
Lisp 更好的实现函数式语言的平台或材料。


.. _5-2-topdown-programming:

5.2 自上而下的编程
==============================

::

  你教不会初学者自顶向下编程，因为他们不知道哪一端是上。 --C.A.R. Hoare

在 [3.2 领域特定语言](chapter03.md) 中，当我们第一次考虑特定域语言时，我们创建了个简单
的宏 **unit-of-time** 。这个宏允许我们用一种直观的、基于符号的语法，可以方便地以不
同的单位指定时间段：

.. code-block:: none
    :linenos:

    * (unit-of-time 1 d)

    86400

**unit-of-time** 是个很方便的特定域语言，因为程序员不必去记住一些东西，比如说，一天
有多少秒。 **unit-of-time** 是用简单的宏实现的，该宏使用 case 语句作为底层展开式的核心。

宏设计的一个重要原则就是 *自上而下*  :sup:`（5）` 编程。在设计一个 lisp 宏时，你想要首先要从抽象开始。你想要这样编写程序：
在你编写这个宏之前就使用这个宏。有点矛盾的是，在为该语言编写简洁的定义/实现之前，
你需要知道怎么用这个语言编程。
   
.. note:: （5）
  自上而下 top-down
  
因此，构造个正规的宏的第一步是编写宏的 *用例* ，即使无法测试或使用它们。如果用新语言编
写的程序足够全面的话，那么接下来就会有个很棒的想法，即用于为该语言实现编译器或解释器所需要的内容。

考虑我们的 **unit-of-time** 宏，有没有办法将它提升到另一个级别的规格，并为创建这些单位的便捷的宏去创建一种语言呢？嗯， **unit-of-time** 是个宏，为了实现这点，我们就需要用宏来定义宏……

停！到此为止。

我们不是从考虑语言的实现开始的，而是问我们自己要用这个语言做什么。答案是我们想要个
简单的方法，用来定义这类帮助转换单位的助手（程序）。在接下来的使用示例中，我们希望使用一种单位类
型——时间，其基本单位为：秒，用 **s** 来指代，以及一批由单位和这个单位到基本单位的转换
因子的 pairs （配对）：

.. code-block:: none
    :linenos:

    (defunits% time s
      m 60
      h 3600
      d 86400
      ms 1/1000
      us 1/1000000)

**defunits%** 会展开成定义宏的代码，就像在 :ref:`3-2-domain-specific` 中编写的
**unit-of-time** ，允许我们将任意的时间单位转换为秒。还能写的更好吗？

这是在设计头脑风暴中的重点，在大多数编程语言中，创新都停滞不前。刚刚我们创建了一种将不同单位的
乘数值映射到代码中的方法，这种方法让我们能够方便地转换单位。但是一个专业的 lisp
程序员会意识到这个映射本身就是一个程序，并且可以用我们经常增强 lisp 程序的方法来增
强它。
当我们输入多种不同的单位时，依据其他单位来指定单位可能会很有用。现在，让我们授权那些，因子用来倍乘单位，（因子）也可以是一个对应其他单位的值列表，如
下所示：

.. code-block:: none
    :linenos:

    (defunits%% time s
      m 60
      h (60 m)
      d (24 h)
      ms (1/1000 s)
      us (1/1000 ms))

这种 *链式* 的单位就感觉自然了。分钟依据我们的基础单位，秒，小时基于分钟，天基
于小时。为了以迭代的方式实现这个宏，我们首先需要用 **defunits%** 来实现非链式的版本，
然后用 **defunits%%** 实现链版本，最后添加适当的错误检查，就有了最终的版本：
**defunits** 。

注意，这种新语言不仅可以提供方便的语法来添加新的单元类型。还允许我们在计算中延迟
 *四舍五入* 的影响，并让 lisp 使用尽可能精确的算法。例如,弗隆（ furlong ）正相当于 1/8 英
里，所以我们如果使用链式对其进行编码，而不是说，度量近似值，我们就可以得到更准确的
结果，或者说更重要的是，与其他使用米计算出的结果尽可能保持一致。这是因为
我们可以添加找到的最精确的转换因子，而不必自己费心进行任何转换，这个宏让我们在表达式级别上构建转换例程，（而这）在其他语言
中无法实现。

使用我们在 :ref:`3-5-unwanted-capture` 中的描述的自动化 **gensym** ， **defunits%** 就很容易编写。
Graham （注： on lisp 的作者）的 **symb** 函数可以让我们为转换宏生成新的名字。例如，当 **time** 是提供的单位类型，
那么转换宏就是 **unit-of-time** 。 **defunits%** 是由最初定义的 **unit-of-time** 构建
的， **unit-of-time** 是在 :ref:`3-2-domain-specific` 中定义的，在 **defunits%** 中，
由 **defmacro!** 和反引号组成，用来替换每次宏调用时需要重新生成的部分。

.. note::

  Graham 是 On Lisp 的作者，会经常出现一些上面的内容，如果有时间的话，推荐去看一下这本书。

.. code-block:: none
    :linenos:

    (defmacro! defunits% (quantity base-unit &rest units)
      `(defmacro ,(symb 'unit-of-quantity) (,g!val ,g!un)
        `(* ,,g!val
            ,(case ,g!un
              ((,base-unit) 1)
              ,@(mapcar (lambda (x)
                          `((,(car x)) ,(cadr x)))
                        (group units 2))))))

**defunits%** 用了 *嵌套了的反引号 * （ **`** ）：一个非常难以理解的结构。用反引号编程就像在代码
中增加了一个维度的含义。在其他的语言中，给定的语句通常都有非常简单的求解语义。你知道每段代码会在什么时候执行，因为每段代码都必须同一时间执行：运行时（run-time）。
梯度往上提了一级：我们的代码是一个后续可能求解也可能不求解的列表。
但在里面的原始列表中，每遇到逗号，就将我们带回到上一个引用梯度，然后以合适的方式
执行对应梯度的代码。

因此，有一种简单的算法可以确定何时 lisp 代码会被求值。只需从表达式的根部开始，在遇到
反引号后，标记一层引号。每遇到一个逗号，就把引号调低一级。正如 Steel 所指出的，
遵循这种级别的引用很具挑战性。追踪当前引用深度的这种困难，让使用反引用感觉像是在
常规编程中添加了另一个维度。在其他语言中，可以随意向“东南西北”四个方向走，但 lisp
还提供了向上的选择。

**defunits%** 是个好的第一步，但仍然没有实现链式。目前，实现该语言的宏主要是简单的替换。
要实现链行为需要更复杂的程序逻辑。简单的替代不起作用，因为宏的部分依赖于宏的其
他部分，所以在扩展时，需要完整地处理提供给宏的表单，而不仅仅是考虑可以插入的各
个部分。

记住，宏实际上就是函数，我们创建一个在宏定义中使用的实用函数： **defunits-chaining%** 。
这个实用函数接收一个单位，例如像 **S** 、 **M** 或是 **H** 这样的符号，以及单位规格列表。这个单位规格既可以是单个数字，这个数字被解释为基础单位，如 **(M 60)** ，
也可以是一个列表，该列表内部链式地指向另一个单位，如 **(H (60 M))** 。

.. code-block:: none
    :linenos:

    (defun defunits-chaining% (u units)
      (let ((spec (find a units :key #'car)))
        (if (null spec)
          (error "Unknown unit ~a" u)
          (let ((chain (cadr spec)))
            (if (listp chain)
              (* (car chain)
                  (defunits-chaining%
                    (cadr chain)
                    units))
              chain)))))

这个实用函数是递归的。为了找到相对于基本单位的乘数，我们将链中的每一步乘以另一个实用
函数的调用，从而算出链的其余部分。当调用堆栈返回时，就会得到将给定单元的值转
换为基本单元的乘数。例如，当我们在构建小时的乘数时，我们发现一小时是六十分钟，进一步
递归，然后发现一分钟是六十秒，我们再次递归时发现秒是这条链的末尾——分钟是直接根据(这个）基本单位指定的。因此，从递归回退，我们求解： **(\* 60 (\* 60 1))** ，也就是
**3600** ，在一个小时中有 3600 秒。

有了这个实用函数后，为每个单位计算乘数只需要对 **defunits%** 进行简单的修改，
正如我们已经在 **defunits%%** 中做过的那样。我们将每个单位和整个单位规格传递给 **defunits-chaining%** 实用程序， 而不是直接从单元规格中拼接值。如上所述，这个函数递归地计算出将每
个单位转换为基本单位所需的乘数。通过这个乘数， **defunits%%** 可以像 **defunits%**
一样将值拼接到 **case** 语句中。

然而，这些宏并不完整。 **defunits%** 宏不支持链式。 **defunits%%** 支持链式，但缺少 *错误检查* 。专业的宏编写人员总是小心地处理任何可能出现的错误条件。在无限循环或是
在 REPL 中难以调试的情况中，（错误检查）尤为重要。

**defunits%%** 的问题实际上是我们设计的语言的一个属性：可以编写有环的程序。如：

.. code-block:: none
    :linenos:

    (defunits time s
      m (1/60 h)
      h (60 m))

为了提供适当的调试输出，需要稍微增强我们的实现。最终的版本， **defunits** ：

.. code-block:: none
    :linenos:

    (defun defunits-chaining (u units prev)
      (if (member u prev)
        (error "~{ ~a~~ depends on ~}"
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

    (defmacro! defunits (quantity base-unit &rest units)
      `(defmacro ,(symb 'unit-of- quantity)
                (,g!var ,g!un)
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

**defunits** 不但支持链式，而且如果该语言的用户指定了具有这种循环依赖关系的程序，
它还提供了有用的调试输出。之所以能做到是因为使用了 **defunits-chaining** ——
**defunits-chaining%** 的升级版， **defunits-chaining%** 维护了一个以前访问过的所
有单元的列表。这样，当再次通过链式访问同一个单位时，就会抛出异常来简明的描述
这个问题:

.. code-block:: none
    :linenos:

    * (defunits time s
        m (1/60 h)
        h (60 m))

    Error in function DEFUNITS-CHAINING:
      M depends on H depends on M

**defunits** 宏与 **defunits%%** 完全相同，除了传递了一个额外的参数 **nil** 给
**defunits-chain** ，这是表示已经到了访问过的单位记录列表的末尾。如果一个
新单位被搜索，而我们已经访问过它，那么一个循环就被检测到了。我们可以用这个
访问过的单元历史记录来向宏的用户（很可能是我们自己）显示有用的信息，这些
用户可能无意中写入了循环。

因此， **defunits** 是种将单元输入到转换例程领域的专用语言。实际上，它特定于比这更精细的领域；这里有很多可能的写法。由于在 **Blub** （语言）中创建语言很困难，而在
lisp 中却很容易，所以 lisp 程序员通常不会对把所有东西都塞到一个领域中而感到费心。相反，
它们只是使语言越来越精确到问题领域，直到最终目标变得微不足道。

一个使用 **defunits** 的例子是 **unit-of-distance** 。

.. code-block:: none
    :linenos:

    (defunits distance m
      km 1000
      cm 1/100
      mm (1/10 cm)
      nm (1/1000 mm)

      yard 9144/10000  ; Defined in 1956
      foot (1/3 yard)
      inch (1/12 foot)
      mile (1760 yard)
      furlong (1/8 mile)

      fathom (2 yard)  ; Defined in 1929
      nautical-mile 1852
      cable (1/10 nautical-mile)

      old-brit-nautical-mile  ; Dropped in 1970
        (6080/3 yard)
      old-brit-cable
        (1/10 old-brit-nautical-mile)
      old-brit-fathom
        (1/100 old-brit-cable))

以防你想知道：1970 年采用国际海里制缩短了英寻（至少对英国水手而言）
的 1/76，也就 2 厘米多一点：

.. code-block:: none
    :linenos:

    * (/ (unit-of-distance 1 fathom)
        (unit-of-distance 1 old-brit-fathom))
    * (coerce
        (unit-of-distance 1/76 old-brit-fathom)
        'float)

    0.024384


.. _5-3-implicit-contexts:

5.3 隐式上下文
==============================

宏可以利用一种被叫做 *隐式上下文* 的技术。在常用的代码中，或者说是需要绝对简洁且缺少任何周边账簿繁琐的东西，
我们有时选择要隐式地在表达式的某些部分添加 lisp 代码，这样我们就不必每次使用抽象时都去编写它。之前
也有介绍过隐式上下文，而且也很清楚的表达了，即便是不使用宏，隐式上下文也是 lisp 编程的
基础部分： **let** 和 **lambda** 表达式就有个 *隐式的 progn* 。因为这两个表达式是顺序的执
行表达式的主体并返回最后的那个结果。 **defun** 会在表达式外添加 *隐式的 lambda* ，因此不需
要在已命名的函数中使用 lambda 格式。

本节介绍的是本书中后面要用到的 *遍历代码 的宏—— **tree-leaves** :sup:`【4】` 的派生以及构造。和
**flatten** 一样， **tree-leaves** 宏会检查一段 lisp 代码，将这段代码当作一个树
（ **tree** ），然后做一些改动后返回一个新树。原表达式的列表结构不会被更改： **flatten** 和
**tree-leaves** 都是构建新的结构。这两者之间的不同之处在于， **flatten** 的目的是移除嵌套列表
然后返回一个不是真正的 lisp 代码的扁平（ **flat** ）列表，而 **tree-leaves** 则是
保留了表达式的形状，但修改了（树结构的）特定原子（ **atom** ）的值。

.. hint:: 【4】 
  另请参阅 COMMON LISP 函数 subst 。
  
.. note::
  这里的树指的是数据结构中的树。原语指的是一个词，为最小单位，不可再分割。
  具体参考: https://www.gnu.org/software/emacs/manual/html_node/eintr/Lisp-Atoms.html

现在，先从简单的草图开始吧。 **tree-leaves%** 是个函数，

.. code-block:: none
    :linenos:

    (defun tree-leaves% (tree result)
      (if tree
        (if (listp tree)
          (cons
            (tree-leaves% (car tree)
                          result)
            (tree-leaves% (cdr tree)
                          result))
          result)))

该函数在 **tree** 表达式中递归，然后构造出一个新的具有相同形状 :sup:`【5】`的列表结构。
当遇到一个（数状结构的）原子时，函数会返回 **result** 参数的值，而不是返回这个原子。

.. hint:: 【5】 
  在 **if** 结构中，如果 **else** 部分没有的话，那么 **else** 的部分就返回 **nil** ，  即空列表。

.. code-block:: none
    :linenos:

    * (tree-leaves%
        '(2 (nil t (a . b)))
        'leaf)

    (LEAF (NIL LEAF (LEAF . LEAF)))

所以， **tree-leaves%** 返回了个新的树结构，其中所有的原语被转换成了提供的
参数 **leaf** 。注意， **cons** 结构中 **car** 位置的原子 **nil** 没有变，和 **cdr** 位置
一样，都不会变( **cdr** 为 **nil** 时即表示空列表）。

当然，更改每个元素,是没有什么意义的。我们真正想要的是一种挑选和选择具体化原子方法，并有选择地对它们应用转换，以使新原子插入到新的列表结构中，让我们不感兴趣的原子保持不变。择性地对其进行转换，之后再将转换后的原语插入到新的列表结构中，对不相关
的就不用去管他了。在 lisp 中，编写一个可自定义的使用函数的最直接的方法就是有 *插
件* ——即用户可以使用自定义的代码来控制实用程序的功能。 **COMMON LISP** 内置的
**sort** 函数就是典型的代表。以下的代码中，小于（<）函数对 **sort** 来说就是个插件：

.. code-block:: none
    :linenos:

    * (sort '(5 1 2 4 3 8 9 6 7) #'<)
    (1 2 3 4 5 6 7 8 9)


使用函数作为参数来控制程序的行为的这个理念很方便，因为我们可以创建适合
手头任务的匿名函数。或者说，当需要更强大的功能时，可以创建一个生成匿名函数的
函数。这种行为被称为 *函数组合* :sup:`（6.1）` 。尽管函数组合没
有 *宏组合* :sup:`（6.2）` 那么有趣 :sup:`【6】` ，但这仍是个很有用的技术，且这个
技术是专业 lisp 程序员必须掌握的。

.. hint:: 【6】 
  这就是为什么函数组合只得到本书的几个段落而宏组合却得到本书大部分内容的原因。
   
.. note:: （6）
  函数组合 function composition； 宏组合 macro composition

有个关于函数组合的简单示例是 —— **predicate-splitter** 。

.. code-block:: none
    :linenos:

    (defun predicate-splitter (orderp splitp)
      (lambda (a b)
        (let ((s (funcall splitp a)))
          (if (eq s (funcall splitp b))
            (funcall orderp a b)
            a))))

该函数是将两个（谓词）断言函数组合成一个新的（谓词）断言。第一个断言函数接收两个参数，用来
排序元素。第二个断言接收一个参数，并确定元素是否符合你想要分割（谓词）断言的特殊类别。
例如，下面这个例子就是用 **predicate-splitter** 来创建个新的（谓词）断言，该（谓词）断言
和小于判断工作原理是一致的，只不过该（谓词）断言认为偶数要小于奇数：

.. code-block:: none
    :linenos:

    * (sort '(5 1 2 4 3 8 9 6 7)
            (predicate-splitter #'< #'evenp))

    (2 4 6 8 1 3 5 7 9)

所以，我们要怎么样才能使用函数作为插件来控制 **tree-leaves%** 工作呢？在
**tree-leaves%** 的更新版本 —— **tree-leaves%%** 中，我们添加了两个不同的函数
插件，一个用来控制哪些（树结构的）叶子改变，另一个用来指明怎么将旧（树结构的）叶子转换成新（树结构的）叶子，这两个函数分别称为测试（ **test** ）和结果（ **result** ）。

.. code-block:: none
    :linenos:

    (defun tree-leaves%% (tree test result)
      (if tree
        (if (listp tree)
          (cons
            (tree-leaves%% (car tree) test result)
            (tree-leaves%% (cdr tree) test result))
          (if (funcall test tree)
            (funcall result tree)
            tree))))

我们使用 **tree-leaves%%** ，通过给它传递两个 lambda 表达式，这两个表达式都只接受单个
参数 —— **x** 。在这种情况中，我们想要一个新的树：该树的结构与我们 **tree** 参数相同，除了会将所有的偶数都变成符号 **even-number** ：

.. code-block:: none
    :linenos:

    * (tree-leaves%%
        '(1 2 (3 4 (5 6)))
        (lambda (x)
          (and (numberp x) (evenp x)))
        (lambda (x)
          'even-number))

    ; Note: Variable X defined but never used.

    (1 EVEN-NUMBER (3 EVEN-NUMBER (5 EVEN-NUMBER)))

除了 lisp 正确地提醒我们在第二个插件函数中没有使用到 **x** 变量以外，函数似乎正常工作。
当我们没有使用一个变量时，代码中通常都会有这么一个提示。即便是故意的，就像上面
代码那样，编译器重视有关应忽略哪些变量的信息。通常，我们都会使用这个变量，
但存在这么一些情况，就像上面的例子一样，实际上是不想用到这个变量。遗憾的是
我们必须要传给函数一个参数，毕竟不管怎么说我们都忽略了那个参数。这种情况
通常时在编写灵活的宏时会遇到。解决办法就是向编译器声明可以忽略变量 **x** 。因为
声明一个变量是可忽略后再使用这个变量并没有什么危害 :sup:`【7】` ，因此可以将两个变量 **x**
都声明为可忽略：

.. hint:: 【7】 
  Lisp 会发现它实际上是不容忽视的。
   
.. code-block:: none
    :linenos:

    * (tree-leaves%%
        '(1 2 (3 4 (5 6)))
        (lambda (x)
          (declare (ignorable x))
          (and (numberp x) (evenp x)))
        (lambda (x)
          (declare (ignorable x))
          'even-number))

    (1 EVEN-NUMBER (3 EVEN-NUMBER (5 EVEN-NUMBER)))

这就是这个教程变得有趣的地方。看起来对我们来说 **tree-leaves%%** 工作得刚刚好，我们可以通过提供插件函数来更改树结构
中的任何叶子，这些函数可以验证叶子是否应该更改和应该更改成什么。在除 lisp 之外的编程语言中，改实用工具的优化就到此为止了。但拥有 lisp ，
我们可以做的更好。

尽管 **tree-leaves%%** 中提供了我们想要的所有功能，但它的接口不是很方便而且
有点冗余。试验一个实用工具越简单，我们越容易找到它的有趣的未来用法。为了减少
代码遍历实用工具周围的混乱，我们创建一个宏，该宏为其用户（可能是我们自己）提供了
隐式上下文。

但我们需要的不是像隐式的 **progn** 或 **lambda** 那么简单，而是完整的隐式词法
上下文，用来节省创建这些插件函数的开销，并在运行转换树这样的常见任务时只需
要输入最少量的代码。

.. code-block:: none
    :linenos:

    (defmacro tree-leaves (tree test result)
      '(tree-leaves%%
        ,tree
        (lambda (x)
          (declare (ignorable x))
          ,test)
        (lambda (x)
          (declare (ignoreable x))
          ,result)))

该词法隐式上下文不像简单的隐式，某种意义上我们并不仅仅发现通用隐式模式的另一
种用法。相反，在我们开发 **tree-leaves%%** 的遍历接口时，我们一步一步地开发了个
不同寻常的模式。
对于我们的隐式宏的构造，在之前的 REPL 中的 **tree-leaves%%** 直接有效地复制粘贴到
**tree-leaves** 的定义中，然后我们期望根据这个宏的不同用法来改变的部分是使用反引号进行参数化了的。现在，通过这个宏，使用 **tree-leaves%%** 这个实
用工具时的冗余接口就更少了，当然，该接口完全是任意的，因为有多种编写它的可能方式。
然而，这似乎是最直观的、最不臃肿的方法，至少就我们迄今为止所设想的用途而言。宏
允许我们以一种简单、直接的方式创建一个高效的程序员接口，这在其他语言中是不可
能的。下面是我们如何使用这个宏的例子：

.. code-block:: none
    :linenos:

    * (tree-leaves
        '(1 2 (3 4 (5 6)))
        (and (numberp x) (evenp x))
        'even-number)

    (1 EVEN-NUMBER (3 EVEN-NUMBER (5 . EVEN-NUMBER)))

注意，变量 **x** 实际上是在没有定义的情况下就使用了。这是因为围绕后面两个表达式都有
一个 *隐式词法变量* 绑定。这种不可见变量的引入被认为违反了 *词法透明性* 。另一种说法是，
引入了一个名为 **x** 的重复词供这些形式使用。我们将在[第六章：回指(Anaphoric) 宏]
中进一步介绍。


.. _5-4-code-walking-with-macrolet:

5.4 使用 **macrolet** 遍历代码
==============================

::

  Lisp 不是门语言，而是构建语言的原料。 —— Alan Kay

写就的表达式的形式很少说话，诸如计算机代码，经常滋生不同的发音习惯。大多数
程序员在头脑中运行着对话，推理表达式和读出运算符，有时是有意识的，经常是无意
识的。例如，lisp 的 **macrolet** 的发音最简单的方法就是把两个 lisp 关键字 （ **macro** 和
**let** ）连起来读。但看过 Steele 的评论 [CLTL2-P153] 后，部分程序员会用 *Chevrolet* 押韵的
方式来读 **macrolet** ，这种幽默的发音很难从脑中的对话中去掉。

不管 **macrolet** 是怎么读的，它都是 lisp 高级编程里很重要的一部分。 **macrolet** 是个特殊
的 COMMON LISP 结构，它在其封闭的词法作用域中引入新的宏。编写 **macrolet** 的语法转换和用
**defmacro** 定义全局的宏一样。就像 lisp 会在代码中展开成 **defmacro**-定义的宏，当它 *代码遍历*你的表达式， **macrolet**-定义宏也会被展开。

但 **macrolet** 不仅仅是便捷。与 **defmacro** 相比，用于定义宏， **macrolet** 有很多重要的优
点。首先，如果你想要在一个表达式中给定词法上下文来使用一个不同的宏展开，创建不同的 **macrolet**
语境是必需的。 **defmarco** 并不奏效。

最重要的是， **macrolet** 很有用，因为遍历 COMMON LISP 表达式的代码很难。经常我们有一个任意的 lisp
代码树，因为我们是宏-处理它，然后我们想要改变这个树不同分支的值或含义。为了实现某些确定结构
的临时含义，以及临时重写某些特定宏，可能只是在表达式中词法上下文的特定部分，我们需要
遍历代码。具体来说，需要递归地遍历代码，在它正被求解时寻找我们所需的宏或函数名，然后
用我们自己的表达式在它的位置进行替换。

很简单，对吧？难点在于，很多合法的 lisp 代码段会破坏天真的“代码遍历”的实现。假设我们想
要去对一个特点符号的函数的求解执行特殊替换，如 **blah** ，如果我们被给到以下表达式，就很容易辨识出替
换应该发生：

.. code-block:: none
    :linenos:

    (blah t)

**blah** 出现在一个列表的函数位置，当表达式求解时（它）将会被求解，因此我们应该明显进行替换。目前来说还不错，但如果传入下面这个结构会怎么样呢？

.. code-block:: none
    :linenos:

    '(blah t)

因为表达式是被引用的，这一部分代码意味着返回一个完全按原文的列表。这里进行替换的话就会
出错。所以我们的代码遍历器必须知道在它撞到一个引用时停止，同时不会去替换引用的结构（中
的内容）。很好，这也很简单。但考虑一下其他无法展开 blah 的场景。假如 **blah** 是个词法
变量的变量名呢？

.. code-block:: none
    :linenos:

    (let ((blah t))
      blah)

尽管 **blah** 作为一个列表的第一元素出现，在这里它是出现在 **let** 结构的局部绑定中，并且使用那个绑定，因此不应该被展开。但这也不算太糟糕。我们可以在代码遍历器中添加一些特殊的事例逻辑，这样
代码遍历器就知道在遇到 **let** 结构时该怎么处理。不幸的是，ANSI COMMON LISP 中还有
不止23 个这种的特殊结构 :sup:`【8】` ，这些结构也需要添加特定的逻辑。更重要的是，许多特殊结构复杂到
不能正确的进行遍历。 正如上面所见的 **let** ，比较棘手，而且还有更糟的情况。下面一段
可能合规的 COMMON LISP 代码结构中含有一个 **blah** 的使用需要被展开。但是具体是哪一个呢？

.. hint:: 【8】 
   ANSI CL 中有 25 种特殊形式，23 种没有 let 和 quote 。
   
.. code-block:: none
    :linenos:

    (let (blah (blah (blah blah)))
      blah)

所以说遍历代码是很难的，因为要正确地处理所有特殊结构很困难（见 [special-forms] 和
[USEFUL-LISP-ALGOS]）。注意，对定义为宏的结构，我们不需要任何特殊的事例逻辑。在遇到宏时，
可以简单地展开它，直到它变成函数调用或特殊的结构。如果它是个函数的话，我们知道函数
遵循 lambda 的从左到右且仅执行一次的语义。（当）它是特殊形式时我们（才）需要开发特定的事例逻辑。

听起来好像有大量的工作，不是吗？事实确实是这样的。完整的 COMMON LISP 代码遍历器，
尤其是设计成可移植时，是段庞大且复杂的一块代码。那为什么 COMMON LISP 不为我们提供个用来
遍历 COMMON LISP 代码的接口呢？嗯，事实证明，在某种程度上，COMMON LISP 确实提供了这个接
口，而这个接口的就叫做 **macrolet** 。代码遍历正是 Common Lisp 系统在求解或编译表达式
之前需要做的事情。就像我们假设的代码遍历器一样，COMMON LISP 需要理解并处理 **let**
和其他特殊结构的特殊语义。

因为 COMMON LISP 为了求解它不得不遍历我们的代码，所以通常很少需要一个单独的代码遍历程序。
如果想要以一种关于什么内容确实会被求解的智能方式对表达式进行选择性转换，我们可以简单地将我们的转换编码成一个宏，然后在表达式周围打包一个 **macrolet** 形式。当这个表达式被执行或编译时，
COMMON LISP 会遍历其代码，然后应用由 **macrolet** 指定的宏转换。当然，由于
**macrolet** 定义了这些宏，所以它不会在运行时增加任何额外的开销。 **macrolet** 用于与
COMMON LISP 的代码遍历程序通信，而 COMMON LISP 对宏何时展开的唯一保证是它将在
编译了的函数的运行时之前完成。

使用 **macrolet** 最常见的场景之一就是，当你想假装一个函数绑定在某个词法上下文中，
但你想要的动作是使用这个形式来成为某代码而不是函数调用。 **flet** 和 **labels** 就不行了：他们只能定义
函数。所以我们的选择是去编写一个代码遍历器来寻找这个函数的调用，并将其替换为别的，用 **defmacro**
定义一个全局宏所以该“函数”将会展开成别的，或是将这个结构封装在 **macrolet** 中然后让
系统的代码遍历器来为我们进一步整理。

综上所述，实现一个代码遍历器很难。如果可能的话，我们最好避开这条路。用全局的
**defmacro** 有时可以实现，但通常都有问题。最大的问题是 COMMON LISP 无法保证
宏展开的时间或频次，因此无法可靠地使相同的变量名（而让其）在不同的词法上下文中具有不同的
含义。当重写全局宏时，我们无法确定 COMMON LISP 之前使用过的宏，是否已经展开过，
或者之后是不是还需不需要再次进行展开。

举一个这个代码遍历是有用的的例子，让我们重新讨论在 [3.3 控制结构](chapter03.md) 中忽略
的问题。我们的Scheme “命名的 let （ named let ）”宏的初始版本—— **nlet** ，这个 **label** 的使用允许我们去
临时定义一个函数，以便在“命名的 let （ named let ）”的代码体中使用，这样可能允许我们去递归，就像我们为 **let** 绑定而中再次用新值开始 **let over** 一样。当我们定义这个函数时，我们提起到，因为 COMMON LISP 不能保证它将优化掉“尾部调用”，
所以（这个 **let** 控制结构）每次迭代都可能会占用不必要的额外堆栈空间。换句话说，不同
于 Scheme ， COMMON LISP 函数调用不能保证是“优化的尾部调用”。

即使大部分像样的 COMMON LISP 编译器都会执行适当的 *尾部调用优化* ，有时我们需要
确认优化已经执行过了。最简单的、可移植的完成（此意图的）方法是改变 **nlet** 宏，这样它生成展开式不会使用不必要的堆栈空间。

.. code-block:: none
    :linenos:

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

在 **nlet-tail** 中，我们围绕宏提供的代码体，然后将它封装在一些形式中。我们用 **block** 和
**return-from** 语句来返回最后那个表达式的值，因为我们想要模拟 **let** 结构的行为
和它的隐式 **progn** 。注意，我们为这个块（ **block** ）的名称使用了 **gensym** ，同时在每个 **let**
形参都使用 **gensym** ，来避免不想要的捕获，以及 **loop** 宏 :sup:`【9】` 汇集这
些 **gensyms** 。

.. hint:: 【9】 
  很奇怪 Loop 是 COMMON LISP 中最具争议的问题之一。然而，大多数反对它是完全没有根据的。 Loop 是一种非常方便的领域特定语言，用于循环领域。
   
**nlet-tail** 的使用方式和我们最初的 **nlet** 是一样的，除了非尾部位置处的“命名的 let （ named let ）”是被
禁用的，因为它们会被展开成尾部调用。下面是一个和我们介绍 **nlet** 时使用的一样无趣
的例子，不同的是这个例子中可以保证，即使在不执行尾部调用优化的 lisp 中，也不会
消耗额外的堆栈空间。

.. code-block:: none
    :linenos:

    (defun nlet-tail-fact (n)
      (nlet-tail fact ((n n) (acc 1))
        (if (zerop n)
          acc
          (fact (- n 1) (* acc n)))))

因为这是本节的励志的示例，注意，我们用 **macrolet** 对提供的内容进行代码遍历，来查找
**fact** 的使用。在之前的 **nlet** 使用 **labels** 特殊形式绑定函数的地方，我们希望确保当我们调用
“命名的 let （ named let ）”时不会消耗额外的堆栈空间。从技术上来说，我们希望改变一些词法环境中的
绑定，然后跳转回“命名的 let （ named let ）”的顶部。因此 **nlet-tail** 接受提供了的“命名的 let （ named let ）”，我们上面示例中的 fact ，然后创建一个 *局部宏* ，该宏只在提供的代码主体中生效。这个宏展开的代码，使用
**psetq** 将 **let** 的绑定设为提供的新的值，然后跳转回顶部，不需要堆栈空间。最重要
的是，我们可以在我们的程序中为其他不相关的宏使用 **fact** 这个名字 :sup:`【10】` 。

.. hint:: 【10】 
  不管什么样类型的编程书怎么会没有几个阶乘实现？
   
为了实现这个跳转， **nlet-tail** 使用了 lisp 的特殊结构 —— **tagbody** 和 **go** 的组
合。这两个结构提供了个跳转（ goto ）系统。尽管 goto 给结构化编程带来的问题被广泛讨论着，不管这意味着什么，COMMON LISP 提供这些特殊结构正是我们在这里使用它们的原因。通过
控制 *程序计数器* ，在我们正在执行的代码的当前位置，我们可以创建很有效的宏展开式。虽然在现代高级语
言中，通常都不鼓励使用 **goto** ，但快速浏览任意块的汇编代码，就会发现 **goto** 在我们计算机软件
最底层上非常活跃。即使是最坚定的反 **goto** 倡导者也不建议抛弃像 C 语言和汇编语言这样的低级语言的
**goto** 和跳转（ **jump** ）指令。似乎是这样，在底层编程中，至少编写高效代码，我们只需要 **goto** ，

然而，正如 Alan Kay 所说， lisp 不是一门语言，而是一个构建原料。讨论 lisp 是否是高级还是
低级语言完全没有意义。这里有有很高级的 lisp ，如特定域（ domain specific ）语言。通过我们编写的
用于处理这些语言的宏，我们将它们的用法转换为较低层次的 lisp 。当然，这些展开式也是
lisp 代码，只是不像原始版本那样压缩。接下来，我们通常将这个中间层次的 lisp 代码交给编译器，
编译器会成功地将这些代码转换为越来越低级别的 lisp 代码。用不了多久，诸如 **go-to** 、条件分支和位
填充等概念就会出现在代码中，但即便如此，代码仍还是 lisp 。最后，使用本地代码编译器，
你的高级 lisp 程序将会被向下层转换成汇编语言。但即便如此，你的程序依然还是 lisp 。这是因为大部分 lisp
汇编程序都是用 lisp 本身编写的，所以很自然地将这些汇编程序都保存为 lisp 对象，这样就产生
真正的 lisp 底层程序。只有程序真正组装成二进制机器码时，它才不再是 lisp 。难道不是吗？

高阶或低阶的区别在 lisp 中不适用，lisp 程序的级别完全取决于视角。 Lisp 不是门语言，而是
迄今为止所发现的最灵活的软件构建原料。


.. _5-5-recursive-expansions:

5.5 递归展开式
==============================

在用例子教初学者 lisp 时，在课程中不可避免地会出现一个问题

..

  cadr 是什么玩意?

这时有两种方法来继续。第一种方法就是向学生解释 lisp 的列表（ list ）是由 **cons**
单元组成，每个 **cons** 单元都有两个指针： **car** 和 **cdr** 。一旦理解了这个概念，就很容易
展示如何将这些指针的访问器函数（也称为 **car** 和 **cdr** ），这两个函数可以组合成 **cadr**
函数，而 **cadr** 函数会遍历列表并检索列表中的第二个元素。

第二种方法就是给学生引入 **second** 这个 COMMON LISP 函数，然后完全地忽略 **cadr** 。而
**cadr** 和 **second** 效果是一样的：获取列表中的第二个元素。不同之处在于 **second** 是以它做的事命名，而 **cadr** 是以它如何做事来命名的。 **cadr** 是 *显然的具体说明* ，而
**second** 是个容易记住的函数名，但它不合需要地掩盖了操作的含义 :sup:`【11】` 。 易读的具体说明通常更好，
因为我们可以考虑使用 **cadr** 函数来不仅仅是获取列表的第二个元素。 例如，我们显然可以用
**cadr** 作为获取 **lambda** 形式的参数解构列表的概念。 **cadr** 和 **second** 在底层都执行相同的任务，但可以概念上代表不同的操作。

.. hint:: 【11】 
  特别是因为 second 与 cadr 完全相同：您不能使用它来获取其他序列（如向量）的第二个元素。
   
对易读的具体说明来说，比哲学偏好更重要的是， **car** 和 **cdr** 的组合可以表示更多的列表访问操作，
而且比一小撮英文词组的访问器更加一致。 **car** 和 **cdr** 用处很大，因为可以把他们组合成新的、任意
的函数。例如， **(cadadr x)** 和 **(car (cdr (car (cdr x))))** 是一样的。COMMON
LISP 要求必须定义长度不大于 4 的 **car** 和 **cdr** 的所有组合。 因此，尽管没有函数
**second-of-second** 用于获取列表的第二个元素，然后将其作为列表并获取其第二个元素，
但可以使用 **cadadr** 达到这个效果。

这些预定义的 **car** 和 **cdr** 的组合用在函数的 **:key** 访问参数（关键字参数）上真的很方便，像
**find** ：

.. code-block:: none
    :linenos:

    * (find 'a
        '(((a b) (c d)) ((c d) (b a)))
        :key #'cadadr)

    ((C D) (B A))

使用我们预定义的 **cadadr** 访问器比构建一个等价的英文访问器组合的 lambda 表达式要更精确。

.. code-block:: none
    :linenos:

    * (find 'a
        '(((a b) (c d)) ((c d) (b a)))
        :key (lambda (e)
              (second (second e))))

    ((C D) (B A))

COMMON LISP 也提供了函数 **nth** 和 **nthcdr** ，他们可以用作通用访问器，比如说，
在不能确切地知道编译时想要获取哪个元素。 **nth** 的定义很简单：对列表取 n 次 **cdr** ，
然后取一下 car 。 所以 **(nth 2 list)** 与 **(caddr list)** 、 **(third list)** 是一样
的。 **nthcdr** 也一样，只是它不做最后的 car ： **(nthcdr 2 list)** :sup:`【12】` 和 **(cddr list)** 是一样的。

.. hint:: 【12】 
  由于 COMMON LISP 的第二个命名空间，我们可以轻松地使用 list 作为变量名。这样的例子在像 Scheme 这样的单命名空间 lisp 中会出现问题。
   
但是，如果 **cons** 结构中的位置不能通过上述模式之一，如 **nth** 或 **nthcdr** 访问，就需
要组合访问器。不得不组合不一致的抽象来完成任务通常表明不完整。 我们能否为访问列表的领域定义
一种领域特定语言，以便将这些 **car** 和 **cdr** 组合函数、英语访问器以及像 **nth** 和
**nthcdr** 这样的函数结合起来？

既然 **car** 和 **cdr** 是基础操作符，我们的语言应该有完全通用的方式组合这两个访问器。因为
有无数种这样的组合，为每个可能的访问器定义函数来继续组合显然是不可行的。 我们真正想要
的是一个可以扩展为高效列表遍历代码的宏。

.. code-block:: none
    :linenos:

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

以 C 开头，后面跟着一个或多个 A 或 D 字符，以 R 结尾，指定列表访问器函数的语法非常直观，
这大致就是我们想要为我们的语言复制的内容。宏 **cxr%** 是这些访问器的双关语，其中一个或多个
A 或 D 字符被替换为 X :sup:`【13】` 。 在 **cxr%** 中，第一个参数是个列表，列表中指定这些了 A 和 D。这个
列表是数字和符号 A 或 D 的交替组合。

.. hint:: 【13】 
  除了本质上，cxr 与 Maclisp 中访问大块插槽的 cxr 无关。
   
例如，即使 COMMON LISP 没有提供一个英文单词的函数来访问列表的第十一个元素，我们也可以
简单地定义出来：

.. code-block:: none
    :linenos:

    (defun eleventh (x)
      (cxr% (1 a 10 d) x))

本节的重点是说明 *递归展开* 的实际用途。当一个宏将一个形式展开位将一个结构展开为一个新的结构时，递归展开就会出现。 与所有递归一样，此过程必须有个基本的终止条件。宏最终会展开
为不包含使用相关宏的结构，然后这个展开器就会结束。

下面我们将 **cxr%** 宏的实例宏展开（ **macroexpand** ）成一个同样使用 **cxr%** 的结构：

.. code-block:: none
    :linenos:

    * (macroexpand
        '(cxr% (1 a 2 d) some-list))

    (CAR (CXR% (2 D) SOME-LIST))
    T

当我们拷贝这个新的递归结构，然后宏展开它，又会得到一个递归：

.. code-block:: none
    :linenos:

    * (macroexpand
        '(CXR% (2 D) SOME-LIST))

    (CDR (CXR% (1 D) SOME-LIST))
    T

下面这个递归的结果展示了 **xcr%** 另一种可能的用法：空列表访问器 :sup:`【14】` ：

.. hint:: 【14】 
  如果 COMMON LISP 包含这个，它可能被称为 cr 。
   
.. code-block:: none
    :linenos:

    * (macroexpnad
        '(CXR% (1 D) SOME-LIST))

    (CDR (CXR% NIL SOME-LIST))
    T

空列表访问器就是基本终止条件，然后直接展开被访问的列表：

.. code-block:: none
    :linenos:

    * (macroexpand
        '(CXR% NIL SOME-LIST))

    SOME-LIST
    T

用 CMUCL 的拓展（函数） **macroexpand-all** （一个完整的代码遍历器组件），可以看到
**cxr%** 结构的整体展开式：

.. code-block:: none
    :linenos:

    * (walker:macroexpand-all
        '(cxr% (1 a 2 d) some-list))
    (CAR (CDR (CDR SOME-LIST)))

多亏了我们出色的 lisp 编译器，就所有的意图和目的而言， **cxr%** 的使用和 **caddr** 与 **third** 一样。

但是，正如名字暗示的那样， **cxr%** 还不完善。这只是我们最终版宏 **cxr** 的第一版草稿。我们这个草稿的第一个问题
就是只接受整型作为 A 和 D 的数目。因为这个限制，有些 **nth** 和 **nthcdr** 能做的事情我们的宏
却做不到。

我们需要检查将非整数作为 A 或 D 符号的数字前缀的情况。 在这种情况下，我们的代码展开式应该
求解所提供的内容，并将此值 :sup:`【15】` 用作要遍历的 **cars** 或 **cdrs** 的数量。

.. hint:: 【15】 
  希望这个值应该是一个数字。在 lisp 中，我们可以放心地将这种情况留给 lisp 的异常系统来处理和描述给程序员。
   
**cxr%** 的第二个问题是，当给到特别巨大的数字作为 A 和 D 符号的前缀时， **cxr%** 会 *内联* 所有的 **car**
和 **cdr** 的组合。对小的数字来说，内联可以提高性能，但通常内联过多的 **car** 和 **cdr**
没有意义； 相反，应该使用像 **nth** 或 **nthcdr** 这样的循环函数。

为了修复这两种情况，我们添加了一个替代展开式。如果 A 或 D 前面的参数不是整型的话，就会调用
新的操作，而且，如果我们不想内联大量的 **car** 或 **cdr** ，也可以选择调用新的操作。任选 *内联
阈值* 为 10，这个新的操作由 **cxr** 宏提供。

.. code-block:: none
    :linenos:

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

使用 **cxr** ，我们可以直接根据 **car** 和 **cdr** 的具体说明来定义 **nthcdr** ：

.. code-block:: none
    :linenos:

    (defun nthcdr% (n list)
      (cxr (n d) list))

同样的， **nth** ：

.. code-block:: none
    :linenos:

    (defun nth% (n list)
      (cxr (1 a n d) list))

因为编写宏是个迭代的、分层次的过程，我们经常被鼓励 *组合* 或 *结合* 我们之前实现的宏。
例如，在 **cxr** 的定义中，替代展开式用到了上一节中定义的宏： **nlet-tail** 。
**nlet-tail** 很方便，因为它允许我们给迭代构造命名，同时，因为我们只计划将迭代作为
尾部调用，我们就能确保我们可以使用它来避免不必要的堆栈消耗。

下面是 **xcr** 在 **nthcdr%** 中的展开：

.. code-block:: none
    :linenos:

    * (macroexpand
      '(cxr (n d) list))
    (LET ()
      (NLET-TAIL #:NAME1632
                ((#:COUNT1633 N)
                  (#:VAL1634 (CXR NIL LIST)))
        (IF (>= 0 #:COUNT1633)
          #:VAL1634
          (#:NAME1632 (- #:COUNT1633 1)
    T

注意，复杂的宏展开式经常写的是程序员从不会去写的代码。特别要注意 **nil cxrs** 的使用
和无效 **let** 的使用，这两者都剩下的是留待进一步的宏展开和编译器优化。

因为宏可以让宏的用户看到更多的展开式，所以易读的具体说明使通常在其他语言中是不可能实现的方式成为了可能。例如，
正如每个 **cxr** 的设计，当 A 和 D 前面的参数是小于 **cxr-inline-thresh** 的整数时，
就会被内联调用给 **car** 和 **cdr** ：

.. code-block:: none
    :linenos:

    * (macroexpand '(cxr (9 d) list))
    (LET ()
      (CDR (CXR (8 D) LIST)))
    T

但多亏了 **cxr** 的易读的具体说明，我们可以传递一个值，尽管它本身不是整数，但在计算时将
成为整数。当我们这么做时，我们知道不会有内联发生，因为这个宏会造就 **nlet-tail** 展开式。
求解为一个整数的最简单的形式就是简单的将那个整数引用起来：

.. code-block:: none
    :linenos:

    * (macroexpand '(cxr ('9 d) list))
    (LET ()
      (NLET-TAIL #:NAME1638
                ((#:COUNT1639 '9)
                  (#:VAL1640 (CXR NIL LIST)))
        (IF (>= 0 #:COUNT1639)
          #:VAL1640
          (#:NAME1638 (- #:COUNT1639 1)
    T

通常我们会发现将宏组合起来很有用： **cxr** 可以展开成之前写的宏 **nlet-tail** 。同样的，
有时将宏自身组合起来也很有用，这样就会有递归展开。


.. _5-6-recursive-solutions:

5.6 递归方案
==============================

似乎在上一节中我们定义的 **cxr** 宏已经包含了函数 **car** 和 **cdr** 的组合，以及普通的扁平列表
**(flat list)** 访问器函数 **nth** 和 **nthcdr** 。但是像 **first**, **second** 和 **tenth**
这样的英语访问器呢？

这些函数没有用吗？绝对不是。当表示访问列表中第四个元素的操作时，不论是在书写代码
或是阅读代码的效率上，用 **fourth** 肯定要比数 **cadddr** 中三个 D 要更好。

事实上，英文单词访问器最大的问题是： COMMON LISP 中只有 10 个访问器 —— 从
**first** 到 **tenth** 。但是本节或者说本书的主题之一是，lisp 洋葱（  lisp onion ）的每一层都可以使用
其他层。在 lisp 中没有原语。如果我们想定义更多的英文单词访问器，如 **eleventh** ，很容易
就能做到，就像之前展示的那样。用 **defun** 定义的 **eleventh** 函数与 ANSI 中定义的
**first** 和 **tenth** 访问器没有差别。因为没有原语，并且我们可以在我们的宏的定义中使用所有的
lisp ，所以我们可以在宏定义中受益像 **loop** 和 **format**  :sup:`【16】` 这样的高级特性。

.. hint:: 【16】 
  format 是 COMMON LISP 的一个颇有争议的特性。然而，就像对 loop 的反对意见一样，大多数都是基于对领域特定语言的概念和范围的误解。
   
.. code-block:: none
    :linenos:

    (defmacro def-english-list-accessors (start end)
      (if (not (<= 1 start end))
        (error "Bad start/end range"))
      `(progn
        ,@(loop for i from start to end collect
            `(defun
              ,(symb
                (map 'string
                    (lambda (c)
                        (if (alpha -char -p c)
                          (char-upcase c)
                          #\ -))
                    (format nil "~:r" i)))
              (arg)
              (cxr (1 a ,(- i 1) d) arg)))))

宏 **def-english-list-accessors** 使用格式字符串 **"~:r"** 将（传入的）数字（参数） **i** 转换为
对应英文单词的字符串。按照 lisp 的习惯，我们将所有非字母字符改为连字符。然后
将这个字符串转换为一个符号，然后在 **defun** 结构中使用它，这个字符串运用 **cxr**
宏实现了适当的访问器功能。

例如，我们突然意识到我们需要访问列表的第十一个元素。当然，我们可以用 **nth** 或是
**cdr** 的组合以及英文单词访问器，但这会导致代码风格的不一致。我们可以重写
代码来避免使用英文单词访问器，但是在第一处选择用这种抽象（备注：指英文单词访问器）可能是有原因的。

终于，我们可以自定义缺少的必要的访问器了。在其他语言中，这通常意味着大量的
复制粘贴，或者可能是一些特殊情况下的代码生成脚本，而这两者都不是特别优雅。
但在 lisp 中，我们有宏：

.. code-block:: none
    :linenos:

    * (macroexpand
      '(def-english-list-accessors 11 20))
    (PROGN
      (DEFUN ELEVENTH (ARG) (CXR (1 A 10 D) ARG))
      (DEFUN TWELFTH (ARG) (CXR (1 A 11 D) ARG))
      (DEFUN THIRTEENTH (ARG) (CXR (1 A 12 D) ARG))
      (DEFUN FOURTEENTH (ARG) (CXR (1 A 13 D) ARG))
      (DEFUN FIFTEENTH (ARG) (CXR (1 A 14 D) ARG))
      (DEFUN SIXTEENTH (ARG) (CXR (1 A 15 D) ARG))
      (DEFUN SEVENTEENTH (ARG) (CXR (1 A 16 D) ARG))
      (DEFUN EIGHTEENTH (ARG) (CXR (1 A 17 D) ARG))
      (DEFUN NINETEENTH (ARG) (CXR (1 A 18 D) ARG))
      (DEFUN TWENTIETH (ARG) (CXR (1 A 19 D) ARG)))
    T

能够创建这些英文单词访问器降低了 ANSI COMMON LISP 中只有十个访问器限制的影响。
如果我们想要更多的英语访问器，只需使用 **def- english-list-accessors** 宏来
创建它们。

那关于 ANSI 里面 **car** 和 **cdr** 的组合最多只能是 5 个的限制怎么处理呢？有时，
在编写处理复杂列表的程序时，我们希望有一个无此限制的访问器。例如，如果我们使用函数
**cadadr** 、 **second-of-second** 来访问列表，并且改变数据表示形式，因此，限制的引用需要是 **second-of-third** 或 **cadaddr** ，我们遇到了这个 COMMON LISP 的
限制。

像我们对英文单词访问器做的那样，我们可以书写一个程序来定义额外的 **car** 和 **cdr** 组合。
问题在于，与英文访问器不同，像 **caddr** 这样的组合函数，其深度的增加会导致
需要定义的函数数量呈指数级增加。具体来说，可以使用函数 **cxr-calculator**
找到需要定义的深度为 n 访问器数量。

.. code-block:: none
    :linenos:

    (defun cxr-calculator (n)
      (loop for i from 1 to n
            sum (expt 2 i)))

我们看到 ANSI 规范指定的（深度为 4 的组合）有 30 种：

.. code-block:: none
    :linenos:

    * (cxr-calculator 4)

    30

为了让你了解所需函数的数量增长有多快，参考下面这段代码：

.. code-block:: none
    :linenos:

    * (loop for i from 1 to 16
            collect (cxr-calculator i))

    (2 6 14 30 62 126 254 510 1022 2046
    4094 8190 16382 32766 65534 131070)

显然，要想 **cxr** 函数在深度上包含 **car** 和 **cdr** 的所有组合，我们需要一种
不同于处理英文访问器问题的方法。定义 **car** 和 **cdr** 的所有组合到某个可接受
的深度是不行的。

.. code-block:: none
    :linenos:

    (defun cxr-symbol-p (s)
      (if (symbolp s)
        (let ((chars (coerce
                        (symbol -name s)
                        'list)))
          (and
            (< 6 (length chars))
            (char= #\C (car chars))
            (char= #\R (car (last chars)))
            (null (remove -if
                    (lambda (c)
                      (or (char= c #\A)
                          (char= c #\D)))
                    (cdr (butlast chars))))))))

首先，我们应该对 **cxr** 符号定义有个可靠的说明。 **cxr-symbol-p** 是个简洁
的定义： **cxr** 是所有以 C 开头，R 结尾，中间包含五个及以上个 A 或 D 的符号。
我们不想要去考虑少于五个 A 或 D 的 **cxr** 符号，因为这些函数已经确保在 COMMON
LISP 中被定义了 :sup:`【17】` 。

.. hint:: 【17】 
  重新绑定 COMMON LISP 指定的函数是被禁止的。
   
接下来，因为我们打算用 **cxr** 来实现任意 **car** 和 **cdr** 组合的功能，我们创建了
函数 **cxr-symbol-to-cxr-list** 。
.. code-block:: none
    :linenos:

    (defun cxr-symbol-to-cxr-list (s)
      (labels ((collect (l)
                (if l (list*
                        1
                        (if (char= (car l) #\A)
                          'A
                          'D)
                        (collect (cdr l))))))
      (collect
        (cdr       ; chop off C
          (butlast ; chop off R
            (coerce
                (symbol -name s)
                'list))))))

**cxr-symbol-to-cxr-list** 函数用来将 **cxr** 符号（由 **cxr-symbol-p** 定义）
转换为一个可以用作 **cxr** 第一个参数的列表 :sup:`【18】` 。下面是它的用法示例：

.. hint:: 【18】 
  有趣的是，已弃用的函数 explode 可能在这种情况下被证明是有用的，但却被 COMMON LISP 排除在外，因为没有人能想到它有什么好的用途。
   
.. code-block:: none
    :linenos:

    * (cxr-symbol-to-cxr-list
        'caddadr)
    (1 A 1 D 1 D 1 A 1 D)

注意 **cxr-symbol-to-cxr-list** 中 **list*** 函数的用法。**list*** 基本和 **list**
一致，除了它的最后一个参数会被插入到已创建列表中最后一个 **cons** 单元格的 **cdr**
位置。当编写递归函数构建一个列表（其中每个堆栈结构可能想向列表中添加多个元素）
时， **list*** 就非常方便，。在我们的例子中，每个结构都想向列表中添加两个元素：
数字 1 和符号 A 或 D。

最后，我们决定有效地提供任意深度的 **cxr** 函数的唯一方法是，对提供的表达式进行
代码遍历并只定义必要的函数。 **with-all-cxrs** 宏使用 Graham 的 **flatten** 实用
程序对所提供的表达式进行代码遍历，方法与 [3.5 异常捕获]中的 **defmacro/g!** 宏一样。
**with -all-cxrs** 寻找所有满足 **cxr-symbol-p** 的符号，用 **cxr** 宏创建它们引用的函
数，然后用 labels 形式将这些函数绑定到提供的代码周围 :sup:`【19】` 。

.. hint:: 【19】 
  这种方法的一个问题是此类访问器将不会是可设置的。
   
.. code-block:: none
    :linenos:

    (defmacro with-all-cxrs (&rest forms)
      `(labels
        (,@(mapcar
            (lambda (s)
              `(,s (l)
                (cxr ,(cxr-symbol-to-cxr-list s)
                      l)))
            (remove -duplicates
              (remove-if-not
                #'cxr-symbol-p
		(flatten forms)))))
		,@forms))

现在我们可以封装传给 **with-all-cxrs** 结构中的表达式，并假定这些表达式可以访问任何
可能的 **cxr** 函数。我们可以，如果我们选择的话，简单地返回这些函数以便在别处使用：

.. code-block:: none
    :linenos:

    * (with-all-cxrs #'cadadadadadr)

    #<Interpreted Function>

或者，如下面的宏展开式所示，我们可以使用这个无限的类嵌入任意复杂的 lisp 代码:

.. code-block:: none
    :linenos:

    * (macroexpand
      '(with-all-cxrs
          (cons
            (cadadadr list)
            (caaaaaaaar list))))
    (LABELS
      ((CADADADR (L)
        (CXR (1 A 1 D 1 A 1 D 1 A 1 D) L))
      (CAAAAAAAAR (L)
        (CXR (1 A 1 A 1 A 1 A 1 A 1 A 1 A 1 A) L)))
      (CONS
        (CADADADR LIST)
        (CAAAAAAAAR LIST)))
    T

通常，一个听起来很难的任务，如定义无限个英文列表访问器和 **car-cdr** 组合，
其实就是将简单的问题聚合到一起。与之相反，对单个难题，可以通过递归处理问题
来解决一系列较简单的问题。通过思考如何将一个问题转化为一系列更简单的问题，
我们采用了经过验证的解决方法：*分而治之* 。


.. _5-7-dlambda:

5.7 Dlambda
==============================

在讨论闭包时，我们提到了怎么将闭包当作对象使用，以及一般情况下，不确定范围和词法
作用域能够替代复杂的对象系统。但是，到目前为止，我们忽略了对象通常都有的一个特性：
多 *方法* 。换句话说，虽然我们简单的计数器闭包示例只允许一个操作，即增量，但对象通常
希望能够用不同的行为响应不同的 *消息* 。

尽管闭包可以被认为是个只有一个方法（ **apply** ）的对象，但这个方法可以根据传递给它的参数
来具备不同的行为。例如，如果我们将第一个参数指定为表示正在传递消息
的符号，则可以基于第一个参数用简单的 **case** 语句提供多个行为。

为实现一个具有增加和减少方法的计数器，我们可能会这样写：

.. code-block:: none
    :linenos:

    (let ((count 0))
      (lambda (msg)
        (case msg
          ((:inc)
            (incf count))
          ((:dec)
            (decf count)))))

注意，我们已经选择了 *关键字符号* ，也就是以冒号 **:** 开头的符号，并且总是求解为自身用来
指示消息。关键字很方便，因为我们不需要引用它们或将它们从包中导出，而且也很直观，因为它们
就是被设计用来执行这个和其他的各种 *解构* 。通常在 **lambda** 或 **defmacro** 结构中，关键字
在 *运行时* 不会被 *解构* 。但是由于我们正在实现一个消息传递系统，其是一类运行时解构，
所以我们将关键字处理操作留在运行时执行。如前所述，符号的解构是个高效的操作（只是指针比较）。在我们的计数器例子被编译后，它可能会被缩减为以下机器码：

.. code-block:: none
    :linenos:

    2FC:       MOV  EAX, [#x582701E4]  ; :INC
    302:       CMP  [EBP-12], EAX
    305:       JEQ  L3
    307:       MOV  EAX, [#x582701E8]  ; :DEC
    30D:       CMP  [EBP-12], EAX
    310:       JEQ  L2

但为了方便起见，我们希望避免必须为我们创建的每个对象或类书写条件语句。像这种情况就需要宏了。我喜欢用的宏是 **dlambda** ，他会展开成 lambda 结构。这个展开式包括一种方法，
这个方法可以根据它被应用的参数来执行许多不同的代码分支。这种运行时解构的类型就是
**dlambda** 名称的来源：它是 **lambda** 的 *解构* 或 *分发* 版本。

.. code-block:: none
    :linenos:

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

**dlambda** 被设计为传递关键字符号作为第一个参数。根据使用的关键字符号， **dlambda** 将执行相应
的代码段。例如，我们喜欢的闭包例子：简单的计数器，可以使用 **dlambda** ，根据第一个
参数增加或减少计数。这被称为 **let over dlambda** 模式：

.. code-block:: none
    :linenos:

    * (setf (symbol-function 'count-test)
        (let ((count 0))
          (dlambda
            (:inc () (incf count))
            (:dec () (decf count)))))

    #<Interpreted Function>

既可以递增

.. code-block:: none
    :linenos:

    * (count-test :inc)

    1

也可以递减

.. code-block:: none
    :linenos:

    * (count-test :dec)

    0

闭包取决于传递的第一个参数。尽管在上面的 let over dlambda 中为空，关键字符号后面
的列表实际上是 *lambda 解构* 列表。每个分发实例，或者说每个关键字参数，都可以有它自身特定
的 lambda 解构列表，就像下面对计数器闭包的增强:

.. code-block:: none
    :linenos:

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

现在，我们有几个不同的 lambda 解构列表可以使用，取决于第一个关键词参数，
**:reset** 不需要参数，然后会将 **count** 回置为 0 ：

.. code-block:: none
    :linenos:

    * (count-test :reset)

    0

**:inc** 和 **:dec** 都接受数字参数， **n** ：

.. code-block:: none
    :linenos:

    * (count-test :inc 100)

    100

**:bound** 确保 **count** 的值时在 *边界值* **lo** 和 **hi** 之中。若 **count** 的值落在
边界值之外，那么它会变成离该值较近的那个边界值：

.. code-block:: none
    :linenos:

    * (count-test :bound -10 10)

    10

.. note::
  上面代码的结果之所以为 10 是因为上面的值已经将 **count** 设置为 100 了，加上了
  **:bond** 后就变成 10 了

**dlambda** 一个重要的属性是，它使用 lambda 进行所有的解构，因此保留了由COMMON LISP 环境提供的正常的错误检查和调试（ debugging ）。例如，当我们只给到 **count-test**
一个参数时，就会直接得到一个和 lambda 程序类似的报错：

.. code-block:: none
    :linenos:

    * (count-test :bond -10)

    ERROR: Wrong argument count, wanted 2 and got 1.

特别是当 **dlambda** 嵌入到词法环境中形成一个闭包， **dlambda** 允许我们使用面向
对象的术语编程，就像我们正在创建一个具有多 *方法* 的对象一样。 **dlambda** 被定制，为了在不偏离
lambda 语法和用法的情况下，让函数式（编程）更简单。 **dlambda** 仍然会展开成单个
lambda 表达式，因此，它的求值结果与对 **lambda** 求值的结果完全相同：一个可以保存、
应用的 *匿名函数* ，最重要的是，可以将这个 lambda 控件用作词法闭包。

但 **dlambda** 将这种与 **lambda** 的同步更进一步。为了让 **dlambda** 提供一个包含 **lambda** 宏的代码的尽可能平滑的转换， **dlambda** 也允许我们不传递一个关键字参数作为第一个符号来处理匿名函数的调用。当我们拥有通过正常的 **lambda** 接口使用闭包写就的大量的代码时，
我们希望能够添加特殊情况的 **dlambda** 方法，而不改变其他代码调用接口的方式。

如果说最后可能的方法是给定符号 **t** 而不是关键字参数，在没有发现任何特殊情况的
关键字参数方法适用时，所提供的方法将总是被调用。以下是个特意编造的例子：

.. code-block:: none
    :linenos:

    * (setf (symbol-function 'dlambda-test)
        (dlambda
          (:something-special ()
            (format t "SPECIAL~%"))
          (t (&rest args)
            (format t "DEFAULT: ~a~%" args))))

    #<Interpreted Function>

有了这个定义，调用该函数的主要方法调用默认情况。我们的默认情况使用 **&rest** （剩余参数）的形式的 lambda ，解构
参数来接收所有可能的参数，我们可以通过提供更具体的 lambda 解构参数来
自由地缩小接受到的参数。

.. code-block:: none
    :linenos:

    * (dlambda-test 1 2 3)
    DEFAULT: (1 2 3)
    NIL
    * (dlambda-test)
    DEFAULT: NIL
    NIL

然而，尽管这个匿名函数的行为很像用默认情况定义的常规 lambda 结构，但我们可以
传递一个关键字参数来调用这个特殊方法。

.. code-block:: none
    :linenos:

    * (dlambda-test :something-special)
    SPECIAL
    NIL

一个关键特性(后面的章节将会大量利用)是，默认方法和所有特殊方法当然都在包含
**dlambda** 的词法上下文中被调用。由于 **dlambda** 与 **lambda** 表示法集成得非常紧密，
这使得我们可以将多方法技术引入到创建和扩展词法闭包的领域。
