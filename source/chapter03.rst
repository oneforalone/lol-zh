.. _chapter03:

********************
第三章：宏基础
********************


.. _3-1-iterative:

3.1 迭代开发
=======================

::

  Lisp has assisted a number of our most gifted fellow humans in thinking
  previously impossible thoughts. -—Edsger Dijkstra

宏的构建是个迭代的过程：所有复杂的宏都来自简单的宏。从一个想法开始后，可以创建一个粗略
的实现，最终的宏尤其演变而成，就像雕塑来自石头一样。如果粗略的实现不够灵活，或导致低效
或危险的展开，专业的宏程序员会稍微修改宏，添加功能或删除错误，直到满足所有要求。

宏构造的这种迭代过程的必要性部分是因为这是一般最有效的编程风格，部分是因为宏编程比其他
类型的编程更复杂。因为宏编程要求程序员考虑在多个时间点执行的多个级别的代码，所以复杂性
问题比其他类型的编程更迅速地扩展。与在没有这种持续反馈的情况下编写整个宏相比，迭代过程
有利于确保概念模型更接近于实际创建的内容。

在本章中，我们将通过介绍两个常见的宏概念来编写一些基本的宏：*领域特定语言* :sup:`（1.1）`  和 *控制结构* :sup:`（1.2）`  。 一
旦讲清了这些通用宏概念，就回到讨论编写宏本身的过程。 然后将介绍异常捕获和自由变量注入等
技术，以及用于定义 lisp 宏的新语法的定义，该语法将在本书的其余部分中使用。

.. note:: （1）
  领域特定语言 domain specific languages； 控制结构 control structures

.. _3-2-domain-specific:

3.2 领域特定语言
======================

COMMON LISP 与大多数其他编程环境一样，也有 **sleep** 函数 ，**sleep** 函数会让进程
休眠 **n** 秒，其中 **n** 是一个非负数、非复杂的数字。 例如，想要休眠 3 分钟（180 秒
的话），可以执行下面的语句：

.. code-block:: none

    (sleep 180)

或者，如果喜欢以分钟为单位进行休眠的话，可以改为：

.. code-block:: none

    (sleep (* 3 60))

因为编译器知道如何 *折叠常量* :sup:`（2）`  ，所以上面两个调用都能正常执行。为了更明确地说明我们在做什
么，可以定义个 **sleep-minutes** 函数:

.. note:: （2）
  折叠常量 fold constants

.. code-block:: none

    (defun sleep-minutes (m)
      (sleep (* m 60)))

为想要使用的每个时间单位定义新函数既笨重又不方便。 我们需要的是某种抽象，可以指定时间单位和
值。 我们真正需要的是一种 *特定领域的语言* 。

到目前为止，lisp 的解决方案与任何其他语言的解决方案相同：创建一个函数，该函数接受一个值和一个单
位，并返回该值乘以与给定单位相关的某个常数。 但是当我们考虑代表这个单元的选项时，一个 lispy 改
进变得很明显。 在像 C 这样的语言中，习惯上使用像 int 这样的底层数据类型并分配对应于不同单位的
任意值：

.. code-block:: c

    #define UNIT_SECONDS 1
    #define UNIT_MINUTES 2
    #define UNIT_HOURS 3

    int sleep_units(int value, int unit) {
      switch(value) {
        case UNIT_SECONDS: return value;
        case UNIT_MINUTES: return value*60;
        case UNIT_HOURS: return value*3600;
      }
    }

.. code-block:: none

    (defun sleep-units% (value unit)
      (sleep
        (* value
          (case unit
            ((s) 1)
            ((m) 60)
            ((h) 3600)
            ((d) 86400)
            ((ms) 1/1000)
            ((us) 1/1000000)))))

但在 lisp 中，最明显的方法就是使用符号来代表所需单元。 lisp 中的符号大部分与其他符号不相
等。**eq** 是最快的 lisp 比较运算符，大致对应于指针比较。 由于可以非常快速地比较指针，因此符
号提供了一种非常快速和方便的方法来让两个或多个不同的 lisp 表达式知道指向同一个值。 在 lisp
中，可以定义 **sleep-units%** 函数，这样就能指定单位：

.. code-block:: none

    (sleep-units% 2 'm)
    (sleep-units% 500 'us)

因为比较符号只需要一个指针比较，**sleep-units%** 会编译成一个很快的运行时调度：

.. code-block:: none

    524:       CMP     ESI, [#x586FC4D0]    ; 'S
    52A:       JEQ     L11
    530:       CMP     ESI, [#x586FC4D4]    ; 'M
    536:       JEQ     L10
    538:       CMP     ESI, [#x586FC4D8]    ; 'H
    53E:       JEQ     L9
    540:       CMP     ESI, [#x586FC4DC]    ; 'D
    546:       JEQ     L8

注意 **sleep-units%** 参数中的 uint 必须要引用。 因为当 lisp 执行函数时，它首先执行计算所
有参数，然后将结果绑定到变量后再在函数内部使用。 数字和字符串以及其他原语会自动执行计算，这就是
为什么不需要引用赋予 **sleep-units%** 的数值的原因。 但请注意，它们已被计算，因此只要愿意的
话，都可以进行引用：

.. code-block:: none

    (sleep-units% '.5 'h)

然而，符号通常不会对自己进行计算 :sup:`【1】` 。当 lisp 计算执行符号时，会假定正在引用一个变量并尝试在给定的
词法上下文的情况下查找与该变量关联的值（除非该变量被声明为特殊的，在这种情况下是动态环境）。

.. hint:: 【1】
  一般来说，没有规则没有例外。有些符号会对自己求值，例如：**t** 、 **nil** 和关键字。

.. code-block:: none

    (defmacro sleep-units (value unit)
      `(sleep
        (* ,value
            ,(case unit
              ((s) 1)
              ((m) 60)
              ((h) 3600)
              ((d) 86400)
              ((ms) 1/1000)
              ((us) 1/1000000)))))

为了避免引用单位，我们需要一个宏。 与函数不同，宏不计算其参数。 利用这一事实，我们将
**sleep-units%** 函数换成 **sleep-units** 宏。 现在就不需要引用单位：

.. code-block:: none

    (sleep-units .5 h)

尽管这个宏的主要目的是避免引用 **unit** 参数，但这个宏甚至比函数更高效，因为根本没有运行时调
度：单位和乘数在编译时是已知的。当然，每当发现这种好得令人难以置信的情况时，它可能真的好得令人难
以置信。 这种效率提升并不是免费的。 通过前面的运行时调度，我们失去了在运行时确定时间单位的能
力。 这让这个宏无法执行以下代码：

.. code-block:: none

    (sleep-units 1 (if super-slow-mode 'd 'h))

上面这段代码将无法执行，因为 **sleep-units** 第二个参数要是 case 语句中的符号，而是上面代码
中是个列表，其中第一个元素是符号 **if** 。

.. code-block:: none

    (defmacro unit-of-time (value unit)
      `(* ,value
          ,(case unit
            ((s) 1)
            ((m) 60)
            ((h) 3600)
            ((d) 86400)
            ((ms) 1/1000)
            ((us) 1/1000000))))

回想一下，大多数宏都是为了创建更方便和有用的编程抽象而编写的，而不是为了提高底层代码的效率。 是
否可以从这段代码中提取任何习语，使其对程序的其余部分（以及可能的其他未来程序）更有用？ 即使是现
在，我们也可以预见想要用时间值做其他事情，而不仅仅是调用它们休眠。时间单位宏从
**sleep-units** 宏中抽象出功能，返回一个值而不是对其调用 **sleep**。**value** 参数可以在
运行时确定，因为它会被计算，但 **unit** 不能，因为在编译时需要这个信息，就像
**sleep-units** 一样。 以下是示例：

.. code-block:: none

    * (unit-of-time 1 d)

    86400

像 **unit-of-time** 这样简单的宏为解决特定领域的问题提供了更好的语法，并且可以带来显著的生产
力和正确性优势。 我们将在 :ref:`5-2-topdown-programming` 中继续开发这种单元语言。 与大多数编程语言不同，
lisp 为创建编程环境的人提供了相同的工具。宏足以实现 COMMON LISP 语言，也足以实现领域特定语
言。


.. _3-3-control-structures:

3.3 控制结构
=======================

虽然这本书的重点是 COMMON LISP，但它也是为 Scheme 编程语言编写的，同时也是关于 Scheme 编程
语言的。 Scheme 是一门很棒的语言，尽管缺少 lisp 程序员习以为常的特性，但它仍然为专业 lisp 程
序员提供了足够灵活的核心，可以按需扩展 :sup:`【2】` 。 同样，Scheme 程序员严重依赖的一些 COMMON LISP 没有
专门解决的特性。 但是，除了少数例外，每种语言提供的功能之间的比较是没有意义的。 两种语言之间的
鸿沟可以且经常被弥合。 用来连接两种语言的桥梁当然是——宏。

.. hint:: 【2】
  Scheme 和 COMMON LISP 在他们所服务的社区中差别很大。Scheme 程序员喜欢谈论有一个简短的规范有多伟大；COMMON LISP 程序员喜欢编写程序。

Scheme 的 **let** 结构 在某一方面比 COMMON LISP 对应的结构要更强大。Scheme 的 let 结构
支持一种 *命名的 let* :sup:`（3）` 。在 Scheme 中，可以在 let 结构的绑定列表之前插入一个符
号，Scheme 将在 **let** 主体周围绑定一个由提供的符号命名的函数 :sup:`【3】` 。 此函数接受 **let** 绑定中
提供的值的新参数，提供了一种非常方便的方式来表示循环。

.. hint:: 【3】
  Scheme只有一个名称空间，因此此函数绑定在那里。

.. note:: （3）
  命名的 let ：named let

.. code-block:: none

    (defmacro nlet (n letargs &rest body)
      `(labels ((,n ,(mapcar #'car letargs)
                  ,@body))
        (,n ,@(mapcar #'cadr letargs))))

幸运的是，我们可以用 **nlet** 宏在 Scheme 和 COMMON LISP 之间架起一座桥梁。 **nlet** 通
过模拟 Scheme 的命名然后以 Scheme 风格进行编码。在 **nlet-fact** 中，**nlet** 用于通过使
用命名的 let 来定义阶乘函数：

.. code-block:: none

    (defun nlet-fact (n)
      (nlet fact ((n n))
        (if (zerop n)
          1
          (* n (fact (- n 1))))))

因为 **nlet** 是我们的第一个宏，先放慢一下脚步，深入分析一下。 有时为了理解一个宏， *宏展开* :sup:`（5）` 一个宏
的使用示例会有所帮助 :sup:`【4】` 。 为此，向 **macroexpand** 函数提供一个表示此宏调用的列表。 注意，
**macroexpand** 只会展开其符号位于列表第一个元素中的宏，并且不会展开嵌套的宏调用 :sup:`【5】` 。在下文中，
我们直接从 **nlet-fact** 复制一个 **nlet** 调用，引用它，并将它传给 **macroexpand**
宏：

.. hint:: 【4】
  展开的术语实际上是相当不幸的。没有什么说宏展开某些东西会导致更大的扩展代码。有时表单甚至会展开为无（即 **nil** ）

.. hint:: 【5】
  但是 macroexpand 会继续扩展宏直到第一个元素不再代表宏。 Macroexpand-1 可用于观察此过程的第一步

.. note:: （5）
  宏展开 macroexpand

.. code-block:: none
    :linenos:

    * (macroexpand
        '(nlet fact ((n n))
          (if (zerop n)
            1
            (* n (fact (- n 1))))))

    (LABELS ((FACT (N)
              (IF (ZEROP N)
                1
                (* N (FACT (- N 1))))))
      (FACT N))
    T

上面的展开中使用 **labels** 特殊结构在给定的主体周围绑定一个函数。 该函数根据 *命名的 let* 结构中
使用的符号命名。 它将与 **nlet** 绑定的值作为参数，这里只有 **n** 。 由于这个函数可以是递归
的，所以 **nlet** 实现了一个有用的迭代构造。

尽管简单的宏可能只是填充反引号模板，但大多数复杂的宏至少会使用 lisp 的扩展列表处理函数。
**Mapcar** 将函数应用于列表中的每个元素并返回结果值的列表，在宏中尤其常见。令人注意的是，
**mapcar** 也经常出现在常规的 lisp 代码中。 Lisp 已被调整为对处理列表尽可能有用。 在各种 lisp 编程
中，包括宏构造，我们拼接、合并、归约、映射和过滤列表。 唯一的区别是在编写宏时，输出随后被传递给编译
器或解释器。 在 lisp 中编写宏实际上与编写常规 lisp 的过程相同。

但是，说 **nlet** 是一种新的控制结构是什么意思呢？ 控制结构只是一种奇特方式，用于描述一些不遵循函数行为的构造。 函数将从左到右计算执行每个参数，将结果绑定到环境中，并执行由某种 **lambda** 结构指
定的机器代码。由于 **nlet** 不直接计算执行参数，而是将参数拼接到 lisp 代码中，我们改变了
**nlet** 结构的计算执行流程，从而创建了一个新的控制结构。

通过这个宽泛的定义，几乎所有的宏——至少有趣的宏——都定义了新的控制结构。当别人说“只在函数做不了的
时候使用宏”时，他们的意思是对于任何不想求解某些参数的定义，或者想无序地计算它们，或者（求解）不止一次，
你将需要使用宏。函数，无论编写得多么巧妙，都无法（如此）工作。

**nlet** 宏演示了一种方法，即 COMMON LISP 是为宏编写者设计的。在诸如 **let** 之类的绑定结构
中，如果没有随变量名一起指定值，则将变量绑定为 nil 是种常见约定。 换句话说， **(let ((a))
a)** 返回的结果为 nil  :sup:`【6】` 。 在 Scheme --一种对宏编写器不太友好的语言中，当迭代此类绑定时必须将这
种情况作为特殊情况进行检查，因为 **(car nil)** 和 **(cdr nil)** 会引发类型错误。在
COMMON LISP 中， **(car nil)** 、 **(cdr nil)** ，以及 **(car (cdr nil))** 和 **(cadr
nil)** 定义为返回 **nil** ，即使空的 let 变量约定被使用了， **nlet** 中的第二个
**mapcar** 也能正常运行。 此 COMMON LISP 特性来自于 Interlisp[INTERLISP] 。

.. hint:: 【6】
  COMMON LISP 甚至允许我们写 (let (a) a) 来达到同样的效果

我们的 **nlet** 宏与 Scheme 的 *命名的 let*  有微妙的差异。 在这种情况下，宏的接口是可以接受
的，但展开式可能不是。在跨多个层次进行编程时很常见，我们代码的理想模型很容易与现实略有不同。在
Scheme 中， *命名的 let*  的尾部调用保证不会占用额外的堆栈空间，因为根据标准， Scheme 需要进行这种特
定的优化。然而，在 COMMON LISP 中情况并非如此，因此在 COMMON LISP 版本的 **nlet** 中可
能会发生堆栈溢出，而在 Scheme 中的 *命名的 let* 中不会发生这种情况。 在 :ref:`5-4-code-walking-with-macrolet` 中，我们将看到如何编写具有相同接口但可能更有效的扩展的 **nlet** 版本 :sup:`【7】` 。

.. hint:: 【7】
  实际上，这个版本的 nlet 通常就足够了，因为 COMMON LISP 编译器几乎肯定会优化编译代码中的尾部调用

.. _3-4-free-variables:

3.4 自由变量
=======================

一个 *自由变量* :sup:`（6）` 是在一个表达式中引用的无全局绑定或词法闭包绑定的变量或函数。 在下面的表达式中， **x** 是
自由的：

.. note:: （6）
  自由变量 free variable

.. code-block:: none
    :linenos:

    (+ 1 x)

但下面的代码中，我们在 *捕获* :sup:`（7）` 变量 **x** 的结构外面创建了一个绑定，从而剥夺了它的自由度：

.. note:: （7）
  捕获 captures

.. code-block:: none
    :linenos:

    (let ((x 1))
      (+ 1 x))

自由和捕获的术语起初可能看起来很奇怪。毕竟，自由意味着意识和做出决定的能力——这显然是简单的表达
方式所缺乏的。但是自由并不是指表达式可以做什么，而是作为程序员可以用表达式做什么。例如，我们可以
将表达式 **(+ 1 x)** 嵌入到任意位置，从而允许表达式访问周围代码中名为 **x** 的绑定。然后我们
说代码已经"捕获了"自由变量。在表达式中的自由变量被捕获后，如上面的 **let** 结构，其他周围的代码
没有捕获变量 **x** 的选择权。之前的自由变量已经被捕获。现在完全清楚它指的是哪个 **x** 。因此，
lisp 根本不需要在代码中保留对符号 **x** 的引用。正如 :ref:`2-3-lexical-and-dynamic-scope`
中详细描述的那样， lisp 编译器会忘记用于表示词法变量的符号。

尽管带有表达式的语言都可以拥有使用自由变量的表达式，但 lisp 的宏功能意味着自由变量在 lisp 中比
在其他语言中更有用。在大多数语言中，我们被迫遵守 *引用透明性* :sup:`（8）` 。 如果 Blub 程序中没有定义全局或
对象变量 **x** ，则下面代码毫无疑问是错误的：

.. note:: （8）
  引用透明性 referential transparency

.. code-block:: none
    :linenos:

    some_function_or_method() {
      anythind(1 + x);
    }

**some_function_or_method** 无法为 **x** 创建 *隐式绑定* :sup:`（9）` 。在 Blub 语言中，对变量的使用都
必须有显式的定义 :sup:`【8】` 。具有原始宏系统的语言（如 C 语言）可以在很有限的情况下完成其中的这种绑定。 但正如通
用宏在 C 语言中不切实际或不可能编写一样，涉及自由变量的特殊情况也是如此。

.. hint:: 【8】
  或者，有时，在面向对象的 Blub 语言中，是一个类或一个对象的定义
 
.. note:: （9）
  隐式绑定 implicit binding
 
在 lisp 中，我们可以随意将自由变量放到表达式周围，或者将自由变量拼接成新的表达式以供周围的代码捕
获，又或者定义全局特殊变量来捕获它们。 我们还可以编写宏来修改在一个表达式中的哪些变量是自由的，或者通过重
写表达式来减少自由变量（例如向上面一样将自由变量包裹在 let 结构中），或者通过修改表达式的方式来添
加新的自由变量。这种自由变量的添加与捕获变量相反，被称为 *自由变量注入* :sup:`（10）` 。
 
.. note:: （10）
  自由变量注入 free variable injection

最简单的 *自由变量注入* 就是一个宏展开成（对）一个符号的引用：

.. code-block:: none
    :linenos:

    (defmacro x-injector ()
      'x)

因为宏仅仅是一个函数，它以常规的 lisp 结构执行其函数体。 上面的注入宏求解引用的符号，当然，返回一
个符号——一个自由变量——然后拼接到任何使用 **x-injector** 宏的表达式中。 Paul Graham 在
*On Lisp* 中讨论过这种自由变量注入 :sup:`（11）` 
::

  这种词汇交流通常被视为传染的来源，而不是快乐的来源。通常编写这样的宏是不好的风格。在本书的所有宏中，只有【两个孤立的案例】以这种方式使用调用环境。
 
.. note:: （11）
  This kind of lexical intercourse is usually viewed more as a source of
  contagion than a source of pleasure. Usually it would be bad style to write
  such a macro. Of all the macros in this book, only [two isolated cases] use
  the calling environment in this way.

相比之下，本书从这种词汇交流中获得了很多乐趣。自由变量注入——在完全了解将要在其中扩展的词法环境
的情况下编写宏——只是 lisp 宏编程的另一种方法，当有一些略微不同的词法上下文，而想在其中编写基本
相同的代码时，这种方法很有用。虽然函数调用的主要优点通常是抛弃了词法环境，但有时对 lisp 程序员
来说，这只是个可以通过使用宏而忽略的指南。事实上，一旦习惯了它，一些 lisp 程序员总是尝试编写宏，尽可
能地扩展词法上下文，仅在需要求解参数或只是临阵退缩停止并想要一个新的词法上下文时才使用函数。在  :ref:`3-6-once-only` 中，我们将看到一种方法，可以在需要求解参数时避免丢弃您的词法环境。尽量保持词法环境允许非常有趣的宏 *组合* :sup:`（12）`，其中宏在使用一个或多个其他宏时添加词法上下文。展开成所定义的宏的代码是宏组合的一种特殊情
况，在 :ref:`5-5-recursive-expansions` 中进行了讨论。
 
.. note:: （12）
  组合 combinations

两点之间最短的距离是直线。 自由变量，通俗点说，扩展词法上下文通常是以编程方式构造程序的最简单方
法。 以这种方式使用宏可能看起来像是一种 hack，且在风格上可能会令人反感，但它可以方便且可靠地工
作。 尤其是我们在 :ref:`5-4-code-walking-with-macrolet` 中思考了 **macrolet** 之后，
这种编程风格——结合宏——会开始看起来更舒服。 记住，宏编程与风格无关； 而是关乎能力。 宏允许我们
实现很多语言不可能做的事情。自由变量注入就是其中之一。


.. _3-5-unwanted-capture:

3.5 不想要的捕获
=======================

关于变量捕获有两种观点。变量捕获是一些无法预测的错误的根源，但如果使用得当，它也也可以是个很理
想的宏特性。让我们从 Graham 在 On Lisp 中定义的一个简单宏开始考虑变量捕获： **nif** 。
**Nif** 是个 *数字 if* :sup:`（13）` ，它有四个必要子句，而常规的布尔值 **if** 有两个必要子句和一个可选的
子句。 **Nif** ，或者更确切地说是 **nif** 展开式的代码，计算第一个子句并假设结果是一个非复数。
然后，它根据结果是正数（ **plusp** ）、零（ **zerop** ）还是负数（其他情况）来计算三个相应子句中的一个。我们可以用 **nif** 测试变量 **x** ，如下所示：
 
.. note:: （13）
  数字 if： numeric if

.. code-block:: none
    :linenos:

    (nif x "positive" "zero" "negative")


**nif** 是我们讨论变量捕获的理想函数，我们将使用它来说明几个关键点，并作为宏构造新符号的测试用
例。 在展示 Graham 定义的 **nif** 版本之前，先自己定义个几乎正确但有一点问题的版本：

.. code-block:: none
    :linenos:

    (defmacro nif-buggy (expr pos zero neg)
      `(let ((obscure-name ,expr))
        (cond ((plusp obscure-name) ,pos)
              ((zerop obscure-name) ,zero)
              (t ,neg))))


**Nif-buggy** 展开成一段代码，用 **let** 绑定计算用户传入的 **expr** 结构的结果。 我们需
要这样做，因为计算 **expr** 可能会产生 *副作用* :sup:`（14）` ，我们需要将它的值用于两件不同的事情：将其传给
**plusp** 和 传给 **zerop** 。但是这个临时绑定叫什么呢？ 为了引入一个细微的错误，我们选择一个任意的符号 **obscure-name** 。除非有人看宏展开式，否则没人会看到这个变量名，所以没什么大不了的，对吧？
 
.. note:: （14）
  副作用 side-effects

几乎所有情况下， **Nif-buggy** 都会像 **nif** 一样工作。只要 **nif-buggy** :sup:`【9】` 的参数中没有
用到符号 **obscure-name** ，就不可能有不想要的变量捕获。但如果 **obscure-name** 确实出现在参
数中会发生什么呢？在多数情况下，仍然没有错误：

.. note:: 【9】
  或者在传递给它的形式体的宏展开式中。请参阅子词法范围

.. code-block:: none
    :linenos:

    (nif-buggy
      x
      (let ((obscure-name 'pos))
        obscure-name)
      'zero
      'neg)

即使 **x** 被证明是正数，即使我们将“禁用的符号”注入到 **nif-buggy** 的宏展开式中，这段代码仍然可
以按预期工作。当一个新的绑定被创建，并且该绑定内的引用总是引用创建了的绑定时，不会发生不想要的变量捕
获。只有在对 **obscure-name** 的使用 *跨越了* :sup:`（15）` 其在展开式中的使用时，才会出现问题。这是不想要的变量捕获
的示例：
 
.. note:: （15）
  跨越了 crosses over

.. code-block:: none
    :linenos:

    (let ((obscure-name 'pos))
      (nif-buggy
        x
        obscure-name
        'zero
        'neg))

在这种情况下， **obscure-name** 将绑定到 **x** 的计算结果中，因此符号 **pos** 不会返回预期
结果 :sup:`【10】` 。这是因为我们对符号的使用“跨越了”对绑定的无形使用。 有时，像这样具有不可见绑定的代码被称为不
具有 *引用透明性* 。

.. note:: 【10】
  事实上，当然，这种错误的行为正是本意。很少有这样直接的和设计出来的变量捕获问题。更多的时候，它们是令人惊讶和微妙的。

但这不就只是一个学术问题吗？ 当然，我们可以想出足够稀有的名字，这样问题就永远不会出现。 是的，在
很多情况下，包（ packages ）和智能的变量命名可以解决变量捕获的问题。 但是，多
数严重的变量捕获错误不会出现在程序员直接创建的代码中。大多数变量捕获问题只有在其他宏以未预料到
的方式调用宏（与你的宏结合）时才会出现。 Paul Graham's 对为什么要防止不想要的变量捕获有个直接的答
案： :sup:`（16）`

::

  当您可以编写没有错误的程序时，为什么还要编写带有小错误的程序？
 
.. note:: （16）
  Why write programs with small bugs when you could write programs with no bugs?

我认为可以进一步提炼这个问题：无论错误有多么微小，当你可以正确的去做时，为什么要错误的做事呢？

幸运的是，事实证明，变量捕获，就它是一个问题而言，是一个容易解决的已解决问题。最后一句话对许多人来
说是一个有争议的陈述，尤其是那些不喜欢明显的解决方案而花费大量时间寻找更好解决方案的人。作为专业
的宏程序员，你将接触到许多这些变量捕获解决方案。当前主流的方法是用所谓的 *洁净的宏*  :sup:`（17）` :sup:`【11】`。这些解决方案试图限制或消除不想要的变量捕获的影响，但不幸的是，这样做是以牺牲需要的、理想
的变量捕获为代价。几乎所有用于减少变量捕获影响的方法都只是产生了一个结果，即减少了你使用 **defmacro** 可以做到的事情。在最好的情况下，洁净的宏是初学者的安全护栏；在最坏的情况下，会形成一道电栅栏，将受害者困在一个经过
消毒的、安全的监狱中。此外，最近的研究表明，各种 Scheme 修订版所指定的清洁的宏系统对于许多有趣的
捕获问题来说仍然是脆弱的 [SYNTAX-RULES-INSANE][SYNTAX-RULES-UNHYGIENIC] 。
  
.. hint:: 【11】
  另一个流行的术语是“示例宏”（"macros by example"）

.. note:: （17）
  洁净的宏 hygienic macro 

变量捕获的真正解决方法称为 *生成符号* :sup:`（18.1）` ，或简称 *gensym* 。 一个 gensym 是一种让 lisp 为我们选择变量名称的方法。
但是， lisp 不会像之前那样用 **obscure-name** 这样的蹩脚的名字，而是用个好名字。真正的好名
字。这些名字是如此的好，如此独特，以至于任何人（甚至 gensym 本身）都不会再选择相同的名字。这怎么可
能？在 COMMON LISP 中，符号（名称）与 *包* :sup:`（18.2）` 相关联。包是符号的集合，您可以通过提供字符串来从中获取指针，他们的 symbol-name 字串。这些指针（通常只称为符号）的最重要属性是它们将与在该包中以相同符号名称查找的所有其他指针（符号）相等。 gensym 是在任何包中都不存在的符号，因此 symbol-name 不会返回
一个与 gensym 相等的符号指针。 Gensyms 用于当您想向 lisp 指示某个符号应该与表达式中的某个其他符号相等而无需命名任何内容时。因为您没有命名任何东西，所以不会发生名称冲突。

.. note:: （18）
  生成符号 generated symbol；包 package 

因此，通过遵循这三个简单但很重要的规则，就能轻松地在 COMMON LISP 中避免不想要的变量捕获：
:sup:`（19）`

::

  每当你包装一个词法或动态绑定时，围绕提供给你的宏的代码，用 gensym 命名这个绑定，除非你想从你包装的代码中捕获它。
  每当你在宏代码的周围封装一个函数绑定、或者一个 **macrolet** 宏 或 **symbol-macrolet** 宏时，请使用 gensym 命名此函数或宏，除非你想从你封装的代码中捕获它。 核实此绑定与标准定义的任何特殊结构、宏或函数没有冲突。
  切勿分配或重新绑定 COMMON LISP 指定的特殊形式、宏或函数。

.. note:: （19）
  Whenever you wrap a lexical or dynamic binding around code provided to your macro,
  name this binding with a gensym unless you want to capture it from the code you are wrapping.
  Never assign or re-bind a special form, macro, or function specified by COMMON LISP.

除了 COMMON LISP 之外的一些 lisp ，如 Scheme ，具有将变量命名空间与函数/宏命名空间结合起来的
不幸特性。 有时这些 lisp 被称为 *lisp-1* lisp ，而具有独立名称空间的 COMMON LISP 被称为
*lisp-2* lisp 。 使用假设的 *lisp-1* COMMON LISP ，在构造宏时还必须遵循以下两个附加规则：

- 核实有意引入的词法或动态绑定不会与有意引入的函数或宏绑定或标准定义的任何特殊结构、宏或函数发生冲突。

- 核实有意引入的函数或宏绑定不会与有意引入的词法或动态绑定发生冲突。

COMMON LISP 将变量命名空间与函数命名空间分开的“明智设计决定”消除了整个维度的“不想要的变量捕获”问题。
当然 *lisp-1* lisp 在创建宏时不会遇到任何理论上的障碍：如果我们遵循前面的两条规则，我们可以像
在 COMMON LISP 中一样避免变量捕获。但是，在编写复杂的宏时，很难跟踪在单个隔离的命名空间中的符号。考虑名称的交叉引用只会使宏编写比设想的更困难。

除了不完整的标准之外 :sup:`【12】` ，比任何其他属性更重要的是，单一命名空间的这种缺陷使得 Scheme 这种原本优秀
的语言不适合严肃的宏构造  :sup:`【13】` 。 Richard Gabriel 和 Kent Pitman 用以下令人难忘的引述
[LISP2-4LIFE] 总结了这个问题：:sup:`（20）`
  
.. hint:: 【12】
  尤其是涉及宏和异常时
    
.. hint:: 【13】
  尽管我们将在本书中看到，有很多理由更喜欢 COMMON LISP 而不是 Scheme 

::

  看待有关宏和命名空间的争论有两种方式。首先是单个命名空间至关重要，因此宏是有问题的。第二种（观点）是宏是基本的，因此单个命名空间是有问题的。

.. note:: （20）
  There are two ways to look at the arguments regarding macros and namespaces.
  The first is that a single namespace is of fundamental importance, and
  therefore macros are problematic. The second is that macros are fundamental,
  and therefore a single namespace is problematic.

因为命名空间的数量再怎么重要，也没有比启用宏更重要，所以只能得出结论， Scheme 做出了 *错误* 的决
定，而 COMMON LISP 做出了 *正确* 的决定。

尽管如此，每次我们需要一个无名符号时，都调用 **gensym** ，既笨重又不方便。 难怪 Scheme  设计者决定
使用所谓的 *洁净的宏*系统，以避免在所有地方输入 **gensym** 。 Scheme 采取的错误转变是为了宏构造这
一目的而推广一种特定于领域的语言。 虽然 Scheme 的迷你语言毫无疑问很强大，但忽略了宏的全部要
点：宏很棒，因为它们是用 lisp 编写的，而不是一些愚蠢的预处理器语言。

这本书介绍了一种新的 gensyms 语法，更适合那些有简洁意识的人，但仍然是传统 lisp 表达式的薄薄一层。我们的新符号 gensyms 将用作本书中大多数宏的基础，通过剥离开简单宏的一些层次，这些层次使用我们符号所提供的特征，来
清楚的描述这一语法。 继续上一节中的 **nif** 示例。以下是 Graham 定义的捕获安全的 **nif** （版本）：

.. code-block:: none
    :linenos:

    (defmacro nif (expr pos zero neg)
      (let ((g (gensym)))
        `(let ((,g ,expr))
          (cond ((plusp ,g) ,pos)
                ((zerop ,g) ,zero)
                (t ,neg)))))

这是 **gensym** 的正确用法。 正如上一节中看到的，一个可以将用户输入，展开为可能干扰其自身一个变量的宏，必
须注意变量捕获。 Graham 提出了一个缩写宏 **with-gensyms** ，在需要创建多个 **gensyms** 的
情况下更加简洁：

.. code-block:: none
    :linenos:

    (with-gensyms (a b c)
      ...)

展开成

.. code-block:: none
    :linenos:

    (let ((a (gensym))
          (b (gensym))
          (c (gensym)))
      ...)

因为在 **defmacro** 结构中需要 **gensym** 非常普遍，我们决定进一步追求缩写。 特别要注意的
是，我们必须为每个 **gensym** （如 **a** 、 **b** 和 **c** ）输入至少两次的临时名称：一次是声
明它为 **gensym** ，另一次是调用它时。 那么可以消除这种冗余吗？

首先，想想 **nif** 宏如何使用 **gensyms** 。 当 **nif** 宏展开时，会调用 **gensym** 返回
一个生成的符号。 因为这个符号保证是唯一的，所以可以安全地将它拼接到一个宏展开中，因为这个符号知
道它永远不会捕获意外引用。 但是仍需要在宏的定义中命名这个 **gensym** ，以便能够将它拼接到展开式中正确
位置。 对于 **nif** 宏定义的范围， Graham 将这个 **gensym** 命名为 **g** 。 注意，
此名称从未实际出现在 **nif** 的宏展开式中：

.. code-block:: none
    :linenos:

    * (macroexpand '(nif x 'pos 'zero 'neg))

    (LET ((#:G1605 X))
      (COND ((PLUSP #:G1605) 'POS)
            ((ZEROP #:G1605) 'ZERO)
            (T 'NEG)))
    T

变量名 **g** 在宏展开式中消失了。因为 **g** 只绑定在展开器环境中，所以给到这样一个变量的名称，与展开式
中的捕获无关。在展开式中，所有出现的 **g** 都被替换成打印名称为 **G1605** 的符号。以 **#:**
为前缀，因为该符号没有 *内化* :sup:`（21）` 在任何包中——它是一个 gensym 。当打印结构时，以这种方式为
gensyms 添加前缀是有必要的，因为如果在再次读回该结构后使用（或求解）该结构，我们希望 lisp 会中断。希望
lisp 中断，是因为我们无法通过查看两个 gensym 的打印名称来确定它们是否相等——这就是它们的目
的。 Lisp 以一种有趣的方式中断：因为每次读取 **#:** 符号时都会创建一个新符号，并且因为 **(eq '#:a '#:a)** 永远不为真，所以上述展开式中的内部 **#:G1605** 符号不会引用 let 结构创建的
绑定，所以 lisp 认为表达式有一个自由变量，向我们表明一个带有 gensyms 的结构被再次读入。

.. note:: （21）
  内化 interned

虽有此类非内化符号的默认打印行为，（但）仍然可以保存和重新加载宏展开式。 为了更准确地打印带有 gensyms
的结构，可以在打印结果时打开 *print-circle* 模式 :sup:`【14】` ：

.. hint:: 【14】
  我们返回 **t** 以便我们看不到 **print** 返回的表单。返回（ **values** ）也很常见。

.. code-block:: none
    :linenos:

    * (let ((*print-circle* t))
        (print
          (macroexpand '(nif x 'pos 'zero 'neg)))
        t)

    (LET ((#1=#:G1606 X))
      (COND ((PLUSP #1#) 'POS)
            ((ZEROP #1#) 'ZERO)
            (T 'NEG)))
    T

在上面的代码中， lisp 输出使用 **#=** 和 **##** *读取宏* :sup:`（22.1）`。 这些读取宏可以让我们创建 *自引用* :sup:`（22.2）` 结
构，这将在 :ref:`4-5-cyclic-expressions` 中深入地讨论。 如果我们阅读上面的代码，（代码）里面使用
的符号实际上与 **let** 绑定中使用的符号相同，展开式仍然有效。 似乎上述定义避免了双重命名冗余。
有没有一种办法让我们可以把它拉回一个书写宏模板的宏？

.. note:: （22）
  读取宏 read macros；自引用 self-referential

.. code-block:: none
    :linenos:

    (defun g!-symbol-p (s)
      (and (symbolp s)
          (> (length (symbol-name s)) 2)
          (string= (symbol-name s)
                    "G!"
                    :start1 0
                    :end1 2)))

记住，我们可以在宏定义中给 gensyms 命名任意的名字，甚至像 Graham 所做的那样，像 **g** 这样的简
单名称，并且它们将在宏展开式中消失。由于命名自由，让我们对 gensyms 的命名约定进行标准化。 作为简洁性
和唯一性之间的折衷，任何以 G! （两个字符）开头且后面至少跟一个其他字符的符号都被认为是一种特殊的
gensym 引用符号，称为一个 *G-bang 符号* :sup:`（23）` 。 我们定义了一个谓词 **g!-symbol-p** ，一个用于确定给定
原子是否是 G-bang 符号的谓词。

.. note:: （23）
  G-bang 符号 G-bang symbol

.. code-block:: none
    :linenos:

    (defmacro defmacro/g! (name args &rest body)
      (let ((syms (remove-duplicates
                    (remove-if-not #'g!-symbol-p
                                  (flatten body)))))
        `(defmacro ,name ,args
          (let ,(mapcar
                  (lambda (s)
                    `(,s (gensym ,(subseq
                                    (symbol-name s)
                                    2))))
                  syms)
            ,@body))))

既然我们已经标准化了 G-bang 符号，我们可以创建一个宏来编写宏的定义并利宏书写叫做 *自动 gensyms* :sup:`（24）` 的快
捷方式。 宏 **defmacro/g!** 为宏编写领域定义了一种特定于领域的语言，但尝试保留了 lisp 的所有功
能。 **defmacro/g!** 很简单，但是如何使用它以及它是如何工作的，可能并不是那么简单。 基于此，
且因为这是本书中介绍的第一个真正的宏之一，我们慢慢地对 **defmacro/g!** 进行分析。

.. note:: （24）
  自动 gensyms ： automatic gensyms

剖析宏时，第一步就是是“停留”。 不要将宏视为语法转换或任何其他此类无意义的抽象。 把宏想象成一个函
数。 宏本质是函数，并且以完全相同的方式工作。 该函数以未求解的表达式作为参数，并预计返回即将插入到其他表达式中的lisp代码。

所以，将 **defmacro/g!** 看作一个函数，考虑它的执行。因为我们正在编写一个常规的 lisp 函数，
所以可以访问 lisp 的所有功能，甚至是之后添加到该语言中的实用工具。在 **defmacro/g!** 中，我
们使用 Graham 的 **flatten** 实用工具、 lisp 的 **remove-if-not** 和
**remove-duplicates** 函数以及 G-bang 符号谓词 **g!-symbol-p** 创建一个新的列表，该列表是
由传递给宏的主体形式中的所有 G-bang 符号组成。接下来，使用反引号模板返回一个列表，该列表代表我们希望宏展
开成的代码。在示例中，因为我们正在编写对 **defmacro** 的改进，我们希望我们的代码能够展开为
**defmacro** 结构本身。但是我们正在为 **defmacro** 语言添加新的便捷特性，并希望创建一个稍
微复杂的展开式。为了给宏主体中找到的每个 G-bang 符号一个新的 **gensym** ，我们用 **mapcar**
将一个函数应用到 G-bang 符号列表上，创建一个可以拼接到 **let** 结构的新列表，建立每个
**gensym** 的绑定 :sup:`【15】` 。

.. hint:: 【15】
  可以选择向 gensym 函数传递单个字符串参数。这会更改 gensym 的打印名称，这在阅读扩展时很有帮助。 Defmacro/g！ 为此目的，使用 G-bang 符号中字符的打印名称。

注意，我们映射的 lambda 如何包含一个使用反引号操作符创建的表达式，从而导致看似（但不是） *嵌套反引号* :sup:`（25.1）` 的情
况。 因为应用这个函数的 **mapcar** 是 *不带引号* :sup:`（25.2）` 的，所以嵌套反引号中的不带引号的表达式仍然在我们
原来的上下文中求值。 众所周知，嵌套反引号很难理解，在 :ref:`chapter04` 中深入地研究反引号时，
我们将回到这个概念。

.. note:: （25）
  嵌套反引号 nested backquote ；不带引号 unquoted

那么， **defmacro/g!**  到底可以让我们做什么？ 我们可以利用这种自动生成符号技术，一种检查特定符号是否存在的方法，该特定符号是在提供给宏的词法范围内的 :sup:`【16】` 。 如果我们不使用任何 G-bang 符号，我们可以完全像是要使用 **defmacro** 一样使用 **defmacro/
g!** 。 但出现在宏展开式主体中的任何 G-bang 符号都被解释为：

.. hint:: 【16】
  目前，这是一种简化。请参阅关于子词法范围的部分。

::

  我希望在这个表达式周围绑定一个 gensym，我已经给出了这个符号。 实现它。

我们可以用 **defmacro/g!** 在重新定义 **nif** 时避免显式创建 **gensym** ：

.. code-block:: none
    :linenos:

    (defmacro/g! nif (expr pos zero neg)
      `(let ((,g!result ,expr))
        (cond ((plusp ,g!result) ,pos)
              ((zerop ,g!result) ,zero)
                (t ,neg))))

当我们需要使用一个 **gensym** 时，我们就直接使用它。 当然，我们需要小心，所有对 G-bang 符号的引用仅仅由宏展开式
求解而来，因为这是 **gensym** 将被绑定的唯一位置 :sup:`【17】` 。像上面那样取消出现在反引号内的 G-bang 符
号引用是最明显的方法，可以看到这直接与 Graham 的 **nif** 原始定义中符号 **g** 的取消引用类似。

.. hint:: 【17】
  G-bang 符号尤其不应该出现在展开式本身中——这正是我们试图通过 gensyms 避免的

因此，我们定义了一个与 Graham 的（版本）类似的宏 **nif** ，但这种改进似乎好得令人难以置信。它真
的有效吗？ 在做出决定之前，看一下宏展开式 :sup:`【18】` ：

.. hint:: 【18】
  我们使用 macroexpand-1 ，所以我们只展开 defmacro/g！ 宏，而不是它进一步展开出的 defmacro 宏

.. code-block:: none
    :linenos:

    * (macroexpand-1
        '(defmacro/g! nif (expr pos zero neg)
          `(let ((,g!result ,expr))
              (cond ((plusp ,g!result) ,pos)
                    ((zerop ,g!result) ,zero)
                    (t ,neg)))))

    (DEFMACRO NIF (EXPR POS ZERO NEG)
      (LET ((G!RESULT (GENSYM "RESULT")))
        `(LET ((,G!RESULT ,EXPR))
          (COND ((PLUSP ,G!RESULT) ,POS)
                ((ZEROP ,G!RESULT) ,ZERO)
                (T ,NEG)))))
    T

看起来 **defmacro/g!** 与 Graham 在他编写 nif 原始版本时所作的根本相同。看这个使用 **defmacro/g!** 的示例，我们发现，没有 non-gensym （非 gensym ）绑定将会在它的展开式中被创建。 Nif ，像这样用 **defmacro/g!** 定义的，摆脱了变量捕获问题。
但因为 **defmacro/g!** 本身也是个宏，宏展开环境中是否可能存在不想要的捕获或替换问题？ 与复杂的抽
象一样，（它的）行为在一定程度上是任意的。 在同样的意义上，变量捕获本身就是一个缺陷， **defmacro/g!** 的某些属性也许似乎可能仅仅是其故意设计出来的 :sup:`【19】` 。与往常一样，最好的解决方案是完全理解抽
象。

.. hint:: 【19】
  尽管排除程序员错误，也从来都不安全。

**defmacro/g!** 的一个有趣的极端案例 :sup:`（26）` 是在 G-bang 宏中定义 G-bang 宏。所有 **defmacro/g!**
所做的是将一组绑定引入到展开式环境，如果需要，每个绑定都绑定到宏可以使用的 **gensym** 。在有多种 gensym 绑定
可能性的情况下，因为有上下文，它们总是可以区分开的。 换句话说，始终可以根据你求解它时所处的环境来指定应该使用哪个环境的 **gensym** 。举个人为制造的示例：

.. note:: （26）
  极端案例  corner-case

.. code-block:: none
    :linenos:

    (defmacro/g! junk-outer ()
      `(defmacro/g! junk-inner ()
        `(let ((,g!abc))
            ,g!abc)))

这里创建了两个 gensyms 。 **g!abc** 的用法前面只有一个消引号（逗号），因此我们知道展开式引用 由 **junk-inner** 展开创建的内部 **gensym** 。 如果每个都有两个消引号，它们将引用由 **junk-outer** 展开创建的外部 **gensym** 。

**defmacro/g!** 用了 Graham 的 **flatten** 函数。 **Flatten** ，如第 1.3 节： Lisp 实
用程序，接收一个 cons 树结构——我们的 lisp 代码——并返回所有叶子/原子的新列表。 **defmacro/
g!** 中 **flatten** 的使用是 *代码遍历* 的一个简单示例，我们之后将在本书中重新讨论遍历代码这一主
题。

练习：在上面定义 G-bang 宏的 G-bang 宏中，如果第一个 gensym 前面有一个消引号（逗号），而另
一个前面有两个消引号（两个逗号），会出现什么问题？


.. _3-6-once-only:

3.6 Once Only 仅仅一次
=======================

Peter Norvig 是一位出色的程序员和作家。 在解决我们作为计算机科学家目前面临的许多最困难的问题之前，需
要阅读他关于人工智能的书籍，尤其是《Artificial Intelligence: A Modern Approach》 :sup:`（27.1）`  [AIMA] 。 lisp 程序员可能更熟悉
Norvig 的著作《Paradigms Of Artificial Intelligence Programming: Case Studies in Common Lisp》 :sup:`（27.2）` [PAIP]。这本书可能有点过时，但对于认真
的 lisp 学生来说仍然是必读的，且这本书包含许多重要的 lisp 见解 :sup:`【20】` 。 本节专门针对 Peter
Norvig ，甚至以 PAIP 中描述的宏命名。 在它的最后几页中，隐藏在对序列函数实现的描述中，是 :sup:`（28）` 

.. hint:: 【20】
  来自 PAIP 的一条 COMMON LISP 建议是永恒正确的，那就是永远不要将 &optional 和 &key 参数混合到 lambda 或 defmacro 解构形式中。这会咬你！

.. note:: （27）
  原作为英文，因此译文里仍用原作品名称。同时备注一下书籍名称的译文，便于读者使用。《人工智能：一种现代方法》 Artificial Intelligence: A Modern Approach；《人工智能编程范式： COMMON LISP 中的案例研究》Paradigms Of Artificial Intelligence Programming: Case Studies in Common Lisp
 

::

 
 只求解一次：宏学科的一课。

.. note:: （28）
  once-only：A Lesson in Macrology 

紧接着是句更有趣的话： :sup:`（29）` 


::

  
  如果你能理解如何编写以及何时使用 once-only （技术），那么你就真正的理解了宏。

.. note:: （29）
  If you can understand how to write and when to use once-only, then you truly
  understand macros.

现在我们已经知道了，没有人真正了解宏。 理解一个特定的宏，甚至是一个和 once-only 一样重要的
宏，也不会如同理解一个重要的定理让你真正理解数学那样更进一步理解宏。 因为到目前为止它们的可能性似乎是
无限的，所以真正理解数学或宏是真真不可能的。

这里不会给出 Norvig 的 **once-only** 的定义，但它是一个相当复杂的宏，具有一些有趣的属性，之
后会稍有不同地实现这些属性。 **once-only** 最初是为已经消失的 *lisp 机* :sup:`（30）` 编程环境编写的，因为无关紧要的原因而被排除在 COMMON LISP 之外。

.. note:: （30）
  lisp 机 lisp machine

**once-only** 背后的思想是把将会创建一个新的绑定的代码围绕在一个宏展开式的周围。 执行宏展开式时，这个新的绑定会用一个传递给宏作为参数的形式体的一个求解结果进行初始化。 **once-only** 主体中的代码然后可以使用绑定，当然，不会重新执行传递给宏的形式体。 作为参数传递给宏的形式体只是且总是执行一次。 Once-only （只求解一次）。

Norvig 用 **square** 宏作为 **once-only** 的一个示例。 **square** 表达式接受一个参数并返回
该参数与自身的乘积：

.. code-block:: none
    :linenos:

    (defmacro square (x)
      `(* ,x ,x))

当我们将许多内容传递给 **square** 宏，它都能工作:大部分变量、数字和其他可以根据需要自由
地求解多次的结构。但是一旦有 *副作用* 的结构传给这个版本的 **square** 中，那么所有的预想都不奏效。
当然，行为仍然是确定的，但可能很难去确定。使用这个特定的宏，传递的参数将被计算两次。但是因为
这些很快变得复杂，在一般情况下，所有的预想都没了。让避免这些不必要的副作用  变得方便和容易是
once-only 的重点。注意，如果我们用的是函数，我们将自由获得此行为。在离开人为的教科书示例的范畴后，
到这一步，将 **square** 定义为一个函数，最终看起来像这样：

.. code-block:: none
    :linenos:

    (defun square (x)
      (* x x))

由于 lambda 的工作原理，我们可以用任何结构作为这个 **square** 函数定义的参数。因为这个参数将只
被执行一次，所以我们的想法和副作用概念模型都得到了满足。 在大多数情况下，我们希望写过的一个表达
式只被执行一次。相反，宏的主要强力能力之一是通过操纵执行的频次和顺序来违反这一假设。例如，在循环
之类的事情中，我们可能想要多次执行表达式。甚至我们也可能希望表达式永远不会被执行，因为我们想要源自他们的某物而不是他们的执行结果。

**once-only** 允许我们在宏展开式中指定希望只被执行一次的特定形参，且它们的执行顺序是从左到右的，就
像 lambda 。 以下是我们如何使用传统的 **once-only** 宏来完成这（些）的：

.. code-block:: none
    :linenos:

    (defmacro square (x)
      (once-only (x)
        `(* ,x ,x)))

但当然，如果只想一次执行你的宏的所有参数，你反而可以使用一个函数 (或者 lambda )。 稍后会回到这一点，但是因为本书没
有 **once-only** 的直接实现，所以为我们的宏标记引入了这个功能的替代实现。 尽管在 [PAIP-P853]
[PRACTICAL-CL-P95] 中有很多有趣的 **once-only** 的实现，但本节介绍了一种与 **defmacro/
g!** 组合的新技术。

我们的 **once-only** 实现的第一步是创建一些新的谓词和实用工具。再次以独特性损害简洁性，我们保
留另一组符号供自己使用。所有以字符 O! 开头的符号且其后接有一个或多个字符称为 *O-bang 符号* 。


.. code-block:: none
    :linenos:

    (defun o!-symbol-p (s)
      (and (symbolp s)
          (> (length (symbol-name s)) 2)
          (string= (symbol-name s)
                    "O!"
                    :start1 0
                    :end1 2)))

    (defun o!-symbol-to-g!-symbol (s)
      (symb "G!"
            (subseq (symbol-name s) 2)))

一个区分 O-bang 符号和其他对象的谓词被定义为： **o!-symbol-p** 。这个定义几乎与
**g!-symbol-p** 的定义相同。我们还引入了一个方便的实用函数，将 O-bang 变成 G-bang ，其保
留 bang 之后的字符： **o!-symbol-to-g!-symbol** 。 此实用函数使用 Graham 的实用函数
**symb** 来创建新符号。

.. code-block:: none
    :linenos:

    (defmacro defmacro! (name args &rest body)
      (let* ((os (remove-if-not #'o!-symbol-p args))
            (gs (mapcar #'o!-symbol-to-g!-symbol os)))
        `(defmacro/g! ,name ,args
          `(let ,(mapcar #'list (list ,@gs) (list ,@os))
              ,(progn ,@body)))))


**defmacro!** 代表我们的宏定义语言的最后一步——它增加了个 **once-only** 的特性。
**defmacro!** 结合了上一节的 **defmacro/g!** 。因为 **defmacro!** 直接展开为
**defmacro/g!** 结构，所以 **defmacro!** 将 *继承* 自动生成符号行为。了解所有正在组合的部分对
于复杂的组合至关重要。回顾一下， **defmacro/g!** 查找以 G-bang 开头的符号并自动创建
gensyms 。通过展开为带有 G-bang 符号的结构， **defmacro!** 在实现 **once-only** 时，可以
避免重复 gensym 行为。

**defmacro!** 给出了一种称为“自动 **once-only** ”的快捷方式。 使用 *自动 once-only* ，我
们可以在宏参数中的一个或多个符号前加上 O-bang 前缀，使其成为由 **o!-symbol-p** 定义的 O-bang
符号。 当我们这样做时， **defmacro!** 将知道我们是在生成的代码中创建一个绑定，在求解时，将包含代码求解的结果作为参数提供给宏。宏展开式通过 gensym 将会访问此绑定。但在创建展开时该怎么引用这个
gensym 呢？通过调用上面由 **o!-symbol-to-g!-symbol** 定义的等效 G-bang 符号。

该实现依赖于 **defmacro/g!** 的功能。 使用 **o!-symbol-to-g!-symbol** 实用程序，我们创建新
的 G-bang 符号以添加到 **defmacro/g!** 结构。一旦我们有了自动生成符号， **once-only** 就很容易实现，正如 **defmacro!** 定义那样简洁。

暂时回到人为的教科书示例，我们将重新实现 **square** 宏，这次使用 **defmacro!** ：

.. code-block:: none
    :linenos:

    (defmacro! square (o!x)
      `(* ,g!x ,g!x))

可以使用 `macroexpand` 展开：

.. code-block:: none
    :linenos:

    * (macroexpand
        '(square (incf x)))

    (LET ((#:X1633 (INCF X)))
      (* #:X1633 #:X1633))
    T

上一节中，我提到我们将所有 G-bang 符号的字符串值传递给 **gensym** 。 这使得检查这些结构的展开式
变得很容易。 尽管像 **#:X1633** 这样的 gensyms 的名称没有什么意义，但如果我们正在编写或调试
由 **defmacro!** 定义的 **square** 时，可以直接看到这个符号和宏定义中使用的符号-- **X** 之间的联
系。如果将这些信息保留在 gensyms 的 **print-name** ，就像在 **defmacro/g!** 中
的展开那样 :sup:`【21】` ，就能够将符号从定义匹配到展开式，反之亦然更容易。

.. hint:: 【21】
  这也是由 *gensym-counter* 指定的 gensym 的打印名称中的数字的原因。这个计数器让我们可以区分具有相同打印名称的 gensyms 实例

与传统的 **once-only** 相比，除了不那么冗长的用法和更有用的展开输出之外， **defmacro!** 还提供了
一项额外的关键功能。 在传统的 **once-only** 中，用于访问创建的词法变量的 gensym 的绑定被赋
予与宏展开式参数相同的名称，这会 *隐藏* 宏参数，因此宏定义无法访问它。因为 **defmacro!** 将其分为
两种不同类型的符号， G-bang 和 O-bang ，我们可以编写使用这两个值的宏展开式。 为了演示这一点，这
里 **square** 宏的另一个定义：

.. code-block:: none
    :linenos:

    (defmacro! square (o!x)
      `(progn
        (format t "[~a gave ~a]~%"
                    ',o!x   ,g!x)
        (* ,g!x ,g!x)))

可以如下使用：

.. code-block:: none
    :linenos:

    * (defvar x 4)

    X
    * (square (incf x))
    [(INCF X) gave 5]
    25

注意，上面的 **square** 定义中我们 *引用* :sup:`（31）` 了消引的 O-bang 符号 :sup:`（32）` 。 我们这样做是因为不想再
次执行此结构。 **defmacro!** 生成的展开式已经执行过了。 我们仅仅想把这个形式体传递给 **square** 并且用作其他的目的，这种情况下是某种粗略的调试语句。然而，我们即使已经执行过一次，并且在这种情况下它是不正确的，
如果我们期望的抽象需要它，没有什么能阻止我们再次执行传进来的参数。

.. note:: （31）
  引用 quote 
  
.. note:: （32）
   **',o!x** 

**defmacro!** 语言允许我们对传给宏的参数的执行进行精细、方便的控制。 如果使用 O-bang 给所有在宏定义中表示参数的符号添加前缀，并且只在宏定义中使用相应的 G-bang 符号，我们的展开式将与 lambda 表达式相同——
每个结构执行一次，按照从左到右的顺序。 **defmacro!** 就像常规的 **defmacro** 一样，在args（参数）中没有任何这些符号，也没有在展开式中使用 G-bang 符号。

**defmacro!** 在宏的迭代开发过程中最有用。 因为向宏参数添加两个字符以获得 lambda 形式求解是一件简单的事情，并且使用 gensyms 就像编写它们一样简单，我们可以立即改变对这些决定的看法。
**defmacro!** 感觉像是比  **defmacro** 更贴合 **lambda** 的手套。 正是出于这个原因，迭
代开发，我们将使用 **defmacro!** 作为本书其余部分的主要宏定义接口。

.. code-block:: none
    :linenos:

    (defmacro! nif (o!expr pos zero neg)
      `(cond ((plusp ,g!expr) ,pos)
            ((zerop ,g!expr) ,zero)
              (t ,neg)))

让我们回到 Graham 的 **nif** 宏。 当用 **defmacro!** 更新这个宏时，我们注意到 **expr** 参数，（即）我们
为其创建了一个 gensym 的那个参数，只执行了一次。 这里我们用 **defmacro!** 表示调用该参数
**o!expr** 时只需要执行一次。 这个 **nif** 的实现代表了我们这个宏演变的最后一步。

**defmacro!** 模糊了宏和函数之间的界限。 正是这个特性，在宏参数中提供一些 O-bang 符号和一些
常规符号的能力，使得 **defmacro!** 特别有用。 正如反引号允许你翻转默认引用行为一样，
**defmacro!** 可以将宏参数中的求值语义从常规未求值的宏结构，翻转为单独求值、从左到右的
lambda 参数。


.. _3-7-duality-of-syntax:

3.7 语法二义性 :sup:`（33）` 
=======================
  
.. note:: （33）
   Duality of Syntax 

lisp 有个重要的概念称为 *语法二义性* 。 理解二义性及其重要性是编写宏和本书的基本主题。 二义性有时
是设计出来的，有时是意外发现的。对于非 lisp 语言的程序员来说，二义性语法的现实令人难以置信，以
至于无法在本书中在此处进行描述，所以我们现在回避直接定义。 相反，你，本书的读者，会一次又一次地发现
它，因为它被缓慢而仔细地应用以避免震惊到你。 如果在阅读本书过程中感到头痛或其他不适，建议立即执行
垃圾回收周期（睡一觉），然后以崭新和开放的心态返回。

*引用透明* 有时被视为为代码的一种属性，其中任何表达式都可以插入到任何地方且总是具有相同的含义。 引入句语法二义性是有意识地违反引用透明，探索二义性正在收获一种允许这种违反（引用透明）的语言的果实。 在其
他语言只能用半透明的玻璃板进行构建时，lisp 却可以使用各种烟雾、镜子和棱镜。 这个魔法颗粒就是宏，宏
的大部分精妙的技巧都是基于句法二义性。

本节描述了一种我们已经讨论过但还没完全探索的一个重要的二义性语法： COMMON LISP 使用相同的语法
来访问两种主要类型的变量，即动态变量和词法变量。 本书试图讲解动态和词法作用域的真正威力，以及为
什么 COMMON LISP 决定用二义性语法是这么重要。

动态作用域的目的是提供一种方法，可以根据表达式的执行时间而不是定义或编译的位置，来获得传入和输出
的 lisp 表达式的值。幸运的是， COMMON LISP 为此定义的语法与用于访问词法变量的语法相同，这与动态变量
完全相反，因为它们总是引用它们被编译的位置，而与何时发生访问无关。事实上，如果没有声明形式体的外部
上下文，你.就无法判断表达式所指的是哪种类型的变量。这种二义性语法违反了引用透明， lisp 程序员对此表示欢迎，而不是要避免，
因为就像无法在没有上下文的情况下区分表达式一样，宏也不能。多体会一下这
个观点。首先，先明确认识到，为动态变量创建绑定不会创建词法闭包。例如，重新绑定之前声明的变量
**temp-special** ：

.. code-block:: none
    :linenos:

    * (let ((temp-special 'whatever))
        (lambda () temp-special))

    #<Interpreted Function>

尽管上面是一个 *let over lambda* ，但这不是一个词法闭包。 这是在某些动态上下文中对 lambda 宏结
构的简单执行，这当然会导致匿名函数。 此函数在应用时将访问当前存在的任何动态环境并获取
**temp-special** 的值。 当 lambda 宏求解时， **temp-special** 求解为任何存在的符号的动态绑定，但谁在乎呢？ 记住，lambda 结构是常量对象，只是简单的机器代码指针返回
器，因此执行此 lambda 结构甚至永远不会访问动态环境。 我们的符号会发生什么？ 在 lisp 完成对
lambda 结构的求解后，会将其从动态环境中删除并丢弃，（变成）未使用。

一些早期的 lisp 确实支持 *动态闭包* ，这意味着在一个非空动态环境中定义的每个函数都有自己的（可能部分共
享）动态绑定堆栈。 其效果类似于 COMMON LISP 的词法作用域，并使用称为 *意大利面条堆栈* :sup:`（34）` [SPAGHETTI-STACKS][INTERLISP-TOPS20] 的东西来实现。 这种数据结构
不再是堆栈数据结构，而是实际上是一个多路径、垃圾收集的网络。 COMMON LISP 取消了意大利面条堆
栈，只提供了词法闭包 [MACARONI] 。

.. note:: （34）
   意大利面条堆栈 spaghetti stack：N元树数据结构，其中子节点具有指向父节点的指针

因此词法变量和动态变量实际上是完全不同的，完完全全的不同概念，它们恰好在 COMMON LISP 代码中语
法相同而已。我们到底为什么要这种所谓的语法二义性呢？答案很微妙，只有少数 lisp 程序员有意识地欣
赏它，但它是如此基础，值得仔细研究。这种语法二义性允许我们编写一个具有单个通用接口的宏，用于创建
在动态和词法上下文中都很有用的展开式。尽管宏的展开式的含义在它们的上下文中可能完全不同，即使其在表象之下可
能意味着完全不同的东西，我们仍然可以使用相同的宏以及该宏与其他宏的相同组合。换句话说，宏不仅在其
宏参数的内容上产生 *矛盾* :sup:`（35）` ，在其展开式的不同含义上也可能产生矛盾。我们可以使用宏来理解代码转换，而忽略
代码的语义含义，这一切都是因为代码只有在调用的地方才有意义——在宏处理期间它没有意义。语法的二义性越多，关联的宏就越强大。本书详细介绍了很多二义性语法优势的示例。动态变量和词汇变量之间的二义性
是这种 lispy（ lisp 化） 哲学的一个浅浅（但有用）的例子。一些宏是为具有强大的二义性的特定目的而创建的，有
时一个展开式可能会有两个以上的含义。

.. note:: （35）
   矛盾 ambivalent

COMMON LISP 代码中的传统约定是在特殊变量前后添加星号（ **\*** ）。 例如，可能将
**temp-special** 变量命名为 **\*temp-special\*** 。因为这个默认风格几乎就像为动态变量提供
另一个命名空间，减少了它们与词法变量的二义性，所以本书并没有完全遵循它。 星号只是默认风格，幸运
的是， COMMON LISP 没有强制要求使用。 我们不仅可以将星号从特殊变量名字中去掉，而且可以将它们添加
到词法变量名中。可能这只是风格问题。 哪种风格的弊端更小：带有星号的词法变量或没有星号的特殊变
量？ 我个人认为这两者中更简洁（不带星号）的弊端更小。 此外，词法和特殊变量的名称可以是
gensyms ，这是个超越符号上的打印名称的概念。

因此，如前所述，这本书劫持了通用的星号约定。 本书不用带星号的变量名称表示特殊变量，而是用带星号
的变量名称表示标准定义的特殊变量。

我放弃这些耳罩式变量名的最大动机是简单且主观的：我认为它们打起来很麻烦且让代码很难看。 我不会建
议你为自己的程序这样做，只是提到我多年来一直不使用耳罩式的变量，同时对 COMMON LISP 非常满意。
