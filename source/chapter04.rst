.. _chapter04:

********************
第四章：读取宏
********************


.. _4-1-runtime-at-readtime:

4.1 读取时执行
========================

::

  Syntactic sugar causes cancer of the semicolon. --Alan Perlis

lisp 不仅能直接访问已解析为 cons 单元结构的代码，而且还能访问构成程序的字符，甚至能
在字符串到达该阶段（构成程序）之前。 尽管常规宏以树的形式作用于程序，但一种称为 *读取宏*
的特殊类型的宏可以操作构成程序的原始字符。

在 lisp 中，当要定义一个非 lisp 语法时，使用 lisp 读取器（reader）是没有意义的——
那只是为了读取 lisp。 读取宏是用来在 lisp 读取器上手之前处理非 lisp 语法的方法。 lisp
读取器比其他语言更强大的原因是 lisp 有控制其各个方面行为的钩子。 特别是，lisp 允许扩展
读取器，以便非 lisp 对象实际上作为 lisp 对象读入。 就像在 lisp 之上构建应用程序并使用宏
和函数对其进行扩展一样，lisp 应用程序也可以且经常会渗入这个可扩展性维度。 发生这种情况时，
可以使用 lisp 读取器读取任何基于字符的语法，这意味着已将其转换为 lisp 语法了。

虽然常规宏完成的代码转换仅用于将 lisp 代码转换为新的 lisp 代码，但可以创建读取宏将非 lisp
代码转换为 lisp 代码。 与常规宏一样，读取宏是通过下面的函数实现的，因此可以使用 lisp 环境
的全部功能。 与提高生产力的宏一样，因为它们为程序员创建了更简洁的领域特定语言，读取宏通过将
表达式缩写到甚至不再是 lisp 表达式的程度来提高生产力。 或者说这是真的吗？

如果解析这些非 lisp 领域特定语言就是编写一个简短的读取宏，那么这些非 lisp 语言可能真的是
lisp，只是巧妙地伪装。 如果 lisp 读取器可以直接读取 XML [XML-AS-READ-MACRO]，那么从某种扭
曲的意义上说，也许 XML 实际上是 lisp。 类似地，读取宏可以将正则表达式和 SQL 查询直接读入
lisp，所以也许这些语言真的也是 lisp。 代码和数据、lisp 和非 lisp 之间这种模糊区别是许多有趣
的哲学问题的根源，这些问题从一开始就让 lisp 程序员感到困惑。

COMMON LISP 内置的读取宏是 **#**。 读取时执行宏。 这个读取宏将对象嵌入到读取的无法序列化但可
以使用一些 lisp 代码创建的结构中。 一个有趣的例子是让结构在每次被读取时变成不同的值：

.. code-block:: none

    * '(football-game
        (game-started-at
          #.(get-internal-real-time))
        (coin-flip
          #.(if (zerop (random 2)) 'heads 'tails)))

    (FOOTBALL-GAME
      (GAME-STARTED-AT 187)
      (COIN-FLIP HEADS))

即使是同一个表达式，这种结构每次读入的内容都不同：

.. code-block:: none

    * '(football-game
        (game-started-at
          #.(get-internal-real-time))
        (coin-flip
          #.(if (zerop (random 2)) 'heads 'tails)))

    (FOOTBALL-GAME
      (GAME-STARTED-AT 309)
      (COIN-FLIP TAILS))

注意 **#** 包围的两个结构。 在读取时执行，而不是在执行时执行。 完整的结构在它们执行之后形成，
并且可以通过重新执行读入的最后一个结构并将其与之前的结果进行比较，使用 **\*** 和 **+** 变量
来检查前后的结果是否一样（由 **equal** 定义）的 *REPL* :

.. code-block:: none

    * (equal * (eval +))

    T

注意，因为这些结构实际上是在读取时执行的，所以这与使用反引号不同，这将在下一节中更仔细地研究。
我们可以执行类似反引号的结构：

.. code-block:: none

    * `(football-game
        (game-started-at
          ,(get-internal-real-time))
        (coin-flip
          ,(if (zerop (random 2)) 'heads 'tails)))

    (FOOTBALL-GAME
      (GAME-STARTED-AT 791)
      (COIN-FLIP HEADS))

但是重新执行这段代码时，会得到到不同的结果，因为反引号作为执行代码的读入：

.. code-block:: none

    * (equal * (eval +))

    NIL ; unless you're really fast and lucky


.. _4-2-backquote:

4.2 反引用
=======================

反引号，有时也被叫做 *quasiquote* ，显示为 **`** （即 Esc 键下面那个键），是主流 lisp 编
程相对较新的概念，而且这个概念对于 lisp 外的语言几乎是完全陌生的。

反引号和 lisp 有一段奇异的发展历史。 据报道 [QUASIQUOTATION] 早期没人认为反引号嵌套能正常运
行，直到一位敏锐的程序员意识到它们确实正确地运行——人们对正确的概念是错的。 众所周知，反引号嵌套
很难理解。 就连 COMMON LISP 之父 Steele 也抱怨它[CLTL2-P530]。

原则上，lisp 不需要反引号。 能用反引号完成的事情都能用其他结构构建功能来完成。 然而，反引号对
宏编程非常有用，在 lisp 中意味着所有编程，以至于 lisp 专业人士已经开始严重依赖它。

首先，我们需要了解常规引用。 在 lisp 中，当在一个结构前面加上引号 （**'**） 时，就是告诉
lisp 解释器将以下结构看作原始数据，而不是要执行的代码。 更确切地说，引号作为代码读入，在执行时
会返回一段代码表单。 有时也会说引号 *停止或关闭对代码* 的执行。

反引号用来代替 lisp 中的引号。 除非某些特殊字符（叫做非引用（ **unquote** ）字符）出现在代码
中，否则反引号会和引号一样不执行代码。 顾名思义，这些非引用字符保留了执行语义。 有时会说非引用
是将重启或回到执行。

非引用主要分为三种类型：常规非引用、拼接非引用和破坏性拼接非引用。

要执行常规的非引用，需要用逗号运算符（**,**）：

.. code-block:: none

    * (let ((s 'hello))
        `(,s world))

    (HELLO WORLD)

尽管上面代码中取消引用的表达式只是个简单的 **s** 变量，但这可以是任意 lisp 表达式，在其出现在
反引号模板中的任何上下文，都可以执行计算为有意义的东西。 无论结果是什么，都会插入到出现在反引号
模版中结果列表的 *car* 位置。

在 lisp 结构符号中，可以用 **.** 显示地把一些结构放在正在创建的列表结构的 *cdr* 中。 如果在
那里放一个列表，则反引号的结果代码仍是一个有效的列表。 但如果在其中放置其他内容，将得到一个新的
非列表结构。

在反引号中拥有这种能力，就像在其他地方一样。多亏了反引号的设计，我们甚至可以在这个位置取消引用：

.. code-block:: none

    * (let ((s '(b c d)))
        `(a . ,s))

    (A B C D)

在反引号创建的列表的 *cdr* 位置插入列表插入很常见，以至于反引号通过拼接非引用更进一步。上面的
**.,** 组合很有用，但无法在列表中间插入元素。 为此，就有拼接非引用运算符：

.. code-block:: none

    * (let ((s '(b c d)))
        `(a ,@s e))

    (A B C D E)


**.** 和 **,@** 都不会修改被拼接的列表。例如，在对前面两种结构的反引号进行求值之后，**s** 仍
然会绑定到这三个元素列表 **(B C D)** 。 虽然标准没有严格要求，但允许上面 **(A B C D)** 列
表中的 **(B C D)** 与拼接列表共享列表 **s**。 然而，在列表 **(A B C D E)** 中，这个列表
结构保证在执行反引号时被重新分配，因为禁止修改正在拼接的列表。拼接非引用是非破坏性的，因为通常要
考虑反引号用作可重用的创建列表模板。 每次对反引用代码求值时，破坏性地修改不是最新分配的数据的列
表结构，这可能会对之后的展开产生不良影响。

然而，COMMON LISP 也提供了个破坏性版本的拼接非引用，可以在拼接非引用能使用的地方使用。 要进行
破坏性拼接，使用 **,.** 。 破坏性拼接的工作方式与常规拼接相同，只是在执行反引号模板期间可以修
改正在拼接的列表。 除了一个不同于常规拼接的字符之外，这种表示法还巧妙地重用了上面 **.,** cdr
的位置的取消引用。

为了验证这一点，我们在这里破坏性地修改了 **to-splice** 指向的列表：

.. code-block:: none

    * (defvar to-splice '(B C D))

    TO-SPLICE
    * `(A ,.to-splice E)

    (A B C D E)
    * to-splice

    (B C D E)

破坏性地修改要拼接的列表可能很危险。 想想以下破坏性拼接的使用：

.. code-block:: none

    (defun dangerous-use-of-bq ()
      `(a ,.'(b c d) e))

第一次调用 **dangerous-use-of-bq** 时，返回预期的答案：**(A B C D E)**。 但由于它使用破
坏性拼接并修改了一个不是新生成的列表——引用列表——可以预料到各种不良后果。 在这种情况下，第二次执
行 **dangerous-use-of-bq** 时，**(B C D)** 列表现在实际上是 **(B C D E)** 列表，并且
当反引号试图破坏性地将该列表拼接到反引号模板的其余部分时，**(E)** ——它自己的尾巴——创建了一个
包含循环的列表。 我们在[4.5 循环表达式](chapter04.md)中会详细地讨论循环。

但是，在许多情况下，破坏性拼接是完全安全的。 如果需要提高反引号结构的效率，不要被
**dangerous-use-of-bq** 吓到。 有许多操作可以创建新的列表结构，可能无论如何都要丢弃它们。
例如，拼接 **mapcar** 的结果是如此普遍和安全，以至于以下可能成为编程习惯：

.. code-block:: none

    (defun safer-use-of-bq ()
      `(a
        ,.(mapcar #'identity '(b c d))
        e))

但上面这种格式没有成为编程习惯是有原因的。 反引号最常见的用途是编写宏，这是 lisp 编程中速度最
不重要而清晰度最重要的部分。 如果在创建和解释宏时考虑拼接操作的副作用会让你分心，那可能不值得这
么麻烦。 这本书坚持常规拼接。 反引号最常见的用途是在宏构造中，但这并不是它唯一的用途。 反引号实
际上是一种有用的领域特定语言，用于将列表混在一起的领域，考虑到破坏性拼接的可能性，它变得更加有
用。

反引号是如何工作的？ 反引号是一个读取宏。 反引号结构作为代码读入，在执行时成为所需的列表。 回到
上一节关于读取时求值的示例，可以关掉美观的打印，引用反引号结构的值，并将其打印出来查看反引号结构
确切的读取方式：

.. code-block:: none

    * (let (*print-pretty*) ; bind to nil
        (print
          '`(football-game
              (game-started-at
                ,(get-internal-real-time))
              (coin-flip
                ,(if (zerop (random 2))
                  'heads
                  'tails))))
        t)

    (LISP::BACKQ-LIST
      (QUOTE FOOTBALL-GAME)
      (LISP::BACKQ-LIST
        (QUOTE GAME-STARTED-AT)
        (GET-INTERNAL-REAL-TIME))
      (LISP::BACKQ-LIST
        (QUOTE COIN-FLIP)
        (IF (ZEROP (RANDOM 2))
          (QUOTE HEADS)
          (QUOTE TAILS))))
    T

在上面这个 **打印的很丑（ugly-printed）** 的结构中，函数 **LISP::BACKQ-LIST** 与列表相
同，除了列表的打印输出比较美观。 注意，逗号运算符已消失。 COMMON LISP 在用反引号读入方面相当
自由，特别是对可以共享结构的操作。

反引用还有很多好玩的方法来解决编写一个对自身求值的 lisp 表达式这一有趣的 *难题
（non-problem）* 。 这些表达式在 Willard Quine 对其进行认真的研究之后被普遍称为
*quines*，事实上，Quine 创造了 quasiquote 一词 —— 反引号的替代名称
[FOUNDATIONS-P31-FOOTNOTE3]。 一下是个来自[QUASIQUOTATION] 中 Mike McMahon 的有趣
quine 示例：

.. code-block:: none

    * (let ((let '`(let ((let ',let))
                    ,let)))
        `(let ((let ',let)) ,let))

    (LET ((LET '`(LET ((LET ',LET))
                  ,LET)))
      `(LET ((LET ',LET)) ,LET))

为了让你不需要在心中遍历代码：

.. code-block:: none

    * (equal * +)

    T

练习：在下面的代码中，为什么将反引号展开为常规引号？ 不是引用了吗？（译者注，我自己在 sbcl 中
执行返回的是 **`Q**，而不是 **'Q**，需要和作者讨论以下）。

.. code-block:: none

    * '`q
    'Q


.. _4-3-reading-strings:

4.3 读取字符串
==========================

在 lisp 中，字符串由双引号 ( **"** ) 分隔。虽然字符串可以包含 lisp 实现的字符集中的任何字
符，但和其他语言一样，某些特殊字符是不能直接插入的。如果要在字符串中表示引号和反斜杠，需要在其前
面加上 `\\` 。 `\\` 由被称为转义字符。例如，以下是包含 **"** 和 `\\` 的字符串：

.. code-block:: none

    * "Contains \" and \\."

    "Contains \" and \\."

很明显这能顺利执行，但有时输入 `\\` 字符会变得乏味且容易出错。 当然，这是 lisp，如果不喜欢某
些东西，可以自由地，甚至鼓励去改变它。 本着这种精神，本书提出了一个名为 **#"** 或尖双引号的读
取宏。这个读取宏用于创建包含 **"** 和 `\\` 字符的字符串，而无需调用转义。

.. code-block:: none

    (defun |#"-reader| (stream sub-char numarg)
      (declare (ignore sub-char numarg))
      (let (chars)
        (do ((prev (read-char stream) curr)
            (curr (read-char stream) (read-char stream)))
            ((and (char= prev #\") (char= curr #\#)))
          (push prev chars))
        (coerce (nreverse chars) 'string)))

    (set-dispatch-macro-character
      #\# #\" #'|#"-reader|)

**井双引号** 将在其调用字符 **#** 和 **"** 后立即开始读取字符串。它将继续逐个地读取字符，直
到再次遇到 **"** 和 **#** 两个字符。 当它找到这个终止序列时，将返回由 **#"** 和 **"#** 之
间的所有字符表示的字符串。 井双引号读取宏过去用于位字符串，但 COMMON LISP 通过将位字符串修改
为 **#*** 读取宏[EARLY-CL-VOTES]，就释放了这个有用的宏字符。

这是新的井双引号执行示例：

.. code-block:: none

    * #"Contains " and \."#

    "Contains \" and \\."

注意，当 REPL 打印字符串时，仍然用 **"** 字符作为分隔符，因此 **"** 和 `\\` 字符在字符串的
打印表示中仍然被转义。这些字符串就像手动转义字符一样简单地读入。

但有时 **#"** 不够好。例如，当正在阅读的 U 语言段落中，包含了以下字符 **"#**. 正因为如此，这
一段不能用 **#"** 和 **"#** 分隔。 而且因为我讨厌逃避事情，所以请相信这不是用常规双引号分隔
的。

.. code-block:: none

    (defun |#>-reader| (stream sub-char numarg)
      (declare (ignore sub-char numarg))
      (let (chars)
        (do ((curr (read-char stream)
                  (read-char stream)))
            ((char= #\newline curr))
          (push curr chars))
        (let* ((pattern (nreverse chars))
              (pointer pattern)
              (output))
          (do ((curr (read-char stream)
                    (read-char stream)))
              ((null pointer))
            (push curr output)
            (setf pointer
                  (if (char= (car pointer) curr)
                    (cdr pointer)
                    pattern))
            (if (null pointer)
              (return)))
          (coerce
            (nreverse
              (nthcdr (length pattern) output))
            'string))))

    (set-dispatch-macro-character
      #\# #\> #'|#>-reader|)

我们需要一个读取宏，可以通过上下文自定义分隔符。 通常情况下，只需从 Larry Wall 的 Perl 语言
中寻找编程快捷方式设计的灵感即可。 Perl 是一门美丽的、设计精美的语言，并且拥有许多可以被
lisp 借鉴的好想法。 从某种意义上说，Lisp 是一个大块，也许是一个雪球，不断吸收其他编程语言的思
想，使这些思想成为自己的.

**#>** 读取宏直接受到 Perl 的 **<<** 运算符的启发。 Perl 程序员可以使用这个运算符指定一个
文本字符串作为引用字符串结束的分隔符。 **#>** 读取字符直到找到换行符，然后一个接一个地读取字
符，直到遇到与紧接在 **#>** 之后和换行符之前找到的字符相同的字符序列。

例如：

.. code-block:: none

    * #>END
    I can put anything here: ", \, "#, and ># are
    no problem. The only thing that will terminate
    the reading of this string is...END

    "I can put anything here: \", \\, \"#, and ># are
    no problem. The only thing that will terminate
    the reading of this string is..."


.. _4-4-cl-ppcre:

4.4 CL-PPCRE
======================

CL-PPCRE[CL-PPCRE]是一个基于COMMON LISP编写的高性能正则表达式库。 是由备受尊崇的 lisp 黑
客 Edi Weitz 创建的。替所有从 CL-PPCRE 和 Edi Weitz 的其他软件中受益匪浅的 lisp 专业人
士，本节献给 Edi Weitz。 当其他人在争论时，Edi 在编码； 代码比争论更有说服力。

PPCRE，对于那些还不熟悉的人来说，全称是 Portable Perl Compatible Regular
Expressions。 CL-PPCRE 与本书中的代码一样，是可移植的，因为它可以在任何符合 ANSI 的
COMMON LISP 环境中运行。 CL-PPCRE 和本书中的代码一样，是开源的并且免费提供。 尽管
CL-PPCRE 与 Perl 几乎完美兼容，但它在一些重要方面与 Perl 不同。 CL-PPCRE 为正则表达式提供
了几个值得注意的 lispy 增强。 CL-PPCRE 与 Perl 中正则表达式的实现有三种实质性的不同。

首先，CL-PPCRE 速度很快。 真的很快。 当使用正常的本机代码编译器进行编译时，基准测试表明对于大
多数正则表达式，CL-PPCRE 的速度大约是 Perl 的两倍，通常要快得多。 然而 Perl 拥有最快的非
lisp 正则表达式引擎之一：一个用 C 编写的高度优化的引擎。这怎么可能？ Perl 的底层实现肯定比用
lisp 等高级语言编写的任何东西都具有性能优势。

这种误解被称为性能神话，一般版本如下：低级语言导致代码更快，因为可以更接近硬件进行编程。 正如本
书希望解释的那样，对于复杂的系统，这个神话是错误的。 像 CL-PPCRE 这样的例子就证明了这一点。
语言越低级，就越会阻止程序员和其编译器进行真正重要的效率优化。

使用 CL-PPCRE，性能提升的技术原因很简单：用于实现 CL-PPCRE 的语言 COMMON LISP 比用于实现
Perl 的语言 C 更强大。当 Perl 读入正则表达式时，它可以执行分析和优化，但最终正则表达式将存储
到某种 C 数据结构中，供静态正则表达式引擎在尝试匹配时使用。但是在强大的语言 COMMON LISP 中，
将这个正则表达式转换成一个 lisp 程序，然后将该 lisp 程序传递给优化的、原生代码的 lisp 编译
器，用于构建余下的 lisp 系统部分，基本上不再困难。因为用 C 编译器编译的程序无法访问 C 编译
器，所以 Perl 无法将正则表达式一直编译为机器代码。 Lisp 的编译模型与 C 完全不同。在 COMMON
LISP 中，在运行时（在任何时候）编译东西是可移植的、无缝的、在与 lisp 镜像相同的过程中完成、在
不再需要时收集垃圾，并且由于其增量性质，效率很高。

CL-PPCRE 和 Perl 之间的第二个主要区别是 CL-PPCRE 不依赖于正则表达式的基于字符串的表示法。
CL-PPCRE 已从字符表示中解放出来，并允许我们将正则表达式编码为 lisp 表达式（有时称为 S 表达
式）。 由于这些表达式正是用来编写 lisp 程序和宏的符号，因此我们在抽象中获得了更多凝聚力的机
会。 请参阅 CL-PPCRE[CL-PPCRE] 的文档和代码以获取有关使用此正则表达式表示法的详细信息，以及
精心设计的 lispy 域特定语言的示例。

当然，CL-PPCRE 很棒，但为什么要在关于读取宏的章节中讨论它呢？ 答案是 CL-PPCRE 与 Perl 第三
个不同点，也是最后一个不同点。在 Perl 中，正则表达式与语言密切相关。 虽然 lisp 的语法是适应元
编程的方式，但 Perl 的语法是适应正则表达式和其他类型的语法快捷方式的方式。 在 Perl 代码中频繁
地使用正则表达式的部分原因是因为编写它们的体验很简短和轻松。

要以 Perlish 风格添加方便的程序员的接口，读取宏就会很方便。 因为编写读取宏就是编写 lisp，所以
从一个实用函数开始： **segment-reader**。 给定一个流、一个分隔符和一个计数，
**segment-reader** 将从流中读取字符，直到遇到分隔符。 如果计数大于 1，
**segment-reader** 将返回一个 cons。 这个 cons 的 car 是个字符串，而 cdr 是在给定递减小
计数参数的情况下递归调用 **segment-reader** 的结果，以获取下一个字符片段.

.. code-block:: lisp

    (defun segment-reader (stream ch n)
      (if (> n 0)
        (let ((chars))
          (do ((curr (read-char stream)
                    (read-char stream)))
              ((char= ch curr))
            (push curr chars))
          (cons (coerce (nreverse chars) 'string)
                (segment-reader stream ch (- n 1))))))

例如，从流 **t** 中读取带有 **/** 分隔符的 3 个字符段，如下所示：

.. code-block:: none

    * (segment-reader t #\/ 3)
    abc/def/ghi/

    ("abc" "def" "ghi")

Perl 程序员可能会知道这到底是怎么回事。 向拉里沃尔完全道歉，这个想法盗用两个方便的 Perl 正则
表达式运算符的语法。在 Perl 中，如果要将正则表达式与变量匹配，可以这样写

.. code-block:: perl

    $my_boolean = ($var =~ m/^\w+/);

上面代码是检查 **$var** 的内容是否以一个或多个字母数字字符开头。 类似地，如果要用替换正则表达
式，也可以使用 Perl **=~** 运算符将替换正则表达式用在字符串变量 **$var** 上，以下代码是将第
一次在 **$var** 中出现的 **dog** 替换为 **cat**：

.. code-block:: perl

    $var =~ s/dog/cat/;

Perl 语法的伟大之处在于分隔符可以是任何方便使用的字符。 如果想使用正则表达式或包含 / 字符的替
换，我们可以使用不同的字符来避免冲突：

.. code-block:: perl

    $var =~ s|/usr/bin/rsh|/usr/bin/ssh|;

.. code-block:: lisp

    #+cl-ppcre
    (defmacro! match-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
        (cl-ppcre:scan
          ,(car ,g!args)
          ,',g!str)))

    #+cl-ppcre
    (defmacro! subst-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
        (cl-ppcre:regex-replace-all
          ,(car ,g!args)
          ,',g!str
          ,(cadr ,g!args))))

定义一个读取宏来复制这两种 Perl 语法，就有机会展示一种有趣的宏技术，即双反引号。 这个想法是，
有时，就像在 **match-mode-ppcre-lambda-form** 和 **subst-mode-ppcre-lambda-form**
宏中一样，我们想要编写生成列表的代码。注意，通常在定义宏并使用单个反引号时，正在生成一个表示代码
的列表并将其从宏中返回，以便将其拼接到表达式中进行执行。使用双反引号，仍然会生成一个表示代码的列
表，但此代码在执行时将使用反引号构建的代码以返回一个列表。 在我们的例子中，这两个宏展开为代码，
可以用这些代码来创建对应用 CL-PPCRE 正则表达式有帮助的 lambda 结构。

我们在这些宏和下面的一些其他表达式前面加上 **#+** 读取宏。 在执行以下代码之前，此读取宏会测试
是否有可用的 CL-PPCRE。如果从本书加载源代码时 CL-PPCRE 不可用，则本节的功能将不可用。

.. code-block:: lisp

    #+cl-ppcre
    (defun |#~-reader| (stream sub-char numarg)
      (declare (ignore sub-char numarg))
      (let ((mode-char (read-char stream)))
        (cond
          ((char= mode-char #\m)
            (match-mode-ppcre-lambda-form
              (segment-reader stream
                              (read-char stream)
                              1)))
          ((char= mode-char #\s)
            (subst-mode-ppcre-lambda-form
              (segment-reader stream
                              (read-char stream)
                              2)))
          (t (error "Unknown #~~ mode character")))))

    #+cl-ppcre
    (set-dispatch-macro-character #\# #\~ #'|#~-reader|)

最后，可以定义一个读取器函数来将这些实用程序组合在一起，然后将此函数添加到宏调度表中。 选择用
**#~** 读取宏是因为它很好地模拟了Perl 的 **=~**，这是语法灵感的来源。

**#~** 读取宏旨在方便。 以下是如何创建正则表达式匹配函数：

.. code-block:: none

    * #~m/abc/

    #<Interpreted Function>

现在可以像调用普通函数一样将此函数应用于字符串：

.. code-block:: none

    * (funcall * "123abc")

    3
    6
    #()
    #()

返回的值来自 **cl-ppcre:scan** 函数，其文档可在 [CL-PPCRE] 中找到。 如果只对字符串是否匹
配感兴趣，则返回的第一个值不是 nil 的结果则表明匹配成功。 广义布尔值，以及为什么它们是
COMMON LISP 的一个重要特性，将在[第六章：回指宏]进一步讨论。

我们还可以创建替换正则表达式函数。 Perl 和 read 宏之间的细微差别是替换正则表达式函数不会修改
参数。 它们将返回新字符串，这些字符串是原始字符串的副本，并进行了替换。 另一个区别是，默认情况
下，这个 read 宏会替换所有匹配的模式，而不仅仅是第一个匹配的字符串。 在 Perl 中，需要在正则表
达式中添加一个全局修饰符来获得这种行为，但这里不是：

.. code-block:: none

    * (funcall #~s/abc/def/ "Testing abc testing abc")

    "Testing def testing def"

那么这是如何工作的呢？ **#~** 表达式（显然不是 lisp 表达式）读入是什么？ 表面上看，似乎是作为
函数读入的，但实际并非如此。让我们引用其中一种结构，以便可以根据 lisp 读取器来看看是什么：

.. code-block:: none

    * '#~m|\w+tp://|

    (LAMBDA (#:STR1)
      (CL-PPCRE:SCAN "\\w+tp://" #:STR1))

替换也类似：

.. code-block:: none

    * '#~s/abc/def/

    (LAMBDA (#:STR2)
      (CL-PPCRE:REGEX-REPLACE-ALL
        "abc"
        #:STR2
        "def"))

它们以 lambda 结构读入。 所以就 lisp 读取器而言，我们毕竟不是用一些有趣的非 lisp 语言编写
的。 这是一个函数指示符。由于表达式只是个列表，该列表的第一个符号是 lambda，回想以下 2.4 节：
Let It Be Lambda 中如何在函数调用的第一个参数中使用 lambda 结构来调用匿名函数：

.. code-block:: none

    * (if (#~m/^[\w-.]+$/ "hcsw.org")
        'kinda-looks-like-a-domain
        'no-chance!)

    KINDA-LOOKS-LIKE-A-DOMAIN

当使用 **funcall** 或 **apply** 来调用由 **#~** 读入的对象时，就用了 ANSI **lambda**
宏，但当表达式为第一个参数时则不使用：这是一种有用的二义性语法。 如果 **#~** 表达式读入为井引
号的 lambda 表达式，将无法在表达式的函数位置使用它们 —— 只有函数名和 lambda 表达式可以到那
里。 所以对于这两个任务，只需要一个读取宏，这是幸运的，因为它是一个庞大而复杂的宏。利用二义性语
法让我们专注于得到正确的展开，而不是跟踪不同的语法要求。 我们得到了两个，而不是一个有趣的宏。
为了节省精力，尽可能保持语法一致。

使用 CL-PPCRE 时的一个常见问题是忘记在正则表达式中转义反斜杠。 看看这样做时会发生什么：

.. code-block:: none

    * "\w+"

    "w+"

这是一个长度为 2 的字符串。反斜杠去哪儿了？ 双引号认为我们的意思是转义 **w** 字符而不是写一个
文字 `\\` 字符。 对于 **#~** 读取宏，只读取字符并查找适当的分隔符，这不是问题，可以像在
Perl 中一样编写正则表达式——无需转义。 请参阅上面 URL 正则表达式的引用。

虽然本节定义的 **#~** 读取宏已经很方便了，但仍有改进和增强的空间。 练习：改进它。 第一步明显就
是支持正则表达式修饰符，例如匹配中不区分大小写。 如果使用与 Perl 相同的语法完成，这会用到函数
**unread-char**，这在读取宏中很常见，以避免意外吞掉其他读取宏可能期望的字符。


.. _4-5-cyclic-expressions:

4.5 循环表达式
=========================

所有关于 lisp 程序是 cons 单元树的讨论实际上都是一个小小的谎言。 对此很抱歉。 Lisp 程序实际
上不是树，而是有向无环图 —— 可能具有共享分支的树。 由于执行者不关心所执行的分支来自哪里，因此执
行具有共享结构的代码并没有错。

一个有用的读取宏是 **#=**。 在 :ref:`3-5-unwanted-capture` 中，我们已经看到了如何在序列
化宏展开时使用 **#=** 宏使 lisp 输出表单。**#=** 和它的伙伴 **##** 可以创建自引用的 S 表
达式。 这可以让你毫不费力地做一些事情，例如在有向无环图中表示共享分支和其他有趣的数据结构。

但最重要的是，你可以无需拆卸和重组一个高效的内存数据结构序列化数据，其中大部分数据是共享的。 以
下是个示例，其中读入的两个 lisp 列表是不同的对象（不相等 **eq**）：

.. code-block:: none

    * (defvar not-shared '((1) (1)))

    ((1) (1))
    * (eq (car not-shared) (cadr not-shared))

    NIL

但在以下示例中，用 **#=** 读取宏序列化的数据，这两个列表实际上是同一个列表：

.. code-block:: none

    * (defvar shared '(#1=(1) #1#))

    ((1) (1))
    * (eq (car shared) (cadr shared))

    T

正如之前所提到的，我们可以毫不费力地为执行其提供共享的非循环列表结构：

.. code-block:: none

    * (list
        #1=(list 0)
        #1#
        #1#)

    ((0) (0) (0))

如果打印刚刚执行的最后一个结构，可以看到其执行方式与 lisp 执行器相同：有三个独立分支的普通列
表：

.. code-block:: none

    * +

    (LIST (LIST 0) (LIST 0) (LIST 0))

但是如果在打印时将 **\*print-circle\*** 特殊变量绑定到一个非 **nil** 值，会看到表达式根本
不是一棵树，而是一个有向无环图：

.. code-block:: none

    * (let ((*print-circle* t))
        (print ++)
        t)

    (LIST #1=(LIST 0) #1# #1#)
    T

作为另一个有趣的例子，下面代码展示的是如何通过将 cons 的 cdr 指向自身来打印无限列表，形成所谓
的循环或圆：

.. code-block:: none

    * (print '#1=(hello . #1#))

    (HELLO HELLO HELLO HELLO HELLO HELLO HELLO
    HELLO HELLO HELLO HELLO HELLO HELLO HELLO
    HELLO HELLO HELLO HELLO HELLO HELLO HELLO
    ...

因此，除非希望发生上面这种情况，否则请确保在序列化循环数据结构时设置 **\*print-circle\***
为 **t**：

.. code-block:: none

    * (let ((*print-circle* t))
        (print '#1=(hello . #1#))
        nil)

    #1=(HELLO . #1#)
    NIL

.. code-block:: lisp

    (defun cyclic-p (l)
      (cyclic-p-aux l (make-hash-table)))

    (defun cyclic-p-aux (l seen)
      (if (consp l)
        (or (gethash l seen)
            (progn
              (setf (gethash l seen) t)
              (or (cyclic-p-aux (car l) seen)
                  (cyclic-p-aux (cdr l) seen))))))

有没有一种简单的方法来判断列表结构的一部分是环的还是包含共享结构？ 有的，**cyclic-p** 谓词用
的就是最显然的算法来判断这一点：在结构中递归，使哈希表与迄今为止遇到的所有 cons 单元保持最
新。 如果遇到过一个已经存在哈希表中的 cons 单元格，那么就在那里且因此检测到了一个环或一个共享
分支。 注意，因为它只在 cons 单元中递归，所以 **cyclic-p** 无法在向量等数据结构中发现此类引
用。

最后，因为大多数（参见 [SYNTACTICALLY-RECURSIVE]）lisp 编译器禁止将循环结构传给编译器，执
行以下命令是未定义的，但可能会通过将其放入无限编译循环来破坏编译器：

.. code-block:: lisp

    (progn
      (defun ouch ()
        #1=(progn #1#))
      (compile 'ouch))


.. _4-6-reader-security:

4.6 读取器的安全
========================

可扩展性，让原本不打算或预期的事情发生的能力，几乎总是一件好事。 事实上，尽量鼓励可扩展性是
lisp 这么出色的原因。 但是，有时我们希望事物尽可能不可扩展。 特别是，我们不希望外部人员在我们
不知情或未经同意的情况下将自己的代码扩展到我们的系统中。这被称为被黑客入侵或被入侵。 今天，有趣
的计算主要是关于通信和网络。 当完全控制两个程序交换数据时，显然是信任整个系统。但是，一旦某些不
受信任的一方有可能甚至部分控制其中一个程序，信任系统就会完全崩溃，就像倒塌的纸牌屋一样。

这些安全问题的最大来源是程序员戏称的*阻抗失配（impedance mismatch）*。每当使用不完全理解的东
西时，有可能是用错了。有两种方法可以解决阻抗不匹配问题：样式（不要使用 **strcpy(3)**）和理解
（真正阅读手册页）。 Lisp 是编写安全软件的好语言，因为 lisp 比其他语言更能达到。如果你总是遵
循 lisp 做正确的假设，那么几乎不会出错。例如，如果尝试在字符串或向量的范围之外写入，这明显是有
问题的，lisp 会抛出异常并立即报告该问题。事实上，lisp 做的比预想的更正确：遇到异常后，可以选择
在程序的另一个位置重新启动程序，保留大部分计算状态。换句话说，COMMON LISP 的异常系统不会在发
生异常时自动销毁计算堆栈：可能仍想使用该堆栈。主要是由于篇幅限制，本书没有详细描述异常系统。相
反，我推荐 Peter Seibel 的 Practical COMMON LISP[PRACTICAL-CL]。

但学习 lisp 的一部分是发现一切皆可扩展。 到底该如何限制这一点？ 事实证明，这是以错误的方向思考
问题。 和所有的计算机安全领域一样，在考虑进攻之前，不能考虑防御。 在其他编程领域，可以建设性地
获得不错的结果，即通过构建和使用抽象。 在安全方面，必须进行破坏性思考。 必须试着破坏代码来查找
错误，而不是等待然后修复错误。

那么我们关注哪些攻击呢？ 除非以某种方式控制程序的输入，否则无法攻击该程序。 当然，在网络世界
中，大多数程序都是毫无用处的，除非人们提供输入。 互联网上有很多用于混洗数据的协议。我们想做的事
情种类繁多，无法为数据交换创建通用标准。 做的最好的事情是提供一个可扩展的框架，并允许程序员自定
义协议以适应正在创建的应用程序。 通常，这意味着更少的网络开销、更好的传输算法和更高的可靠性。
然而，主要优点是，当我们设计协议时，可以减少或消除阻抗失配，这就是制作安全协议的方法。

数据交换标准的问题在于，为了支持标准，应用程序要被禁止减少协议可以做的事情。 为了使应用程序符合
标准，通常要满足一些基线行为。为了制定安全协议，我们需要能够确保只接受确定可以处理的内容，除此之
外一概拒绝。

那么 lisp 交换数据的方式是什么？ 将数据输入 lisp 的机制称为 lisp 读取器，将数据取出的机制称
为 lisp 打印机。 如果你已深入本书，那么你已经知道了足够多的知识来设计和使用 lisp 协议。 当编
写 lisp 程序时，你就在使用这样的协议。 向 lisp 提供 lisp 结构与 lisp 进行交互，这通常也是与
世界其他地方交互的最佳方式。 当然，你不信任世界其他地方，因此必须采取预防措施。 记住，要考虑安
全性，就必须考虑攻击。 COMMON LISP 的设计者在设计时考虑了对读取器的攻击。 在本章前面我们描述
了 **#.** 读取宏，让读取器执行 lisp 表达式，因此可以编码不可序列化的数据结构。 为了减轻对
lisp 读取器的显示攻击，COMMON LISP 有 **\*read-eval\*** 。以下是从 CLtL2 摘抄下来的：

::

  Binding ***read-eval*** to **nil** is useful when reading data that came from
  an untrusted source, such as a network or a user-supplied data file; it
  prevents the **#.** read macro from being exploited as a "Trojan Horse" to
  cause arbitrary forms to be evaluated.

当 ANSI COMMON LISP 委员会在 1989 年 6 月投票决定引入 **\*read-eval\*** 时，他们就像攻
击者一样思考。攻击者会有什么样的特洛伊木马？从安全软件作者的角度来看，正确的答案是你能想到的最糟
糕的答案——或者更糟。要始终攻击者想要完全控制你的系统。传统上，这意味着特洛伊木马应该是一种称为
shell 代码的东西。通常是一段精心设计的机器代码，其作用类似于为攻击者提供一个 unix shell 以进
一步攻击受害者。编写此 shell 代码确实是种艺术形式，尤其是因为此类攻击通常利用的不寻常漏洞。例
如，大多数 shell 代码不能包含空字节，因为对 C 风格的字符串，这些字节会终止字符串，从而阻止包含
更多的 shell 代码。下面是一个 lisp shell 代码示例，假设受害者正在运行 CMUCL 并安装了
Hobbit 的原始 *netcat (nc)* [NETCAT] 程序：

.. code-block:: none

    #.(ext:run-program
        "/bin/nc" '("-e" "/bin/sh" "-l" "-p" "31337"))

上面代码会监听 31337 端口上的连接，并将为任何连接的人提供 unix shell 访问权限。 对于传统的渗
透，需要花费大量精力来尝试使其尽可能便携和可靠，这样才能多次成功攻击大多数目标。 这通常来说很困
难。 在 lisp 读取器攻击中，这很容易。 以下是我们如何更新 shell 代码使其在 CMUCL 和 SBCL 之
间可移植：

.. code-block:: none

    #.(#+cmu ext:run-program
      #+sbcl sb-ext:run-program
        "/bin/nc" '("-e" "/bin/sh" "-l" "-p" "31337"))

所以道德底线是在处理所有略微不信任的数据时，始终保证将 **\*read-eval\*** 绑定到 **nil**。
如果你很少使用 **#.** 读取宏，明智的选择是将 **#.** 设为 **nil** 且仅在需要使用时启用。

所以能很简单就禁用 **#.** 读取宏。 但这够了吗？ 这取决于应用程序以及什么被认为是有效的攻击。
对于交互式程序，这可能就足够了。如果我们得到坏数据，会尽快且大声地听到它。 然而，对于互联网服务
器来说，这可能还不够。 想一下这个 shell 代码：

.. code-block:: shell

    )

或是这个：

.. code-block:: none

    no-such-package:rewt3d

Lisp 通常会抛出异常，因为我们试图以不匹配的格式读取或在不存在的包中查找符号。 这很可能导致整个
应用程序停止运行。 这被称为拒绝服务攻击。 更微妙和更难调试的拒绝服务攻击是使用 **##** 和
**#=** 读取宏传递循环结构。 如果我们处理这些数据的代码没有考虑到这种形式，那么结果就是阻抗不匹
配，且很可能是个安全问题。 另一方面，也许应用程序会依赖于能够传递循环和共享数据结构。数据安全的
需求完全取决于应用程序。 幸运的是，无论有什么要求，lisp 读取器和打印机都能胜任。

.. code-block:: none

    (defvar safe-read-from-string-blacklist
      '(#\# #\: #\|))

    (let ((rt (copy-readtable nil)))
      (defun safe-reader-error (stream closech)
        (declare (ignore stream closech))
        (error "safe-read-from-string failure"))

      (dolist (c safe-read-from-string-blacklist)
        (set-macro-character
          c #'safe-reader-error nil rt))

      (defun safe-read-from-string (s &optional fail)
        (if (stringp s)
          (let ((*readtable* rt) *read-eval*)
            (handler-bind
              ((error (lambda (condition)
                        (declare (ignore condition))
                        (return-from
                          safe-read-from-string fail))))
              (read-from-string s)))
          fail)))

**safe-read-from-string** 是对读取器安全问题的部分回答。 与本书中的大多数代码相比，该函数
不太适合生产使用。 建议仔细思考应用程序的安全要求，并为应用程序调整（甚至重写）此代码。
**safe-read-from-string** 是 **read-from-string** 的一个锁定的版本。 它有默认的 lisp
*readtable* 的副本。 此副本已删除大部分有趣的读取宏，包括 **#** 调度宏。 这意味着向量、位向
量、gensyms、循环引用、**#.** 和所有其他的都没有了。 **safe-read-from-string** 甚至不允
许关键字或外来包符号。 但是，它不仅允许格式良好的列表，还允许 cons 结构。同时还允许数字和字符
串。

**safe-read-from-string** 使用 lisp 的异常系统来捕获所有由 lisp
**read-from-string** 函数抛出的异常。 如果从字符串中读取有任何问题，包括遇到不匹配的括号或
遇到在 **safe-read-from-string-blacklist** 变量中列入黑名单的其他读取宏，则
**safe-read-from-string** 将返回第二个参数的值，如果没有第二个参数，则为 **nil** （记住，
你可能希望读取 **nil**）。以下是经典的用法：

.. code-block:: lisp

    (let* ((g (gensym))
          (v (safe-read-from-string
                user-supplied-string g)))
      (if (eq g v)
        (log-bad-data ; careful how it's logged!
          user-supplied-string)
        (process v)))

当然，这个版本的安全读取字符串非常有限，可能需要修改应用程序。 特别是，可能需要关键字符号。 启
用它们很容易：当使用 **safe-read-from-string** 时，只需将不带 **:** 字符的列表绑定到
**safe-read-from-string-blacklist** 并注意符号可能驻留在多个包中（包括 **keyword**
包） 。即使删除 **:** 字符，上面的 shell 代码包也会被阻止，因为我们会在读取过程中捕获所有异
常，包括表示包不存在的错误。 如果决定从黑名单中删除 **#** 字符，将 **\*Read-eval\*** 始终
绑定为 **nil**。这样做后，可能想为 **#** 调度宏创建一个子黑名单（可能是一个大的黑名单）。 竖
线字符被列入黑名单，这样就不会读到古怪的符号。

因此，可以在觉得必要的时候尽可能严格地锁定读取器，事实上，就像应用程序允许的那样严格。但是，即使
在通过用于读取表单的软件确定不存在攻击向量之后，如何才能最大限度地减少我们认为的 lisp 表单的结
构与实际可能的结构之间的阻抗不匹配？我们必须验证它是否符合预期。一些数据标准将此过程称为针对模式
的验证，但 lisp 将其称为针对扩展的 lambda 形式的 **destructuring-bind**。所有这些术语听
起来都比其所代表的简单概念更重要。其思想是，希望确保数据的形式或结构符合给定处理的要求。
**destructuring-bind** 检查这个结构，提供了一种非常有用的模式语言，其中包括关键字和可选参
数，还有一个好处是可以在进行过程中命名结构的不同部分。

我可以举一些例子来说明如何使用 **destructuring-bind**，但实际上没有必要：我们一直在使用解
构。 当我们使用 **defmacro**、 **defmacro!** 或 **destructuring-bind** 时，我们在宏名
称之后立即插入的参数或参数列表称为扩展 lambda 列表，以强调它比对普通 lambda 列表执行的解构更
强大的事实。 使用扩展 lambda 列表，可以嵌套扩展 lambda 列表以解构任意深度的列表结构。Paul
Graham 的 On Lisp 对解构有很好的处理。 尤其是 **with-places** 宏[ON-LISP-P237]，最好在
阅读 :ref:`6-7-pandoric-macros` 之后再去看看 **with-places** 宏。

因此，每次编写宏或函数时，在某种意义上，都将宏或函数将接收的参数视为数据，并将扩展或常规
lambda 列表视为模式。 有鉴于此，数据验证似乎很容易。 Lisp 可以验证数据是否按照应有的结构进行
了构建，如果不是，则会引发错误情况。 和上面的读取器一样，在处理不太信任的数据时，应该非常仔细地
考虑可能的攻击，然后用 lisp 强大的异常和宏系统来构建一个验证方案，只允许应用程序的最低要求，并
直接映射到应用程序如何工作，减少或消除任何阻抗失配。 CL-PPCRE 正则表达式对于这项任务也是必不可
少的。 没有其他语言具有 lisp所具备的安全软件潜力，且随着时间的推移，这一点只会变得更加明显。
