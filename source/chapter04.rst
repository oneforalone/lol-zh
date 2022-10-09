.. _chapter04:

********************
第四章：读取宏 :sup:`（1）` 
********************

.. note:: （1）
   读取宏 Read Macros

.. _4-1-runtime-at-readtime:

4.1 读取时执行  :sup:`（2）` 
========================

.. note:: （2）
   读取时执行 Run-Time at Read-Time

:sup:`（3）` 
::

  句法糖会导致分号癌症。 --Alan Perlis

.. note:: （3）
   Syntactic sugar causes cancer of the semicolon.

lisp 不仅能直接访问已解析成 cons 单元结构的代码，而且还能提供对构成程序的字符的访问，甚至在到达该阶段之前构建你的程序。 尽管常规宏以树的形式在程序中工作 :sup:`（4）` ，但一种称为 *读取宏*
的特殊类型的宏可以操作构成程序的原始字符。

.. note:: （4）
   抽象语法树

在 lisp 中，当要定义一个非 lisp 语法时，使用 lisp 读取器（ reader ）是没有意义的——
那只是为了读取 lisp 。 读取宏是用来在 lisp 读取器上手之前处理非 lisp 语法的设备。 lisp
读取器比其他语言更强大的原因是 lisp 给了你控制其各个层面行为的 *钩子* :sup:`（5.1）` 。 特别是，lisp 允许你 *扩展* :sup:`（5.2）`
读取器，以便非 lisp 对象实际上作为 lisp 对象读入。 就像在 lisp 之上构建应用程序并使用宏
和函数对其进行扩展一样，lisp 应用程序也可以且经常会渗入这个可扩展维度（即扩展读取器）。 发生这种情况时，
可以使用 lisp 读取器读取任何基于字符的语法，这意味着已将其转换为 lisp 语法了。

.. note:: （5）
   钩子 hook； 扩展 extend

虽然常规宏完成的代码转换仅用于将 lisp 代码转换为新的 lisp 代码，但可以创建读取宏将非 lisp
代码转换为 lisp 代码。 与常规宏一样，读取宏是通过底层的函数实现的，因此可以使用 lisp 环境
的全部功能。 与提高生产力的宏一样，因为它们创建了更简洁的领域特定语言供程序员使用，读取宏通过允许将
表达式缩写到甚至不再是 lisp 表达式的程度来提高生产力。 亦或者？

如果我们解析这些非 lisp 领域特定语言所要做的就是编写一个简短的读取宏，那么这些非 lisp 语言可能实质就是
lisp ，只是巧妙地伪装。 如果 lisp 读取器可以直接读取 XML [XML-AS-READ-MACRO]，那么从某种扭
曲的意义上说，也许 XML 实际上是 lisp 。 类似地，读取宏可以将正则表达式和 SQL 查询直接读入
lisp ，所以也许这些语言本质上也是 lisp 。 代码和数据、lisp 和非 lisp 之间这种模糊区别是许多有趣
的哲学问题的根源，这些问题从一开始就让 lisp 程序员感到困惑。

COMMON LISP 内置的基本读取宏是 **#** 。 读取期求解宏。 这个读取宏允许你将对象嵌入到你读取的无法序列化但可
以使用一些 lisp 代码创建的形式体中。 一个有趣的例子是让形式体在每次被读取时变成不同的值：

.. code-block:: none
    :linenos:

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
    :linenos:

    * '(football-game
        (game-started-at
          #.(get-internal-real-time))
        (coin-flip
          #.(if (zerop (random 2)) 'heads 'tails)))

    (FOOTBALL-GAME
      (GAME-STARTED-AT 309)
      (COIN-FLIP TAILS))

注意 **#.** 包围的两个形式体是在读取期求解的，而不是在执行时执行。 完整的结构在它们执行之后成形，
并且可以通过重新执行读入的最后一个形式体并将其与之前的结果进行比较来看到前后等价，使用 *REPL* :sup:`【1】` 的 **\*** 和 **+** 便捷变量:

.. hint:: 【1】
   * 变量包含对前一个形式求值产生的值， + 变量包含该形式

.. code-block:: none
    :linenos:

    * (equal * (eval +))

    T

注意，因为这些结构实际上是在读取期执行的，所以这与使用反引号不同，这将在下一节中更仔细地研究。
我们可以求解使用反引号的类似结构：

.. code-block:: none
    :linenos:

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
    :linenos:

    * (equal * (eval +))

    NIL ; unless you're really fast and lucky 除非你真的又快又幸运


.. _4-2-backquote:

4.2 反引用
----------------------

*反引号* :sup:`（6.1）`  ，有时也被叫做 *准引用* :sup:`（6.2）` :sup:`【2】` ，显示为 **`**  :sup:`（7）` ，是主流 lisp 编
程相对较新的概念，而且这个概念对于 lisp 外的语言几乎是完全陌生的。

.. hint:: 【2】
   Scheme 程序员称之为 quasiquote，COMMON LISP 程序员称之为反引号

.. note:: （6）
   反引号 Backquote；准引用 quasiquote
   
.. note:: （7）
   即 Esc 键下面那个键

反引号和 lisp 有一段奇异的发展历史。 据报道 [QUASIQUOTATION] 早期没人认为反引号嵌套能正常运
行，直到一位敏锐的程序员意识到它们确实正确地运行——人们对什么是正确的概念是错的。 众所周知，反引号嵌套
很难理解。 就连 COMMON LISP 之父 Steele 也抱怨它[CLTL2-P530]。

原则上，lisp 不需要反引号。 能用反引号完成的事情都能用其他列表构造函数来完成。 然而，反引号对
宏编程非常有用，在 lisp 中意味着所有编程，以至于 lisp 专业人士已经开始严重依赖它。

首先，我们需要了解常规引用。 在 lisp 中，当在一个形式体前面加上引号 （**'**）前缀 时，就是告诉
lisp 解释器将以下结构看作原始数据，而不是要执行的代码。 更确切地说，引号作为代码读入，在执行时
会返回一段形式体。 有时也会说引号 *停止* 或 *关闭* 对代码的执行。

反引号用来代替 lisp 中的引号。 除非某些特殊字符（叫做 *消引用*  :sup:`（8）` 字符）出现在代码
中，否则反引号会和引号一样不执行代码。 顾名思义，这些消引用字符保留了执行语义。 有时会说消引用
是将 *重启* 或 *回到* 形式体的执行上。
   
.. note:: （8）
   消引用  **unquote** 

消引用主要分为三种类型：常规销引用、拼接销引用和破坏性拼接销引用。

要执行常规的销引用，需要用逗号运算符（**,**）：

.. code-block:: none
    :linenos:

    * (let ((s 'hello))
        `(,s world))

    (HELLO WORLD)
    

尽管上面代码中取消引用的表达式只是个简单的 **s** 变量，但这可以是任意 lisp 表达式，在其出现在
反引号模板中的任何上下文，都可以执行计算为有意义的东西。 无论结果是什么，都会插入到出现在反引号
模版中结果列表的 *car* 位置。

在 lisp 形式助记符中，可以用 **.** 显式地把一些结构放在我们正在创建的列表结构的 *cdr* 中。 如果在
那里放一个列表，则反引号的结果形式仍是一个有效的列表。 但如果在其中放置其他内容，我们将得到一个新的
非列表结构。

我们在反引号中拥有这种能力，就像在其他地方一样 :sup:`【3】` 。多亏了反引号的设计，我们甚至可以在这个位置取消引用：

.. hint:: 【3】
  因为反引号使用标准的读取函数，就像（几乎）其他地方一样。

.. code-block:: none
    :linenos:

    * (let ((s '(b c d)))
        `(a . ,s))

    (A B C D)

在一个由反引号模板创建的列表的 *cdr* 位置插入列表是如此常见，以至于反引号通过拼接消引用更进一步。上面的
**.,** 组合很有用，但无法在列表中间插入元素。 为此，就有“拼接消引用”运算符：

.. code-block:: none
    :linenos:

    * (let ((s '(b c d)))
        `(a ,@s e))

    (A B C D E)


**.** 和 **,@** 都不会修改被拼接的列表。例如，在对前面两个形式体的反引号进行求值之后，**s** 仍
然会绑定到这三个元素列表 **(B C D)** 。 虽然标准没有严格要求，但允许上面 **(A B C D)** 列
表中的 **(B C D)** 与拼接列表 **s** 共享结构。 然而，在列表 **(A B C D E)** 中，这个列表
结构保证在求解反引号时被重新分配，因为 @ 被禁止修改正在拼接的列表。拼接消引用是非破坏性的，因为通常我们要把反引号用作创建列表的可重复使用的模板。 每次对反引用代码求值时，破坏性地修改列表结构的不是最新分配的数据，可能会对之后的展开式产生不合需要的影响。

然而，COMMON LISP 也提供了个破坏性版本的拼接消引用，可以在拼接消引用能使用的地方使用。 要进行
破坏性拼接，使用 **,.** 。 破坏性拼接的工作方式与常规拼接相同，只是在执行反引号模板期间可以修
改正在拼接的列表。 除了一个不同于常规拼接的字符之外，这种表示法还巧妙地重用了上面 **.,** cdr
的位置的消引用。

为了验证这一点，我们在这里破坏性地修改了 **to-splice** 指向的列表：

.. code-block:: none
    :linenos:

    * (defvar to-splice '(B C D))

    TO-SPLICE
    * `(A ,.to-splice E)

    (A B C D E)
    * to-splice

    (B C D E)

破坏性地修改要拼接的列表可能很危险。 想想以下破坏性拼接的使用：

.. code-block:: none
    :linenos:

    (defun dangerous-use-of-bq ()
      `(a ,.'(b c d) e))

第一次调用 **dangerous-use-of-bq** 时，返回预期的答案： **(A B C D E)**。 但由于它使用破
坏性拼接并修改了一个不是新生成的列表——引用列表——我们可以预料到各种不良后果。 在这种情况下，第二次执
行 **dangerous-use-of-bq** 时，**(B C D)** 列表现在实际上是 **(B C D E)** 列表，并且
当反引号试图破坏性地将该列表拼接到反引号模板的其余部分时， **(E)** ——它自己的尾巴——创建了一个
包含 *循环* 的列表。 我们在[4.5 循环表达式](chapter04.md)中会详细地讨论循环。

但是，在许多情况下，破坏性拼接是完全安全的。 如果需要提高反引号结构的效率，不要被
**dangerous-use-of-bq** 吓到。 有许多操作可以创建新的列表结构，你可能无论如何都要丢弃它们。
例如，拼接 **mapcar** 的结果是如此普遍和安全，以至于以下可能成为编程习惯：

.. code-block:: none
    :linenos:

    (defun safer-use-of-bq ()
      `(a
        ,.(mapcar #'identity '(b c d))
        e))

但上面这种格式没有成为编程习惯是有原因的。 反引号最常见的用途是编写宏，这是 lisp 编程中速度最
不重要而清晰度最重要的部分。 如果在创建和解释宏时考虑拼接操作的 *副作用* 会让你分心，那可能不值得这
么麻烦。 这本书坚持常规拼接。 反引号最常见的用途是在宏构造中，但这并不是它唯一的用途。 反引号实
际上是一种有用的领域特定语言，用于将列表混合在一起的领域，考虑到破坏性拼接的可能性，它变得更加有
用。

反引号是如何工作的？ 反引号是一个读取宏。 反引号结构作为代码读入，在执行时成为所需的列表。 回到
上一节关于读取时求值的示例，我们可以关掉 *美观的打印* :sup:`（9）`  ，引用反引号结构的值，并将其打印出来查看反引号结构是如何读取的 :sup:`【4】` ：
 
.. hint:: 【4】
  我们返回 t ，所以我们看不到 print 返回的值。 **values** 也很常见

.. note:: （9）
   美观的打印 pretty printing

.. code-block:: none
    :linenos:

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

在上面这个 *打印的很丑* :sup:`（10）` 的结构中，函数 **LISP::BACKQ-LIST** 与列表相
同，除了列表的打印输出比较美观。 注意，逗号运算符已消失。 COMMON LISP 在用反引号读入方面相当
自由，特别是对允许共享结构的操作。

.. note:: （10）
   打印的很丑 ugly printed

反引用还有很多好玩的方法来解决编写一个对自身求值的 lisp 表达式这一有趣的 *非问题* :sup:`（11）` 。 这些表达式在 Willard Quine 对其进行认真的研究之后被普遍称为
*quines* ，事实上， Quine 创造了 quasiquote 一词 —— 反引号的替代名称
[FOUNDATIONS-P31-FOOTNOTE3]。 一下是个来自[QUASIQUOTATION] 中 Mike McMahon 的有趣
quine 示例：

.. note:: （11）
   非问题 non-problem

.. code-block:: none
    :linenos:

    * (let ((let '`(let ((let ',let))
                    ,let)))
        `(let ((let ',let)) ,let))

    (LET ((LET '`(LET ((LET ',LET))
                  ,LET)))
      `(LET ((LET ',LET)) ,LET))

为了避免你在心中遍历代码：

.. code-block:: none
    :linenos:

    * (equal * +)

    T

练习：在下面的代码中，为什么将反引号展开为常规引号？ 不是引用了吗？ :sup:`（12）` 

.. note:: （12）
   译者注，我自己在 sbcl 中执行返回的是 **`Q** ，而不是 **'Q** ，需要和作者讨论一下。

.. code-block:: none
    :linenos:

    * '`q
    'Q


.. _4-3-reading-strings:

4.3 读取字符串
-------------------

在 lisp 中，字符串是由双引号 ( **"** ) 定界的字符。虽然字符串可以包含 lisp 实现的字符集中的任何字
符，但你不能直接将某些特殊字符插入到字符串。如果要在字符串中插入双引号（ " ）和反斜杠（ \ ），你需要在其前
面加上反斜杠前缀 `\\` 。 `\\` 由被称为转义字符 :sup:`（13）` 。例如，以下是包含 **"** 和 `\\` 的字符串：

.. note:: （13）
   转义字符  escaping the characters 

.. code-block:: none
    :linenos:

    * "Contains \" and \\."

    "Contains \" and \\."

很明显这能顺利执行，但有时输入 `\\` 字符会变得乏味且容易出错。 当然，这是 lisp ，如果不喜欢某
些东西，可以自由地，甚至鼓励去改变它。 本着这种精神，本书提出了一个名为 **#"** 或井双引号的读
取宏。这个读取宏用于创建包含 **"** 和 `\\` 字符的字符串，而无需调用转义。

.. code-block:: none
    :linenos:

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

**井双引号** :sup:`【5】` 将在其调用字符 **#** 和 **"** 后立即开始读取字符串。它将继续逐个地读取字符，直
到再次遇到 **"** 和 **#** 两个字符。 当它找到这个终止序列时，将返回由 **#"** 和 **"#** 之
间的所有字符表示的字符串。 井双引号读取宏过去用于位字符串，但 COMMON LISP 通过将位字符串修改
为 **#*** 读取宏[EARLY-CL-VOTES]，就释放了这个有用的宏字符。
 
.. hint:: 【5】
  我们命名读取宏底层函数的约定是使用基于读取宏的字符加上一个符号，如 \#\verb " —-reader—，归于 CLtL2 （组织中）的 Steele （提出）。

这是新的井双引号执行示例：

.. code-block:: none
    :linenos:

    * #"Contains " and \."#

    "Contains \" and \\."

注意，当 REPL 打印字符串时，仍然用 **"** 字符作为分隔符，因此 **"** 和 `\\` 字符在字符串的
打印表示中仍然被转义。这些字符串就像手动转义字符一样简单地读入。

但有时 **#"** 不够好。例如，当正在阅读的 U 语言段落中，包含了以下字符 **"#** 。 正因为如此，这
一段不能用 **#"** 和 **"#** 分隔。 而且因为我讨厌转义的事物，所以请相信我并不用常规双引号定界它。

.. code-block:: none
    :linenos:

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

我们需要一个读取宏，来允许我们为每个我们使用到它的上下文自定义定界符。 通常情况下，只需从 Larry Wall 的 Perl 语言
中寻找编程快捷方式设计的灵感即可。 Perl 是一门美丽的、设计精美的语言，并且拥有许多可以被
lisp *借鉴* :sup:`（14）` 的好想法。 从某种意义上说，Lisp 是一个大块，也许是一个雪球，不断吸收其他编程语言的思
想，使这些思想成为自己的 :sup:`【6】` 。
 
.. hint:: 【6】
  这方面被引用最多的例子是对象，但还有无数其他例子，例如 FORTRAN 中的 format （格式函数）。

.. note:: （14）
  借鉴   pilfering

**#>** 读取宏直接受到 Perl 的 **<<** 运算符的启发。这个运算符允许 Perl 程序员指定一个
文本字符串作为引用字符串结束的定界符。 **#>** 读取字符直到找到换行符，然后一个接一个地读取字
符，直到遇到与紧接在 **#>** 之后和换行符之前找到的字符相同的字符序列。
​
例如：

.. code-block:: none
    :linenos:

    * #>END
    I can put anything here: ", \, "#, and ># are
    no problem. The only thing that will terminate
    the reading of this string is...END

    "I can put anything here: \", \\, \"#, and ># are
    no problem. The only thing that will terminate
    the reading of this string is..."


.. _4-4-cl-ppcre:

4.4 CL-PPCRE
----------------------

CL-PPCRE[CL-PPCRE] 是一个基于 COMMON LISP 编写的高性能正则表达式库。 是由备受尊崇的 lisp 黑
客 Edi Weitz 创建的。代表所有从 CL-PPCRE 和 Edi Weitz 的其他软件中受益匪浅的 lisp 专业人
士，本章节献给 Edi Weitz 。 当其他人在争论时，Edi 在编码； 代码比争论更有说服力。

PPCRE ，对于那些还不熟悉的人来说，全称是 Portable Perl Compatible Regular
Expressions 。 CL-PPCRE 与本书中的代码一样，是 *可移植的* :sup:`（15）` ，因为它可以在任何符合 ANSI 的
COMMON LISP 环境中运行。 CL-PPCRE 也和本书中的代码一样，是开源的并且免费提供。 尽管
CL-PPCRE 与 Perl 几乎完美兼容，但它在一些重要方面与 Perl 不同。 CL-PPCRE 为正则表达式提供
了几个值得注意的 lispy （ lisp 化）增强。 CL-PPCRE 与 Perl 中正则表达式的实现有三种实质性的不同。

.. note:: （15）
  可移植的   portable

首先， CL-PPCRE 速度很快。 真的很快。 当使用正常的本机代码编译器进行编译后，基准测试表明对于大
多数正则表达式，CL-PPCRE 的速度大约是 Perl 的两倍，通常要快得更多。 然而 Perl 拥有最快的非
lisp 正则表达式引擎之一：一个用 C 编写的高度优化的引擎。这怎么可能？ Perl 的底层实现肯定比用
lisp 等高级语言编写的任何东西都具有性能优势。

这种误解被称为 *性能神话* :sup:`（16）` ，其一般版本如下：低级语言导致更快的代码，因为可以更接近硬件进行编程。 正如本
书希望解释的那样，对于复杂的系统，这个神话是错误的。 像 CL-PPCRE 这样的例子就证明了这一点。
语言越底层，就越会阻止程序员和其编译器进行真正重要的效率优化。

.. note:: （16）
  性能神话   performance myth

使用 CL-PPCRE ，性能提升的技术原因很简单： COMMON LISP ，用于实现 CL-PPCRE 的语言，比用于实现
Perl 的语言 C 更强大。当 Perl 读入正则表达式时，它可以执行分析和优化，但最终正则表达式将存储
到某种 C 数据结构中，供静态正则表达式引擎在尝试匹配时使用。但是在 COMMON LISP 这强大的语言中，
将这个正则表达式转换成一个 lisp 程序，然后将该 lisp 程序传递给优化的、原生代码的 lisp 编译
器，用于构建你的 lisp 系统的剩下的部分，这基本上不再有困难 :sup:`【7】`  确保在编译 lisp 程序时为你调用它。）。因为用 C 编译器编译的程序无法访问 C 编译
器，所以 Perl 无法将正则表达式一直编译为机器代码。 Lisp 的编译模型与 C 完全不同。在 COMMON
LISP 中，在运行时（在任何时候也一样）编译代码是可移植的、无缝的、在与 lisp 镜像相同的进程中完成、在
不再需要时（进行）垃圾回收，并且由于其增量性质，非常高效。
 
.. hint:: 【7】
  CL-PPCRE 实际上比这里描述的更复杂。它有自己的编译函数，通常（除非你在运行时构建正则表达式）

CL-PPCRE 和 Perl 之间的第二个主要区别是 CL-PPCRE 不依赖于正则表达式的基于字符串的表示法。
CL-PPCRE 已从字符表示中解放出来，并允许我们将正则表达式编码为 lisp 形式（有时被称为 *S 表达
式* :sup:`（17.1）` ）。 由于这些表达式正是用来编写 lisp 程序和宏的符号，因此我们在抽象中获得了更多 *内聚合* :sup:`（17.2）` 的机
会。 请参阅 CL-PPCRE[CL-PPCRE] 的文档和代码以获取有关使用此正则表达式表示法的详细信息，以及
精心设计的 *lisp 风格化* 的领域特定语言的示例。

.. note:: （17）
  S 表达式   S-expressions；内聚合  cohesion 

当然，CL-PPCRE 很棒，但为什么要在关于读取宏的章节中讨论它呢？ 答案是 CL-PPCRE 与 Perl 第三
个不同点，也是最后一个不同点。在 Perl 中，正则表达式与语言密切相关。 lisp 的语法是适应元
编程的方式，而 Perl 的语法是适应正则表达式和其他类型的语法快捷方式的路线。 在 Perl 代码中频繁
地使用正则表达式的部分原因是因为编写它们的体验很简短和轻松。

要以 Perl 语言化 :sup:`（18）` 的风格添加便捷的程序员接口，读取宏就会很方便。 因为编写读取宏就是编写 lisp ，所以
我们从一个实用函数开始： **segment-reader** 。 给定一个流、一个定界符和一个计数，
**segment-reader** 将从流中读取字符，直到遇到定界符。 如果计数大于 1，
**segment-reader** 将返回一个 cons 。 这个 cons 的 car 是个字符串，而 cdr 是递归调用 **segment-reader** 的结果。给定一个递减的计数参数，获取下一个字符片段 :sup:`【8】` 。
 
.. hint:: 【8】
  在 COMMON LISP 中，当 if 形式的 test 子句缺少 else 子句时，被发现为 false ，则从 if 返回 nil 。有经验的 COMMON LISP 程序员经常依赖这种行为，就像我们在 **segment-reader** 中所做的，以一个递归建立一个列表的基本情形。

.. note:: （18）
  Perl 语言化的   Perlish

.. code-block:: none
    :linenos:

    (defun segment-reader (stream ch n)
      (if (> n 0)
        (let ((chars))
          (do ((curr (read-char stream)
                    (read-char stream)))
              ((char= ch curr))
            (push curr chars))
          (cons (coerce (nreverse chars) 'string)
                (segment-reader stream ch (- n 1))))))

例如，从流 **t** :sup:`【9】` 中读取出由 **/** 定界符的确定的 3 个部分，如下所示：
 
.. hint:: 【9】
  当从 REPL 完成时，流 t 对应于标准输入

.. code-block:: none
    :linenos:

    * (segment-reader t #\/ 3)
    abc/def/ghi/

    ("abc" "def" "ghi")

Perl 程序员可能会知道这到底是怎么进行的。 满满的歉意给拉里沃尔（ Larry Wall ），这个想法 *盗用*（他）两个便利的 Perl 正则
表达式运算符的语法。在 Perl 中，如果我们要尝试匹配一个正则表达式和一个变量，我们可以这样写

.. code-block:: perl
    :linenos:

    $my_boolean = ($var =~ m/^\w+/);

来看看是否 **$var** 的内容以一个或多个字母数字字符开头。 类似地，如果要用“替换”正则表达
式，也可以使用 Perl **=~** 运算在字符串变量 **$var** 上应用替换正则表达式，来将第一次在 **$var** 中出现的 **dog** 替换为 **cat** ：

.. code-block:: perl
    :linenos:

    $var =~ s/dog/cat/;

Perl 语法的伟大之处在于定界符可以是任何方便使用的字符。 如果想使用正则表达式或包含 / 字符的“替换”，我们可以使用不同的字符来避免冲突 :sup:`【10】` ：
 
.. hint:: 【10】
  这可能不是来自 Perl ； TeX 的逐字引用提供了类似的东西。

.. code-block:: perl
    :linenos:

    $var =~ s|/usr/bin/rsh|/usr/bin/ssh|;

.. code-block:: none
    :linenos:

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

定义一个读取宏来复制这两种 Perl 语法，给了我们展示一种有趣的宏技术的机会，即双反引号。 这个想法是，有时，就像在 **match-mode-ppcre-lambda-form** 和 **subst-mode-ppcre-lambda-form**
宏中一样，我们想要编写生成列表的代码。注意，在你正常地定义一个宏并使用单个反引号时，你正在生成一个表示代码
的列表并将其从宏中返回，以便将其拼接到表达式中进行求解。使用双反引号，你仍然生成一个表示代码的列
表，但此代码在求解时，将使用反引号构建的代码以便返回一个列表。 在我们的例子中，这两个宏展开为代码，
你可以求解（这些代码）来创建对应用 CL-PPCRE 正则表达式有帮助的 lambda 结构。

我们在这些宏和下面的一些其他表达式前面加上 **#+** 读取宏。 在执行以下代码之前，此读取宏会测试
是否有可用的 CL-PPCRE :sup:`【11】` 。如果从本书加载源代码时 CL-PPCRE 不可用，则本节的功能将不可用。
 
.. hint:: 【11】
  它通过在存储在 *features* 变量中的列表中搜索关键字符号 :CL-PPCRE 的存在来测试 CL-PPCRE 。

.. code-block: lisp
    :linenos:

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

最后，我们可以定义一个读取器函数来将这些实用程序组合在一起，然后将此函数添加到我们的宏调度表中。 选择用
**#~** 读取宏是因为它很好地模拟了Perl 的 **=~** ，这是我们的语法灵感的来源。

**#~** 读取宏旨在方便。 以下是如何创建正则表达式匹配函数：

.. code-block:: none
    :linenos:

    * #~m/abc/

    #<Interpreted Function>

现在可以像调用普通函数一样将此函数应用于字符串 :sup:`【12】` ：
 
.. hint:: 【12】
   * 变量绑定到从 REPL 中输入的最后一个表达式的求值返回的值。在这里它绑定到我们的正则表达式函数。

.. code-block:: none
    :linenos:

    * (funcall * "123abc")

    3
    6
    #()
    #()

这些值由 **cl-ppcre:scan** 函数返回，函数文档可在 [CL-PPCRE] 中找到。 如果只对字符串是否匹
配感兴趣，则返回的第一个值不是 nil 的事实意味着它匹配成功。 广义布尔值，以及为什么它们是
COMMON LISP 的一个重要特性，将在[第六章：回指宏]进一步讨论。

我们还可以创建替换正则表达式函数。 Perl 和读取宏之间的细微差别是<替换正则表达式函数>不会修改
他们的参数。 它们将返回新字符串，这些字符串是原始字符串的副本，并进行了替换。 另一个区别是，默认情况
下，这个读取宏会替换所有匹配的模式，而不仅仅是字符串中匹配的第一个。 在 Perl 中，需要在你的正则表
达式中添加一个全局修饰语来获得这种行为，但（ lisp ）这里不是：

.. code-block:: none
    :linenos:

    * (funcall #~s/abc/def/ "Testing abc testing abc")

    "Testing def testing def"

那么这是如何工作的呢？ **#~** 表达式，显然不是 lisp 表达式，作为什么读入？ 表面上看，似乎是作为
函数读入的，但实际并非如此。让我们引用这些形式的其中一种，以便我们可以根据 lisp 读取器来看看它是什么：

.. code-block:: none
    :linenos:

    * '#~m|\w+tp://|

    (LAMBDA (#:STR1)
      (CL-PPCRE:SCAN "\\w+tp://" #:STR1))

替换也类似：

.. code-block:: none
    :linenos:

    * '#~s/abc/def/

    (LAMBDA (#:STR2)
      (CL-PPCRE:REGEX-REPLACE-ALL
        "abc"
        #:STR2
        "def"))

它们是作为 lambda 结构读入。 所以就 lisp 读取器而言，我们毕竟不是用一些有趣的非 lisp 语言编写
的。 这是一个函数指示符。由于表达式只是个列表，该列表的第一个符号是 lambda，回顾一下 2.4 节：
Let It Be Lambda 中如何在函数调用的第一个参数中使用 lambda 结构来调用匿名函数：

.. code-block:: none
    :linenos:

    * (if (#~m/^[\w-.]+$/ "hcsw.org")
        'kinda-looks-like-a-domain
        'no-chance!)

    KINDA-LOOKS-LIKE-A-DOMAIN

当我们使用 **funcall** 或 **apply** 来调用由 **#~** 读入的对象时，我们使用 ANSI **lambda**
宏，但当形式（ **#~** ）为第一个参数时则不使用：这是一种有用的 *二义性语法* 。 如果我们的 **#~** 表达式作为井引用（#'）
的 lambda 表达式读入，我们将无法在一个表达式的函数位置使用它们 —— 只有函数名和 lambda 形式体可以到那
里。 所以对于这两个任务，只需要一个读取宏，这是幸运的，因为它是一个庞大而复杂的宏。利用二义性语
法的优势让我们专注于得到正确的展开式，而不是跟踪不同的语法要求。 我们得到了两个，而不是一个有趣的宏。
为了节省精力，让你的语法尽可能相似。

使用 CL-PPCRE 时的一个常见问题是在你的正则表达式中忘记 *转义* 反斜杠。 看看这样做时会发生什么：

.. code-block:: none
    :linenos:

    * "\w+"

    "w+"

这是一个长度为 2 的字符串。反斜杠去哪儿了？ 双引号认为我们的意思是转义 **w** 字符而不是写一个
文字上的 `\\` （反斜杠）字符。 对于我们的 **#~** 读取宏来说，只读取字符并查找适当的定界符，这不是一个问题，我们可以像在
Perl 中一样编写正则表达式——无需转义。 请参阅上面的< URL 正则表达式>的引用。

虽然在本节中定义的 **#~** 读取宏已经非常方便了，但仍有改进和增强的空间。 练习：改进它。 最明显的第一步就
是支持正则表达式修饰语，例如匹配中不区分大小写。 如果使用与 Perl 相同的语法完成，这会涉及使用函数
**unread-char** ，这在读取宏中很常见，以避免意外 *吞掉* 其他读取宏可能期望的字符。


.. _4-5-cyclic-expressions:

4.5 循环表达式
---------------------

所有关于 lisp 程序是 cons 单元的树的讨论实际上都是一个小小的谎言。 对此很抱歉。 Lisp 程序实际
上不是树，而是 *有向无环图* :sup:`（19）`  —— 具有可能共享分支的树。 由于执行者不关心所执行的分支来自哪里，因此执
行具有共享结构的代码并没有错。

.. note:: （19）
  有向无环图 directed acyclic graphs 

一个有用的读取宏是 **#=** 。 在[3.5 节：异常捕获]中，我们已经看到了如何在序列化宏展开式时使用
**#=** 宏将 lisp 转换为输出形式体。 **#=** 和它的伙伴 **##** 可以让你创建<自引用>的 S 表达式。 这可以让
你毫不费力地做一些事情，例如在有向无环图中表示共享分支和其他有趣的数据结构。

但最重要的是，它允许你无需拆卸和重组一个高效的内存数据结构来序列化数据，（内存数据结构）其中大部分数据是共享的。 以
下是个示例，其中读入的两个 lisp 列表是不同的对象（不相同（ not **eq** ））：

.. code-block:: none
    :linenos:

    * (defvar not-shared '((1) (1)))

    ((1) (1))
    * (eq (car not-shared) (cadr not-shared))

    NIL

但在以下示例中，用 **#=** 读取宏序列化的数据，这两个列表实际上是同一个列表：

.. code-block:: none
    :linenos:

    * (defvar shared '(#1=(1) #1#))

    ((1) (1))
    * (eq (car shared) (cadr shared))

    T

正如之前所提到的，我们可以毫不费力地将共享的无环列表结构给到求解器：

.. code-block:: none
    :linenos:

    * (list
        #1=(list 0)
        #1#
        #1#)

    ((0) (0) (0))

如果我们打印我们刚执行过的最后一个结构，我们看到与 lisp 求解器相同的方式：有三个独立分支的常规列
表：

.. code-block:: none
    :linenos:

    * +

    (LIST (LIST 0) (LIST 0) (LIST 0))

但是如果我们在打印它时将 **\*print-circle\*** 特殊变量绑定到一个非 **nil** 值，我们会看到表达式根本
不是一棵树，而是一个有向无环图：

.. code-block:: none
    :linenos:

    * (let ((*print-circle* t))
        (print ++)
        t)

    (LIST #1=(LIST 0) #1# #1#)
    T

作为另一个有趣的例子，这里（下面）的代码是如何通过将一个 cons （点对）的 cdr 指向自身来打印无限列表，形成所谓
的 *循环* 或 *圆* ：

.. code-block:: none
    :linenos:

    * (print '#1=(hello . #1#))

    (HELLO HELLO HELLO HELLO HELLO HELLO HELLO
    HELLO HELLO HELLO HELLO HELLO HELLO HELLO
    HELLO HELLO HELLO HELLO HELLO HELLO HELLO
    ...

因此，除非你希望发生上面这种情况，否则请确保在 *序列化* :sup:`（20）` 循环数据结构时设置 **\*print-circle\***
（为 **t** ）：

.. note:: （20）
  序列化 serialising

.. code-block:: none
    :linenos:

    * (let ((*print-circle* t))
        (print '#1=(hello . #1#))
        nil)

    #1=(HELLO . #1#)
    NIL

.. code-block:: none
    :linenos:

    (defun cyclic-p (l)
      (cyclic-p-aux l (make-hash-table)))

    (defun cyclic-p-aux (l seen)
      (if (consp l)
        (or (gethash l seen)
            (progn
              (setf (gethash l seen) t)
              (or (cyclic-p-aux (car l) seen)
                  (cyclic-p-aux (cdr l) seen))))))

有没有一种简单的方法来判断列表结构的一部分是环的还是包含共享结构？ 有的， **cyclic-p** 谓词用
的就是最公认的算法来判断这一点：在结构中递归，使 *哈希表* 与迄今为止遇到的所有 cons 单元保持最
新。 如果遇到过一个已经存在于你的哈希表中的 cons 单元格，那么就在那里且因此已经检测到了一个环或一个共享
分支。 注意，因为它只在 cons 单元中递归，所以 **cyclic-p** 无法在向量等数据结构中发现此类引
用。

最后，因为大多数（参见 [SYNTACTICALLY-RECURSIVE] ） lisp 编译器禁止将循环结构传给编译器，执
行以下(命令)是未定义的，但可能会通过将其放入无限编译循环来破坏你的编译器：

.. code-block:: none
    :linenos:

    (progn
      (defun ouch ()
        #1=(progn #1#))
      (compile 'ouch))


.. _4-6-reader-security:

4.6 读取器的安全
---------------------------

可扩展性，让原本不打算或未预期的事情发生的能力，几乎总是一件好事。 事实上，尽量鼓励可扩展性是
lisp 这么出色的原因。 但是，有时我们希望事物尽可能不可扩展。 特别是，我们不希望外部人员在我们
不知情或未经同意的情况下将他们自己扩展到我们的系统中。这被称为 *被黑客入侵* :sup:`（21.1）` 或 *被入侵* :sup:`（21.2）` 。 今天，有趣
的计算主要是关于通信和网络。 当完全控制两个程序交换数据时，显然是信任整个系统。但是，一旦某些不
受信任的一方有可能甚至部分控制其中一个程序，信任系统就会完全崩溃，就像倒塌的纸牌屋一样。

.. note:: （21）
  被黑客入侵 hacked；被入侵 rooted

这些 *安全* :sup:`（22.1）` 问题的最大来源是程序员戏称的 *阻抗失配* :sup:`（22.2）`。每当你使用一些你不完全理解的东
西时，就有可能是你用错了。有两种方法可以解决阻抗不匹配问题：样式（不要使用 **strcpy(3)** ）和理解
（真正阅读手册页）。 Lisp 是编写安全软件的好语言，因为 lisp 比任何其他语言更能达到预期。如果你总是遵
循< lisp *正确* 地做事>的假设，那么你几乎不会出错。例如，如果尝试在字符串或向量的边界之外写入，这明显是有
问题的， lisp 会抛出异常并立即大声地通知你这个问题。事实上， lisp 做的比你预想的更 *正确* ：遇到异常后，你可以选择
在你的程序的另一个位置 *重新启动* 程序，保留大部分计算状态。换句话说， COMMON LISP 的异常系统不会在发
生异常时自动销毁计算堆栈 :sup:`【13】` ：你可能仍想使用该堆栈。主要是由于篇幅限制，本书没有详细描述异常系统。相
反，我推荐 Peter Seibel 的 *Practical* COMMON LISP[PRACTICAL-CL] 。
 
.. hint:: 【13】
   实际上称为状况系统（ condition system ），因为它不仅仅对异常有用。
   
.. note:: （22）
  安全 security ；阻抗失配 impedance mismatch
 
但学习 lisp 的一部分是认识到一切皆可扩展。 到底该如何限制这一点？ 事实证明，我们正在以错误的方向思考问题。 和所有的计算机安全领域一样，除非你已经考虑到进攻，否则无法考虑防御。 在其他编程领域，可以建设性地
获得不错的结果，即通过构建和使用抽象。 在安全方面，你必须进行破坏性思考。 你通过破坏代码来尝试找出
错误，而不是等待（错误出现）然后修复错误。

那么哪些攻击是我们关注的呢？ 除非你以某种方式控制程序的输入，否则无法攻击该程序。 当然，在我们的网络世界
中，大多数程序都是毫无用处的，除非人们可以给其输入。 在互联网上有很多用于混洗数据的协议 :sup:`【14】` 。我们想做的事
情种类太繁多，以至于无法为数据交换创建通用标准。 可以做的最好的事情是提供一个可扩展的框架，并允许程序员自定
义协议以适应正在创建的应用程序。 通常，这意味着更少的网络开销、更好的传输算法和更高的可靠性。
然而，主要优点是，当我们设计协议时，可以减少或消除阻抗失配，这就是制作安全协议的方法。
 
.. hint:: 【14】
  我帮助 Nmap Security Scanner（ Nmap 漏洞扫描器）项目维护的 nmap-service-probes 文件是此类服务中最全面、更新最频繁的数据库之一。
   
数据交换标准的问题在于，为了支持标准，应用程序被禁止减少协议的可用性。 为了使应用程序符合
标准，通常必须要满足一些基准行为。为了制定安全协议，我们需要能够确保只接受我们确定可以处理的内容，除此之
外一概拒绝。

那么 lisp 交换数据的方式是什么？ 将数据输入 lisp 的机制称为 *lisp 读取器* :sup:`（23.1）` ，将数据取出的机制称
为 *lisp 打印器* :sup:`（23.2）` 。 如果你已深入本书，那么你已经知道了足够多的知识来设计和使用 lisp 协议。 当你编
写 lisp 程序时，你就在使用这样的协议。 向 lisp 提供 lisp 结构与 lisp 进行交互，这通常也是与
世界其他地方交互的最佳方式。 当然，你不信任世界其他地方，因此必须采取预防措施。 记住，要考虑安
全性，就必须考虑攻击。 COMMON LISP 的设计者在设计时考虑了对读取器的攻击。 在本章前面我们描述
了 **#.** 读取宏，让读取器执行 lisp 表达式，因此可以编码不可序列化的数据结构。 为了减轻对
lisp 读取器的明显攻击，COMMON LISP 有 **\*read-eval\*** 。以下是从 CLtL2 摘抄下来的：
   
.. note:: （23）
  lisp 读取器 lisp reader ；lisp 打印器 lisp printer
 
::

  Binding ***read-eval*** to **nil** is useful when reading data that came from
  an untrusted source, such as a network or a user-supplied data file; it
  prevents the **#.** read macro from being exploited as a "Trojan Horse" to
  cause arbitrary forms to be evaluated.
  将 *read-eval* 绑定到 nil 在读取来自不受信任的来源（例如网络或用户提供的数据文件）的数据时很有用；它可以防止读取宏“#.”被“特洛伊木马”利用而导致任意形式被求解。

当 ANSI COMMON LISP 委员会在 1989 年 6 月投票决定引入 **\*read-eval\*** 时，他们就像攻
击者一样思考。攻击者会有什么样的特洛伊木马？从安全软件作者的角度来看，正确的答案是，你能想到的最糟
糕的——或者再糟糕一些的，要始终认为攻击者想要完全控制你的系统。传统上，这意味着特洛伊木马应该是一种称为
shell 代码 :sup:`（24）` 的东西。通常是一段精心设计的机器代码，其作用类似于为攻击者提供一个 unix shell , 进
一步以 root 权限攻击受害者。编写此 shell 代码确实是种艺术形式，尤其是因为此类攻击通常利用的不寻常情况（漏洞）。例
如，大多数 shell 代码不能包含空字节，因为对 C 风格的字符串，这些字节会终止字符串，从而阻止包含
更多的 shell 代码。下面是一个 lisp shell 代码示例，假设受害者正在运行 CMUCL 并安装了
Hobbit 的原始 *netcat* ( **nc** ) [NETCAT] 程序：
   
.. note:: （24）
  shell 代码 shell code 
 
.. code-block:: none
    :linenos:

    #.(ext:run-program
        "/bin/nc" '("-e" "/bin/sh" "-l" "-p" "31337"))

上面代码会监听 31337 端口上的连接，并将为任何连接的人提供 unix shell 访问权限。 对于传统的渗
透，需要花费大量精力来尝试使其尽可能的可移植和可靠，这样才能频繁地成功攻击（获取 root 权限）最多数量的目标。 通常这是非常困
难的。 在 lisp 读取器攻击中，这很容易。 以下是我们如何更新 shell 代码使其在 CMUCL 和 SBCL 之
间可移植：

.. code-block:: none
    :linenos:

    #.(#+cmu ext:run-program
      #+sbcl sb-ext:run-program
        "/bin/nc" '("-e" "/bin/sh" "-l" "-p" "31337"))

所以训诫是在处理你甚至略微不信任的数据时，始终确保将 **\*read-eval\*** 绑定到 **nil**。
如果你很少使用 **#.** 读取宏，明智的选择是将 **#.** 设为 **nil** 且仅在需要使用时启用它。

所以我们可以很简单地就禁用 **#.** 读取宏。 但这够了吗？ 这取决于应用程序以及什么被认为是有效的攻击。
对于交互式程序，这可能就足够了。一旦我们得到坏数据，就会尽快且大声地听到它。 然而，对于互联网服务
器来说，这可能还不够。 考虑一下这个 shell 代码：

.. code-block:: none
    :linenos:

    )

或是这个：

.. code-block:: none
    :linenos:

    no-such-package:rewt3d

Lisp 通常会抛出异常，因为我们试图读入不平衡的形式或者在不存在的包中查找符号。 这很可能导致整个
应用程序停止运行。 这被称为 *拒绝服务* :sup:`（25）` 攻击。 更微妙和更难调试的拒绝服务攻击是使用 **##** 和
**#=** 读取宏传递循环结构。 如果我们处理这些数据的代码没有考虑到这种形式，那么结果就是阻抗不匹
配，且很可能是个安全问题。 另一方面，也许应用程序会依赖于能够传递循环和共享的数据结构。数据安全的要求完全取决于应用程序。 幸运的是，无论有什么要求， lisp 读取器和打印器都能胜任。
   
.. note:: （25）
  拒绝服务  denial of service 
 
.. code-block:: none
    :linenos:

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
不太适合生产使用。 建议你仔细思考你的应用程序的安全要求，并为你的应用程序调整（甚至重写）此代码。
**safe-read-from-string** 是 **read-from-string** 的一个锁定的版本。 它有默认的 lisp *可读取* :sup:`（26）` 的副本。 此副本已删除大部分有趣的读取宏，包括 **#** 调度宏。 这意味着向量、位向
量、 gensyms 、循环引用、**#.** 和所有其他的都没有了。 **safe-read-from-string** 甚至不允
许关键字或外来包符号。 但是，它不仅允许格式良好的列表，还允许 cons 结构。同时还允许数字 :sup:`【15】` 和字符
串。
 
.. hint:: 【15】
  练习：不允许的一类数字是什么？
   
.. note:: （26）
  可读取  readtable
 
**safe-read-from-string** 使用 lisp 的异常系统来捕获所有由 lisp
**read-from-string** 函数抛出的错误。 如果从字符串中读取有任何问题，包括遇到不平衡的括号或
遇到在 **safe-read-from-string-blacklist** 变量中列入黑名单的其他读取宏，则
**safe-read-from-string** 将返回第二个参数传递的值，如果没有提供第二个参数，（返回值）则为 **nil** （记住，
你可能希望读取 **nil** ）。以下是经典的用法 :sup:`【16】` ：
 
.. hint:: 【16】
  当然，如果我们在宏中使用它，我们会使用 defmacro！ 及其自动生成符号。
   
.. code-block:: none
    :linenos:

    (let* ((g (gensym))
          (v (safe-read-from-string
                user-supplied-string g)))
      (if (eq g v)
        (log-bad-data ; careful how it's logged!
          user-supplied-string)
        (process v)))

当然，这个版本的 **safe-read-from-string** 是受限的，可能需要针对你的应用程序进行修改。 特别是，你可能需要（适用）关键字符号。 启
用它们很容易：当你使用 **safe-read-from-string** 时，只是将不带 **:** 字符的列表绑定到
**safe-read-from-string-blacklist** ，并留意你的符号可能驻留在多个包中（包括 **keyword**
包） 。即使你删除 **:** 字符，上面包的 shell 代码包会被阻止，因为我们会在读取过程中捕获所有异
常，包括表示包不存在的错误。 **\*Read-eval\*** 始终
绑定为 **nil** ，以防你决定从黑名单中删除 **#** 字符。如果你这样做，你可能想为 **#** 调度宏创建一个子黑名单（可能是一个大的黑名单）。 竖
线字符被列入黑名单，这样就不会读到古怪的符号。

因此，我们可以根据需要尽可能严格地锁定读取器，事实上，就像应用程序允许的那样严格。但是，即使通过用于读取一个形式体的软件确定了不存在<攻击向量>，如何才能最大限度地减少我们认为的 lisp 形式体的结
构与实际可能的结构之间的阻抗不匹配？我们必须 验证它是否符合我们预期。一些数据标准将此过程称为针对一个 *模式* 
的 *验证* ，但 lisp 将其称为针对 *扩展的 lambda 形式* 的 **destructuring-bind** （解构绑定）。所有这些术语听
起来都比其所代表的简单概念更重要。其构思是，希望你想要确保你的数据是，对于给定的处理，你期望的形式或结构。
**destructuring-bind** 为我们检查这个结构，提供了一种非常有用的模式语言，其中包括关键字参数和可选参
数，还有一个好处是可以在进行过程中命名结构的不同部分。
   
我可以举一些例子来说明如何使用 **destructuring-bind** ，但实际上没有必要：我们一直在使用解
构。 当我们使用 **defmacro** 、 **defmacro!** 或 **destructuring-bind** 时，我们在宏名
称之后立即插入的参数或参数列表被称为“扩展了的 lambda 列表”，以强调它比对普通 lambda 列表执行的解构更
强大的事实。 使用扩展了的 lambda 列表，我们可以嵌套扩展 lambda 列表以解构任意深度的列表结构。Paul
Graham 的 On Lisp 对解构有很好的论述。 尤其是 **with-places** 宏[ON-LISP-P237]，最好在
阅读[6.7 潘多拉宏]之后再去看看 **with-places** 宏。

因此，每次你编写宏或函数时，在某种意义上，都将宏或函数将接收的参数视为数据，并将扩展或常规
lambda 列表视为模式。 有鉴于此，数据验证似乎很容易。 Lisp 可以验证我们的数据是否按照应有的结构进行
了构建，如果不是，则会引发错误情况。 和上面的读取器一样，在处理我们甚至稍微不太信任的数据时，我们应该非常仔细地
考虑可能的攻击，然后使用 lisp 强大的异常和宏系统来构建一个验证方案，只允许应用程序要求的最低限度，并
直接映射到应用程序如何工作，减少或消除任何阻抗失配。 CL-PPCRE 正则表达式对于这项任务也是必不可
少的。 没有其他语言具有 lisp 所具备的安全软件潜力，且随着时间的推移，这一点只会变得更加明显。
