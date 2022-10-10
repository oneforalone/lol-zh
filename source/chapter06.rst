.. _chapter06:

***********************************
第六章：回指 :sup:`（1）` 宏
***********************************
   
.. note:: （1）
  回指  Anaphoric


.. _6-1-more-phors:

6.1 更多的回指语？ :sup:`（2）`
====================
   
.. note:: （2）
  More Phors?

Paul Graham 的 OnLisp_ 中一些最有趣的宏是回指宏（ anaphoric macros ）。回指宏是特意从
提供给宏的结构中捕捉变量的宏。多亏了它们的明显的具体说明，这些特意捕获的变量给出我们控制宏展开
的窗口。通过这些窗口，我们可以通过组合来控制宏展开。

经典的回指像那些在 OnLisp_ 中被命名为字面上的单词 :sup:`【1】` anaphor 和它的复数形式 anaphora 。回指
的意思是捕获 U 语言的一个空闲单词，然后将这个单词用在之后的 U 语言中。在编程术语中，
实现典型的回指意味着在你的代码中——或者你即将编写的代码中——找到能够受益于之前相关的表达式的结果的位置。
Graham 的回指和相关代码值得深究。特别是宏 **defanaph** ，它支持一些有趣的自动回指
（ automatic anaphor ）编程的类型。
   
.. hint:: 【1】 
  一种U语言引语
   
经过一段时间的使用， **alambda** 被发现是 OnLisp_ 中最好用的回指宏。它也是一个最简单和
最优雅的回指宏及特意捕捉变量的展示。

.. code-block:: none
    :linenos:

    ;; Graham 's alambda
    (defmacro alambda (parms &body body)
      `(labels ((self ,parms ,@body))
          #'self))

通过 **alambda** ，我们捕捉到 **self** 这个变量名，所以我们就可以使用它来引用到我们构建的非常匿名的函数。换句话说，递归就像调用 **self** 一样简单。例如，下面的函数返回从 **n** 到 1 的一个数字列
表 :sup:`【2】` ：
  
.. hint:: 【2】 
  如果条件为假，则 if 形式体上的缺席的第三个子句返回 nil ，它是一个列表。

.. code-block:: none
    :linenos:

    (alambda (n)
      (if (> n 0)
        (cons
          n
          (self (- n 1)))))

**alambda** 让我们的代码更直观、更易读，并允许我们会改变关于是否一个匿名函数应该能够像添加一个字母那样轻松地调用自身的思维 :sup:`【3】` 。因为 **alambda** 对 **self** 绑定的明显的具体说明，而且使用 **alambda** 的唯
一原因就是利用这个绑定，不想要的变量捕获不再是一个问题。
  
.. hint:: 【3】 
  这是没有尖引号 lambda 形式的另一个原因。将一个带引号的 lambda 形式改变为 alambda 形式也需要删除两个字符。
  
OnLisp* 中的另一个方便的回指宏是 **aif** ，这个宏将测试子句的结果绑定到 **it** ，以便
true 子句(次要的子句或随之而来的子句)使用 :sup:`【4】` 。 **aif** 用了一个 COMMON LISP 很重要的特性：
广义布尔值。在 COMMON LISP 中，任何非 nil 值都是一个布尔值真，所以 COMMON LISP 程序员
通常会在真值中嵌入有趣的信息。保留了真值和假值的语言(尤其是 Scheme )使用显式布尔值，
这有时会强制你抛出额外的信息来满足冗余的类型约束。 Scheme 实际上添加了一个蹩脚产品（ kludge ），以支持 **if** 、 **cond** 、 **and** 、 **or** 和 **do** 来接受非布尔值 :sup:`【5】` 。当然，COMMON LISP 的设计
才是对的 —— 一切都是布尔值。
  
.. hint:: 【4】 
  练习：为什么 false （第三或交替）子句永远不会使使用这个回指？
    
.. hint:: 【5】 
  根据 Scheme 的 boolean？ 谓词。
  
.. code-block:: none
    :linenos:

    ;; Graham 's aif
    (defmacro aif (test then &optional else)
      `(let ((it ,test))
        (if it ,then ,else)))

还必须指出， **aif** 和 **alambda** ，就像所有的回指宏一样，违反了词法透明。用现在流行的话来
说，它们是不清洁（ unhygienic ）的宏。也就是说，像本书中的许多宏一样，回指宏无形地引入了词法绑
定，因此不能用严格执行清洁的宏系统创建。即使是绝大多数 Scheme 系统，这个在清洁方面试验最多的
平台，也有不清洁的 defmacro 风格的宏 —— 大概是因为就连 Scheme 的实现者也没有非常认真地对待清洁吧。
就像自行车上的辅助轮一样，清洁系统多半是玩具，甚至在获得了些许层次的技能之后，应该被丢弃。

是的，还有很多关于特定变量捕获的有趣的事我们可以做。还有很多（回指语） phors 。本书和 Graham 的 OnLisp_
只描述了这种技术内在潜力的一小部分。更多不可思议的创造将会出现在回指宏的智能应用中。

在通过读取宏穿插简短地介绍了下回指宏后，本章的剩余部分将介绍些许的、具体的回指宏应用，
一个与本书的中心主旨相关的：词汇闭包 —— lambda 之上的 let ( let over lambda )。本章的大部分内容将介绍一些用于
定制、调整和扩展闭包的有趣的回指宏。虽然这些主题在实际代码中的应用非常实用，但它们的
主要目的是作为一个平台来讨论回指宏的属性和多样性。用宏来扩展闭包的概念是当前的一个热点
研究课题。


.. _6-2-sharp-backquote:

6.2 尖反引号（ Sharp-Backquote：#` ）
========================================

尽管大多数回指由常规宏引入，但读取宏（ read 宏）也具有潜力引入代码，为我们无形地创建绑定。当读取宏（ read
宏）这样做时，它们被称为读取回指（ read anaphora ）。本节介绍了一个这样的读取宏，虽然
它本身很不太大，但它却是本书中最有用的宏之一，连我自己都感到惊讶。我已经尽可能快地引入
了这个宏，以便它可以用于其余的代码。已经有几个宏使用了它

.. code-block:: none
    :linenos:

    (defun |#`-reader| (stream sub-char numarg)
      (declare (ignore sub-char))
      (unless numarg (setq numarg 1))
      `(lambda ,(loop for i from 1 to numarg
                      collect (symb 'a i))
          ,(funcall
            (get-macro-character #\`) stream nil)))

      (set-dispatch-macro-character
        #\# #\` #'|#`-reader|)

尖反引号就是个读取宏，作为 lambda 结构读入。默认情况下，这个 lambda 结构只接收一个
参数： **a1** 。然后这个读取宏递归地调用所提供的流的 **read** 函数。下面是一个停止求值(通过
引用)的例子，这样我们就可以观察“读取回指宏” :sup:`【6】` 的透明的介绍：
  
.. hint:: 【6】 
  捕获符号的前缀，“ a ”，当然代表回指语。
   
.. code-block:: none
    :linenos:

    * '#`((,a1))

    (LAMBDA (A1)
      `((,A1)))

该读取宏抽象出一个通用宏模式。例如，如果我们有一个（多）变量的列表，并且要创建一个 let 绑定的列表（译者注：也就是 let 操作符使用的格式内容，如 let 用法：
(let ((a 1)
      (b 2)))
      这里就是指创建出 let 后面的那部分内容，即：
      ((a 1)
       (b 2))
       。）
将每个变量绑定到一个符号(假设为 **empty** )，就可以像这样使用 **mapcar** ：

.. code-block:: none
    :linenos:

    * (mapcar (lambda (a)
                (list a ''empty))
        '(var-a var-b var-c))

    ((VAR-A 'EMPTY)
    (VAR-B 'EMPTY)
    (VAR-C 'EMPTY))

但特别是对复杂的列表结构，这样写就显得有点乱，所以 lisp 程序员喜欢用反引号将其引用提高
一层：

.. code-block:: none
    :linenos:

    * (mapcar (lambda (a)
                `(,a 'empty))
        '(var-a var-b var-c))

    ((VAR-A 'EMPTY)
    (VAR-B 'EMPTY)
    (VAR-C 'EMPTY))

我们新的回指-引进读取宏隐藏了 lambda 结构：

.. code-block:: none
    :linenos:

    * (mapcar #`(,a1 'empty)
        '(var-a var-b var-c))

    ((VAR-A 'EMPTY)
    (VAR-B 'EMPTY)
    (VAR-C 'EMPTY))

上述代码中在符号 **a1** 中的字符是 **1** 的原因是，读取宏的使用者可以依赖于提供给 **numarg** 参数的数
字来引入读取宏的一个变量回指数字：

.. code-block:: none
    :linenos:

    * '#2`(,a1 ,a2)

    (LAMBDA (A1 A2)
      `(,A1 ,A2))

所以我们可以同时在不止一个列表中映射（ **mapcar** ）尖反引号（ sharp-backquote（#`) ） 表达式：

.. code-block:: none
    :linenos:

    * (let ((vars '(var-a var-b var-c)))
        (mapcar #2`(,a1 ',a2)
          vars
          (loop for v in vars
                collect (gensym
                          (symbol-name v)))))

    ((VAR-A '#:VAR-A1731)
    (VAR-B '#:VAR-B1732)
    (VAR-C '#:VAR-C1733))

另一种考虑尖反引号（ sharp-backquote ）的方法是，它像 **format** 函数是字串插值一样的列表插值（ interpolation ）。就像 **format** 让我们使用带有槽（ slot ）的模板，槽是用单独的参数的值填充
一样，尖反引号（ sharp-backquote ）让我们将列表插值的结构与想要拼接的值分开。由于前面描述的列表的函数位置的 lambda 结构和使用 **lambda** 宏展开成函数的 lambda 结构之间的语法二义性，还可以
使用尖反引号（ sharp-backquote ）作为函数调用中的第一个元素：

.. code-block:: none
    :linenos:

    * (#3`(((,a1)) ,@a2 (,a3))
          (gensym)
          '(a b c)
          'hello)

    (((#:G1734)) A B C (HELLO))

与 **format** 不同的是，尖反引号（ sharp-backquote ）不使用顺序定位，相反，它使用我们的回指绑定上面的数字。因此，顺序可以
打乱，甚至可以在绑定中多次拼接：

.. code-block:: none
    :linenos:

    * (#3`(((,@a2)) ,a3 (,a1 ,a1))
          (gensym)
          '(a b c)
          'hello)

    (((A B C)) HELLO (#:G1735 #:G1735))

练习： **gensym** 符号 **#:G1735** 的引用看起来是指向同一个符号，但是，当然，你永远无法通过查看
它们的打印名称来真正地分辨出 **gensym** 。这些符号相同（ **eq** ）吗？相同或不相
同的原因是什么？


.. _6-3-alet-and-finite-state-machines:

6.3 **alet** 和有限状态机
========================================

使用 **lambda** 和 **if** ，这里只有一个有用的回指配置，但却是最有趣的回指宏类型，该宏是以
不可预见的方式使用展开式。本节 —— 甚至本章的大部分内容 —— 是基于这样一个宏： **alet** 。
有哪些额外的绑定对 let 结构主体中的结构可能有用呢？ **let** 真正的目的就是创建这样的绑定，因此捕捉给到已经完成的 let 形式的变量引入。但是， **let** 宏的增强可以完全访问所有给到它的结构，甚至是打算用新绑定来求解的表达式的主体。那么主体中最有用的部位是什么呢？在大多数情况
下，主体中最有用的部分就是主体中的最后一个结构，因为该结构的结果将从 let 语句本身
返回 :sup:`【7】` 。我们已经看到，当返回一个引用由 **let** 创建的绑定的 lambda 表达式时，结果是一个
词法闭包 —— 一个通常存储并用于以后访问 let 语句中的变量的对象。因此，扩展我们的
闭包对象模拟， **alet%** 宏的行为与 **let** 特殊结构完全相似，除了 **alet%** 从主体中捕获
符号 **this** 并将其绑定到该结构主体中的最后一个表达式 —— 来作为闭包被返回 :sup:`【8】`。
  
.. hint:: 【7】 
  因为 let 提供了一个隐含的 progn 。
      
.. hint:: 【8】 
  使用 Setq 以便绑定到 this 的形式体在通过 letargs 给出的其他参数的词法范围内定义。
   
.. code-block:: none
    :linenos:

    (defmacro alet% (letargs &rest body)
      `(let ((this) ,@letargs)
        (setq this ,@(last body))
        ,@(butlast body)
        this))

当我们在 lambda 结构中有初始化的代码，且不想重复初始化时， **alet%** 就很有用。因为
**this** 绑定到要返回的 lambda 结构，所以我们可以在 let 封装返回它之前执行它。
下面是一个闭包，它的构造展示了一个简单的 **alet%** 用例，避免了重复它的重置和
初始化代码：

.. code-block:: none
    :linenos:

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

我们可以依次调用它来改变 **sum** 、 **mul** 和 **expt** 的值:

.. code-block:: none
    :linenos:

    * (loop for i from 1 to 5 collect (funcall * 2))

    ((2 2 4)
    (4 4 16)
    (6 8 256)
    (8 16 65536)
    (10 32 4294967296))

我们现在也可以调用 **:reset** 方法来重置这个闭包。注意多亏了 **alet%** ，我们只需要在一个
位置写重置的基本情况（将 **sum** 置为 0 ， **mul** 置为 1 ， **expt** 置为 2 ）：

.. code-block:: none
    :linenos:

    * (funcall ** :reset)

    NIL

现在，该闭包中的变量都被重置了，从一开始我们就可以看到一个新的序列了：

.. code-block:: none
    :linenos:

    * (loop for i from 1 to 5 collect (funcall *** 0.5))

    ((0.5 0.5 1.4142135)
    (1.0 0.25 1.1892071)
    (1.5 0.125 1.0905077)
    (2.0 0.0625 1.0442737)
    (2.5 0.03125 1.0218971))

值得一提的是 **alet%** 改变了 let 主体中结构的求解顺序。如果你去看一下这个的展开式的话，
你就会发现主体中最后一个结构实际上是第一个执行的，随后该结果在其他结构执行前会绑定
到词法变量 **this** 上。但是，一旦最后一个参数是常量，这个重新排序不会产生差异。记住，
lambda 表达式 :sup:`【9】` 是个常量值，因此特别适合用在 **alet%** 中。
  
.. hint:: 【9】 
  Dlambda 展开为 lambda 形式。
   

.. code-block:: none
    :linenos:
       
(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

与许多宏增强一样，因为有许多可用的自由度，对该宏的改进是反常的。虽然这里有许多
可能性，但本节将考虑一种这类具体的改进。可以让 **alet%** 不返回其主体的最后一个
结构（我们预期是 lambda 结构)，而是一个在 let 结构词法作用域内查找另一个函数的
函数，然后调用该函数。这有时又被称为间接调用（ indirection ），我们返回的是一个
使用指针间接引用查找函数的函数，然后使用该函数，而不是返回一个函数来执行某些操作。
出于好的缘由，间接在编程语言中是一个普遍存在的概念。它允许我们在运行时改变在编译已被确定的没有间接的（部分）。 Lisp 让我们用比许多其他编程语言更简洁、更有效的方式使用间接。 **alet**
是 **alet%** 加入了间接的版本，允许我们作为闭包返回的函数现在被 alet 主体内部的代码
访问或替换，或者，如果我们使用 **dlambda** 的话（很快就会介绍），甚至是 alet 主体
外部。

既然我们可以用 **alet** 宏更改在调用闭包时执行的函数，我们可以使用名为 alet over
alambda 的模式创建一对相互引用的函数。只要所有的状态都变回原来的状态 ——
而不是相互转换 ——封装在 lambda 之外的 alet （ alet over alambda ） 是指定无名状态机的一种便捷的方法。

下面就是个典型的计数器闭包，接收参数 **n** ，当传递符号 **invert** 作为参数而不是
数字时，它的方向可以在递增和递减之间通过 **n** 切换：

.. code-block:: none
    :linenos:

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
    :linenos:

    * (setf (symbol-function 'alet-test) *)

    #<Interpreted Function>

开始时，是增加的：

.. code-block:: none
    :linenos:

    * (alet-test 10)

    10

但是，我们可以通过将符号  **invert** 传递给闭包来改变要调用内部 lambda 表达式的实际函数：

.. code-block:: none
    :linenos:

    * (alet-test 'invert)

    #<Interpreted Function>

现在就变成递减了：

.. code-block:: none
    :linenos:

    * (alert-test 3)

    7

最后，多亏了 **alambda** 的 **self** 绑定，我们可以用 **invert** 参数再次修改函数：

.. code-block:: none
    :linenos:

    * (alert-test 'invert)

    #<Interpreted Function>


又回到了刚开始时的状态，递增：

.. code-block:: none
    :linenos:

    * (alert-test 5)

    12

这个闭包被绑定到函数命名空间中的符号 **alet-test** 上了。但和常规的闭包略有不同。虽然这个
闭包和常规闭包都是（指向）单个环境的指针，这个环境可以有任意数量的引用，这个闭包
使用间接来改变它在被调用时运行哪些代码段。尽管可以安装任何一段代码，但只有 **alet**
的词法范围内的、 **this** 回指符可用的代码才能访问它的词法绑定。但是，仍然
不能阻止我们安装一个新的闭包，它有自己的词法绑定，可能还会通过 **alet** 安装的
间接环境改变行为。本章剩下的大部分内容是通过 **alet** 创建的间接环境我们可以做的有用
的事情。

一种常见的宏技术被非正式地称为将宏由内打开（ turning a macro inside out ）。
当你打开一个宏时，你可以选择一个典型的结构，该结构使用与你想要创建的宏类似的宏，
并将其展开。然后使用该展开式作为所需宏的模板。例如，我们希望有一种比前面介绍
的封装在 lambda 外围的 alet（  alet over alambda ）计数器更通用的方法来创建具有多个状态的闭包。下面是上面
由内而外展开的可逆计数器 alambda 用例:

.. code-block:: none
    :linenos:

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
      
如果我们稍微重构上面的展开式，来利用 labels 允许我们来创建多个函数绑定的事实的优势 :sup:`【10】` ，将会得到以下结果：
  
.. hint:: 【10】 
  因此有多个 labels 
   
.. code-block:: none
    :linenos:

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
对它的函数主体都可用。而且，我们现在有一个针对我们最终宏的已经很完整的模版了。

.. code-block:: none
    :linenos:

    (defmacro alet-fsm (&rest states)
      `(macrolet ((state (s)
                    `(setq this #',s)))
          (labels (,@states) #',(caar states))))

**alet-fsm** 提供了一种便捷的语法，该语法可以用来表达我们的闭包存在的多种可能状态。
就像是在 **labels** 上的宏包裹了一层薄薄的糖衣，结合代码遍历 **macrolet**
转换，允许我们假装像是有了一个 **state** 函数，来改变闭包的当前状态，该函数通过
**alet** 提供的 **this** 回指来访问。下面是可逆计数器的更简洁的版本的例子：

.. code-block:: none
    :linenos:

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

**alet-fsm** 是一项我们之前没有见过的技术的实例：回指注入（ anaphor injection ）。使用这种回指语在很多方面违反了词法透明性，以至于它实际上在某种程度上是词法不可见的
（ lexically invisible ）。 **alet** 不仅无形地绑定了 **this** ，而且 **alet-fsm** 宏对
**this** 的使用也是同样隐形的。 **Alet-fsm** 将一个自由变量注入到我们词法上下文中，
让我们在词汇上下文中一点也不会看到它。

这其中的格式问题是不确定的 :sup:`【11】` ，当然，宏编程与格式无关。这关乎性能。有时，插入
自由变量可以在两个宏之间创建共生关系 —— 与两个孤立的扩展相比，一个可以更好地以编程方式构建扩展。由于这种宏编程非常复杂，因此可以再次与 C 语言指针进行类比。
就像学习 C 语言指针会产生可疑的文体建议一样，自由变量注入也是如此。
  
.. hint:: 【11】 
  本质上，所有风格问题都是如此。一旦某事被完全理解，风格就变得无关紧要了。自由变量注入尚未完全理解
   
对于自由变量注入难以理解的原因，最合理的假设是它的故障安全行为 :sup:`【12】` 。有了回指，
如果提供的用户代码没有使用绑定，那么代码很可能会继续运行，不管你是否希望它这样做。
它可能已经悄无声息地失败了，因此不安全。然而，当你注入一个自由变量，并且
没有捕获它的环境时，你的整个表达式就开始变得释放了。当这种情况发生时，你需要在你能够求解表达式
之前决定要做什么。因为它有故障安全。
  
.. hint:: 【12】 
  安全，从某种意义上说，与现实世界相反，尽可能快速和尽可能大声的失败是最安全的。
   
除了格式之外，当我们想要两个相关的宏来回通信时，自由变量注入有时正是我们需要的。注入和
回指的操作其实是一样的，只是方向相反。因为你正在你的宏之间打开了一个新的沟通信道，
复杂性问题的扩展速度甚至更快。想象一下坐在一个满是易碎玻璃的房子里。你可以
安全地向房子外面的人扔东西，即使他们不用费心去抓这些东西，但你最好确保你能
抓住扔向你的任何东西。


.. _6-4-indirection-chains:

6.4 间接链
====================

我们有很多方法来利用 **alet** 提供的 **this** 回指语的优点。由于环境是通过虚拟闭包来访问的，它将所有
调用转发给 **this** 所指向的真实闭包，我们可以到处传递虚拟闭包引用，根据需要经常复制它。
这样的间接很有用，因为我们可以改变调用这个虚拟闭包时发生的事情，而不必改变对虚拟
闭包的引用。

.. code-block:: none
    :linenos:

    (defmacro! ichain -before (&rest body)
      `(let ((,g!indir-env this))
        (setq this
          (lambda (&rest ,g!temp-args)
            ,@body
            (apply ,g!indir -env
                  ,g!temp-args)))))

**ichain-before** 旨在被展开成 **alet** 结构。它添加了一个新的代码体，以便在调用主闭包之前执行。
回到计数器例子， **ichain-before** 让我们添加了一个新的闭包，在关闭变量 **acc** 继续之前，打印它先前的值并增加它：

.. code-block:: none
    :linenos:

    * (alet ((acc 0))
        (ichain-before
          (format t "Changing from ~a~%" acc))
        (lambda (n)
          (incf acc n)))

    #<Interpreted Function>

和设想的一样：

.. code-block:: none
    :linenos:

    * (funcall * 2)
    Changing from 0
    2
    * (funcall ** 2)
    Changing from 2
    4

不过，我们把 chain 放在 **ichain-before** 这个名字中是有原因的。我们可以根据需要，让尽可能多的闭包来执行：

.. code-block:: none
    :linenos:

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
    :linenos:

    * (funcall * 2)
    C
    B
    A
    2

在更改宏以避免通过添加新的周围代码来重新构造宏时，静态添加间接链有时很用的。但在我们动态
添加它们时，间接链的最有趣的可能性就会出现。因为我们可以在运行时创建新的闭包，还因为我们可以通过回指语访
问闭包的内部，所以我们可以重写函数在运行时的工作方式。下面是一个简单的例子，每个闭包调用
都会添加另一段代码，在运行时输出 “Hello world”：

.. code-block:: none
    :linenos:

    * (alet ((acc 0))
        (lambda (n)
          (ichain-before
            (format t "Hello world~%"))
          (incf acc n)))

    #<Interpreted Function>

每次调用都会向间接链添加一个新的闭包：

.. code-block:: none
    :linenos:

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
添加到执行链的另一端：在主闭包被调用之后。 **ichain-after** 用了 **prog1** ， **prog1**
连续执行提供的形式体，然后返回第一个形式体的求值结果。

.. code-block:: none
    :linenos:

    (defmacro! ichain -after (&rest body)
      `(let ((,g!indir-env this))
          (setq this
            (lambda (&rest ,g!temp-args)
              (prog1
                (apply ,g!indir -env
                      ,g!temp-args)
                ,@body)))))

**ichain-before** 和 **ichain-after** 可以组合在一起，这样， before 结构在主闭包计算之前执行，after 结构在主闭包计算之后执行:

.. code-block:: none
    :linenos:

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

**ichain-before** 和 **ichain-after** 是将自由变量注入其展开式的宏。这两个宏注入了符号
**this** ，我们依赖的它会被 **alet** 宏的展开式捕获。这种类型的符号注入可能看起来格式
不好或容易出错，但实际上是一种常见的宏技术。事实上，几乎所有的宏都向展开式中注入了符号。例如，随着 **this** 一起，宏 **ichain-before** 还会注入像 **let** 、 **setq** 和 **lambda** 这样的
符号，来拼接到宏被展开的任何位置。符号（如 **this** ）和 预定义的符号（如 **setq** ）之间的区别在于，
**lambda** 总是指向一个单独的易于理解的 ANSI 宏，而像 **this** 这样的符号可以取决于它们被展开的环境而指向不同的东西。

在执行原始闭包表达式之前或之后运行闭包（这样)的代码进行标记时， **ichain-before** 和 **ichain-after** 是很
有用的，但这绝不是 **this** 回指语唯一能做的。另一个常见的任务是在调用闭包之后检查闭包数据的有效性。

.. code-block:: none
    :linenos:

    (defmacro! ichain -intercept% (&rest body)
      `(let ((,g!indir-env this))
        (setq this
            (lambda (&rest ,g!temp-args)
              (block intercept
                (prog1
                  (apply ,g!indir -env
                        ,g!temp-args)
                  ,@body))))))

**ichain-intercept%** 是另一个用在 **alet** 中的宏。其设想是，我们希望能够拦截闭包的调用，并验
证它们执行的操作没有导致闭包中的某种不一致状态。所以我们可以像这样在常规的计数器闭包中添加一个拦截：

.. code-block:: none
    :linenos:

    * (alet ((acc 0))
        (ichain-intercept%
          (when (< acc 0)
            (format t "Acc went negative~%")
            (setq acc 0)
            (return-from intercept acc)))
        (lambda (n)
          (incf acc n)))

    #<Interpreted Function>

当计数器低于 0 时， **ichain-intercept%** 插入的代码将给我们告警：

.. code-block:: none
    :linenos:

    * (funcall * -8)
    Acc went negative
    0

计数器被重置为 0 ：

.. code-block:: none
    :linenos:

    * (funcall ** 3)

    3

**ichain-intercept%** 最有趣的地方是，引入了名为 **intercept** 的块回指（ block anaphor ）。
我们用 **return-from** 来调用这个回指。代码块将从闭包调用中返回这个值，拦截原始值。

.. code-block:: none
    :linenos:

    (defmacro! ichain-intercept (&rest body)
      `(let ((,g!indir-env this))
        (setq this
            (lambda (&rest ,g!temp-args)
              (block ,g!intercept
                (macrolet ((intercept (v)
                          `(return-from
                          ,',g!intercept
                          ,v)))
                  (prog1
                    (apply ,g!indir-env
                          ,g!temp-args)
                    ,@body )))))))

不是捕获块回指语 **intercept** , **ichain-intercept** 创建一个局部宏，该宏允许 **ichain-intercept** 中的代码使用
**intercept** 来展开成一个由 gensym 指定的 **return-from** 。

.. code-block:: none
    :linenos:

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
    :linenos:

    * (funcall * -8)
    Acc went negative
    0
    * (funcall ** 3)
    3

当然，将所有这些闭包透明地引入操作会影响运行时性能。幸运的是，现代 lisp 编译器擅长优化闭包。
如果你的应用程序可以忍受几个间接引用指针（通常是可以的），那么间接链就可能是构建它的最佳方式。关于
间接链的另一个有趣的思考方式，请参阅第 [7.4 指针作用域](chapter07.md) 。还可以查看 CLOS 的
**before** 、 **after** 和 **around** 函数。


.. _6-5-hotpatching-closures:

6.5 热修复闭包
====================

重要的本章节的目的有三个。首先，描述 **alet** 中 **this** 回指的另一个有趣的用法。其次，讨论
*alet over dlambda* 模式。最后，介绍了一种很有用的宏技术，称为回指语闭合（ *anaphor closing* ）。
为了清楚地说明回指语闭合，我们将不用 **alet** 宏，而是使用一个由内而外的展开式。 **alet-hotpatch%**
是提供了一个特殊的 lambda 结构的 **alet** 的展开式。该 **lambda** 结构检查第一个参数 :sup:`【13】` 是否为关
键字符号 **:hotpatch** ，如果是，则用另一个提供的参数替换间接闭包。
  
.. hint:: 【13】 
  用指针比较
   
.. code-block:: none
    :linenos:

    (defmacro alet-hotpatch% (letargs &rest body)
      `(let ((this) ,@letargs)
          (setq this ,@(last body))
          ,@(butlast body)
          (lambda (&rest args)
            (if (eq (car args) ':hotpatch)
              (setq this (cadr args))
              (apply this args)))))

能够在运行时更改另一个转发闭包中使用的闭包称为热补丁（ *hotpatching* ）。例如，这里我们创建
了一个热补丁闭包，并将其存储在符号 **hotpatch-test** 的 **symbol-function** 单元格中，以便
之后使用：

.. code-block:: none
    :linenos:

    * (setf (symbol-function 'hotpatch-test)
        (alet-hotpatch% ((acc 0))
          (lambda (n)
            (incf acc n))))

    #<Interpreted Function>

现在可以像这样使用:

.. code-block:: none
    :linenos:

    * (hotpatch-test 3)

    3
    * (hotpatch-test 4)

    7

我们可以通过调用这个闭包的符号 **:hotpatch** 和替换函数或闭包来替换 lambda 结构及其相关的环境:

.. code-block:: none
    :linenos:

    * (hotpatch-test
        :hotpatch
        (let ((acc 0))
          (lambda (n)
            (incf acc (* 2 n)))))

    #<Interpreted Function>

现在闭包将具有新的、热补丁的行为：

.. code-block:: none
    :linenos:

    * (hotpatch-test 2)

    4
    * (hotpatch-test 5)

    14

注意计数器的值是怎么重置为 0 的，因为我们还用计数器的累加器 **acc** 的一个新值热补丁了闭包的环境。
我们之前见过这种关键字符号的运行时解构吗？没错，实际上我们在 [5.7 Dlambda]中编写了个宏来完成这个操
作。 **alet-hotpatch** 是 **alet-hotpatch%** 的吸纳了 **dlambda** 优点的版本。有时甚至在没有意识到
的情况下，通过在新的宏定义中使用之前定义过的宏，我们实现了宏组合（ *macro combination*  ）。使用精心设
计的宏可以完全理解展开式，尽管它可能在许多方面违背词汇透明性，但不会有组合问题出现，因为所有组件都能有
意义地组合在一起。

.. code-block:: none
    :linenos:

    (defmacro! alet-hotpatch (letargs &rest body) ;;; 二译者注：原文为 defmacro 。
      `(let ((,g!this) ,@letargs)
        (setq ,g!this ,@(last body))
        ,@(butlast body)
        (dlambda
            (:hotpatch (closure)
              (setq ,g!this closure))
            (t (&rest args)
              (apply ,g!this args)))))

**alet-hotpatch** 创建了一个可热补丁的闭包，但在概念上有一个小缺陷。因为使用
**alet-hotpatch** 的唯一真正原因是创建这种可热补丁的闭包，我们可能忘记，它还将 **this** 回指语引入
到所提供的作用域中。当我们忘记创建的的回指语时，就有不想要的变量捕获问题的风险。为了避免这些问题，我们可以选择使用
一种叫做回指闭合的技术。当要我们关闭一个回指语时，我们不需要改变我们的回指宏函数，只是限制他们组合的方式。

因为我们已经把 **alet** 展开式由内而外翻出，我们可以在 **alet-hotpatch** 的定义中从词法上看到
**this** 回指语的创建。同时因为 **alet-hotpatch** 也包含了使用 **this** 回指实现热补丁的代码，我们因而可以关闭回指语，这样符号 **this** 就不再被宏捕获了。通常该如何避免引
入预期之外的绑定？当然，我们用 gensyms 来命名绑定。

.. code-block:: none
    :linenos:

    (defmacro! let-hotpatch (letargs &rest body)
      `(let ((,g!this) ,@letargs)
        (setq ,g!this ,@(last body))
        ,@(butlast body)
        (dlambda
          (:hotpatch (closure)
            (setq ,g!this closure))
          (t (&rest args)
            (apply ,g!this args)))))

**let-hotpatch** 是将 **this** 回指语闭合为一个更被包含的版本的示例 —— 一个在只需要进行热补丁
时，更安全的版本。删掉了名字前面的（字母） **a** ，表示这个新的宏不再在提供的代码的主体中引入回指语。当然，如果我们出于某种
原因而不是因为热补丁而想要引用 **this** ，就应该让这个回指语保持开启。

在你编写足够多类似的宏后，这种开启和关闭回指语的技巧就变成了第二天性。就像我们可以编写注入自由变量到其展开式的宏，而不考虑我们将会如何捕捉它们的，直到我们编写在它们将会展开到的词法上下文（时，才会考虑自由变量的捕捉问题），在开发回指宏的组合和自由变量
注入宏的试验时，我们有时选择保持回指语开启。一旦找到了最适用的组合，我们就可以将宏合并在一起，用 gensyms
替换开发过程中使用的所有回指语。像 **let-hotpatch** 一样，该技术可以用 **defmacro!** 将回指语
的作用域从宏展开式移到宏定义。我们没有从词法上引入回指语，而是引入了另一种类型的回指语 —— 这种
回指语并不是在展开式的整个词法作用域内起生效，而只在另一个更加有限的范围内生效。下节将进一步讲解
这个有效范围。


.. _6-6-sub-lexical-scope:

6.6 子词法作用域
====================

在 [3.5 异常捕获]中定义的 **defmacro!** 宏中用了 Graham 的 **flatten** 实用工具来查找提供的代
码中的自动 gensyms 。现在是时候承认本书撒的一个小谎了。在此之前，因为没有解释自由变量注入和回
指，我们假装在 **defmacro!** 的定义中的 G-bang 符号名称适用于宏定义的词法范围。实质上这是不对的 ——
**defmacro!** 在略微不同类型的作用域（叫做 *子词法作用域 sub-lexical scope* ）下提供了这
些绑定。

.. note::

  G-bang 指的是以 **g!** 开头的变量， **gensym** 是个宏，会自动生成个随机变量名，防止变量名
  突。

记住，作用域意味着对变量的引用是有效的，而词法作用域是指名称对于一个绑定构造（比如 let 构造）的文本主体内的代码来说是适用的 该名称适用的。词法作用域和子词法作用域之间的重要区别是，词法作用域包括了在 **let** 主体中
代码的所有宏展开式。因此，将词法作用域描述为创建只有在绑定构造文本主体中的代码才能访问的
变量实际上也是错误的 —— 宏可以注入变量引用。这些变量是从绑定构造的文本主体外部被注入的。

通过限制访问词法变量的可能方式来实现真正的文本作用域，会产生子词法作用域。只有当表示
子词法作用域变量的符号，出现在，宏展开之前传给 lisp 的原始列表中时，对该子词法作用域变量的引用才有效。

因为 **defmacro!** 对给出的代码进行预处理，并在代码开始展开之前创建所有 G-bang 的列表，所以
G-bang 是子词法绑定。我们不能编写将 G-bang 符号注入到 **defmacro!** 的宏，因为
G-bang 的词法绑定从未创建过。下面是 G-bang 的经典用法：

.. code-block:: none
    :linenos:

    * (defmacro! junk ()
        `(let ((,g!var))
          ,g!var))

    JUNK

两个 G-bang 变量都在 **defmacro!** 的子词法作用域中，所以展开式如我们所料：

.. code-block:: none
    :linenos:

    * (macroexpand '(junk))

    (LET ()
      (LET ((#:VAR1663))
        #:VAR1663))
    T

然而，为了探索子词法作用域的概念，我们将定义一个注入一个 G-bang 符号的宏：

.. code-block:: none
    :linenos:

    * (defmacro injector-for-g!var ()
        ''g!var)

    INJECTOR-FOR-G!VAR

现在我们可以编写 **junk2** 。 **junk2** 和 **junk** 基本一致，除了我们用一个展开成一个G-bang符号的宏替换了我们的 G-bang 符号：

.. code-block:: none
    :linenos:

    * (defmacro! junk2 ()
        `(let ((,(injector-for-g!var)))
          ,(injector-for-g!var)))

    JUNK2

但是因为 G-bang 符号是子词法绑定的 —— 因此不必留心结构的宏展开式 —— **defmacro!** 就不会将这些
符号转换成自动 gensyms：

.. code-block:: none
    :linenos:

    * (macroexpand '(junk2))

    (LET ()
      (LET ((G!VAR))
    G!VAR))
    T

虽然上面的代码仍然可以正常工作，但当有些变量引用在子词法作用域中存在，有些不存在时，子词法作用域
内的变量引用可能会破坏表达式：

.. code-block:: none
    :linenos:

    * (defmacro! junk3 ()
      `(let ((,g!var))
          ,(injector-for-g!var)))

    JUNK3
    * (macroexpand '(junk3))

    (LET ()
      (LET ((#:VAR1672))
    G!VAR))
    T

子词法作用域在复杂宏中惊人的频繁出现。还有 **defmacro!** ，我们已经至少在另外一个例子中看到过它：在[5.6 递归方案]中的
**with-all-cxrs** 宏的子词法绑定列表访问器函数（中就用到了这个宏）。子词法绑定的结果是，我们不能从宏
展开式中引用这种绑定。有时这种访问限制很有用，有时不是。在 **with-all-cxrs** 中，子词法可能被
认为是不可取的。当我们的访问器在 **with-all-cxrs** 的子词法作用域中时，没有问题：

.. code-block:: none
    :linenos:

    * (with-all-cxrs
        (cadadadr nil))

    NIL

我们甚至可以编写扩展到这些访问器中的宏，只要宏定义是在 **with-all-cxrs** 的子词法范围内的:

.. code-block:: none
    :linenos:

    * (with-all-cxrs
        (macrolet ((accessor (l)
                    `(cadadadr ,l)))
          (accessor nil)))

    NIL

但要注意， **with-all-cxrs** 以子词法的方式绑定访问器函数，所以我们不能定义宏来注入访问器：

.. code-block:: none
    :linenos:

    * (macrolet ((accessor (l)
                  `(cadadadr ,l)))
        (with-all-cxrs
          (accessor nil)))

    This function is undefined: CADADADR

既然已经熟悉了回指，并且也见过这么多复杂宏的例子 —— 包括一些使用子词汇范围的宏 ——
我们可以讨论个有趣的理论宏： **sublet** 。这个宏设计用来为使用类似 let 结构语法的语法的代码创建子词法绑定。与许多 lisp 宏一样，对 **sublet** 的讨论先从一个实用程序
开始。

.. code-block:: none
    :linenos:

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
在下面代码中， **a** 被归一化为 **(a)** ：

.. code-block:: none
    :linenos:

    * (let-binding-transform
        '(a (b) (c nil)))

    ((A) (B) (C NIL))

**sublet** 还需要用到 [5.3 隐式上下文](chapter05.md) 中的 **tree-leaves** 。回想一下，
**tree-leaves** 宏有三个参数：一个任意的列表结构，一个用 **x** 变量来确定是否应该更改叶子
的表达式，以及另一个用不同的 **x** 来确定应该更改哪些有效叶子的表达式。


选择隐式化具有相同名称 **x** 的绑定证明是一种有用的 *二元语法* （ *duality of syntax* ）。
当不用通用的方式在表达式中分解公共代码时，有时我们可以用其他方式使用语法对偶来获得这种简洁的优势。
**sublet** 的定义用到了 [4.5 循环表达式](chapter04.md) 中的自引用读取宏。特别是对于像访问器
这样在编写程序的过程中可以多次更改的东西，读取宏允许我们有且只有一种结构来表示访问器。幸亏使用了隐式的
**tree-leaves** 宏，很容易找到和理解代码重复，因为代码紧密地结合在一起。

.. code-block:: none
    :linenos:

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

**sublet** 接受表示let绑定的结构，并应用我们的 **let-binding-transform** 工具，在这个过程中生成新的
列表结构。然后，将gensym 前置拼接到每个绑定 :sup:`【14】` ，并带有与绑定名称相对应的打印名称。 **sublet** 展开
为 let 结构，通过 let 结构将这些 gensym 符号绑定到传递给绑定结构的值，然后用
**tree-leaves** 将代码中所有出现的绑定名称符号替换为对应的 gensym 。 **sublet** 不会展开任
何宏或解析主体中的任何特殊结构来查找这些绑定名称符号的出现，因为 **sublet** 会创建子词法绑
定。例如，如果所有 **a** 的引用都是子词法的，将用 gensym 替换它们:
  
.. hint:: 【14】 
  前置拼接而不是附加拼接，因此我们仍然可以支持没有默认值的绑定，例如( a )。
   
.. code-block:: none
    :linenos:

    * (macroexpand
        '(sublet ((a 0))
              (list a)))

    (LET ((#:A1657 0))
      (LIST #:A1657))
    T

但是，由于子词法作用域不涉及展开宏，因此不可避免地不会解析 **quote** 这样的特殊结构，不被认为是变量引用的
符号 **a** 的实例也会被改变：

.. code-block:: none
    :linenos:

    * (macroexpand
      '(sublet ((a 0))
          (list 'a)))

    (LET ((#:A1658 0))
      (LIST '#:A1658))
    T

子词法作用域在列表结构被系统代码遍历程序解释为 lisp 代码之前生效。这是个重要的观测结果，
但其结果仍未被完全探索。 **sublet** 对代码的解释不同于 COMMON LISP 的代码遍历程序。

这里，我们处于宏理解的众多边缘之一。在未扩展的子词法作用域和完全扩展的词法作用域之间有哪些
有趣的作用域类型？因为没有更好的名称，我们将这个无限大的范围称为 *超级子词法作用域* （ *super_
_sub-lexical scope* ） :sup:`【15】` 。
  
.. hint:: 【15】 
  我给它起这个愚蠢的名字是因为我希望当这个概念被更好地理解时，更好的名字会开始显现
   
.. code-block:: none
    :linenos:

    (defmacro sublet*
      (bindings &rest body)
      `(sublet ,bindings
        ,@(mapcar #'macroexpand -1 body)))

一个相当明显的超级子词法作用域使用 **sublet*** 。这个宏底层使用 **sublet** ，但是用
**macroexpand-1** 函数的宏展开来修改主体中对应的结构。现在，对符号的引用必须出现在宏展开的第
一步之后，而不是出现在原始列表结构中。这种类型的超级子词法作用域允许每个 let 结构主体中的宏从作用
域中注入或移除引用。如果宏没有做这两件事 —— 或者如果结构根本不是宏 —— 这种超级子词法作用域的行为
就像子词法作用域：

.. code-block:: none
    :linenos:

    * (macroexpand
      '(sublet* ((a 0))
          (list a)))

    (LET ((#:A1659 0))
      (LIST #:A1659))
    T

但我们可以定义另一个注入器（ injector ）宏来测试这个超子词法作用域：

.. code-block:: none
    :linenos:

    * (defmacro injector-for-a ()
        'a)

    INJECTOR-FOR-A

**sublet*** 将展开这个注入器宏:

.. code-block:: none
    :linenos:

    * (macroexpand-1
      '(sublet* ((a 0))
          (injector-for-a)))

    (SUBLET ((A 0))
      A)
    T

然后，通过 **sublet** 将对其进行子词法解释，这意味着插入的变量 **a** 存在于 **sublet*** 提供的
超级子词法作用域的类型中：

.. code-block:: none
    :linenos:

    * (macroexpand-1 *)

    (LET ((#:A1663 0))
      #:A1663)

但是表达式中的嵌套宏不会被 **macroexpand-1** 展开，所以 **sublet*** 不会把嵌套宏放到
**sublet** 的子词法作用域中：

.. code-block:: none
    :linenos:

    * (macroexpand-1
      '(sublet* ((a 0))
          (list (injector-for-a))))

    (SUBLET ((A 0))
      (LIST (INJECTOR-FOR-A)))
    T

所以 **a** 不会被子词法捕获 :sup:`【16】` ：
  
.. hint:: 【16】 
   Walker:macroexpand-all 是 CMUCL 中具有完整代码遍历功能的组成部分。
   
.. code-block:: none
    :linenos:

    * (walker:macroexpand-all *)

    (LET ((#:A1666 0))
      (LIST A))

依托 **sublet** 和 **sublet*** ，我们可以用词法作用域或超词法作用域来控制在什么级别的宏展开式中变
量 **a** 是有效的。如上所述，超级子词法作用域实际上是一个无限类的范围，一个几乎完全未被智力探索的
范围。超级子词法作用域的方法和遍历代码的方法（很多）一样多。这类作用域引出了另一类基本未被探索的
宏：这类宏改变 lisp 宏如何执行，何时展开，引用在哪里有效，特殊形式如何解释等。最终，就有了个可
编程宏（ macro-programmable ）的宏展开器。


.. _6-7-pandoric-macros:

6.7 潘多拉宏
====================

潘多拉魔盒是个关于世界上第一个女人的希腊神话：潘多拉。潘朵拉，U 语言的符号，希腊语
翻译过来是全能。潘多拉，这个女人，在好奇心的诱惑下，打开了一个小盒子，无可挽回地释放
了人类所有的邪恶和罪恶。虽然本节中描述的宏非常强大，可能会教你一种永远不会忘记的编程
方法，但请放心，结果要比可怜的潘多拉好得多。现在开始，打开这个盒子。

首先，稍微绕过另一本著名的 lisp 书：克里斯蒂安·奎奈克（ Christian Queinnec ）的《Lisp in Small Pieces》。
Queinnec 是一位广受尊敬的 lisp 专家，对 lisp 知识做出了很大的贡献。Queinnec 的书的内容
是在 Scheme 编程语言中实现各种复杂的编译器和解释器 :sup:`【17】` 。
  
.. hint:: 【17】 
  虽然它有时会描述其他 lisp 及其特征。
   
..

  Lisp in Small Pieces: https://pages.lip6.fr/Christian.Queinnec/WWW/LiSP.html

《Lisp In Small Pieces》中有个简短但有趣的宏的讨论。感谢 Scheme 宏规范的模糊性 :sup:`【18】` ，这本书中提及到描述不同的宏系统变化，但是也有许多有趣的注意事项，关于为什么我们可能想要使用宏以及如何使用它们。如果你已经阅读并理解了 [第三章：宏基础](chapter03.md)，那么 《Lisp in Small
Pieces》章节中介绍的大多数宏，对你来说，都属于微不足道的类别，除了我们现在要讨论的这个诱人的宏。
  
.. hint:: 【18】 
  感谢，但是不必谢.
   
和许多编程书籍一样，《Lisp in Small Pieces》将我们带到并留在了一个面向对象编程系统的实现中。
通常这些实现用来概述 CLOS（ COMMON LISP  Object System）的一个子集。Queinnec
称他的子集为 MEROONET 。 Queinnec 指出，在为 MEROONET 类定义方法时，最好能够
直接引用所定义对象的字段，而不是使用访问器。把 Queinnec 的话翻译过来就是:
让我们以 CLOS 中的 **with-slots** 宏为例；我们会改变它使之适应 MEROONET 的上下文（环境）。对象的字段 ——
比方说 **Point** 实例的字段 —— 是通过像 **Point-x** 或 **set-Point-y!** 这样的读取和写入函数
来处理的。在定义方法的上下文中，直接通过字段的名称(例如 **x** 或 **y** )来处理会更简单。

下面是 Queinnec 预想的接口（他称之为 **define-handy-method** ）定义新方法 **double** ：

.. code-block:: none
    :linenos:

    (define-handy-method (double (o Point))
      (set! x (* 2 x))
      (set! y (* 2 y))
      o)

这比必需的 MEROONET 语法更让程序员高兴:

.. code-block:: none
    :linenos:

    (define-method (double (o Point))
      (set-Point-x! o (* 2 (Point-x o)))
      (set-Point-y! o (* 2 (Point-y o)))
      o)

换句话说，如果我们可以使用宏来访问外部绑定（在本例中是对象槽），就像是词法绑定一样，那就
太好了。虽然，不可否认的是这对缩写的目的很有用，但最重要的含义是它能够为现有的和未来
的宏提供二元（ dualities ）语法。

正如 Queinnec 所提出的， COMMON LISP  通过 **with-slots** 宏为 CLOS 实现了这个功能。
这是  COMMON LISP  实现其设计目的的一个例子：允许基于精炼的、标准化的宏系统进行抽象。
大多数语言被设计成易于实现，而  COMMON LISP  被设计成具有强大的编程功能。Queinnec
的结论是，语言的限制使得 Scheme 几乎不可能实现这一点，特别是在需要可移植性的地方。

缺乏关于语言及其实现的反射性信息，我们无法在 Scheme 中编写可移植的代码遍历程序，
因此我们不得不放弃编写 **define-handy-method** 。

尽管  COMMON LISP  仍然允许使用大量合法的方式来实现宏系统，但它的设计目的是提供通用
的元编程工具，这些工具以标准和可移植的方式组合在一起。这两个先进  COMMON LISP  宏特性
允许我们实现像 CLOS 的 **with-slots** 一样的东西，它们是 *泛化变量（ generalised
variables* ） 和 *符号宏（ symbol macro* ）。本节就借此机会展示  COMMON LISP  特性的奇妙组合，
并将我们迄今为止见过所有关于回指宏的内容集合在一起，在这个过程中发现了一个有趣的宏类，称为
潘多拉（ *pandoric* ）宏。

.. code-block:: none
    :linenos:

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

**pandoriclet** 背后的思想是打开闭包，允许外部访问它们别的封闭的词法变量。与之前的一些宏
（如 **alet-hotpatch** ）一样， **pandoriclet** 编译一个间接环境，根据传递的参数选择不同的运行时行为。

我们再次从 **alet** 由内而外的展开式开始，记住这里引入了个叫 **this** 的回指语。
**pandoriclet** 与我们见过的其他宏类似。和所有的回指 **let** 变体一样，我们假设
**pandoriclet** 主体中的最后的结构将是一个 lambda 结构。就像 **alet-hotpatch** 一样，
**pandoriclet** 用 **dlambda** 宏来在 **pandoriclet** 被调用返回闭包时分发执行不同可能的代
码。 **pandoriclet** 还用了上一节介绍的 **let-binding-transform** 实用函数来处理已创建的
空绑定，如 **(let (a) ...)** 。这个实用函数对 **pandoriclet** 是必需的，原因与需要
**sublet** 一样：这些宏遍历提供给 **let** 的绑定，而之前的宏盲目地将绑定拼接到另一个 **let** 。

我们调用了两个没定义的创建列表的实用函数： **pandoriclet-get** 和 **pandoriclet-set** ，
它们分别接受一个 **let** 绑定列表。注意，我们可以引用还不存在的函数，只要在宏展开之前定义
它们就可以，显然，在使用宏之前不能这样做。使用辅助函数来帮助定义宏是一个很好的习惯。
它不仅可以使定义更具可读性，还可以在测试宏的组件时提供帮助，并可以在将来的宏中证明是
有用的。这种抽象最好的部分是，当组合宏时，保持词法上下文可供实用程序使用。

因此，记住这个词法上下文，我们编写 **pandoriclet-get** 和 **pandoriclet-set** 。对于
**pandoriclet-get** ，我们牢记 **dlambda** 已经绑定了变量 **sym** ，在这里列表将被拼接进去。我们在
**case** 结构中使用 **sym** ，将其与传递给 **pandoriclet** 的符号进行比较 :sup:`【19】` 。如果找到这个符号，则返回它所引用的绑定的当前值。如果没找到，则抛出错误。 **pandoriclet-set** 差不多一样，除
了 **dlambda** 为它绑定了一个额外的符号： **val** 。 **pandoriclet-set** 用 **setq**
将 **sym** 引用的绑定更改为 **val** 。
  
.. hint:: 【19】 
  回想一下，带有符号的 case 编译为每个 case 的单个指针比较。
   
.. code-block:: none
    :linenos:

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

**prandoriclet** 提供和我们所有的回指 let 变体一样的接口，因此可以使用它来创建常见的 counter 闭包：

.. code-block:: none
    :linenos:

    * (setf (symbol-function 'pantest)
        (pandoriclet ((acc 0))
          (lambda (n) (incf acc n))))

    #<Interpreted Function>

如预期般：

.. code-block:: none
    :linenos:

    * (pantest 3)
    3
    * (pantest 5)
    8

但是，现在在创建闭包时我们直接访问被称为 **acc** 的绑定：

.. code-block:: none
    :linenos:

    * (pantest :pandoric-get 'acc)
    8

然后我们可以类似地修改这个绑定的值：

.. code-block:: none
    :linenos:

    * (pantest :pandoric-set 'acc 100)
    100
    * (pantest 3)
    103

甚至， **this** 回指语的值是可访问的，因为我们特意保持这个回指语开启，并且在宏被展开时将符号 **this** 
添加到 **letargs** 绑定列表中：

.. code-block:: none
    :linenos:

    * (pantest :pandoric-get 'this)
    #<Interpreted Function>

所以我们用 **pandoriclet** 创建的这个闭包已经不再闭合了。这个闭包所使用的环境 —— 即使在编译器
已经删除了所有的词法变量符号时 —— 仍然可以通过 **pandoriclet** 返回的匿名函数来访问。这是
怎么做到的呢？依托 pandoric 宏，额外的代码被编译进来，以提供从外部访问闭包的方法。但 pandoric 宏的威力并不能从这个
正在发生的事情的低层次视角被看到。我们所做的是创建一个闭包间协议，
或消息传递系统，用于闭包之间的通信。

在继续讨论 pandoric 宏之前，首先我们需要指出一个在 COMMON LISP中， 语法二元性的最重要的例子：
泛化变量（ *generalised variables* ）。这方面的细节很复杂，这里不会做详细的介绍。为此，
推荐去阅读 Graham 的  OnLisp* ，这是目前所知道的最好的解决方法。细节是微妙的，想法
很简单：访问一个泛化变量是在语法上双重的设置它。只有一种 setter 结构： **setf** ， **setf** 能够通过
使用与你访问变量时使用的相同语法设置所有类型的变量。

例如，对于常规的变量你通常通过它的符号来访问其值，比方说 **x** 。可以用 **(setf x 5)** 来
设置 **x** 的值为 5。同样，要想访问某个调用的 cons 的 car 单元，假设也为 **x** ，可以使用
**(car x)** ，也可以通过 **(setf (car x) 5)** 来设置其值。这隐藏了一个事实，设置 cons 的
实际方法是使用 **rplaca** 函数。通过实现这种二义性语法，我们将需要记住的访问器和设置器的数量
减少了一半，更重要的是，为使用宏提供了的新方法。

.. code-block:: none
    :linenos:

    (declaim (inline get-pandoric))

    (defun get-pandoric (box sym)
      (funcall box :pandoric -get sym))

    (defsetf get-pandoric (box sym) (val)
      `(progn
          (funcall ,box :pandoric -set ,sym ,val)
          ,val))

**get-pandoric** 函数是对内部闭包协议 getter 语法的封装。它被定义为内联，以消除这种封装所造
成的任何性能影响。

**defsetf** 是一个有趣的 COMMON LISP 宏，完全不像由 **defmacro** 扩展出的 **defmacro!**
那样隐式地将 gensyms 绑定在提供的结构周围。 **defsetf** 非常适合定义泛化变量二元性的 setter 端，只要
getter 可以表示为一个函数或者所有参数都只求解一次的宏。注意，虽然可以将 **get-pandoric** 定义
为宏，但这样做的唯一原因是为了内联。宏不是用来内联的，编译器是用来内联的。

所以回到在符号函数 **pantest** 中存储的 pandoric 计数器，我们可以使用这个新的 getter 函数来获
取 **pantest** 中 **acc** 当前绑定的值：

.. code-block:: none
    :linenos:

    * (get-pandoric #'pantest 'acc)
    103

并且现在，多亏了泛型变量和 **defsetf** ，可以用一个语法对偶来设置 **acc** 的值:

.. code-block:: none
    :linenos:

    * (setf (get-pandoric #'pantest 'acc) -10)
    -10
    * (pantest 3)
    -7

由函数封闭的环境 —— 我们在 *let over lambda* 中调用的 let —— 开始看起来像常规可访问
的通用变量，就像 cons 单元格或哈希表条目。闭包现在是比过去更一流的数据结构。以前对外部代码封闭
的绑定现在对我们开放，即使这些绑定被编译成高效的东西，或者它们的访问器符号早就被遗忘了。

但是，任何关于泛型变量的讨论，如果不提到它的近亲：符号宏（ *symbol macro* ），都是不完整的。像其名字所
提示的那样， **symbol-macrolet** 可以将符号扩展成一般的 lisp 结构。因为它很直观以及更灵活的
使用形式，看起来像函数调用来代表宏转换 :sup:`【20】` ， **symbol-macrolet** 没有太多用处，除了它至关重要的一个重要应用是：符号宏让我们隐藏了泛型变量，这样宏的使用者认为他们正在访问常规词法变量。
  
.. hint:: 【20】 
  符号宏不带参数，因此符号宏定义始终扩展相同
   
符号宏的引入导致了 COMMON LISP 语言中最奇怪的组合之一：通常在设置个通过常规符号访问的变量时，
比如 **(setf x t)** ， **setf** 将展开成 **setq** 结构，因为这就是设计 **setq** 最初目
的：设置词法变量和动态变量（总是由符号引用）。但是 **setq** 特殊结构不能设置泛型变量，所以当引入符
号宏时，对于符号来说，不仅表达词法/动态绑定，而且表达任何泛化变量都变得可能，有必要指出的是，通过 **setq**
结构设置由符号宏定义的符号会被转换回 **setf** 结构。奇怪的是，这确实是正确的做法，因为它允许宏
对宏的用户完全隐藏泛型变量的存在，即使他们选择使用 **setq** 。真正正确的解决办法是从语言中删除冗余的
**setq** 结构，支持的更通用的 **setf** ，但这不会发生，原因是明显的兼容性以及宏创建期间，
**setq** 也可以是个有用的安全快捷方式 —— **setf** 加上对已被拼接符号的检查，而不是列表结构。
在使用 **setq** 时，记住只有在其拼接安全属性有用；正如我们所看到的，多亏了
**symbol-macrolet** ，符号可以引用任何泛型变量。

.. code-block:: none
    :linenos:

    (defmacro! with-pandoric (syms o!box &rest body)
      `(symbol -macrolet
        (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                  syms ))
        ,@body))

**with-pandoric** 宏会展开成一个 **symbol-macrolet** ， **symbol-macrolet** 为
**syms** 中提供的每个符号定义了符号宏。每个符号宏会把在symbol-macrolet词法作用域中的符号的引用展开为使用我们的get-pandoric 访问器/设置器的泛型变量的引用，来访问宏的第二个参数的求值结果： **o!box** （保存在
**g!box** 中）。

因此 **with-pandoric** 让我们窥探到了闭包的封闭变量绑定：

.. code-block:: none
    :linenos:

    * (with-pandoric (acc) #'pantest
        (format t "Value of acc: ~a~%" acc))
    Value of acc: -7
    NIL

正如我们使用广义变量来塑造这个变量的 setting 和 getting 的语法对偶的设计，我们甚至可以假装它是个常规的词法变
量，然后通过 setq 设置它：

.. code-block:: none
    :linenos:

    * (with-pandoric (acc) #'pantest
        (setq acc 5))
    5
    * (pantest 1)
    6

现在，我们已经研究了构成 pandemic 宏的大多数部分（组成）。首先，用于创建闭包的宏：
**pandoriclet** ，这个宏捕获回指语： **this** ， **this** 引用在调用闭包时使用的实际
函数。这个宏还会编译成一些特殊的代码，这些代码会拦截这个闭包的某些调用，而访问或修改它的封闭的词
法变量。其次， **get-pandoric** 和 **defsetf** 实现了访问和设置访问器的单一语法。最后，
**with-pandoric** 宏使用 **symbol-macrolet** 来将这些泛型变量设置为与封闭变量同名的看似新的词法变量。这些变量引用 **pandoriclet** 创建的原始环境，但是，来自分开的词法上下文。

作为示例，通过与 :ref:`6-5-hotpatching-closures` 中的热补丁宏对比，我们将这种能力同开启闭包相关联。回顾一下 **alet-hotpatch** 及其相近的回指语表亲： **let-hotpatch** ，这两个宏使用间接
环境创建闭包，以便可以动态更改在调用闭包时调用的函数。这些宏的最大限制是，当你对它热补丁时，它们会强制抛出所有在先前匿名函数上封闭的词法绑定。这种情况是不可避免的，因为在编写这些宏时，闭包
对我们关闭了。


对于 **let-hotpatch** 和 **let-hotpatch** ，我们不得不将特殊目的的代码编译到每个闭包中，这些闭包
能够将 **this** 回指的词法绑定设置为它的新值。但是由于我们现在可以打开由 **pandoriclet** 定义
的闭包并在外部运行这个 **setter** 代码，所以我们可以定义一个将会处理任何 pandoric 闭包的热补
丁函数 **pandoric-hotpatch** 。

.. code-block:: none
    :linenos:

    (defun pandoric-hotpatch (box new)
      (with-pandoric (this) box
        (setq this new)))

有时抽象在感觉很对，很难确切地说出为什么。也许是因为大多数编程都是不相交部分的不和谐
组合，当你碰巧发现抽象完美地结合在一起的时，它是令人惊讶和愉快的。 **pandoric-hotpatch**
看起来和其工作原理完全一样：它打开一个 pandoric 接口，从闭包 box 的词法作用域中取出变量 **this** ，
然后使用 **setq** 将 **this** 设置为要热补丁的闭包 **new** 。

甚至在我们意识到我们想要它成为可热补丁之前，我们可以在 pandoric 闭包上使用 **pandoric-hotpatch** 。还记得
贯穿本章节的我们已经打交道的计数器闭包吗？它仍应该被绑定到 **pantest** 的符号函数。上次的结果是 6：

.. code-block:: none
    :linenos:

    * (pantest 0)
    6

现在设置个新闭包 —— 一个拥有新的 acc 绑定，初始值为 100，之后就递减的闭包：

.. code-block:: none
    :linenos:

    * (pandoric-hotpatch #'pantest
        (let ((acc 100))
          (lambda (n) (decf acc n))))
    #<Interpreted Function>

果不其然，热补丁成功了：

.. code-block:: none
    :linenos:

    * (pantest 3)
    97

因此现在，我们的 counter 闭包中有个新值绑定到 **this** 上，它用来执行计数。但是，这个 hotpatch
改变了 **acc** 变量绑定的 pandoric 值吗?

.. code-block:: none
    :linenos:

    * (with-pandoric (acc) #'pantest
        acc)
    6

并没有。 **acc** 还是之前的值 6，因为这里只修改了 pandoric 环境中 **this** 的绑定，并且我们更改成一个拥有自己的 acc 绑定的新闭包。

.. code-block:: none
    :linenos:

    (defmacro pandoric-recode (vars box new)
      `(with-pandoric (this ,@vars) ,box
        (setq this ,new)))

**pandoric-recode** 宏采用种略微不同方法来热补丁。它保留了代码的原始词法环境，
同时还要在闭包被一些编码的事物调用和外部编译时，设法改变将要执行的函数。听起来有点
难以置信？记住，在原来的 pandoric 环境中， **acc** 的当前值是 6，我们可以使用
**pandoric-recode** 设置一个利用这个原始值的新函数，哦，或者说，缩减计数器中提供的n值的一半:

.. code-block:: none
    :linenos:

    * (pandoric-recode (acc) #'pantest
        (lambda (n)
          (decf acc (/ n 2))))
    #<Interpreted Function>

果然，我们有了新的行为，即将 **acc** 减去 **(\* 1/2 2)** ，从 6 变为 5:

.. code-block:: none
    :linenos:

    * (pantest 2)
    5

那这和最初的 pandoric 绑定有关联吗？

.. code-block:: none
    :linenos:

    * (with-pandoric (acc) #'pantest
        acc)
    5

对的，有关联。那 **pandorc-code** 是如何工作的呢？它封闭带有原始闭包的潘多拉式开启的绑定所提供的 lambda 形式。

.. code-block:: none
    :linenos:

    (defmacro plambda (largs pargs &rest body)
      (let ((pargs (mapcar #'list pargs)))
        `(let (this self)
          (setq
            this (lambda ,largs ,@body)
            self (dlambda
                    (:pandoric-get (sym)
                      ,(pandoriclet-get pargs))  ;;; 原翻译此处代码错误
                    (:pandoric-set (sym val)
                      ,(pandoriclet-set pargs))
                    (t (&rest args)
                      (apply this args)))))))

到目前为止，用来创建 pandoric 闭包的宏是 **pandoriclet** 。 **plambda** 是个由内到外
重写的 **pandoriclet** ，增加了一些重要的特性。首先也是最重要的， **plambda** 不再
创建 pandoric 访问器使用的 let 环境。相反， **plambda** 接受一组符号的列表，这些符号指向期望会在调用者的词法环境中的变量。 **plambda** 可以导出任何在你的词法环境中的变量，使它们对于其他词法作用域是透明地可访问的——甚至是在 **plambda** 结构之前或之后编写和编译的（变量）。

这是对 *let over lambda* 闭包系统的一个增量改进，该系统旨在最大化二元语法。多亏了
pandoric 宏（其中最重要的是 **plambda** 和 **with-pandoric** ），我们可以在需要时轻松而
有效地超越词法作用域的界限。闭包不再关闭；我们可以轻松地开启闭包，就像将
lambda 结构重写为 plambda 结构一样。我们使用 **plambda** 导出词法变量，然后用
**with-pandoric** 将它们作为完全等价的词汇变量导入。事实上，这些新变量是
如此等价，以至于它们甚至一点都不是真正的新变量。理解 pandoric 变量的一种更好的方法是，它们只是
原始词法作用域的扩展。以 **plambda** 的使用做个简单示例，这是一个 pandoric 计数器，
它从两个潜在不同的词法环境导出变量：

.. code-block:: none
    :linenos:

    * (setf (symbol-function 'pantest)
        (let ((a 0))
          (let ((b 1))
            (plambda (n) (a b)
              (incf a n)
              (setq b (* b n))))))
    #<Interpreted Function>

请注意，导出这些词法引用是多么容易。让闭包潘多拉化就像在 **lambda** 之前添加个
**p** 字符，或者在 **lambda** 参数后添加一个要导出的变量列表一样简单。
我们可以打开这个闭包 —— 或者是任何导出 **a** 和 **b** 的 pandoric 闭包 —— 通过
使用 **with-pandoric** ：

.. code-block:: none
    :linenos:

    * (defun pantest-peek ()
        (with-pandoric (a b) #'pantest
          (format t "a=~a, b=~a~%" a b)))
    PANTEST-PEEK
    * (pantest-peek)
    a=0, b=1
    NIL

**plambda** 是一个（表明）分解宏展开式的常规组成部分可以是如何有帮助的的例子。还记得在我们编写 **pandoriclet**
时决定将 getter 和 setter 代码的 case 语句的创建移动到 pandoriclet-get 函数中吗？
**plambda** 利用这些相同的函数。尽管这些宏将函数的结果拼接到相当不同的词法上下
文中，但由于两个宏都已经被编写为使用相同的变量命名约定和内部闭包协议，所以代码是可重
用的。

因此，pandoric 宏打破了词法边界。它们允许你在需要的时候打开闭包，同时也代表了各种
COMMON LISP 语言特性的美丽融合：回指宏、泛型变量和符号宏。但它们到底有什么好
的呢?

pandoric 的宏很重要，因为它们在我们不需要脱离更自然的 let-lambda 组合编程风格的情况下，
给我们（提供） CLOS 等对象系统的主要优势。尤其是在不必重新实例化已经创建了的对象实例的情
况下，就可以为闭包添加功能或方法。

.. code-block:: none
    :linenos:

    (defun make-stats-counter
          (&key (count 0)
                (sum 0)
                (sum-of-squares 0))
      (plambda (n) (sum count sum-of-squares)
        (incf sum-of-squares (expt n 2))
        (incf sum n)
        (incf count)))


**make-stats-counter** 是一个我们已经创建的用来创建计数器的 lambda over let over plambda，只不过
它维护了三条信息。除求和外，还保留平方和以及到目前为止处理的项目数。如果我们在
**make-stats-counter** 的定义中已经使用 **lambda** 而不是 **plambda** ，那么可能大多数信息对我们
是不可访问的。我们可能被拒之门外，因为可能这些变量对我们是关闭。

那么我们要怎么编写 **pandoric** 方法？我们可以像上面演示的那样简单地使用 **with-pandoric**
访问变量，或者，既然这是 lisp，那么就设计个更具体的接口。

.. code-block:: none
    :linenos:

    (defmacro defpan (name args &rest body)
      `(defun ,name (self)
        ,(if args
          `(with-pandoric ,args self
            ,@body)
        `(progn ,@body))))

**defpan** 是 **defun** 和 **with-pandoric** 两个宏的组合。 **defpan** 的主要目的是在
使用 **defun** 编写函数和使用 **with-pandoric** 访问外部词法范围之间实现语法的二元性。尽管我们像在 lambda 结构中那样使用相同的语法——符号列表——将参数提供给 **defpan** ，但 **defpan** 参数的含义
不同。这些 pandoric 函数不是创建了新的词法环境，而是扩展了它们所应用的 pandoric
闭包的词法环境。对于 **defun** 和常规的 lambda 结构，你给的变量的名称（符号）是不重要的。但
在 pandoric 函数中，变量名称就是一切。此外，在 pandoric 函数中，参数的顺序并不重
要，你可以如愿地选择使用尽可能少或者尽可能多的“导出的词法变量”。

**defpan** 还有一个称之为 **self** 的回指语，允许我们可以执行一种叫做 *回指链（ anophor chaining ）* 的有用
技术。通过在 pandoric 函数之间隐式地传递 **self** 的值，就可以在整个函数调用链中维护
这个回指语的值。与所有的链接结构一样，要确保这个链不会以无限循环结束。

.. code-block:: none
    :linenos:

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
必要变量名的 pandoric 闭包。 **stats-counter-mean** 只是返回传递给闭包的所有值的
平均值。 **stats-counter-variance** 通过跟踪链中的链接来计算这些值的方差，而
**stats-counter-stddev** 通过跟踪另一个链接来计算标准差。注意，链中的每个链接
只需要传递一个回指 **self** 来引用闭包的完整词法上下文。我们可以看到，单个的 pandoric
函数只需要引用它们实际使用的变量，这些变量可以随意调整引用顺序。

所以 **plambda** 创建了另一个回指语 —— **self** 。 **this** 指向的是要调用的实际闭包，而
**self** 指的是调用这个闭包的间接环境。虽然听起来有点奇怪，但 **plambda** 内部的代码
可以使用 **self** 来大规模访问它自己的词法环境，而不是直接访问它。到目前为止，这似乎
只对为在词法作用域内工作而编写的 **defpan** 方法有用。

.. code-block:: none
    :linenos:

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
**stats-counter-mean** 、 **stats-counter-variance** 和 **stats-counter-stddev** 。
**plambda** 和 **with-pandoric** 可以随意改写词汇范围。我们以这样一个例子结束本章。
词法作用域的一个局限性有时令人遗憾，即当 COMMON LISP 函数 **eval** 求解传递给
它的结构时，它会丢弃当前的词法环境。换句话说， **eval** 在空词法环境中求解结构。
在 COMMON LISP 中没有其他方法： **eval** 是一个函数。那么问题就来了:

.. code-block:: none
    :linenos:

    * (let ((x 1))
        (eval
          '(+ x 1)))
    Error: The variable X is unbound.

有时，将词法环境扩展到 **eval** 显然是可取的。但是要小心。经常有人说，如果你正在
使用 **eval** ，那么你可能正在做一些错误的事情。 **eval** 的误用会导致程序速度变慢，
因为 **eval** 是非常昂贵的操作 —— 主要是因为它需要展开出现在传递给它的结构中的宏。
假如在编程时突然发现需要 **eval** ，问一下自己，为什么不能早点做想做的事情。
如果答案是你不能，比如说因为刚刚读取了结构，那么恭喜，你找到了 **eval** 的一个
罕见的合法用法。其他任何答案都将直接导致可能一开始就应该使用的方法：使用宏。

.. code-block:: none
    :linenos:

    (defvar pandoric-eval-tunnel)

    (defmacro pandoric-eval (vars expr)
      `(let ((pandoric-eval-tunnel
              (plambda () ,vars t)))
        (eval `(with-pandoric
                  ,',vars pandoric-eval-tunnel
                  ,,expr))))

但是假设你真的想要求解（ **eval** ）某样东西，只要你能使用那个讨厌的词法上下文。
**pandoric-eval** 宏是一个使用 **plambda** 和 **with-pandoric** 的有趣示例。
**pandoric-eval** 使用被我们命名为 **pandoric-eval-tunnel** 的特殊变量，通过动态环境使
**pandoric** 闭包对于 **eval** 函数是可用的。通过提供所有符号的列表
作为 **pandoric-eval** 的第一个参数，我们可以精确地选择要在动态环境中使用的词法
变量。这里我们将它应用到前面的例子中:

.. code-block:: none
    :linenos:

    * (let ((x 1))
        (pandoric-eval (x)
          '(+ 1 x)))
    2

同时通过 **pandoric-eval** 求解的表达式会修改原有的词汇环境； **pandoric-eval**
是一个双向隧道:

.. code-block:: none
    :linenos:

    * (let ((x 1))
        (pandoric-eval (x)
          '(incf x))
        x)
    2

这一节虽然很长，但仍然只触及了 **pandoric** 宏及其许多可能的变体的皮毛。
我期待他们在未来的许多有趣的发展。

思考1： **pandoric-eval** 可以嵌套调用吗？也就是说，可以使用 **pandoric-eval**
来计算 **pandoric-eval** 的结构吗？为什么或为什么不？

思考2：虽然这里的 pandoric 宏的实现效率很高，但还可以改进。可以尝试改进
**pandoriclet-get** 和 **pandoriclet-set** ，以生成使用哈希表而不是 **case**
的代码，然后对这两个实现分别进行小量和大量的 pandoric 变量进行基准测试。
研究你最喜欢的 CLOS 实现，模拟调度是如何进行的，重新进行基准测试。

.. _OnLisp: http://www.paulgraham.com/onlisp.html
