.. _pandoric:

==================================
6.7 潘多拉（ *Pandoric* ）宏
==================================

:Author: Doug Hoyte
:Translator: Yuqi Liu <yuqi.lyle@outlook.com>

潘多拉魔盒是个关于世界上第一个女人的希腊神话：潘多拉。潘朵拉，U 语言的符号，希腊语翻译过来是全能。潘多拉，这个女人，在好奇心的诱惑下，打开了一个小盒子，无可挽回地释放了人类所有的罪恶和罪恶。虽然本节中描述的宏非常强大，可能会教你一种永远不会忘记的编程方法，但请放心，结果要比可怜的潘多拉好得多。现在开始，打开这个盒子。

首先，稍微绕过另一本著名的 lisp 书：克里斯蒂安·奎奈克的《Lisp in Small Pieces》。Queinnec 是一位广受尊敬的 lisp 专家，对 lisp 知识做出了很大的贡献。Queinnec 的书的内容是在 Scheme 编程语言中实现各种复杂的编译器和解释器。

.. note:: Lisp in Small Pieces: https://pages.lip6.fr/Christian.Queinnec/WWW/LiSP.html

《Lisp In Small Pieces》中有个简短但有趣的宏的讨论。由于 Scheme 宏规范的模糊性，它涉及到描述不同的宏系统变化，但是为什么我们可能想要使用宏以及如何使用它们，有些有趣的注意事项。如果你已经阅读并理解了 :doc:`../Chapter03/index`，那么 《Lisp in Small Pieces》章节中介绍的大多数宏，对你来说，都属于微不足道的类别，除了我们现在要讨论的这个诱人的宏。

和许多编程书籍一样，《Lisp in Small Pieces》将我们带到了一个面向对象编程系统的实现。通常这些实现用来概括 CLOS（ COMMON LISP  Object System）的一个子集。Queinnec 称他的子集为 MEROONET。Queinnec 指出，在为 MEROONET 类定义方法时，最好能够直接引用所定义对象的字段，而不是使用访问器。把 Queinnec 的话翻译过来就是 ::

  以 CLOS 中的 ``with-slots`` 宏为例；将它放到 MEROONET 环境中。对象的字段 —— 假设 ``Point`` 实例的字段 —— 是通过像 ``Point-x`` 或 ``set-Point-y!`` 这样的读和写函数来处理的。在定义方法的上下文中，直接通过字段的名称(例如 ``x`` 或 ``y`` )来处理会更简单。

下面是 Queinnec 预想的接口（他称之为 ``define-handy-method`` ）定义新方法 ``double`` ：

.. code-block::

  (define-handy-method (double (o Point))
    (set! x (* 2 x))
    (set! y (* 2 y))
    o)

这比 MEROONET 语法更让程序员高兴:

.. code-block::

  (define-method (double (o Point))
    (set-Point-x! o (* 2 (Point-x o)))
    (set-Point-y! o (* 2 (Point-y o)))
    o)

换句话说，如果可以使用宏来访问外部绑定（在本例中是对象槽），像是词法绑定一样，那就太好了。虽然，不可否认的是这对缩写的目的很有用，但最重要的含义是它能够为现有的和未来的宏提供二元（dualities）语法。

正如 Queinnec 所提出的， COMMON LISP  通过 ``with-slots`` 宏为 CLOS 实现了这个功能。这是  COMMON LISP  实现其设计目的的一个例子：允许基于精炼的、标准化的宏系统进行抽象。大多数语言被设计成易于实现，而  COMMON LISP  被设计成具有强大的编程功能。Queinnec 的结论是，语言的限制使得 Scheme 几乎不可能实现这一点，特别是在需要可移植性的地方 ::

  由于缺乏关于语言及其实现的反射性信息，我们无法在 Scheme 中编写可移植的代码遍历程序，因此我们不得不放弃编写 ``define-handy-method``。

尽管  COMMON LISP  仍然可以使用大量合法的方法来实现宏系统，但它的设计目的是提供通用的元编程工具，这些工具以标准和可移植的方式组合在一起。这两个先进  COMMON LISP  宏特性允许我们实现像 CLOS 的 ``with-slots`` 一样的东西，它们是 *泛化变量（generalised variables）* 和 *符号宏（symbol macro）*。本节就借此机会展示  COMMON LISP  特性的奇妙组合，并将我们迄今为止见过所有关于回指宏的内容集合在一起，在这个过程中发现了一个有趣的宏类，称为 *pandoric* 宏。

.. code-block::

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

``pandoriclet`` 背后的思想是打开闭包，允许外部访问它们本来封闭的词法变量。与之前的一些宏（如 ``alet-hotpatch`` ）一样，``pandoriclet`` 编译一个间接环境，根据传递的参数选择不同的运行时行为。

我们再次从 ``alet`` 由内而外的展开开始，记住这里引入了个叫 ``this`` 的回指词。``pandoriclet`` 与我们见过的其他宏类似。和所有的回指 ``let`` 变体一样，假设 ``pandoriclet`` 主体中的最后的结构将是 lambda 结构。就像 ``alet-hotpatch`` 一样，``pandoriclet`` 用 ``dlambda`` 宏来在调用 ``pandoriclet`` 返回的闭包时执行不同可能的代码。``pandoriclet`` 还用了上一节介绍的 ``let-binding-transform`` 实用函数来处理已创建的空绑定，如 ``(let (a) ...)``。这个实用函数对 ``pandoriclet`` 是必需的，原因与需要 ``sublet`` 一样：这些宏遍历 ``let`` 中的绑定，而之前的宏盲目地将绑定拼接到另一个 ``let`` 中。

我们调用了两个没定义的创建列表的实用函数：``pandoriclet-get`` 和 ``pandoriclet-set``，它们分别接受一个 ``let`` 绑定列表。注意，我们可以引用还不存在的函数，只要在宏展开之前定义它们就可以，显然，在使用宏之前不能这样做。使用辅助函数来帮助定义宏是一个很好的习惯。它不仅可以使定义更具可读性，还可以在测试宏的组件时提供帮助，并可以在将来的宏中证明是有用的。这种抽象最好的部分是，当组合宏时，保持词法上下文可供实用程序使用。

因此，记住这个词法上下文，现在要写 ``pandoriclet-get`` 和 ``pandoriclet-set`` 。对于 ``pandoriclet-get``，其中 ``dlambda`` 绑定了变量 ``sym``，在这里列表将被拼接进去。在 ``case`` 结构中使用 ``sym`` ，将其与传递给 ``pandoriclet`` 的变量进行比较。如果找到这个变量，则返回它所引用的绑定的当前值。如果没找到，则抛出异常。``pandoriclet-set`` 差不多一样，除了 ``dlambda`` 为它绑定了一个额外的变量：``val`` 。``pandoriclet-set`` 用 ``setq`` 将 ``sym`` 引用的绑定更改为 ``val``。

.. code-block::

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

``prandoriclet`` 也有和回指 let 变体一样的接口，因此可以使用它来创建常见的 counter 闭包：

.. code-block::

  * (setf (symbol-function 'pantest)
      (pandoriclet ((acc 0))
        (lambda (n) (incf acc n))))

  #<Interpreted Function>

如预期般：

.. code-block::

  * (pantest 3)
  3
  * (pantest 5)
  8

同时，现在在创建闭包时可以直接访问 ``acc`` 的绑定：

.. code-block::

  * (pantest :pandoric-get 'acc)
  8

同样的也可以修改这个绑定的值：

.. code-block::

  * (pantest :pandoric-set 'acc 100)
  100
  * (pantest 3)
  103

甚至是 ``this`` 回指的值也能访问，因为我们特意将这个回指打开同时在宏展开时将 ``this`` 变量添加到 ``letargs`` 绑定列表中：

.. code-block::

  * (pantest :pandoric-get 'this)
  #<Interpreted Function>

所以 ``pandoriclet`` 创建的这个闭包已经不再闭包了。这个闭包所使用的环境 —— 即使编译器已经删除了所有的词法变量符号 —— 仍可以通过 ``pandoriclet`` 返回的匿名函数来访问。这是怎么做到的呢？通过 pandoric 宏，将编译额外的代码，以提供从外部访问闭包的方法。但从这个正在发生的低级角度看，并不能看到 pandoric 宏的威力。我们所做的是创建一个闭包间协议，或消息传递系统，用于闭包之间的通信。

在继续讨论 pandoric 宏之前，首先需要指出一个 COMMON LISP 语法二元性的最重要的例子：泛化变量（ *generalised variables* ）。这方面的细节很复杂，这里不会做详细的介绍。为此，推荐去阅读 Graham 的 《On Lisp》，这是目前所知道的最好的解决方法。细节是微妙的，想法很简单：访问一个泛化变量在语法上是双重的。只有一种 setter 结构：``setf``，``setf`` 能够通过使用访问变量时使用的相同语法设置所有类型的变量。

例如，通常是通过变量的变量名来访问其值，假设这个变量名为 ``x``。可以用 ``(setf x 5)`` 来设置 ``x`` 的值为 5。同样，要想访问个调用的 cons 的 car 单元，假设也为 ``x``，可以使用 ``(car x)``，也可以通过 ``(setf (car x) 5)`` 来设置其值。。这隐藏了个事实，机设置 cons 的实际方法是使用 ``rplaca`` 函数。通过实现这种二义性语法，我们将需要记住的访问器和设置其的数量减少了一半，更重要的是，为宏提供了的新方法。

.. code-block::

  (declaim (inline get-pandoric))

  (defun get-pandoric (box sym)
    (funcall box :pandoric -get sym))

  (defsetf get-pandoric (box sym) (val)
    `(progn
        (funcall ,box :pandoric -set ,sym ,val)
        ,val))

``get-pandoric`` 函数是对内部闭包协议 getter 语法的封装。它被定义为内联，以消除这种封装所造成的任何性能影响。

``defsetf`` 是一个有趣的 COMMON LISP 宏，完全不像 ``defmacro`` 的拓展 ``defmacro!`` 隐式地绑定提供的结构的 gensyms。``defsetf`` 非常适合定义泛化变量二元性的 setter 端，只要 getter 可以表示为一个函数或宏，对其所有参数精确计算。注意，虽然可以将 ``get-pandoric`` 定义为宏，但这样做的唯一原因是为了内联。宏不是用来内联的，编译器是用来内联的。

回到 ``pantest`` 中的符号函数中存储的 pandoric 计数器，我们可以用这个新的 getter 函数来获取 ``pantest`` 中 ``acc`` 当前绑定的值：

.. code-block::

  * (get-pandoric #'pantest 'acc)
  103

现在，多亏了泛型变量和 ``defsetf``，可以用一个语法对偶来设置 ``acc`` 的值:

.. code-block::

  * (setf (get-pandoric #'pantest 'acc) -10)
  -10
  * (pantest 3)
  -7

通过函数关闭的环境 —— 该函数是在 *let over lambda* 中调用的 let —— 开始看起来像常规可访问的通用变量，就像 cons 单元格或哈希表条目。闭包现在是比过去更一流的数据结构。以前对外部代码封闭的绑定现在对我们开放，即使这些绑定被编译成高效的东西，或者它们的访问器符号早就被遗忘了。

但是，任何关于泛型变量的讨论，如果不提到它的近亲：*symbol macro*，都是不完整的。像其名字所提示的那样，``symbol-macrolet`` 可以讲符号扩展成一般的 lisp 结构。因为它很直观以及更灵活的使用形式，看起来像函数调用代表宏转换，没有大量使用 ``symbol-macrolet`` 的一个重要应用的关键是：符号宏隐藏了泛型变量，这样宏的使用者认为他们正在访问常规词法变量。

符号宏的引入导致了 COMMON LISP 语言中最奇怪的组合之一：通常在设置个通过常规符号访问的变量时，比如 ``(setf x t)``， ``setf`` 将展开成 ``setq`` 结构，因为这就是设计 ``setq`` 最初目的：设置词法变量和动态变量（通常由符号引用）。但是 ``setq`` 结构不能设置泛型变量，所以当引入符号宏时，符号不仅可以表示词法/动态绑定，还可以表示任何泛化变量，有必要指出的是，通过 ``setq`` 结构设置由符号宏定义的符号会被转换回 ``setf`` 结构。奇怪的是，这确实是正确的做法，因为它允许宏对宏的用户完全隐藏泛型变量的存在，即使他们用 ``setq``。真正正确的解决办法是从语言中删除冗余的 ``setq`` 结构，支持的更通用的 ``setf`` ，但这不会发生，原因是明显的兼容性以及宏创建期间，``setq`` 也可以是个有用的安全快捷方式 —— ``setf`` 加上个检查符号是拼接的，而不是列表结构。在用 ``setq`` 时，记住只有在其拼接安全属性有用；正如我们所看到的，多亏了 ``symbol-macrolet``，符号可以引用任何泛型变量。

.. code-block::

  (defmacro! with-pandoric (syms o!box &rest body)
    `(symbol -macrolet
      (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                syms ))
      ,@body))

``with-pandoric`` 宏会展开成个 ``symbol-macrolet``，``symbol-macrolet`` 为 ``syms`` 中提供的每个符号定义了符号宏。每个符号宏将在符号宏的词法作用域中展开对其符号的引用，用 ``get-pandoric`` 访问器/设置器 来访问宏的第二个参数的求值结果：``o!box`` （保存在 ``g!box`` 中）。

因此 ``with-pandoric`` 让我们窥探到了闭包的闭变量绑定：

.. code-block::

  * (with-pandoric (acc) #'pantest
      (format t "Value of acc: ~a~%" acc))
  Value of acc: -7
  NIL

根据广义变量来形成 setting 和 getting 变量的语法对偶的设计，甚至可以假设它是个常规的词法变量，然后通过 setq 设置它：

.. code-block::

  * (with-pandoric (acc) #'pantest
      (setq acc 5))
  5
  * (pantest 1)
  6

现在，我们已经研究了构成 pandemic 宏的大多数部分组成。首先，用于创建闭包的宏：``pandoriclet``，这个宏捕获回指变量：``this``，``this`` 变量引用了在调用闭包时使用的实际函数。这个宏还会编译成一些特殊的代码，这些代码会拦截这个闭包的某些调用，然后访问或修改它的闭包词法变量。其次，``get-pandoric`` 和 ``defsetf`` 实现了访问和设置访问器的单一语法。最后，``with-pandoric`` 宏用 ``symbol-macrolet`` 来设置这些泛型变量，这些泛型变量看起来是新的词法变量，其名称与闭合变量相同。这些变量引用了 ``pandoriclet`` 创建的原始环境，但是，这些环境是不同的词法上下文。

作为个例子，我们将这种打开闭包的功能与 :doc:`../Chapter06/hotpatching` 中的 ``hotpatch`` 宏进行了比较。回顾一下 ``let-hotpatch`` 及其同名的闭包 ``let-hotpatch``，这两个宏使用间接环境创建闭包，以便可以动态更改在调用闭包时调用的函数。这些宏的最大限制是，当对前一个匿名函数进行热补丁时，会强制抛出所有在该函数上关闭的词法绑定。这种情况是不可避免的，因为在编写这些宏时，闭包对我们关闭了。

对于 ``let-hotpatch`` 和 ``let-hotpatch``，必须将特殊目的的代码编译到每个闭包中，这些闭包能够将 ``this`` 回指的词法绑定设置为它的新值。但是由于现在可以打开由 ``pandoriclet`` 定义的闭包并在外部运行这个 ``setter`` 代码，所以可以定义一个可以处理任何 pandoriclet 闭包的热补丁函数 ``pandoric-hotpatch``。

.. code-block::

  (defun pandoric-hotpatch (box new)
    (with-pandoric (this) box
      (setq this new)))

有时抽象在感觉很对，很难确切地说出为什么。也许是因为大多数编程都是不相关部分的不和谐组合，当碰巧发现抽象完美地结合在一起的时，会感到很惊讶和愉快。``pandoric-hotpatch`` 看起来和其工作原理完全一样：打开个 pandoric 接口，从闭包的词法范围中取变量 ``this``，然后使用 ``setq`` 将 ``this`` 设置为要热补丁的闭包 ``new``。

甚至在我们意识到我们需要个 pandoric 闭包热补丁前使用 ``pandoric-hotpatch``。还记得本节中一直用的计数器闭包吗？它仍要绑定到 ``pantest`` 的符号函数。上次的结果是 6:

.. code-block::

  * (pantest 0)
  6

现在设置个新闭包 —— acc 有个新绑定，初始值为 100，之后就递减：

.. code-block::

  * (pandoric-hotpatch #'pantest
      (let ((acc 100))
        (lambda (n) (decf acc n))))
  #<Interpreted Function>

显然，热补丁成功了：

.. code-block::

  * (pantest 3)
  97

现在，counter 闭包中有个新值绑定到 ``this`` 上，用来执行计数。但这个 hotpatch 改变了 ``acc`` 变量绑定的 pandoric 值吗?

.. code-block::

  * (with-pandoric (acc) #'pantest
       acc)
  6

并没有。 ``acc`` 还是之前的值 6，因为这里只修改了 pandoric 环境中 ``this`` 的绑定因为我们在这个混乱的环境中更改的唯一绑定是这个，然后将其变成了个有自己绑定的 ``acc`` 的新闭包。

.. code-block::

  (defmacro pandoric-recode (vars box new)
    `(with-pandoric (this ,@vars) ,box
      (setq this ,new)))

``pandoric-recode`` 宏采用种略微不同的 hotpatch 方法。其保留了代码的原始词法环境，同时还要在闭包被调用到外部代码和外部编译时，设法改变要执行的函数。听起来有点难以置信？记住，在原来的 pandoric 环境中，``acc`` 的值是 6，可以用 ``pandoric-recode`` 设置个新函数来使用这个原始值，哦，或者说，将计数器的值减去 ``n/2``:

.. code-block::

  * (pandoric-recode (acc) #'pantest
      (lambda (n)
        (decf acc (/ n 2))))
  #<Interpreted Function>

当然，就有了新的行为，会将 ``acc`` 减去 ``(* 1/2 2)``，从 6 变为 5:

.. code-block::

  * (pantest 2)
  5

那这和最初的 pandoric 绑定有关联吗？

.. code-block::

  * (with-pandoric (acc) #'pantest
      acc)
  5

对的，有关联。那 ``pandorc-code`` 是如何工作的呢？它在提供的 lambda 结构中关闭了原始闭包打开的绑定。

.. code-block::

  (defmacro plambda (largs pargs &rest body)
    (let ((pargs (mapcar #'list pargs)))
      `(let (this self)
        (setq
          this (lambda ,largs ,@body)
          self (dlambda
                  (:pandoric-get (sym)
                    ,(pandoriclet-get pargs
                  (:pandoric-set (sym val)
                    ,(pandoriclet-set pargs))
                  (t (&rest args)
                    (apply this args)))))))

到目前为止，用来创建 pandoric 闭包的宏是 ``pandoriclet``。``plambda`` 是个由内到外重写的 ``pandoriclet``，增加了一些重要的特性。首先也是最重要的，``plambda`` 不再创建 pandoric 访问器使用的 let 环境。相反，``plambda`` 接受一组符号，这些符号指向的变量应该在调用者的词法环境中。``plambda`` 可以在词法环境中导出任何变量，透明地让其他词法作用域可以访问——甚至是在 ``plambda`` 结构之前或之后编写和编译的变量。

这是对 *let over lambda* 闭包系统的一个增量改进，该系统旨在最大化双语法。多亏了 pandoric 宏（其中最重要的是 ``plambda`` 和 ``with-pandoric``），可以在需要时轻松有效地超越词法作用域的界限。闭包不再关闭；我们可以轻松地打开闭包，就像将 lambda 结构重写为 lambda 结构一样。用 ``plambda`` 导出词法变量，然后用 ``with-pandoric`` 将它们作为完全等价的词汇变量导入。事实上，这些新变量是等价的，它们根本就不是新变量。理解 pandoric 变量的一种更好的方法是，它们只是原始词法作用域的扩展。以 ``plambda`` 的使用做个简单示例，有个 pandoric 计数器，它从两个可能不同的词法环境导出变量：

.. code-block::

  * (setf (symbol-function 'pantest)
      (let ((a 0))
        (let ((b 1))
          (plambda (n) (a b)
            (incf a n)
            (setq b (* b n))))))
  #<Interpreted Function>

请注意，导出这些词法引用是多么容易。让闭包 pandoric 就像在 ``lambda`` 之前添加个 ``p`` 字符一样简单，或者是像在 ``lambda`` 参数后添加一个要导出的变量列表一样简单。我们可以打开这个闭包 —— 或者是任何导出 ``a`` 和 ``b`` 的 pandoric 闭包 —— 像这样使用 ``with-pandoric``：

.. code-block::

  * (defun pantest-peek ()
      (with-pandoric (a b) #'pantest
        (format t "a=~a, b=~a~%" a b)))
  PANTEST-PEEK
  * (pantest-peek)
  a=0, b=1
  NIL

``plambda`` 就是个例子，说明了如何分解宏展开的一般组件。还记得编写 ``pandoriclet`` 时决定将 getter 和 setter 代码的 case 创建语句移到 pandoriclet-get函数中吗？``plambda`` 用到了与之相同的函数。尽管这些宏将函数的结果拼接到相当不同的词法上下文中，但由于两个宏都是用相同的变量命名约定和内部闭包协议编写的，所以代码是可重用的。

因此，pandoric 宏打破了词法界限。它们允许在需要的时候打开闭包，同时也代表了各种 COMMON LISP 语言特性的美丽融合：回指宏、泛型变量和符号宏。但它们到底有什么好的呢?

pandoric 的宏很重要，因为它们在不需要脱离更自然的 let-lambda 组合编程风格的情况下，提供了 CLOS 等对象系统的主要优势。尤其是在不重新实力化已经创建了的对象实力的情况下，就可以为闭包添加功能或方法。

.. code-block::

  (defun make-stats-counter
        (&key (count 0)
              (sum 0)
              (sum-of-squares 0))
    (plambda (n) (sum count sum-of-squares)
      (incf sum-of-squares (expt n 2))
      (incf sum n)
      (incf count)))

``make-stats-counter`` 是个 lambda over let over dlambda，用来创建计数器，只不过它维护了三条信息。除求和外，还保留平方和以及到目前为止处理的项目数。如果在 ``make-stats-counter`` 的定义中使用 ``lambda`` 而不是 ``plambda``，那么大多数信息都是不可访问的。这样就被卡住了，因为这些变量是关闭。

那么要怎么写 ``pandoric`` 方法？可以像上面演示的那样简单地使用 ``with-pandoric`` 访问变量，或者，既然是 lisp，那么就设计个更具体的接口。

.. code-block::

  (defmacro defpan (name args &rest body)
    `(defun ,name (self)
      ,(if args
        `(with-pandoric ,args self
          ,@body)
      `(progn ,@body))))

``defpan`` 是 ``defun`` 和 ``with-pandoric`` 两个宏的组合。``defpan`` 的主要目的是在 ``defun`` 编写函数和 ``with-pandoric`` 访问外部词法范围之间实现语法的二元性。尽管 ``defpan`` 的参数和lambda 结构的语言相同 —— 符号列表 —— 但 ``defpan`` 参数的含义不同。这些 pandoric 函数不是创建了新的词法环境，而是扩展了它们所应用的 pandoric 闭包的词法环境。对于 ``defun`` 和常规的 lambda 结构，变量的名称（符号）不重要。但在 pandoric 函数中，变量名称就是一切。此外，在 pandoric 函数中，参数的顺序并不重要，可以随意地选择使用导出的词法变量数量。

``defpan`` 还有个 ``self`` 的回指，可以执行一种叫做 *回指链（anophor chaining）* 的有用技术。通过在 pandoric 函数之间隐式地传递 ``self`` 的值，就可以在整个函数调用链中维护这个回指的值。与所有的链接结构一样，要确保这个链不会以无限循环结束。

.. code-block::

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

本文给出了三种方法，它们可以用于 ``make-stats-counter`` 创建的闭包或任何其他导出必要变量名的 pandoric 闭包。``stats-counter-mean`` 只是返回传递给闭包的所有值的平均值。``stats-counter-variance`` 通过跟踪链中的链接来计算这些值的方差，而 ``stats-counter-stddev`` 通过跟踪另一个链接来计算标准差。注意，链中的每个链接只需要传递一个回指 ``self`` 来引用闭包的完整词法上下文。可以看到，单个的 pandoric 函数只需要引用它们实际使用的变量，这些变量可以随意调整引用顺序。

所以 ``plambda`` 创建了另一个回指 —— ``self``。``this`` 指的是要调用的实际闭包，而 ``self`` 指的是调用这个闭包的间接环境。虽然听起来有点奇怪，但 ``plambda`` 内部的代码可以用 ``self`` 来大规模访问它自己的词法环境，而不是直接访问它。到目前为止，这似乎只对为在词法作用域内工作而编写的 ``defpan`` 方法有用。

.. code-block::

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

``make-noise-stats-counter`` 和 ``make-stats-counter`` 类似，不同之处是 ``make-noisy-stats-counter`` 用 ``self`` 回指来调用 ``defpan`` 函数 ``stats-counter-mean``、``stats-counter-variance`` 和 ``stats-counter-stddev``。

``plambda`` 和 ``with-pandoric`` 可以随意改写词汇范围。我们以这样一个例子结束本章。词法作用域的一个局限性有时令人遗憾，即当 COMMON LISP 函数 ``eval`` 计算传递给它的结构时，它会丢弃当前的词法环境。换句话说，``eval`` 在空词法环境中计算结构。在 COMMON LISP 中没有其他方法：``eval`` 是一个函数。那么问题就来了:

.. code-block::

  * (let ((x 1))
      (eval
        '(+ x 1)))
  Error: The variable X is unbound.

有时，将词法环境扩展到 ``eval`` 显然是可取的。但是要小心。经常有人说，如果正在使用 ``eval``，那么可能正在做一些错误的事情。``eval`` 的误用会导致程序速度变慢，因为 ``eval`` 是非常昂贵的操作 —— 主要是因为它需要展开传递给它的结构中的宏。假如在编程时突然发现需要 ``eval``，问一下自己，为什么不能早点做想做的事情。如果答案是不能，比如说因为刚刚读取了结构，那么恭喜，你找到了 ``eval`` 的一个罕见的合法用法。其他任何答案都将直接导致可能一开始就应该使用的方法：使用宏。

.. code-block::

  (defvar pandoric-eval-tunnel)

  (defmacro pandoric-eval (vars expr)
    `(let ((pandoric-eval-tunnel
            (plambda () ,vars t)))
      (eval `(with-pandoric
                ,',vars pandoric-eval-tunnel
                ,,expr))))

但是假设你真的想要 ``eval`` 计算某样东西，只要你能使用那个讨厌的词法上下文。 ``pandoric-eval`` 宏是个用 ``plambda`` 和 ``with-pandoric`` 的有趣示例。``pandoric-eval`` 使用了 ``pandoric-eval-tunnel`` 的特殊变量，使 ``pandoric`` 闭包可以通过动态环境提供给 ``eval`` 函数。通过提供所有符号的列表作为 ``pandoric-eval`` 的第一个参数，可以精确地选择要在动态环境中使用的词法变量。这里我们将它应用到前面的例子中:

.. code-block::

  * (let ((x 1))
      (pandoric-eval (x)
        '(+ 1 x)))
  2

同时 ``pandoric-eval`` 计算的表达式会改变原有的词汇环境；``pandoric-eval`` 是一个双向隧道:

.. code-block::

  * (let ((x 1))
      (pandoric-eval (x)
        '(incf x))
      x)
  2

这一节虽然很长，但仍然只触及了 ``pandoric`` 宏及其许多可能的变体的皮毛。期待他们在未来的许多有趣的发展。

思考1：``pandoric-eval`` 可以嵌套调用吗？也就是说，可以使用 ``pandoric-eval`` 来计算 ``pandoric-eval`` 的结构吗？为什么或为什么不？

思考2：虽然这里的 pandoric 宏的实现效率很高，但还可以改进。可以尝试改进 ``pandoriclet-get`` 和 ``pandoriclet-set`` ，以生成使用哈希表而不是 ``case`` 的代码，然后对这两个实现分别进行小量和大量的 pandoric 变量进行基准测试。研究你最喜欢的 CLOS 实现，模拟调度是如何进行的，重新进行基准测试。
