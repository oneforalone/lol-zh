.. _top_down_programming:

==================================
5.2 自上而下的编程
==================================

:Author: Doug Hoyte
:Translator: Yuqi Liu <yuqi.lyle@outlook.com>

.. code-block::

  你教不会初学者自顶向下编程，因为他们不知道哪一端是上。 --C.A.R. Hoare

在 :doc:`../Chapter03/domain-specific` 中，当我们第一次考虑特定域语言时，我们创建了个简单的宏 ``unit-of-time`` 。这个宏允许我们用一种直观的、基于符号的语法，可以方便地以不同的单位指定时间段：

.. code-block::

  * (unit-of-time 1 d)

  86400

``unit-of-time`` 是个很方便的特定域语言，因为程序员不必去记住一些东西，比如说，一天有多少秒。``unit-of-time`` 是用简单的宏实现的，该宏使用 case 语句作为底层展开的核心。

宏设计的一个重要原则就是自上而下编程。设计一个 lisp 宏时，首先要从抽象开始。你需要在编写这个宏之前就想要使用这个宏。有点矛盾的是，在为该语言编写简洁的定义/实现之前，你需要知道怎么用这个语言编程。

因此，构造个正规的宏的第一步是编写宏的用例，即使无法测试或使用它们。如果用新语言编写的程序足够全面的话，那么接下来就会有个很棒的想法，即该语言实现编译器或解释器需要什么。

回到 ``unit-of-time`` 宏，有没有办法将它提升到另一个级别的规格，并创建一种语言来创建这些单位的方便的宏呢？好吧，``unit-of-time`` 是个宏，为了实现目的就需要用宏来定义宏……

停！到此为止。

我们不是从考虑语言的实现开始的，而是问我们自己要用这个语言做什么。答案是我们想要个简单的方法，用来定义这类帮助转换单位的工具。以下这个示例中，我们希望使用一种单位类型——时间，其基本单位为：秒，用 ``s`` 来指代，以及一组单位和这个单位到基本单位的转换因子：

.. code-block::

  (defunits% time s
    m 60
    h 3600
    d 86400
  ms 1/1000
  us 1/1000000)

``Defunits%`` 会展开成定义宏的代码，就像在 :doc:`../Chapter03/domain-specific` 中编写的 ``unit-of-time`` ，允许我们将任意的时间单位转换为秒。还能写的更好吗？

在设计头脑风暴中，创新在大多数编程语言中都停滞不前。刚刚我们创建了一种将不同单位的乘数值映射到代码中的方法，这种方法让我们能够方便地转换单位。但作为一个专业的 lisp 程序员会意识到这个映射本身就是一个程序，并且可以用我们经常增强lisp程序的方法来增强它。

当我们输入多种不同的单位是，用来指定对应的单位就会很有用。现在，让我们规定一个因子，这个因子用来增加单位的种类，可以是一个列表，该列表中的值与单位相对应，如下所示：

.. code-block::

  (defunits%% time s
    m 60
    h (60 m)
    d (24 h)
   ms (1/1000 s)
   us (1/1000 ms))

上面这个单位的列表看起来就比较自然了。我们以分钟为基础单位，秒、时基于分钟，天基于小时。为了使用迭代的方法实现这个宏，首先需要用 ``defunits%`` 来实现非链的版本，然后用 ``defunits%%`` 实现链版本，最后添加适当的错误检查，就有了最终的版本：``defunits``。

注意，这种新语言可以提供更多方便的语法来添加新的单元类型。这种语言还允许我们延迟四舍五入对计算的影响，并允许 lisp 使用尽可能精确的算法。例如, furlong 相当于 1/8 英里，所以我们使用链版本来对其进行编码，也就是说，近似的距离，就可以得到更准确的结果，或者说更重要的是，与其他计算结果尽可能保持一致，都使用英里做单位。这是因为我们可以添加找到的最精确的转换因子，而不需要自己进行任何转换，宏让我们在其他语言中无法实现的表达式级别上构建转换例程。

使用 :doc:`../Chapter03/unwanted-capture` 中的 ``gensym`` ，``defunits%`` 就很容易编写。Graham 的 ``symb`` 函数可以将转换宏生成个新的名字。例如，当 ``time`` 是内置的表示单位，那么转换宏就是 ``unit-of-time``。 ``defunits%`` 是由最初定义的 ``unit-of-time`` 构建的，``unit-of-time`` 是在 :doc:`../Chapter03/domain-specific` 中定义的，在 ``defunits%`` 中，由 ``defmacro!`` 和反引号组成，用来替换宏调用时需要重新生成的部分。

.. note::

  Graham 是 On Lisp 的作者，会经常出现一些上面的内容，如果有时间的话，推荐去看一下这本书。

.. code-block::

  (defmacro! defunits% (quantity base-unit &rest units)
    `(defmacro ,(symb 'unit-of-quantity) (,g!val ,g!un)
       `(* ,,g!val
           ,(case ,g!un
             ((,base-unit) 1)
             ,@(mapcar (lambda (x)
                         `((,(car x)) ,(cadr x)))
                       (group units 2))))))

``defunits%`` 用了反引号（ `````）嵌套：一个非常难以理解的结构。用反引号编程就像在代码中增加了一个维度的含义。在其他的语言中，给定的语句通常都有非常简单的语义计算。你能清除的指导每段代码会在什么时候执行，因为每段代码都必须同一时间执行：运行时（run-time）。但在 lisp 中，我们可以通过反引用嵌套来缩放引用的梯度。每次使用反引号时，都将我们的梯度往上提了一级：反引号内的代码是一个列表，之后这个列表可能会被求值也可能不会。但在里面的原始列表中，每遇到逗号时，有会将我们会回到上一个引用梯度，然后以合适的方式执行对应梯度的代码。

因此，有一种简单的算法可以确定何时 lisp 代码会被求值。只需从表达式的根开始，在遇到反引号后，标记一层引号。每遇到一个逗号，就把引号调低一级。正如 Steel 所指出的，遵循这种级别的引用很具挑战性。追踪当前引用深度的这种困难，让使用反引用感觉像是在常规编程中添加了另一个维度。在其他语言中，可以随意向“东南西北”四个方向走，但 lisp 还提供了向上的选择。

``defunits%`` 是个好的开始，但却没有实现链。目前，实现该语言的宏主要是简单的替换。要实现链行为需要更复杂的程序逻辑。简单的替代不起作用，因为宏的部分依赖于宏的其他部分，所以在扩展时，需要完整地处理提供给宏的表单，而不仅仅是考虑可以插入的各个部分。

记住，宏实际上就是函数，现在来创建了一个实用函数在宏定义中使用：``defunits-chaining%``。这个实用函数接收一个单位，例如像 ``S``、``M`` 或是 ``H`` 这样的符号，同时接收该单位规格列表。这个单位规格既可以是单个数字，这个数字被解释为基础单位，如 ``(M 60)``，也可以是一个列表，该列表内部链式地指向另一个单位，如 ``(H (60 M))``。

.. code-block::

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

这个实用函数是递归的。为了求基本单位的乘数，我们将链中的每一步乘以另一个实用函数的调用，从而算出链的其余部分。当调用堆栈返回时，就会得到将给定单元的值转换为基本单元的乘数。例如，在构建小时的乘数时，可以求得一小时是六十分钟，然后递归得到一分钟是六十秒，再次递归时发现秒是这条链的末尾，然后就会直接将分钟设为基础单位。因此，递归堆栈返回需要计算的是：``(* 60 (* 60 1))``，也就是 ``3600``，这样就得到了一小时等于 3600 秒。

有了这个实用函数后，计算每个单位之间的乘数只需要对 ``defunits%`` 进行简单的修改，如下面的 ``defunits%%``。我们不是直接从单元规格中拼接值，而是将每个单元和整个单元规格传给 ``defunits-chaining%`` 实用程序。如上所述，这个函数递归地计算出将每个单元转换为基本单元所需的乘数。通过这个乘数， ``defunits%%`` 可以像 ``defunits%`` 一样拼接到 ``case`` 语句中。

然而，这些宏并不完整。``defunits%`` 宏不支持链式。 ``defunits%%`` 支持链式，但没有错误检查。专业的宏编写人员总是小心地处理任何可能出现的错误条件。在无限循环或是在 REPL 中难以调试的情况中，错误检查尤为重要。

``defunits%%`` 的问题实际上是我们设计的语言的一个属性：可以编写有环的程序。如：

.. code-block::

  (defunits time s
    m (1/60 h)
    h (60 m))

为了提供适当的调试输出，需要稍微增强实现。最终的版本，``defunits``：

.. code-block::

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

``defunits`` 不但支持链式，而且如果该语言的用户指定了具有这种循环依赖关系的程序，它还提供了有用的调试输出。之所以能做到是因为使用了 ``defunits-chaining``——``defunits-chaining%`` 的升级版，``defunits-chaining%`` 维护了以前访问过的所有单元的列表。这样，当再次通过链式访问同一个单位时，就会抛出异常来简明的描述这个问题:

.. code-block::

  * (defunits time s
      m (1/60 h)
      h (60 m))

  Error in function DEFUNITS-CHAINING:
    M depends on H depends on M

``defunits`` 宏与 ``defunits%%`` 完全相同，除了传递了个额外的参数 ``nil`` 给``defunits-chain``，这是表示已经到了访问过的单位记录列表的末尾。如果一个新单位被搜索，而我们已经访问过它，那么一个环就被检测到了。我们可以用这个访问过的单元历史记录来向宏的用户（很可能是我们自己）显示有用的信息，这些用户可能无意中写入了环。

因此，``defunits`` 是种将单元输入到转换例程领域的专用语言。实际上，它精确到更细的领域；也有很多可能的写法。由于在 ``Blub`` 中创建语言很困难，而在 lisp 中却很容易，所以lisp程序员通常不会把所有东西都塞到一个域中。相反，它们只是使语言越来越精确到问题领域，直到问题变得很细致。

使用 ``defunits`` 的例子是 ``unit-of-distance``。

.. code-block::

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

如果你想知道的话，1970 年采用国际海里制缩短了英寻（至少对英国水手而言）的 1/76，也就 2 厘米多一点：

.. code-block::

  * (/ (unit-of-distance 1 fathom)
       (unit-of-distance 1 old-brit-fathom))

.. code-block::

  * (coerce
      (unit-of-distance 1/76 old-brit-fathom)
      'float)

  0.024384