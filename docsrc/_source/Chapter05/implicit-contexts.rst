.. _implicit_contexts:

==================================
5.3 隐式上下文
==================================

:Author: Doug Hoyte
:Translator: Yuqi Liu <yuqi.lyle@outlook.com>

宏可以使用隐式上下文的技术。在常用的代码中，或者说是需要绝对简洁且没有很细节的代码中，有时要隐式地在表达式的某些部分添加 lisp 代码，这样就不必每次使用抽象时都去编写它。之前也有介绍过隐式上下文，而且也很清楚的表达了，即便是不使用宏，隐式上下文也是 lisp 编程的基础部分： ``let`` 和 ``lambda`` 表达式就有个隐式的 ``progn``。因为这两个表达式是顺序的执行表达式的主题并返回最后的那个结果。``defun`` 会在表达式外添加隐式的 ``lambda``，因此不需要在已命名的函数中使用 lambda 格式。

本节介绍的是本书中后面要用到的遍历代码的宏——``tree-leaves`` 的推导以及构造。和 ``flatten`` 一样，``tree-leaves`` 宏会检查一段 lisp 代码，将这段代码当作一个树（``tree``），然后做一些改动后返回一个新树。原表达式的列表结构不会被更改：``flatten`` 和 ``tree-leaves`` 都是构建新的结构。这两者之间的不同之处在于，``flatten`` 会将嵌套列表中的嵌套移除然后返回一个不是真正的 lisp 的扁平（``flat``）列表，而 ``tree-leaves`` 则是保留了表达式的结构，但修改了特定原语（``atom``）的值。

.. note::

  这里的树指的是数据结构中的树。原语指的是一个词，为最小单位，不可再分割。具体参考: https://www.gnu.org/software/emacs/manual/html_node/eintr/Lisp-Atoms.html

现在，先从简单的初稿开始吧。``tree-leaves%`` 是个函数，

.. code-block::

  (defun tree-leaves% (tree result)
    (if tree
      (if (listp tree)
        (cons
          (tree-leaves% (car tree)
                        result)
          (tree-leaves% (cdr tree)
                        result))
        result)))

该函数会递归的去遍历提供的 ``tree`` 表达式参数，然后将同类型的构造成列表结构。当遇到原语时，函数会返回 ``result`` 参数的值，而不是返回原语的值：

.. note::

  在 ``if`` 结构中，如果 ``else`` 部分没有的话，那么 ``else`` 的部分就返回 ``nil``，即空列表。

.. code-block::

  * (tree-leaves%
      '(2 (nil t (a . b)))
      'leaf)

  (LEAF (NIL LEAF (LEAF . LEAF)))

所以，``tree-leaves%`` 返回了个新的树结构，其中所有的原语都被转换成了提供的参数 ``leaf``。注意，``cons`` 结构中 ``car`` 位置的原语 ``nil`` 没有变，和 ``cdr`` 位置一样，都不会变( ``cdr`` 为 ``nil`` 时即表示空列表）。

当然，更改每个元素是没有什么意义的。我们真正想要的是一种选择特定原语的方法，并选择性地对其进行转换，之后再将转换后的原语插入到新的列表结构中，对不相关的就不用去管他了。在 lisp 中，编写个可自定义的使用函数的最直接的方法就是有插件——即用户可以使用自定义的代码来控制实用程序的功能。``COMMON LISP`` 内置的 ``sort`` 函数就是典型的代表。以下的代码中，小于函数对 ``sort`` 来说就是个插件：

.. code-block::

  * (sort '(5 1 2 4 3 8 9 6 7) #'<)
  (1 2 3 4 5 6 7 8 9)

使用函数作为参数来控制程序的行为的这个理念很方便，因为这样就可以创建写适合手头任务的匿名函数。或者说，当需要更强大的功能时，可以创建个生成匿名函数的函数。这种行为被称为函数组合（``function composition``）。尽管函数组合没有宏组合（``macro composition``）那么有趣，但这仍是个很有用的技术，且这个技术是专业 lisp 程序员必须掌握的。
