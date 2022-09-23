.. _preface:

*************
译者序
*************

我的 Lisp 之路
==========================

其实，最开始接触 Lisp 是因为大二无聊时看了长铗的 `屠龙之技 <https://www.513gp.org/book/4709/224736.html>`_ ，
然后暑假就看了一下 `Practical Common Lisp <https://gigamonkeys.com/book/>`_ 和 `ANSI Common Lisp <http://www.paulgraham.com/acl.html>`_ 。
结果也只是看了一下，因为我是上大学后才开始接触计算机，然后大一学的又是 C，所以也只是看了一遍，
并没有说真正的入门 Lisp，因为我对 REPL 这种模式很陌生，同时对计算机这一学科的基础知识积累的
也很少。当时验证书中的例子使用的是 `Lispbox <https://lispbox.common-lisp.dev>`_ ，
但那只是将 ``sbcl + slime + emacs`` 封装了一下，而且版本还都不是最新的，然后我就转而自己来
配置，结果就去玩 Emacs 了，对 CL 也就不了了之了。其实正如 :ref:`appendix-d` 中 Doug
说的一样，Emacs 是个很好的编辑器，但是其设计的最终目的是辅助进行编辑，而不是辅助编写代码。在之
后的两年时间中，我花了大部分的精力在配置一个自己舒服的编辑器，找各种插件，自己写配置。最终的结果
是：对 lisp 的理解并没有加深，而 Emacs 也因为各种插件导致启动较慢。期间把 `SICP <https://mitpress.mit.edu/sites/default/files/sicp/index.html>`_ 看了一遍，发现重要的还是编程的思想，而不是工具的配置，就开始慢慢对 Emacs 的热情褪去，开始找对应的Lisp 书籍看。

.. note::

  Emacs 启动慢也研究了一段时间，一开始是通过 **autoload** 来延后加载插件来降低启动时间，
  进步一是启动时以 server 的模式启动，然后每次打开都是直接开启一个 buffer，虽然也很快，但是
  第一次启动还是比较慢，同时如果是在新机器上使用还需要等很久去下载那些插件。Anyway，青菜萝卜，
  各有所爱，就像 `oh-my-zsh <https://ohmyz.sh>`_ 一样，一开始是觉得很酷，可是我自己后来
  发现花费大量的时间和精力去配置得到的收获并不大，或者说投入回报比不高。就不去玩这些了，老老实
  实去看原理书籍。

直到大学毕业工作后，无聊时准备好好学习一下 Lisp 时，无意中从 `Common-Lisp.net <https://common-lisp.net>`_
找到了 `The CommonLisp Cookbook <https://lispcookbook.github.io/cl-cookbook/>`_
，才算是把 CL 给入门了。同时也是粗略的翻译了一下，具体的翻译的地址为 `The CommonLisp Cookbook Chinese Version <https://oneforalone.github.io/cl-cookbook-cn/#/>`_ 。
随后决定把 `Let Over Lambda <https://letoverlambda.com>`_ 看一遍然后翻译出来。
随着翻译的进行，对 Lisp 的宏也有了一定的了解，才知道为什么作者会说宏是 Lisp 的核心。因为
Lisp 的宏和其他语言的宏完全不是一个性质 —— 其他语言的宏都只是将对应的代码进行替换，而 Lisp
的宏是能根据参数生成对应的 Lisp 代码。

结合我自身的经历，如果是自身计算机基础比较薄弱，或者说是编程基础差的初学者来说，推荐先看一下
`The CommonLisp Cookbook`_，看完这本书前面几章后，你就能够比较轻松的去看其他的书籍了，毕
竟，如果在初学一门语言时，连 ``Hello, World`` 都写不出来，那很打击学习的激情的。遇到不懂的系
统内置函数或宏的时候，可以到 Lispworks 的 `HyerSpec <http://www.lispworks.com/documentation/lw70/CLHS/Front/Contents.htm>`_ 中查看具体使用方法。


关于本书
=====================

关于这本书名，我目前还找不到很确切的中文来表达，因为 Let Over Lambda 是 Doug 对闭包的描述，
这种说法很生动形象，所谓闭包就是函数和变量的绑定，用 Lisp 的代码写出来就是这样的：

.. code-block:: lisp

    (let ((counter 0))
      (lambda () (incf counter)))

上面代码返回的是里面的 lambda 函数，但是每次调用这个函数时，这个函数都会修改 `counter` 这个
变量，而 **let** 关键词是在 **lambda** 关键词上面，所以就叫 Let Over Lambda，很直观。

.. note::

  在 Lisp 中，函数和变量是分开存储的，所以函数名和变量是可以完全相同的，同时在 lisp 中，
  函数也是一个类型，可以直接作为返回值。

当然这段代码直接执行解释器中会返回一个 `#<FUNCTION (LAMBDA ()) {70051DA5EB}>` 这样的
东西，这是提示你返回的是一个 lambda 函数。所以如果想要确认结果是不是如我们所说的，有两种方法
来进行验证。

- 将 lambda 函数绑定到一个变量上：

.. code-block::

    * (defvar counter
        (let ((counter 0))
          (lambda () (incf counter))))

    * (funcall counter)
    1
    * (funcall counter)
    2
    * (funcall counter)
    3

- 使用内置的 `*` 符号，其中 `*` 表示上个表达式的结果，`**` 表示上上个表达式的结果，
  `***` 表示上上上个表达式的结果，目前 sbcl 只支持三个 `*` 。

.. code-block::

    * (let ((counter 0))
        (lambda () (incf counter))))
    #<FUNCTION (LAMBDA ()) {70051DA5EB}>
    * (funcall *)
    1
    * (funcall **)
    2
    * (funcall ***)
    3

.. note::

  在 CommonLisp 中，调用执行函数需要使用 `funcall` 关键字

其实对于 Lisp 中闭包，可以类比 C 语言中的结构体或事面向对象语言中的类，都是函数和变量进行
了绑定。实际上，本书中有介绍如何使用宏和闭包完成面向对象的方法，即
:ref:`6-7-pandoric-macros` 中的 pandoric 宏，很有趣的一节，很值得研究。

.. note::

  虽然说近期很火的 Rust 里的宏比 C 的有一定的改进，从代码替换变成了正则匹配，但个人感觉
  还是比不上 Lisp 的宏，因为 Lisp 的宏是能够执行对应的参数后然后在生成对应的代码，然后解释器
  再对生成的代码进行执行。同时 Rust 中的 Ownership 给我的感觉就是 Lisp 中的词法作用域。


关于翻译
=====================

因为本人的能力和时间有限，本书的翻译是我先从 pdf 中拷贝出来，然后通过翻译软件翻译一遍，
再然后自己去修改，因此会有一些拼写错误，同时也可能有些翻译出来并不是很准确或者说很绕，
如果有条件的话，推荐去阅读英文的原文。同时因为这本书是 Doug 看了 Paul 的 `On Lisp <http://paulgraham.com/onlisptext.html>`_
后受到了启发，所以有时间的话，也推荐去看一下 `On Lisp <http://paulgraham.com/onlisptext.html>`_ 这本书。

还有什么呢？就是这本书中的第一人称均指代的是作者，而不是译者本人，如果是译者本人的观点或
想法，会额外的标明。同时，因为将 PDF 的格式转换成 reStructured 格式，书中的一些脚注就
还没又进行添加（P.S. 因为当时我忙着翻译，不想去研究 reStructured 怎么添加脚注，准备
留给后期校对时在进行）。毕竟，翻译和写作都是个体力活，不像写代码可以使用 CV 大法 🤪 

.. note::

  之后应该不会再进行翻译了，只会在翻译的两本书的初版的基础上进行校对。然后以原创为主，记录在我自己的博客上：http://oneforalone.github.io 。

  如果对翻译感兴趣的话，推荐 `思果 <https://baike.baidu.com/item/思果/8413483>`_ 的 `翻译研究 <https://book.douban.com/subject/1234604/>`_ 和 `翻译新究 <https://book.douban.com/subject/1234605/>`_ 。


最后的最后
===================

Happy Lisping!

Yuqi Liu
