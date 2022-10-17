.. _specification:

=====================
术语表
=====================


.. toctree::
   :maxdepth: 2

   implementation
   indefinite extent
   unwanted capture
   predicate
   transparent specification
   form
   body
   fixnum
   lispy
   duality-of-syntax
   indirection
   pointer scope
   dereference
   slot 
   anaphor 
   destructuring

implementation 
----------------------

编译器

.. note::

  CommonLisp 相关的书籍的汉译版本均将其翻译为“实现”。在牛津词典中可以查到 `implementation <https://www.oxfordlearnersdictionaries.com/definition/english/implementation?q=+implementation>`_ 。然而，
  在中文里“实现”有两种词性，动词和名词，出现在译文中很容易让人摸不到头脑。原文中是指根据 lisp 理论开发出的编译器，
  这些编译器实现了对 lisp 语言开发程序的支持，又因为在 lisp 发展的中间阶段缺乏统一的标准，同样遵循 lisp 理论，但是
  却形成了不同的 lisp 方言。因而 lisp 有 `Scheme <http://groups.csail.mit.edu/mac/projects/scheme/>`_ 、 `CommonLisp <https://common-lisp.net/>`_ 、 `ulisp <http://www.ulisp.com/>`_  、Autolisp（即vlisp）等。同时同一种 lisp 也有不同的编译器，比如对于
  CommonLisp 来说，有 `CMUCL <https://cmucl.org/>`_ 、 `SBCL <http://www.sbcl.org/>`_ 、
  `Lispworks <http://www.lispworks.com/>`_ 、 
  `Ecl <https://common-lisp.net/project/ecl/>`_ 、 `ALLEGROCL <https://franz.com/products/allegrocl/>`_ 、 `CLOZURECL <https://ccl.clozure.com/>`_ 、 `CLISP <http://www.clisp.org/>`_ ,  `ABCL <https://www.abcl.org/>`_ , `MKCL <https://cliki.net/MKCL>`_ 等。这些也就是常说的“ CommonLisp implementation ”。
  翻译成编译器对于读者来说会更一目了然，易于理解。

indefinite extent 
----------------------

不限定范围

.. note::

  indefinite 无限期的; 期限不定的; 模糊不清的; 不明确的。
  extent 程度; 限度; 大小; 面积; 范围。
  indefinite extent 直译可以是“无限范围； 不限定范围； 不明确范围”。翻译成“不限定范围”更贴合语境。

unwanted capture 
----------------------

不想要的捕捉

.. note::

  unwanted capture 可以直译为“不想要的捕捉； 不需要的捕捉； 异常捕捉”（捕捉也可以译为捕获）。
  虽然“异常捕捉”是专业术语。然而对于 CommonLisp 的宏来说， 利用自由变量以及特定的回指并不是“异常的”，
  也不会有报错。翻译成“不想要的捕捉”对于读者来说，脑海中更有意象。

predicate
----------------------

谓词

.. note::

  predicate 有断言、谓词的意思。一般的读者听到谓词会不知所云， 然而稍微检索一下， 就知道在语言文法中， 谓词就是指的
  有判断意思的逻辑词。

transparent specification
----------------------

易读的具体说明

.. note::

  transparent 透明的;清澈的;易识破的;易看穿的;显而易见的;易懂的
  specification 规格;规范;明细单;说明书
  transparent specification 直译为”透明规范”。然而， 原文中这样翻译“易读的具体说明”更合适。  indefinite 无限期的; 期限不定的; 模糊不清的; 不明确的。
  
form 
----------------------

形式体

.. note::

  form 形式；结构；表单。出现在原文中高频的有“let form” 。虽然确实是一个列表。但是翻译成“表单”，总是有点含混不清。翻译成“形式”，
  也是缺点意思。“形式体”就能比较好的指代那部分代码。其他地方的 form 也一样。翻译成“结构”的话，有地方会有歧义。

body 
----------------------

代码主体

.. note::

  body 身体；主体；主要部分。这是个文中出现的高频词汇。如果只是翻译成“主体”，也有点令读者难以理解。
  
fixnum 
----------------------

固定数字

.. note::

  fixnum 有时是一种类型。有时指的计算机内存结构
    
lispy
----------------------

lisp 化

.. note::

  指的是 lisp 风格化的内容
    
duality-of-syntax
----------------------

语法二义性

.. note::

  xxxxxx
    
indirection
----------------------

间接

.. note::

  xxxxxx chapter07 L707

pointer scope
----------------------

指针作用域

.. note::

  xxxxxx chapter07.4 L834

dereference
----------------------

间接引用

.. note::

  xxxxxx chapter07.4 L908
 
  
  
slot 
----------------------

槽

.. note::

  Lisp 中结构体元素被称为“槽”， slot.
 
 anaphor 
----------------------

回指语

.. note::

  Lisp 中结构体元素被称为“槽”， slot.


   
destructuring
----------------------

解构

.. note::

  Lisp 中数据块解析为树结构，并且用符号命名其中部分，从而建立对该部分的引用。
