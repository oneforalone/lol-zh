.. lol-zh documentation master file, created by
   sphinx-quickstart on Sun Sep  5 21:23:02 2021.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _lol_zh:

Let Over Lambda 中文文档
==================================

.. toctree::
   :maxdepth: 2

   Chapter01/index.rst
   Chapter02/index.rst
   Chapter03/index.rst
   Chapter04/index.rst
   Chapter05/index.rst
   Chapter06/index.rst
   Chapter07/index.rst
   Chapter08/index.rst
   Appendices/index.rst


如何加入
==================================

本文档使用 `Sphinx <http://www.sphinx-doc.org/>`_ 将 ``docsrc/_sources`` 下的 `reStructuredText <http://docutils.sourceforge.net/rst.html>`_ 文件生成的文档。当然，为了照顾免去大部分人学习 ``reStructuredText`` 的格式，同时也支持使用 `markdown` 格式。

预备知识
-----------------------------------

首先，``Sphinx`` 是个 Python 三方库，所以，你需要懂得 Python 的基础知识。然后因为这个文档是 `Github <https://github.com/>`_ 上，所以你需要有 `Git <https://git-scm.com/>`_ 和 ``Github`` 的基础知识。

.. note::

  这些预备知识其实很基础，所谓的 Python 的基础知识指代的是：python 的安装、配置系统环境变量、pip 的使用。而对于 ``git`` 和 ``github`` 的基础知识，无非就是：``git clone``、``git add``、``git commit``、``git push`` 以及在 Github 上 `克隆库 (fork) <https://guides.github.com/activities/forking/#fork>`_ 和发起 `pull request <https://guides.github.com/activities/forking/#making-a-pull-request>`_ 。

所以你需要现在系统上安装好 ``Git``、``Make`` 和 ``Python3``


Sphinx 安装
-----------------------------------

本人的使用 ``Python3.9`` 进行开发的。为了避免后续环境版本不统一，推荐使用 python3.9，同时使用 ``virtualenv-3`` 安装Sphinx::

  $ python3 -m pip install virtualenv
  $ python3 -m virtualenv sphinx
  $ source sphinx/bin/activate
  (sphinx) $ pip install -U Sphinx sphinx-autobuild sphinx_rtd_theme recommonmark

.. note::

  Windows 下无法执行 source 命令，需要用以下步骤进入虚拟环境::

    $ cd sphinx/Scripts
    $ activate.bat


获取源代码
-----------------------------------

在 Github 上先 fork `代码库 <https://github.com/oneforalone/lol-zh/>`_ 到自己的 Github 上，然后将 Github 上的库 clone 到本地::

  (sphinx)$ git clone https://github.com/<your-github-id>/lol-zh.git
  (sphinx)$ cd lol-zh/docsrc

在对应的章节目录下创建对应的章节文档，如第一章第一节::

  (sphinx)$ cd lol-zh/docsrc/_sources
  (sphinx)$ touch Chapter01/macros.rst

.. note::

  如果是 markdown 格式的则创建 .md 为后缀的文件。其中英文文档为 ``lol-zh/ori/lol.pdf``，同时，我使用爬虫爬过有道翻译，结果保存在 ``lol-zh/text`` 目录下。

编译完后进行本地预览::

  (sphinx)$ sphinx-autobuild _source _build/html

其中 ``_source``、 ``_build/html`` 均在 ``lol-zh/docsrc`` 目录下，所以以上命令是在 ``lol-zh/docsrc`` 目录下执行的。确认没问题后执行::

  (sphinx) $ make github


之后就是 ``git add <添加的文件>``、``git commit -m "<添加的文件>"``、``git push``


todolist

* Chapter01 翻译
* Chapter02 翻译
* Chapter03 翻译
* Chapter04 翻译
* Chapter05 翻译
* Chapter06 翻译
* Chapter07 翻译
* Chapter08 翻译
* Appendices 翻译
