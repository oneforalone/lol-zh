# 附录 D：Lisp 编辑器

---

Author: Doug Hoyte

Translator: Yuqi Liu <[yuqi.lyle@outlook.com](mailto:yuqi.lyle@outlook.com)>

---


## D.1 emacs


> Editing is a rewording activity
> --Alan Perlis



说之前不可能不冒犯别人，所以我就直言不讳：emacs 很差劲。 尽管 emacs 是个非常灵活且功能强大的编辑器，并具有丰富的历史，但我永远不会使用它。 当然，这是我极少数的意见。 世界上绝大多数 lisp 程序员都爱上了 emacs 及其 lisp 可编程架构。 显然有一个名为 ILISP 的环境，让你使用编辑器中的 REPL、不通的颜色区分 lisp 语法、根据当前包的内容自动补全 lisp 结构，无论如何，我还是坚持我的看法，我永远不会使用它。


Emacs 本身包含一个完整的 lisp 环境，称为 elisp。 Elisp 是一种非词法 lisp，人们在其上构建了大型复杂的 lisp 应用程序。 这些应用程序允许执行所有操作，从自动修改正在处理的源代码到浏览网络和从编辑器中查看电子邮件。 在编辑时执行任意 lisp 代码的能力非常强大，可以改变你写代码方式，但同样，我还是不用它。


这就是我所说的 emacs 陷阱。 在我刚刚描述的各个方面，emacs 听起来都很棒。 太棒了，当聪明的程序员尝试它并发现它不是那么棒时（并没有真正帮助他们的编码），他们不知为了错过了一个明显的结论，即这可能是一个不值得解决的问题，而是开始思考如何追求潜在的令人敬畏的方法。 emacs 陷阱导致许多聪明的 lisp 程序员浪费了无数时间来配置和记忆键的绑定映射，调整语法颜色，以及编写大部分无用的 elisp 脚本。


Emacs 让你思考如何编写程序来为你进行编辑，而不是如何编写程序来为你编写程序。 当正在编辑的代码被冗余编写时，只需要编写编辑过程的脚本。 当完全专注于开发应用程序时，编辑文件的实际过程应该是个透明的、微不足道的过程，其中的机制甚至从未进入思考过程。 Emacs 不是这样的。 Emacs 有一个又一个的插件，一个又一个的玩具，一个又一个的噱头，让程序员无休止地玩弄、困惑和陷入困境。有时我会设想更先进的世界可能已经有了发明 emacs 的 lisp 天才，他们忽略了文本编辑这一非问题，而是将全部注意力集中在真正的编程问题上。


什么是 Blub 风格的集成开发环境？ IDE 通常是大型的、过度设计的 emacs 克隆，甚至不提供 lisp 定制，呈现所有可用的按钮或菜单供你使用，甚至不如 emacs 有用。 与 emacs 相比，大多数 IDE 是个糟糕概念的糟糕实现——冗余语言的冗余编辑器。


emacs 的问题在于它的倒退哲学。 它将编辑视为目的而不是手段。 编辑本身并不是一个有趣的过程； 有趣的是我们编辑的内容。


## D.2 vi


Vi 与 emacs 的编辑器截然相反。 它有一组规范的键绑定，所有 vi 用户都普遍理解其中的子集。 一旦了解了 vi，就可以坐在任何 unix 计算机上，以零知识开销编辑文件。 就像你在家一样。 vi 没有烦人的 emacs 键和键值映射，而是具有强大的模态设计，可透明且高效地执行简单和复杂的文本操作。 Vi 经过精心设计，以最大限度地减少需要执行的击键次数，并且即使在高延迟的网络连接上也能舒适地使用。


考虑到 vi 中的 % 命令可以复制/粘贴/删除/- 即移动 lisp 结构、正则表达式以及灵活的移动和搜索命令，vi 可以降低编辑 lisp 代码的开销。 与 emacs 形成直接对比的是，vi 键和命令背后的含义永远不会改变。 短时间使用后，vi 成为大脑的原始运动技能部分的延伸。 能够在脑海中排列两个、三个、四个或更多编辑命令，并在继续思考应用程序时让手指执行它们，这有点神奇。 与几次击键几乎无法进入某些选项菜单的 IDE 不同，vi 击键高效、强大、透明，而且最重要的是，恒定不变。


在编写 lisp 代码时，我特别喜欢用 Keith Bostic 的 nvi，它是 4BSD 中 vi 的直接继承者。 比其他编辑器更快且响应更快，不会因为无用的细节或颜色分散我的注意力，而且从来没有崩溃过。 我不想再从编辑器那里得到任何东西了。 编辑是 unix 正确而 lisp 错误的少数事情之一——Worse is better。