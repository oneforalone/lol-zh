# 附录 C：解释器/编译器

---

Author: Doug Hoyte

Translator: Yuqi Liu <[yuqi.lyle@outlook.com](mailto:yuqi.lyle@outlook.com)>

---

## C.1 CMUCL/SBCL


CMUCL/SBCL 系统的历史实际上要早于 Common Lisp。 SBCL 是 CMUCL 的 fork。 在我看来，CMUCL 是目前可用的最好的 lisp 环境——SBCL 紧随其后。 CMUCL 和 SBCL 都提供了卓越的开发和生产环境。 这两种实现都包括了不起的 Python2 优化机器代码编译器。 经过相当简单的声明过程后，使用 Python 编译的 Common Lisp 代码将具有与所有其他语言的编译器相媲美或超过编译器的性能，尤其是在用于大型和复杂的应用程序时。 CMUCL 和 SBCL 之间最大的区别在于 SBCL 强制使用 Python 来处理所有事情，但 CMUCL 提供了几种编译器和解释器，每种编译器和解释器都有自己的优点和缺点。


本书中的大多数示例和输出都来自 CMUCL，虽然有时会用 SBCL 是因为其反汇编器输出更好。 CMUCL 有更少的烦人、多余的警告和更舒适的 REPL，但 SBCL 可以有更清晰的注释并且更适合某些 Unicode 任务。 SBCL 还做出了一些我不认同的设计决策，例如破坏 CMUCL 的 top-level `setq`功能，对系统符号使用不太明显的名称，以及毫无意义地复制 unix 的 `getopt(3)`。


CMUCL 和 SBCL 享有丰富的库和充满活力的在线用户社区。 听到有人抱怨库的可用性是 lisp 的一个问题，我感到很奇怪。 我觉得恰恰相反。 与其他语言相比，Common Lisp 库通常有更好的支持和更高的质量。 CMUCL/SBCL 的外部函数接口也非常出色——比我遇到的任何其他语言都更灵活、更稳定、更高效。 在 Common Lisp 中开发某些东西时，库来说从来都不是障碍。


## C.2 CLISP


CLISP 是另一个优秀的 Common Lisp 环境。 它比 CMUCL/SBCL 更便携，几乎可以在 C 编译器可以编译到的任何地方运行。 由于 CLISP 不编译为机器码，而是编译为字节码格式，因此它通常比 CMUCL/SBCL 慢，有时在部署时避免使用。 但如果需要可移植性，部署到 CLISP 有时是最明智的选择。 CLISP 还支持快速的 bignum。


## C.3 Others


ECL 是用 C 语言编写的 Common Lisp 实现，用在嵌入式中。 它所占用的空间大约是 CLISP 的三分之一，但仍然相当完整。 对于内存极度受限的机器来说，这可能是一个很好的部署环境。 得益于 GMP 库，它还具有良好的 bignum 性能。


GCL 是一个 Common Lisp 实现，它使用 GNU C 编译器通过 C 中介将 lisp 编译为本机机器代码。 它基于有影响力的 Kyoto CL 系统[KYOTO-CL-REPORT]。 通常不如 CMUCL/SBCL 或 CLISP 好，但仍具有研究价值。


Armed Bear 是运行在 Java 虚拟机之上的 Common Lisp 实现。 它对于与现有的 Java 应用程序集成可能很有用。


Perl 的 Parrot 虚拟机有一个 Common Lisp 前端，我也一直在关注它。


Clozure CL（以前称为 OpenMCL）是 Common Lisp 的开源实现，只要它不在专有操作系统上运行，应该可以正常工作。


Common Lisp 也有许多专有的实现。 其中一些质量很高，但都存在严重缺陷——阻止你做事或了解事情的非技术障碍。 考虑到开源 lisps 的质量和数量，如今通常没有必要将自己锁定在昂贵的、无源代码的实现或受限的免费试用中（可能是在 cue LispWorks）。


当今优秀的开源 Common Lisp 实现与优秀的免费操作系统（如 OpenBSD 和 GNU/Linux）相结合，正在带来计算的黄金时代。 如今，除了自身的智力和创造力的障碍之外，没有什么能阻止程序员。
