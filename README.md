# Let Over Lambda 中文翻译项目

[![Auto build and deploy to Github Pages](https://github.com/oneforalone/lol-zh/actions/workflows/build-and-deploy.yml/badge.svg)](https://github.com/oneforalone/lol-zh/actions/workflows/build-and-deploy.yml)

本项目文档由 Sphinx 引擎生成。原文链接文：https://letoverlambda.com。

源文件在 docsrc 目录下，编辑完源文件后执行 `make github` ，然后在提交到 Github 上。

目前已完成初步翻译，以下是后续需要做的事情：

- 添加文章中的代码文件
- 添加原文中的脚注及参考引用
- 校对

## Setups

- Git: https://git-scm.com
- Make: https://www.gnu.org/software/make/
- Python3: https://www.python.org/downloads/
- Sphinx: https://www.sphinx-doc.org/en/master/

我已经很久没有使用 windows 了，所以 windows 的话自行解决 Make 的问题，其中 make 命令可能在
windows 上失败，一个简单的解决办法是使用 git bash（安装 git 时会安装上）。Python3 也自行到官网
下载对应的安装包进行安装。

顺便记录一下 markdown 和 reStructured 语法的参考文档：

- markdown：https://www.markdown.xyz/basic-syntax/
- reStructured: https://docs.readthedocs.io/en/stable/index.html

**UPDATE from Sep 1, 2022**：改用 `venv` 来保持项目环境的统一，Python 的版本有点烦。
同时添加了 Github Actioon 的 CI/CD，每次 push 后会自动生成 html 并部署成 Github Pages。
简化操作，每次只需要编辑源文件然后提交修改然后再 push 到 repo 上就可以了。

下载安装好 Git，Make，Python3 后，然后执行以下命令安装 Sphinx 即其插件：

```sh
git clone https://github.com/oneforalone/lol-zh.git
cd lol-zh/
python3 -m venv .
source ./bin/activate
python3 -m pip install -r requirements.txt
```

## Local Preview

将代码 clone 下来后，可以进行本地预览。在预览前首先需要激活 venv
环境：

```sh
source ./bin/activate
```

* reStructured 格式

```sh
cd lol-zh/docsrc
sphinx-autobuild source _build/html
```

* Markdown 格式

```sh
cd lol-zh/docsrc
sphinx-autobuild markdown _build/html
```
