# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = source
BUILDDIR      = _build
MARKDOWNDIR   = markdown


# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

# for github pages
github:
	@rm -rf ../docs/*
	@rm -rf _build/html
	@make html
	@cp -a _build/html/. ../docs

# for Markdown format
md:
	@$(SPHINXBUILD) -M html "$(MARKDOWNDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(0)

# build markdown format for github
md-github:
	@rm -rf ../docs/*
	@rm -rf _build/html
	@make md
	@cp -a _build/html/. ../docs