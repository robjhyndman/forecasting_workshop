SOURCES=$(shell find . -name *.Rmd)
SOURCES := $(wildcard *.Rmd)
TARGETS=$(SOURCES:%.Rmd=%.pdf)

%.pdf: %.Rmd
	@echo "$< -> $@"
	@Rscript -e "rmarkdown::render('$<')"

default: $(TARGETS) README.md

README.md: README.qmd
	quarto render README.qmd

clean:
	rm -rf $(TARGETS)
	rm -rf *_cache
	rm -rf *_files
