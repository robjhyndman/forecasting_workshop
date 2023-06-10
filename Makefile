SOURCES := $(wildcard *.qmd)
TARGETS=$(SOURCES:%.qmd=%.pdf)

%.pdf: %.qmd
	@echo "$< -> $@"
	quarto render '$<'

all: $(TARGETS)

clean:
	rm -rf $(TARGETS)
	rm -rf *_cache
	rm -rf *_files
