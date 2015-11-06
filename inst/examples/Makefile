RMD_FILES := $(wildcard *.Rmd)
HTML_FILES := $(patsubst %.Rmd, %.html, $(RMD_FILES))

all: $(HTML_FILES)

%.html: %.Rmd
	Rscript -e "bookdown::render_book('$<', 'bookdown::html_chapters')"

pdf: $(wildcard *.pdf)

%.pdf: %.Rmd
	Rscript -e "bookdown::render_book('$<', 'bookdown::pdf_book')"
