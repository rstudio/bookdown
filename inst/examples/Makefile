RMD_FILES := $(filter-out _%, $(wildcard *.Rmd))

gitbook: $(RMD_FILES)
	Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"

all: $(RMD_FILES)
	Rscript build-all.R
