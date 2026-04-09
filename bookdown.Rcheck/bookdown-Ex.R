pkgname <- "bookdown"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('bookdown')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("render_book")
### * render_book

flush(stderr()); flush(stdout())

### Name: render_book
### Title: Render multiple R Markdown documents into a book
### Aliases: render_book preview_chapter

### ** Examples

# see https://yihui.org/bookdown for the full documentation
if (file.exists("index.Rmd")) bookdown::render_book("index.Rmd")
## Not run: 
##D # will use the default format defined in index.Rmd or _output.yml
##D bookdown::render_book("index.Rmd")
##D # will use the options for format defined in YAML metadata
##D bookdown::render_book("index.Rmd", "bookdown::pdf_book")
##D # If you pass an output format object, it must have all the options set
##D bookdown::render_book("index.Rmd", bookdown::pdf_book(toc = FALSE))
##D 
##D # will render the book in the current directory
##D bookdown::render_book()
##D # this is equivalent to
##D bookdown::render_book("index.Rmd")
##D # will render the book living in the specified directory
##D bookdown::render_book("my_book_project")
## End(Not run)



cleanEx()
nameEx("resolve_refs_html")
### * resolve_refs_html

flush(stderr()); flush(stdout())

### Name: resolve_refs_html
### Title: Resolve figure/table/section references in HTML
### Aliases: resolve_refs_html
### Keywords: internal

### ** Examples

library(bookdown)
resolve_refs_html(c("<caption>(#tab:foo) A nice table.</caption>",
    "<p>See Table @ref(tab:foo).</p>"), global = TRUE)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
