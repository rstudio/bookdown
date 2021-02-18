# bookdown <img src='man/figures/logo.png' align="right" height="139" />


<!-- badges: start -->
[![R-CMD-check](https://github.com/rstudio/bookdown/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/bookdown/actions)
[![CRAN release](https://www.r-pkg.org/badges/version/bookdown)](https://CRAN.R-project.org/package=bookdown)
[![Codecov test coverage](https://codecov.io/gh/rstudio/bookdown/branch/master/graph/badge.svg)](https://codecov.io/gh/rstudio/bookdown?branch=master)
<!-- badges: end -->


A open-source (GPL-3) R package to facilitate writing books and long-form articles/reports with R Markdown. Features include:

- Generate printer-ready books and ebooks from R Markdown documents
- A markup language easier to learn than LaTeX, and to write elements such as section headers, lists, quotes, figures, tables, and citations
- Multiple choices of output formats: PDF, LaTeX, HTML, EPUB, and Word.
- Possibility of including dynamic graphics and interactive applications (HTML widgets and Shiny apps)
- Support for languages other than R, including C/C++, Python, and SQL, etc.
- LaTeX equations, theorems, and proofs work for all output formats
- Can be published to GitHub, bookdown.org, and any web servers
- Integrated with the RStudio IDE
- One-click publishing to <https://bookdown.org>

## Book

<a href="https://bookdown.org/yihui/bookdown/"><img src="https://bookdown.org/yihui/bookdown/images/cover.jpg" height="400"></a>

## Installation

You can install the package from CRAN as follows:

```r
install.packages("bookdown")
```

If you want to use the development version of the **bookdown** package, you can install the package from GitHub via the [**remotes** package](https://remotes.r-lib.org):

```r
remotes::install_github('rstudio/bookdown')
```

## Usage

The easiest way to start a new Bookdown project is from within RStudio IDE. Go to _File > New Project > New Directory > Book project using bookdown_.

This will create a new directory with an example book as template. You can build the HTML version of this example book without doing any modification: 

* Go into the Build Pane in the RStudio IDE
* Click on _Build Book > bookdown::gitbook_

You can also run `bookdown::render_book()` in the R console.

See more in the "Get Started" section of <https://bookdown.org/home/about/> to know how to get started with writing a book, including without RStudio IDE.

See also <https://bookdown.org> for more information and featured books as examples.

## Getting help

There are two main places to get help:

1. The [RStudio community](https://community.rstudio.com/c/R-Markdown/10) is a friendly place to ask any questions about **rmarkdown** and the R Markdown family of packages. You can use the tag _bookdown_ with your question

1. [Stack Overflow](https://stackoverflow.com/questions/tagged/r-markdown) is a great source of answers to common **rmarkdown** questions. It is also a great place to get help, on specific package from the R Markdown family. You can use the tags [`[r][bookdown]`](https://stackoverflow.com/questions/tagged/bookdown+r)
