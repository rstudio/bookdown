# bookdown <a href="https://pkgs.rstudio.com/bookdown/"><img src="man/figures/logo.png" align="right" height="138"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/bookdown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/bookdown/actions/workflows/R-CMD-check.yaml) [![CRAN release](https://www.r-pkg.org/badges/version/bookdown)](https://CRAN.R-project.org/package=bookdown) [![Codecov test coverage](https://codecov.io/gh/rstudio/bookdown/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rstudio/bookdown?branch=main)

<!-- badges: end -->

A open-source (GPL-3) R package to facilitate writing books and long-form articles/reports with R Markdown. Features include:

-   Generate printer-ready books and ebooks from R Markdown documents
-   A markup language easier to learn than LaTeX, and to write elements such as section headers, lists, quotes, figures, tables, and citations
-   Multiple choices of output formats: PDF, LaTeX, HTML, EPUB, and Word.
-   Possibility of including dynamic graphics and interactive applications (HTML widgets and Shiny apps)
-   Support for languages other than R, including C/C++, Python, and SQL, etc.
-   LaTeX equations, theorems, and proofs work for all output formats
-   Can be published to GitHub, bookdown.org, and any web servers
-   Integrated with the RStudio IDE
-   One-click publishing to <https://bookdown.org>

## Book

<a href="https://bookdown.org/yihui/bookdown/"><img src="https://bookdown.org/yihui/bookdown/images/cover.jpg" alt="bookdown: Authoring Books and Technical Documents with R Markdown" class="book" height="400"/></a>

## Installation

You can install the package from CRAN as follows:

``` r
install.packages("bookdown")
```

If you want to use the development version of the **bookdown** package, you can install the package from GitHub via the [**pak** package](https://pak.r-lib.org):

``` r
# install.packages("pak")
pak::pak('rstudio/bookdown')
```

## Usage

The easiest way to start a new Bookdown project is from within RStudio IDE. Go to *File \> New Project \> New Directory \> Book project using bookdown*.

This will create a new directory with an example book as template. You can build the HTML version of this example book without doing any modification:

-   Go into the Build Pane in the RStudio IDE
-   Click on *Build Book \> bookdown::gitbook*

You can also run `bookdown::render_book()` in the R console.

Learn more about using bookdown in the [Getting started section](https://pkgs.rstudio.com/bookdown/articles/bookdown.html).

## Getting help

There are two main places to get help:

1.  The [RStudio community](https://community.rstudio.com/tags/c/R-Markdown/10/bookdown) is a friendly place to ask any questions about **bookdown**. Be sure to use the `bookdown` tag.

2.  [Stack Overflow](https://stackoverflow.com/questions/tagged/bookdown) is a great source of answers to common **bookdown** questions. Use the tags [`[r][bookdown]`](https://stackoverflow.com/questions/tagged/bookdown+r) if you ask a question.

## Code of Conduct

Please note that the bookdown project is released with a [Contributor Code of Conduct](https://pkgs.rstudio.com/bookdown/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
