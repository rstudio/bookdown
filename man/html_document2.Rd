% Please edit documentation in R/html.R, R/latex.R
\name{html_document2}
\alias{html_document2}
\alias{pdf_document2}
\alias{tufte_book2}
\alias{tufte_handout2}
\alias{tufte_html2}
\title{Output formats that allow numbering and cross-referencing figures/tables}
\usage{
html_document2(..., number_sections = TRUE)

tufte_html2(..., number_sections = FALSE)

pdf_document2(...)

tufte_handout2(...)

tufte_book2(...)
}
\arguments{
\item{...}{Arguments to be passed to a specific output format function. For a
function \code{foo2()}, its arguments are passed to \code{foo()}, e.g.
\code{...} of \code{html_document2()} are passed to \code{html_document()}.}

\item{number_sections}{Whether to number section headers: if \code{TRUE},
figure/table numbers will be of the form \code{X.i}, where \code{X} is the
current first-level section number, and \code{i} is an incremental number
(the i-th figure/table); if \code{FALSE}, figures/tables will be numbered
sequentially in the document from 1, 2, ..., and you cannot cross-reference
section headers in this case.}
}
\value{
An R Markdown output format object to be passed to
  \code{rmarkdown::\link{render}()}.
}
\description{
These are simple wrappers of the output format functions like
\code{rmarkdown::\link{html_document}()}, and they added the capability of
numbering figures/tables and cross-referencing them. See References for the
syntax. Note you can also cross-reference sections by their ID's using the
same syntax as figures/tables.
}
\note{
These function are expected to work with a single R Markdown document
  instead of multiple documents of a book, so they are to be passed to
  \code{rmarkdown::render()} instead of \code{bookdown::render_book()}. The
  functions \samp{tufte_*()} are wrappers of funtions in the \pkg{tufte}
  package.
}
\references{
\url{http://rstudio.github.io/bookdown/figures.html}
}
