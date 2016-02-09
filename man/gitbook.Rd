% Please edit documentation in R/gitbook.R
\name{gitbook}
\alias{gitbook}
\title{The GitBook output format}
\usage{
gitbook(fig_caption = TRUE, lib_dir = "libs", ..., use_rmd_names = FALSE, 
    split_level = 2)
}
\arguments{
\item{fig_caption, lib_dir, ...}{Arguments to be passed to
\code{rmarkdown::\link{html_document}()} (\code{...} not including
\code{toc}, \code{number_sections}, \code{self_contained}, \code{theme},
and \code{template}).}

\item{use_rmd_names}{Whether to use the base filenames of the input Rmd files
to create the HTML filenames, e.g. generate \file{chapter1.html} for
\file{chapter1.html}. Note this argument only works when this output format
is used by \code{\link{render_book}()}.}

\item{split_level}{When \code{use_rmd_names = FALSE}, the level by which the
HTML output file is split. \code{0} means do not split the file; \code{1}
means split the file by the first level headers; \code{2} means the second
level headers. The HTML filenames will be determined by the header numbers
and ID's, e.g. the filename for the first chapter with a chapter title
\code{# Introduction} will be \file{1-introduction.html} by default.}
}
\description{
This output format function ported a style provided by GitBook
(\url{https://www.gitbook.com}) for R Markdown.
}
