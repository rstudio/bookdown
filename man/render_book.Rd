% Please edit documentation in R/render.R
\name{render_book}
\alias{render_book}
\title{Render multiple R Markdown documents into a book}
\usage{
render_book(input, output_format = NULL, ..., clean = TRUE, envir = parent.frame(), 
    output_dir = NULL, new_session = FALSE, force_knit = FALSE)
}
\arguments{
\item{input}{Ignored. All R Markdown files under the current working
directory are merged as the actual input to
\code{rmarkdown::\link[rmarkdown]{render}()}.}

\item{output_format, ..., clean, envir}{Arguments to be passed to
\code{render()}.}

\item{output_dir}{The output directory. If not specified, a field named
\code{output_dir} in the configuration file \file{_config.yml} will be used
(possiblely not specified, either). If not \code{NULL}, the output files
will be moved to this directory.}

\item{new_session}{Whether to use new R sessions to compile individual Rmd
files.}

\item{force_knit}{Whether to force knitting all Rmd files.}
}
\description{
A wrapper function to render all R Markdown files under the current working
directory into a book. It can be used in the RStudio IDE (specifically, the
\code{knit} field in YAML).
}
\details{
There are two ways to render a book from Rmd files. The default way
(\code{new_session = FALSE}) is to merge Rmd files into a single file and
render this file. You can also choose to render each individual Rmd file in a
new R session (\code{new_session = TRUE}). In this case, Rmd files that have
not been updated from the previous run will not be recompiled the next time
by default, and you can force compiling them by \code{force_knit = TRUE}.
}
