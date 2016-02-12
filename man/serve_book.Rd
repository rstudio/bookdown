% Please edit documentation in R/utils.R
\name{serve_book}
\alias{serve_book}
\title{Continously preview the HTML output of a book}
\usage{
serve_book(dir = ".", output_dir = NULL, ...)
}
\arguments{
\item{dir}{The root directory of the book (containing the Rmd source files).}

\item{output_dir}{The directory for output files; see
\code{\link{render_book}()}.}

\item{...}{Other arguments passed to \code{servr::\link[servr]{httw}()} (not
including the \code{handler} argument, which has been set internally).}
}
\description{
When any files are modified or added to the book directory, the book will be
automatically recompiled, and the current HTML page in the browser will be
refreshed. This function is based on \code{servr::\link[servr]{httw}()} to
continuously watch a directory.
}
