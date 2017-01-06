% Please edit documentation in R/ebook.R
\name{calibre}
\alias{calibre}
\title{A wrapper function to convert e-books using Calibre}
\usage{
calibre(input, output, options = "")
}
\arguments{
\item{input}{The input filename.}

\item{output}{The output filename or extension (if only an extension is
provided, the output filename will be the input filename with its extension
replaced by \code{output}; for example, \code{calibre('foo.epub', 'mobi')}
generates \file{foo.mobi}).}

\item{options}{A character vector of additional options to be passed to
\command{ebook-convert}.}
}
\value{
The output filename.
}
\description{
This function calls the command \command{ebook-convert} in Calibre
(\url{http://calibre-ebook.com}) to convert e-books.
}
