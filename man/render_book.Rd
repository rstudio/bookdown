% Please edit documentation in R/render.R
\name{render_book}
\alias{render_book}
\title{Render multiple R Markdown documents into a single output file}
\usage{
render_book(input, ...)
}
\arguments{
\item{input}{Ignored. All R Markdown files under the current working
directory are merged as the actual input to
\code{rmarkdown::\link[rmarkdown]{render}()}.}

\item{...}{Other arguments to be passed to \code{render()}, such as
\code{encoding}.}
}
\description{
A wrapper function to merge all R Markdown files under the current working
directory, and render the merged R Markdown file. It was mainly designed to
be used in the RStudio IDE (specifically, the \code{knit} field in YAML).
}
\note{
The R Markdown files that start with an underscore \code{_} are ignored
  when merging all \file{.Rmd }files.
}
