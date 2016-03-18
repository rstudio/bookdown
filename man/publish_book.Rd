% Please edit documentation in R/publish.R
\name{publish_book}
\alias{publish_book}
\title{Publish a book to the web}
\usage{
publish_book(name = NULL, account = NULL, server = NULL, 
    render = getOption("bookdown.render_on_publish", FALSE))
}
\arguments{
\item{name}{Name of the book (this will be used in the URL path of the
published book). Defaults to the \code{book_filename} in
\code{_bookdown.yml} if not specified.}

\item{account}{Account name to publish to. Will default to any previously
published to account or any single account already associated with
\code{server}.}

\item{server}{Server to publish to (by default beta.rstudioconnect.com but
any RStudio Connect server can be published to).}

\item{render}{\code{TRUE} to render all formats prior to publishing (defaults
to \code{FALSE}, however, this can be modified via the
\code{bookdown.render_on_publish} option). Note that this requires the use
of either an R script \file{_render.R} (or a \file{Makefile} if
\file{_render.R} is not found) to provide the implementaiton of rendering
all formats that you want to publish.}
}
\description{
Publish a book to the web. Note that you should be sure to render all
versions of the book before publishing, unless you have specified
\code{render = TRUE}.
}
