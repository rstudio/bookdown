% Please edit documentation in R/html.R
\name{build_chapter}
\alias{build_chapter}
\title{Combine different parts of an HTML page}
\usage{
build_chapter(head, toc, chapter, link_prev, link_next, rmd_cur, html_cur, foot)
}
\arguments{
\item{head}{A character vector of the HTML code before the document title.}

\item{toc}{A character vector of the table of contents.}

\item{chapter}{The body of a chapter.}

\item{link_prev, link_next}{The URL of the previous/next chapter (may be
\code{NULL}).}

\item{rmd_cur}{The Rmd filename of the current chapter (may be \code{NULL}).}

\item{html_cur}{The HTML filename of the current chapter (may be
\code{NULL}).}

\item{foot}{A character vector of the HTML code after the chapter body.}
}
\description{
Given the HTML header, body, and footer, etc, build an HTML page.
}
\details{
This function is for expert use only. The \code{head} and \code{foot}
arguments may not be strictly the HTML header and footer. It depends on the
HTML comment tokens in the template (see \code{\link{html_chapters}}).
}
