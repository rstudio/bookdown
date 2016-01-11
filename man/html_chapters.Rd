% Please edit documentation in R/html.R
\name{html_chapters}
\alias{html_chapters}
\title{Build book chapters into separate HTML files}
\usage{
html_chapters(toc = TRUE, number_sections = TRUE, fig_caption = TRUE, lib_dir = "libs", 
    template = bookdown_file("templates/default.html"), ...)
}
\arguments{
\item{toc, number_sections, fig_caption, lib_dir, template}{See
\code{rmarkdown::\link[rmarkdown]{html_document}}.}

\item{...}{Other arguments to be passed to \code{html_document()}.}
}
\description{
Merge individual R Markdown documents (assuming each document is one chapter)
into one main document, render it into HTML, and split it into chapters while
updating relative links (e.g. links in TOC, footnotes, citations,
figure/table cross-references, and so on). This function is expected to be
used in conjunction with \code{\link{render_book}()}. It is almost
meaningless if it is used with \code{rmarkdown::render()}.
}
\note{
If you want to use a different template, the template must contain
  three pairs of HTML comments: \samp{<!--token:title:start-->} and
  \samp{<!--token:title:end-->} to mark the title section of the book (this
  section will be placed only on the first page of the rendered book);
  \samp{<!--token:toc:start-->} and \samp{<!--token:toc:end-->} to mar the
  table of contents section (it will be placed on all chapter pages);
  \samp{<!--token:body:start-->} and \samp{<!--token:body:end-->} to mark the
  HTML body of the book (the HTML body will be split into separate pages for
  chapters). You may open the default HTML template
  (\code{bookdown:::bookdown_file('templates/default.html')}) to see where
  these comments were inserted.
}
