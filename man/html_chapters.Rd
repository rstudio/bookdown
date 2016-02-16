% Please edit documentation in R/html.R
\name{html_chapters}
\alias{html_chapters}
\title{Build book chapters into separate HTML files}
\usage{
html_chapters(toc = TRUE, number_sections = TRUE, fig_caption = TRUE, 
    lib_dir = "libs", template = bookdown_file("templates/default.html"), 
    ..., base_format = rmarkdown::html_document, page_builder = build_chapter, 
    split_by = c("section+number", "section", "chapter+number", "chapter", 
        "rmd", "none"))
}
\arguments{
\item{toc, number_sections, fig_caption, lib_dir, template}{See
\code{rmarkdown::\link{html_document}}, or the documentation of the
\code{base_format} function.}

\item{...}{Other arguments to be passed to \code{base_format}.}

\item{base_format}{An output format function to be used as the base format.}

\item{page_builder}{A function to combine different parts of a chapter into a
page (an HTML character vector). See \code{\link{build_chapter}} for the
specification of this function.}

\item{split_by}{How to name the HTML output files from the book: \code{rmd}
uses the base filenames of the input Rmd files to create the HTML
filenames, e.g. generate \file{chapter1.html} for \file{chapter1.Rmd};
\code{none} means do not split the HTML file (the book will be a single
HTML file); \code{chapter} means split the file by the first-level headers;
\code{section} means the second-level headers. For \code{chapter} and
\code{section}, the HTML filenames will be determined by the header ID's,
e.g. the filename for the first chapter with a chapter title \code{#
Introduction} will be \file{introduction.html}; for \code{chapter+number}
and \code{section+number}, the chapter/section numbers will be prepended to
the HTML filenames, e.g. \file{1-introduction.html} and
\file{2-1-literature.html}.}
}
\description{
Split the HTML output into chapters while updating relative links (e.g. links
in TOC, footnotes, citations, figure/table cross-references, and so on). This
function is expected to be used in conjunction with
\code{\link{render_book}()}. It is almost meaningless if it is used with
\code{rmarkdown::render()}.
}
\note{
If you want to use a different template, the template must contain
  three pairs of HTML comments: \samp{<!--bookdown:title:start-->} and
  \samp{<!--bookdown:title:end-->} to mark the title section of the book
  (this section will be placed only on the first page of the rendered book);
  \samp{<!--bookdown:toc:start-->} and \samp{<!--bookdown:toc:end-->} to mar
  the table of contents section (it will be placed on all chapter pages);
  \samp{<!--bookdown:body:start-->} and \samp{<!--bookdown:body:end-->} to
  mark the HTML body of the book (the HTML body will be split into separate
  pages for chapters). You may open the default HTML template
  (\code{bookdown:::bookdown_file('templates/default.html')}) to see where
  these comments were inserted.
}
