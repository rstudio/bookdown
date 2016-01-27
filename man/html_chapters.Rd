% Please edit documentation in R/html.R
\name{html_chapters}
\alias{html_chapters}
\title{Build book chapters into separate HTML files}
\usage{
html_chapters(toc = TRUE, number_sections = TRUE, fig_caption = TRUE, 
    lib_dir = "libs", template = bookdown_file("templates/default.html"), 
    ..., base_format = rmarkdown::html_document, page_builder = build_chapter, 
    use_rmd_names = TRUE, split_level = 1)
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
