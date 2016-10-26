% Please edit documentation in R/latex.R
\name{pdf_book}
\alias{pdf_book}
\title{Convert R Markdown to a PDF book}
\usage{
pdf_book(toc = TRUE, number_sections = TRUE, fig_caption = TRUE, ..., 
    base_format = rmarkdown::pdf_document, toc_unnumbered = TRUE, toc_appendix = FALSE, 
    toc_bib = FALSE, quote_footer = NULL, highlight_bw = FALSE)
}
\arguments{
\item{toc, number_sections, fig_caption}{See
\code{rmarkdown::\link{pdf_document}}, or the documentation of the
\code{base_format} function.}

\item{...}{Other arguments to be passed to \code{base_format}.}

\item{base_format}{An output format function to be used as the base format.}

\item{toc_unnumbered}{Whether to add unnumberred headers to the table of
contents.}

\item{toc_appendix}{Whether to add the appendix to the table of contents.}

\item{toc_bib}{Whether to add the bibliography section to the table of
contents.}

\item{quote_footer}{If a character vector of length 2 and the quote footer
starts with three dashes (\samp{---}), \code{quote_footer[1]} will be
prepended to the footer, and \code{quote_footer[2]} will be appended; if
\code{NULL}, the quote footer will not be processed.}

\item{highlight_bw}{Whether to convert colors for syntax highlighting to
black-and-white (grayscale).}
}
\description{
Convert R Markdown files to PDF after resolving the special tokens of
\pkg{bookdown} (e.g., the tokens for references and labels) to native LaTeX
commands.
}
\details{
This function is based on \code{rmarkdown::\link{pdf_document}} (by default)
with better default arguments. You can also change the default format to
other LaTeX/PDF format functions using the \code{base_format} argument.
}
