% Please edit documentation in R/latex.R
\name{pdf_book}
\alias{pdf_book}
\title{Convert R Markdown to a PDF book}
\usage{
pdf_book(toc = TRUE, number_sections = TRUE, fig_caption = TRUE, ...)
}
\arguments{
\item{toc, number_sections, fig_caption}{See
\code{rmarkdown::\link[rmarkdown]{pdf_document}}.}

\item{...}{Other arguments to be passed to \code{pdf_document()}.}
}
\description{
Convert R Markdown files to PDF while resolving the special tokens of
\pkg{bookdown} (e.g., the tokens for references and labels) to native LaTeX
commands. This function is based on
\code{rmarkdown::\link[rmarkdown]{pdf_document}} with better default argument
valuess for books.
}
