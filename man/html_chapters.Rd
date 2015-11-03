% Please edit documentation in R/html.R
\name{build_html_chapters}
\alias{build_html_chapters}
\title{Build book chapters into separate HTML files}
\usage{
build_html_chapters(main = merge_rmd(), toc = TRUE, number_sections = TRUE, 
    lib_dir = "libs")
}
\arguments{
\item{main}{The main R Markdown document (by default, a document that is the
combination of all R Markdown documents under the current working
directory).}

\item{toc, number_sections, lib_dir}{See
\code{rmarkdown::\link[rmarkdown]{html_document}}.}
}
\description{
Merge individual R Markdown documents (assuming each document is one chapter)
into one main document, render it into HTML, and split it into chapters while
updating relative links (e.g. links in TOC, footnotes, citations,
figure/table cross-references, and so on).
}
