% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bs4_book.R
\name{bs4_book}
\alias{bs4_book}
\alias{bs4_book_theme}
\title{HTML book built with bootstrap4}
\usage{
bs4_book(
  theme = bs4_book_theme(),
  repo = NULL,
  ...,
  lib_dir = "libs",
  pandoc_args = NULL,
  extra_dependencies = NULL
)

bs4_book_theme(primary = "#0068D9", ...)
}
\arguments{
\item{theme}{A named list or \code{\link[bslib:bs_theme]{bslib::bs_theme()}} object.
The default, \code{bs4_book_theme()}, resets the base font size to 1rem to
make reading easier and uses a primary colour with greater constrast
against the background.}

\item{repo}{Either link to repository where book is hosted, used to generate
view source and edit buttons or a list with repository \code{base} link, default
\code{branch}, \code{subdir} and \code{icon} (see "Specifying the repository").}

\item{lib_dir, pandoc_args, extra_dependencies, ...}{Passed on to
\code{\link[rmarkdown:html_document]{rmarkdown::html_document()}}.}

\item{primary}{Primary colour: used for links and background of footer.}
}
\description{
This output format is built with \href{https://getbootstrap.com}{bootstrap},
using carefully crafted features to provide a clean reading experience whether
you are on a phone, tablet, or desktop.

Some of the main features:
\itemize{
\item Easy customisation of colours and fonts with
\href{https://rstudio.github.io/bslib/}{bslib}
\item Built-in search (broken down by section) that helps readers quickly find what
they are looking for.
\item A sidebar containing a within-chapter table of contents that makes
navigation easy and helps provide context about your current position
within the chapter.
\item Thoughtful typography to make the contents as easy as possible to read,
regardless of the size of your device. A sticky header gets out of your
way when reading, but is easily accessible if you need it.
\item In-line footnotes mean you can read asides next to the next they refer
to. This theme is best paired with a reference style that generates
footnotes.
\item R syntax highlighting and autolinking by
\href{https://downlit.r-lib.org/}{downlit} is paired with a accessible
colour scheme designed by Alison Hill.
}

This theme is designed for books that use one chapter per page.
}
\section{Specifying the repository}{


If your book has a default branch called main you can use\if{html}{\out{<div class="yaml">}}\preformatted{bookdown::bs4_book:
  repo:
    base: https://github.com/hadley/ggplot2-book
    branch: main
}\if{html}{\out{</div>}}

If your book is furthermore located in a subdirectory called "book" you can use\if{html}{\out{<div class="yaml">}}\preformatted{bookdown::bs4_book:
  repo:
    base: https://github.com/hadley/ggplot2-book
    branch: main
    subdir: book
}\if{html}{\out{</div>}}

By default if the repo URL contains "github" it will get a GitHub font-awesome
icon, and otherwise a GitLab font-awesome icon.
To use another icon, specify it with the correct prefix (\code{fas}, \code{fab}, ...) (Font Awesome 5).\if{html}{\out{<div class="yaml">}}\preformatted{bookdown::bs4_book:
  repo:
    base: https://github.com/hadley/ggplot2-book
    branch: main
    subdir: book
    icon: "fas fa-air-freshener"
}\if{html}{\out{</div>}}
}

\section{References/Bibliography}{


As this theme makes footnotes appear in line, making your citations \emph{footnotes}
allows readers to read them near the text they are referred in.
To do that, download a footnote style CSL file
(e.g. chicago-fullnote-bibliography.csl)
put this in your index.Rmd:\if{html}{\out{<div class="yaml">}}\preformatted{bibliography: refs.bib
csl: chicago-fullnote-bibliography.csl
}\if{html}{\out{</div>}}

And then optionally, if you no longer want a reference section
at the back of the book:\if{html}{\out{<div class="yaml">}}\preformatted{suppress-bibliography: true
}\if{html}{\out{</div>}}
}

\section{HTML metadata}{


HTML \verb{<meta>} tag will be set based on Pandoc's variables set in \code{index.Rmd}:
\itemize{
\item \code{pagetitle} (or \code{title} if unset) will be used as \code{og:title}
and \code{twitter:title} content.
\item \code{description} will be used as \code{description}, \code{og:description} and
\code{twitter:description} content.
\item \code{url} will be used as \code{og:url} and as base for \code{cover-image}
\item \code{cover-image} is the path to your local cover image, and will be used as
\code{og:image}, appended to \code{url} value.
\item \code{twitter-handle} will be used as \code{twitter:site} content.
}

Some value are not configurable:
\itemize{
\item \code{twitter:card} will be set as \code{summary}.
\item \code{og:type} will be set as \code{book}.
}

A YAML header in \code{index.Rmd} would look like this:\if{html}{\out{<div class="yaml">}}\preformatted{---
title: "A Minimal Book Example"
author: "John Doe"
date: "`r Sys.Date()`"
site: bookdown::bookdown_site
output: bookdown::bs4_book
url: https://bookdown.org/johndoe/bookdown-demo
cover-image: cover.png
description: |
  This is a minimal example of using the bookdown package to write a book.
  The output format for this example is bookdown::bs4_book
---
}\if{html}{\out{</div>}}

If this values are set, then the associated \verb{<meta>} tag will be set in the
resulting HTML file. The provided values will be used asis in \code{index.html}.
They will then be tweaked for each chapter to produce different metadata for each
page as expected by social media:
\itemize{
\item All \code{description} related meta will be auto generated from page content,
\item \code{og:url} will be set to the specific HTML page url,
\item All the other \verb{<meta>} will stay the same.
}

You can have a look at \href{https://github.com/maelle/bookdown/blob/master/inst/templates/bs4_book.html}{\code{bs4_book()} HTML template}
for details on how these variables are used.
}

