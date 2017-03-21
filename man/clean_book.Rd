% Please edit documentation in R/render.R
\name{clean_book}
\alias{clean_book}
\title{Clean up the output files and directories from the book}
\usage{
clean_book(clean = getOption("bookdown.clean_book", FALSE))
}
\arguments{
\item{clean}{Whether to delete the possible output files. If \code{FALSE},
simply print out a list of files/directories that should probably be
deleted. You can set the global option \code{bookdown.clean_book = TRUE} to
force this function to delete files. You are recommended to take a look at
the list of files at least once before actually deleting them, i.e. run
\code{clean_book(FALSE)} before \code{clean_book(TRUE)}.}
}
\description{
After a book is rendered, there will be a series of output files and
directories created in the book root directory, typically including
\file{*_files/}, \file{*_cache/}, \file{_book/}, and some HTML/LaTeX
auxiliary files. These filenames depend on the book configurations. This
function identifies these files and directories, and delete them if desired,
so you can rebuild the book with a clean source.
}
