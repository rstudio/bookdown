bookdown_skeleton = function(path) {

  # ensure directory exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path = xfun::normalize_path(path)

  # copy 'resources' folder to path
  resources = bookdown_file('rstudio', 'templates', 'project', 'resources')

  files = list.files(resources, recursive = TRUE, include.dirs = FALSE)

  source = file.path(resources, files)
  target = file.path(path, files)
  lapply(unique(dirname(target)), dir_create)
  file.copy(source, target)

  # add book_filename to _bookdown.yml and default to the base path name
  f = file.path(path, '_bookdown.yml')
  x = read_utf8(f)
  write_utf8(c(sprintf('book_filename: "%s"', basename(path)), x), f)

  TRUE
}

#' Create a book skeleton
#'
#' Write Rmd files (named in the form \file{\%02d-chapter-title.Rmd}) for
#' chapters with the chapter titles specified, create the output format file
#' \file{_output.yml}, and generate the book configuration file
#' \file{_bookdown.yml}.
#'
#' If you use RStudio v1.1.28 or a greater version, you do not really need to
#' use this function, since you can create a new RStudio project and select the
#' project type to be book.
#' @param name An ID for the book to be written to the \code{book_filename}
#'   field in \code{_bookdown.yml} and used as the \code{name} argument of
#'   \code{\link{publish_book}()}. You can use the current directory name here.
#' @param title,author The title and author of the book.
#' @param chapters The chapter titles.
#' @param documentclass The LaTeX document class.
#' @param references The title of the references section.
#' @param path The directory in which to create the book.
#' @noRd
book_skeleton = function(
  name, title, author, chapters = c('Preface {-}', 'Introduction'),
  documentclass = 'book', references = 'References', path = getwd()
) {
  rmd_files = gsub('[^-a-zA-Z0-9]', '', gsub('\\s+', '-', c(chapters, references)))
  rmd_files = sprintf('%02d-%s.Rmd', seq_along(rmd_files) - 1, rmd_files)
  rmd_files[1] = 'index.Rmd'
  write_file = function(x, f) {
    if (file.exists(f)) stop('The file ', f, ' exists.')
    write_utf8(x, f)
  }
  titles = c(chapters, sprintf("`r if (knitr::is_html_output()) '# %s {-}'`", references))
  titles = paste('#', titles)
  for (i in seq_along(rmd_files)) {
    content = c(titles[i], '')
    # special handling for file which will be index.Rmd
    if (i == 1) {
      index_metadata = list(title = title, author = author,
                            documentclass = documentclass,
                            site = 'bookdown::bookdown_site')
      content = c(
        '---', yaml::as.yaml(index_metadata), '---', '',
        content,
        'Start writing your book here. If you are in RStudio,',
        'Click the Build button to build the book.'
      )
    }
    write_file(content, file.path(path, rmd_files[i]))
  }
  write_file(
    sprintf('bookdown::%s: default', c('gitbook', 'pdf_book', 'epub_book', 'bs4_book')),
    file.path(path, '_output.yml')
  )
  write_file(sprintf('book_filename: %s', name), file.path(path, '_bookdown.yml'))
}

