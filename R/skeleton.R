bookdown_skeleton = function(path) {

  # ensure directory exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # copy 'resources' folder to path
  resources = bookdown_file('rstudio', 'templates', 'project', 'resources')

  files = list.files(resources, recursive = TRUE, include.dirs = FALSE)

  source = file.path(resources, files)
  target = file.path(path, files)
  file.copy(source, target)

  # add book_filename to _bookdown.yml and default to the base path name
  f = file.path(path, '_bookdown.yml')
  x = readUTF8(f)
  writeUTF8(c(sprintf('book_filename: "%s"', basename(path)), x), f)

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
#' @noRd
book_skeleton = function(
  name, title, author, chapters = c('Preface {-}', 'Introduction'),
  documentclass = 'book', references = 'References'
) {
  rmd_files = gsub('[^-a-zA-Z0-9]', '', gsub('\\s+', '-', c(chapters, references)))
  rmd_files = sprintf('%02d-%s.Rmd', seq_along(rmd_files) - 1, rmd_files)
  rmd_files[1] = 'index.Rmd'
  write_file = function(x, f) {
    if (file.exists(f)) stop('The file ', f, ' exists.')
    writeUTF8(x, f)
  }
  titles = c(chapters, sprintf("`r if (knitr:::is_html_output()) '# %s {-}'`", references))
  titles = paste('#', titles)
  for (i in seq_along(rmd_files)) {
    content = c(titles[i], '')
    if (i == 1) {
      content = c(
        '---', sprintf('title: "%s"', title), sprintf('author: "%s"', author),
        sprintf('documentclass: "%s"', documentclass),
        'site: bookdown::bookdown_site', '---', '', content,
        'Start writing your book here. If you are in RStudio,',
        'Click the Build button to build the book.'
      )
    }
    write_file(content, rmd_files[i])
  }
  write_file(
    sprintf('bookdown::%s: default', c('gitbook', 'pdf_book', 'epub_book')), '_output.yml'
  )
  write_file(sprintf('book_filename: %s', name), '_bookdown.yml')
}

