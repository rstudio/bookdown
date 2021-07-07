# this is the function used for the RStudio project template
bookdown_skeleton = function(path, output_format) {

  # ensure directory exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path = xfun::normalize_path(path)

  # Get correct resources
  resources = bookdown_file('rstudio', 'templates', 'project', 'resources')
  files = list.files(resources, recursive = TRUE, include.dirs = FALSE)
  files = files[grepl("^.*(?<!-index[.]yml)$", files, perl = TRUE)]
  files = files[grepl("^.*(?<!-output[.]yml)$", files, perl = TRUE)]
  yaml_index = sprintf("%s-index.yml", output_format)
  yaml_output = sprintf("%s-output.yml", output_format)
  files = c(yaml_index, yaml_output, files)
  # copy them to path
  source = file.path(resources, files)
  target = file.path(path, files)
  lapply(unique(dirname(target)), dir_create)
  file.copy(source, target)

  # tweak files
  xfun::in_dir(path, {
    # build index.Rmd
    index = xfun::read_utf8("index.Rmd")
    placeholder = grep("yaml: goes here", index, fixed = FALSE)
    yml = xfun::read_utf8(yaml_index)
    index = c(index[seq_len(placeholder - 1)], yml, index[seq.int(placeholder + 1, length(index))])
    xfun::write_utf8(index, "index.Rmd")
    unlink(yaml_index)

    # build _output.yaml
    f = "_output.yml"
    x = xfun::read_utf8(f)
    yml = xfun::read_utf8(yaml_output)
    xfun::write_utf8(c(yml, x), f)
    unlink(yaml_output)

    # tweak _bookdown.yml
    f = '_bookdown.yml'
    x = read_utf8(f)
    # add book_filename to _bookdown.yml and default to the base path name
    prepend = sprintf('book_filename: "%s"', basename(path))
    # add new_session to _bookdown.yml if bs4_book
    if (output_format == "bs4_book") {
      prepend = c(prepend, 'new_session: true', 'before_chapter_script: _common.R')
    }
    write_utf8(c(prepend, x), f)
  })

  TRUE
}

activate_rstudio_project <- function(dir) {
  if (xfun::pkg_available("rstudioapi") && rstudioapi::isAvailable("1.1.287")) {
    rstudioapi::initializeProject(dir)
  }
}

#' @export
create_gitbook = function(path) {
  create_html_book(path, output_format = "gitbook")
  activate_rstudio_project(path)
}

#' @export
create_bs4_book = function(path) {
  create_html_book(path, output_format = "bs4_book")
  activate_rstudio_project(path)
}

create_html_book = function(path, output_format = c("gitbook", "bs4_book")) {
  output_format = match.arg(output_format)
  bookdown_skeleton(path, output_format)
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
#' @param description The description of the book.
#' @param url The URL of the book.
#' @noRd
book_skeleton = function(
  name, title, author, chapters = c('Preface {-}', 'Introduction'),
  documentclass = 'book', references = 'References', path = getwd(),
  description = NULL, url = NULL
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
                            site = 'bookdown::bookdown_site',
                            description = description, url = url)
      index_metadata = Filter(length, index_metadata)
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

