# this is the function used for the RStudio project template
bookdown_skeleton = function(path, output_format) {

  # ensure directory exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path = xfun::normalize_path(path)

  # Get common resources
  files = skeleton_get_files("common")
  files_format = skeleton_get_files(output_format)
  # copy them to path
  source = file.path(skeleton_get_dir(), c(files, files_format))
  # common resource are copied without folder
  target = file.path(path, c(xfun::relative_path(files, "common"), files_format))

  lapply(unique(dirname(target)), dir_create)
  file.copy(source, target)

  # Tweak template file
  skeleton_build_index(path, output_format)
  skeleton_build_output_yml(path, output_format)
  skeleton_build_bookdown_yml(path, output_format)
  move_dir(file.path(path, output_format), path) # move left format files
  skeleton_remove_blocks(path, output_format)

  invisible(TRUE)
}

skeleton_remove_blocks = function(path, output_format) {
  rmd_files = list.files(path, "[.]Rmd$", recursive = TRUE, full.names = TRUE)
  unkept_format = setdiff(skeleton_formats(), output_format)
  for (file in rmd_files) {
    content = xfun::read_utf8(file)

    # remove block
    r1 = sprintf("<!--(%s):start-->", paste(unkept_format, collapse = "|"))
    s_remove = grep(r1, content)
    r3 = sprintf("<!--(%s):end-->", paste(unkept_format, collapse = "|"))
    e_remove = grep(r3, content)
    if (length(s_remove) != 0 && length(s_remove) == length(e_remove)) {
      block = unlist(mapply(function(s, e) seq.int(s, e), s = s_remove, e = e_remove))
      content = content[-block]
    }

    # remove magick comment only
    r2 = sprintf("<!--(%s):start-->", output_format)
    s_keep = grep(r2, content)
    r4 = sprintf("<!--(%s):end-->", output_format)
    e_keep = grep(r4, content)
    if (length(s_keep) > 0) content = content[-c(s_keep, e_keep)]

    xfun::write_utf8(content, file)
  }
  invisible(TRUE)
}

skeleton_formats = function() {
  c("gitbook", "bs4_book")
}

skeleton_insert_yml = function(index_rmd, index_yml, placeholder) {
  index = xfun::read_utf8(index_rmd)
  pos = grep(placeholder, index, fixed = FALSE)
  if (length(pos) <= 0) return(invisible(FALSE))
  yml = if (file.exists(index_yml)) {
    on.exit(unlink(index_yml), add = TRUE)
    xfun::read_utf8(index_yml)
  }
  index = c(index[seq_len(pos - 1)], yml, index[seq.int(pos + 1, length(index))])
  xfun::write_utf8(index, index_rmd)
  invisible(TRUE)
}

skeleton_build_index = function(path, format_dir) {
  index_file = file.path(path, "index.Rmd")
  index_format_yml = file.path(path, format_dir, "index.yml")
  skeleton_insert_yml(index_file, index_format_yml, "# additional yaml goes here")
}

skeleton_append_yml = function(main_yml, child_yml, prepend = NULL) {
  yml_main = xfun::read_utf8(main_yml)
  if (!file.exists(child_yml)) return(invisible(FALSE))
  yml_child = xfun::read_utf8(child_yml)
  on.exit(unlink(child_yml), add = TRUE)
  prepend = c(prepend, yml_child)
  xfun::write_utf8(c(prepend, yml_main), main_yml)
  invisible(TRUE)
}

skeleton_build_output_yml = function(path, format_dir) {
  file = "_output.yml"
  main_file = file.path(path, file)
  child_file = file.path(path, format_dir, file)
  skeleton_append_yml(main_file, child_file)
}

skeleton_build_bookdown_yml = function(path, format_dir) {
  file = "_bookdown.yml"
  main_file = file.path(path, file)
  child_file = file.path(path, format_dir, file)
  prepend = sprintf('book_filename: "%s"', basename(path))
  skeleton_append_yml(main_file, child_file, prepend)
}

skeleton_get_dir = function(...) {
  bookdown_file('rstudio', 'templates', 'project', 'resources', ...)
}

skeleton_get_files <- function(subdir = NULL, relative = TRUE) {
  resources = skeleton_get_dir()
  subdir = file.path(resources, subdir %n% "")
  if (!dir.exists(subdir)) return(NULL)
  files = list.files(subdir, recursive = TRUE, include.dirs = FALSE, full.names = TRUE)
  if (relative) xfun::relative_path(files, resources) else files
}

activate_rstudio_project = function(dir) {
  if (xfun::pkg_available("rstudioapi") && rstudioapi::isAvailable("1.1.287")) {
    rstudioapi::initializeProject(dir)
  }
}

#' Create a bookdown project
#'
#' Create a bookdown project with multiple book output formats,
#' including HTML. Choose one of two HTML book output formats:
#'
#' * Use `create_gitbook()` to use `bookdown::gitbook()` output format
#' * Use `create_bs4_book()` to use a `bookdown::bs4_book()` output format
#'
#' The function will create a folder with file structure for a bookdown project,
#' and example files with information on how to start.
#'
#' @param path Absolute path to an empty directory in which to create the bookdown project.
#'   In the RStudio IDE, if \pkg{rstudioapi} package available,
#'   an RStudio project will be created.
#' @name create_book
#' @md

#' @rdname create_book
#' @export
create_gitbook = function(path) {
  create_html_book(path, output_format = "gitbook")
  activate_rstudio_project(path)
  path
}

#' @rdname create_book
#' @export
create_bs4_book = function(path) {
  create_html_book(path, output_format = "bs4_book")
  xfun::in_dir(path, {
    try_download_asset(
      "https://www.zotero.org/styles/chicago-fullnote-bibliography",
      "chicago-fullnote-bibliography.csl"
    )
  })
  activate_rstudio_project(path)
  path
}


try_download_asset <- function(url, asset_name) {
  msg <- tryCatch({
    xfun::download_file(url, output = asset_name, quiet = TRUE)
    NULL
  },
  warning = function(w) invokeRestart("muffleWarning"),
  error = function(e) {
    c(">>> ",
      sQuote(asset_name),
      " can't be downloaded. Please retrieve it manually at ",
      sQuote(url),
      " or remove its usage from the template."
    )
  }
  )
  if (!is.null(msg)) message(msg)
  invisible(NULL)
}

create_html_book = function(path, output_format = skeleton_formats()) {
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

