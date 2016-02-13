#' Render multiple R Markdown documents into a book
#'
#' Render mulitple R Markdown files under the current working directory into a
#' book. It can be used in the RStudio IDE (specifically, the \code{knit} field
#' in YAML). The \code{preview_chapter()} function is a wrapper of
#' \code{render_book(preview = TRUE)}.
#'
#' There are two ways to render a book from Rmd files. The default way
#' (\code{new_session = FALSE}) is to merge Rmd files into a single file and
#' render this file. You can also choose to render each individual Rmd file in a
#' new R session (\code{new_session = TRUE}). In this case, Rmd files that have
#' not been updated from the previous run will not be recompiled the next time
#' by default, and you can force compiling them by \code{force_knit = TRUE}.
#' @param input An input filename (or multiple filenames). If \code{preview =
#'   TRUE}, only files specified in this argument are rendered, otherwise all R
#'   Markdown files specified by the book are rendered.
#' @param output_format,...,clean,envir Arguments to be passed to
#'   \code{rmarkdown::\link[rmarkdown]{render}()}. For \code{preview_chapter()},
#'   \code{...} is passed to \code{render_book()}.
#' @param output_dir The output directory. If not specified, a field named
#'   \code{output_dir} in the configuration file \file{_bookdown.yml} will be
#'   used (possiblely not specified, either). If not \code{NULL}, the output
#'   files will be moved to this directory.
#' @param new_session Whether to use new R sessions to compile individual Rmd
#'   files.
#' @param force_knit Whether to force knitting all Rmd files (this argument is
#'   only for \code{new_session = TRUE}).
#' @param preview Whether to render and preview the input files specified by the
#'   \code{input} argument. Previewing a certain chapter may save compilation
#'   time as you actively work on this chapter, but the output may not be
#'   accurate (e.g. cross-references to other chapters will not work).
#' @export
render_book = function(
  input, output_format = NULL, ..., clean = TRUE, envir = parent.frame(),
  output_dir = NULL, new_session = FALSE, force_knit = FALSE, preview = FALSE
) {

  format = NULL  # latex or html
  if (is.list(output_format)) {
    format = output_format$bookdown_output_format
    if (!is.character(format) || !(format %in% c('latex', 'html'))) format = NULL
  } else if (is.character(output_format)) {
    if (length(output_format) != 1) stop('output_format must be of length one')
    format = switch(
      output_format,
      `html_chapters` = 'html', `bookdown::html_chapters` = 'html',
      `pdf_book` = 'latex', `bookdown::pdf_book` = 'latex'
    )
  }

  on.exit(opts$restore(), add = TRUE)
  config = load_config()  # configurations in _bookdown.yml
  if (missing(output_dir)) output_dir = config[['output_dir']]
  if (length(output_dir)) {
    dir_create(output_dir)
    # ignore output_dir that is just the current working directory
    if (normalizePath(output_dir) == normalizePath(getwd())) output_dir = NULL
  }
  # store output directory and the initial input Rmd name
  opts$set(
    output_dir = output_dir, input_rmd = basename(input), preview = preview
  )

  # you may set, e.g., new_session: yes in _bookdown.yml
  if (missing(new_session)) {
    new_session = FALSE
    if (is.logical(config[['new_session']])) new_session = config[['new_session']]
  }

  # a list of Rmd chapters
  files = list.files('.', '[.]Rmd$', ignore.case = TRUE)
  if (is.character(config[['rmd_files']])) {
    files = config[['rmd_files']]
    if (!is.null(format) && is.list(files)) files = files[[format]]
  } else {
    files = grep('^[^_]', files, value = TRUE)  # exclude those start with _
    index = match('index', with_ext(files, ''))
    # if there is a index.Rmd, put it in the beginning
    if (!is.na(index)) files = c(files[index], files[-index])
  }
  check_special_chars(files)
  if (new_session && any(dirname(files) != '.')) stop(
    'All input files must be under the current working directory'
  )

  main = if (is.character(config[['book_filename']])) {
    config[['book_filename']][1]
  } else if (new_session) '_main.md' else '_main.Rmd'
  if (!grepl('[.][a-zA-Z]+$', main)) main = paste0(main, if (new_session) '.md' else '.Rmd')
  opts$set(book_filename = main)  # store the book filename
  on.exit(unlink(main), add = TRUE)

  if (new_session) {
    render_new_session(files, main, force_knit, output_format, clean, envir, ...)
  } else {
    render_cur_session(files, main, config, output_format, clean, envir, ...)
  }
}

#' @rdname render_book
#' @export
preview_chapter = function(..., envir = parent.frame()) {
  render_book(..., envir = envir, preview = TRUE)
}

render_cur_session = function(files, main, config, output_format, clean, envir, ...) {
  merge_chapters(
    files, main,
    insert_chapter_script(config, 'before'),
    insert_chapter_script(config, 'after')
  )
  rmarkdown::render(main, output_format, ..., clean = clean, envir = envir)
}

render_new_session = function(files, main, force_, output_format, clean, envir, ...) {

  if (packageVersion('rmarkdown') < '0.9.5') stop(
    'Rendering R Markdown in new R sessions requires rmarkdown >= 0.9.5'
  )

  # save a copy of render arguments in a temp file
  render_args = tempfile('render', '.', '.rds')
  on.exit(unlink(render_args), add = TRUE)
  saveRDS(
    list(output_format = output_format, ..., clean = FALSE, envir = envir),
    render_args
  )
  # an RDS file to save all the metadata after compiling each Rmd
  render_meta = with_ext(main, '.rds')

  files_md = output_path(with_ext(files, '.md'))
  # which Rmd's should be recompiled?
  rerun = if (force_) TRUE else {
    !utils::file_test('-ot', files, files_md)  # Rmd not older than md
  }
  # compile chapters in separate R sessions
  for (f in files[rerun]) Rscript_render(f, render_args, render_meta)
  file.copy(files_md[!rerun], basename(files_md[!rerun]), overwrite = TRUE)

  meta = clean_meta(render_meta, files)
  on.exit(file.rename(unlist(meta), files_md), add = TRUE)

  merge_chapters(unlist(meta), main, orig = files)

  knit_meta = unlist(lapply(meta, attr, 'knit_meta', exact = TRUE), recursive = FALSE)
  intermediates = unlist(lapply(meta, attr, 'intermediates', exact = TRUE))
  if (clean) on.exit(unlink(intermediates, recursive = TRUE), add = TRUE)

  rmarkdown::render(
    main, output_format, ..., clean = clean, envir = envir,
    run_pandoc = TRUE, knit_meta = knit_meta
  )

}
