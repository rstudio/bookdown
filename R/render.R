#' Render multiple R Markdown documents into a book
#'
#' A wrapper function to render all R Markdown files under the current working
#' directory into a book. It can be used in the RStudio IDE (specifically, the
#' \code{knit} field in YAML).
#'
#' There are two ways to render a book from Rmd files. The default way
#' (\code{new_session = FALSE}) is to merge Rmd files into a single file and
#' render this file. You can also choose to render each individual Rmd file in a
#' new R session (\code{new_session = TRUE}). In this case, Rmd files that have
#' not been updated from the previous run will not be recompiled the next time
#' by default, and you can force compiling them by \code{force_knit = TRUE}.
#' @param input Ignored. All R Markdown files under the current working
#'   directory are merged as the actual input to
#'   \code{rmarkdown::\link[rmarkdown]{render}()}.
#' @param output_format,...,clean,envir Arguments to be passed to
#'   \code{render()}.
#' @param output_dir The output directory. If not specified, a field named
#'   \code{output_dir} in the configuration file \file{_config.yml} will be used
#'   (possiblely not specified, either). If not \code{NULL}, the output files
#'   will be moved to this directory.
#' @param new_session Whether to use new R sessions to compile individual Rmd
#'   files.
#' @param force_knit Whether to force knitting all Rmd files.
#' @export
render_book = function(
  input, output_format = NULL, ..., clean = TRUE, envir = parent.frame(),
  output_dir = NULL, new_session = FALSE, force_knit = FALSE
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
  config = load_config()  # _config.yml
  if (missing(output_dir)) output_dir = config[['output_dir']]
  if (length(output_dir)) {
    dir_create(output_dir)
    # ignore output_dir that is just the current working directory
    if (normalizePath(output_dir) == normalizePath(getwd())) output_dir = NULL
  }
  # store output directory and the initial input Rmd name
  opts$set(output_dir = output_dir, input_rmd = basename(input))

  # you may set, e.g., new_session: yes in _config.yml
  if (missing(new_session)) {
    new_session = FALSE
    if (is.logical(config[['new_session']])) new_session = config[['new_session']]
  }

  # a list of Rmd chapters
  files = list.files('.', '[.]Rmd$')
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
  i = grep('[.]Rmd$', files, invert = TRUE)
  if (length(i)) warning(
    "Some input files do not have the '.Rmd' extension: ", paste(files[i], collapse = ', ')
  )

  main = if (is.character(config[['book_filename']])) {
    config[['book_filename']][1]
  } else if (new_session) '_main.md' else '_main.Rmd'
  if (!grepl('[.]R?md$', main)) main = paste0(main, if (new_session) '.md' else '.Rmd')
  opts$set(book_filename = main)  # store the book filename
  on.exit(unlink(main), add = TRUE)

  if (new_session) {
    render_new_session(files, main, force_knit, output_format, clean, envir, ...)
  } else {
    render_cur_session(files, main, config, output_format, clean, envir, ...)
  }
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

  # save a copy of render arguments in a temp file
  render_args = tempfile('render', '.', '.rds')
  on.exit(unlink(render_args), add = TRUE)
  saveRDS(
    list(output_format = output_format, ..., clean = FALSE, envir = envir),
    render_args
  )
  # an RDS file to save all the metadata after compiling each Rmd
  render_meta = with_ext(main, '.rds')

  # which Rmd's should be recompiled?
  rerun = if (force_) TRUE else {
    files_md = with_ext(files, '.md')
    !utils::file_test('-ot', files, files_md)  # Rmd not older than md
  }
  # compile chapters in separate R sessions
  for (f in files[rerun]) Rscript_render(f, render_args, render_meta)

  meta = clean_meta(render_meta, files)

  merge_chapters(meta, main)

  knit_meta = unlist(lapply(meta, attr, 'knit_meta', exact = TRUE), recursive = FALSE)
  intermediates = unlist(lapply(meta, attr, 'intermediates', exact = TRUE))
  if (clean) on.exit(unlink(intermediates, recursive = TRUE), add = TRUE)

  rmarkdown::render(
    main, output_format, ..., clean = clean, envir = envir,
    run_pandoc = TRUE, knit_meta = knit_meta
  )

}
