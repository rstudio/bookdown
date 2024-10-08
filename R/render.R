#' Render multiple R Markdown documents into a book
#'
#' Render multiple R Markdown files under the current working directory into a
#' book. It can be used in the RStudio IDE (specifically, the \code{knit} field
#' in YAML). The \code{preview_chapter()} function is a wrapper of
#' \code{render_book(preview = TRUE)}.
#'
#' There are two ways to render a book from Rmd files. The default way
#' (\code{new_session = FALSE}) is to merge Rmd files into a single file and
#' render this file. You can also choose to render each individual Rmd file in a
#' new R session (\code{new_session = TRUE}).
#' @param input A directory, an input filename or multiple filenames. For a
#'   directory, \file{index.Rmd} will be used if it exists in this (book)
#'   project directory. For filenames, if \code{preview = TRUE}, only files
#'   specified in this argument are rendered, otherwise all R Markdown files
#'   specified by the book are rendered.
#' @param output_format,...,clean,envir Arguments to be passed to
#'   \code{rmarkdown::\link[rmarkdown]{render}()}. For \code{preview_chapter()},
#'   \code{...} is passed to \code{render_book()}. See
#'   \code{rmarkdown::\link[rmarkdown]{render}()}
#'   and \href{https://bookdown.org/yihui/bookdown/build-the-book.html}{the
#'   bookdown reference book} for details on how output formatting options are
#'   set from YAML or parameters supplied by the user when calling
#'   \code{render_book()}.
#' @param clean_envir This argument has been deprecated and will be removed in
#'   future versions of \pkg{bookdown}.
#' @param output_dir The output directory. If \code{NULL}, a field named
#'   \code{output_dir} in the configuration file \file{_bookdown.yml} will be
#'   used (possibly not specified, either, in which case a directory name
#'   \file{_book} will be used).
#' @param new_session Whether to use new R sessions to compile individual Rmd
#'   files (if not provided, the value of the \code{new_session} option in
#'   \file{_bookdown.yml} is used; if this is also not provided,
#'   \code{new_session = FALSE}).
#' @param preview Whether to render and preview the input files specified by the
#'   \code{input} argument. Previewing a certain chapter may save compilation
#'   time as you actively work on this chapter, but the output may not be
#'   accurate (e.g. cross-references to other chapters will not work).
#' @param config_file The book configuration file.
#' @export
#' @examples
#' # see https://bookdown.org/yihui/bookdown for the full documentation
#' if (file.exists('index.Rmd')) bookdown::render_book('index.Rmd')
#' \dontrun{
#' # will use the default format defined in index.Rmd or _output.yml
#' bookdown::render_book("index.Rmd")
#' # will use the options for format defined in YAML metadata
#' bookdown::render_book("index.Rmd",  "bookdown::pdf_book")
#' # If you pass an output format object, it must have all the options set
#' bookdown::render_book("index.Rmd", bookdown::pdf_book(toc = FALSE))
#'
#' # will render the book in the current directory
#' bookdown::render_book()
#' # this is equivalent to
#' bookdown::render_book("index.Rmd")
#' # will render the book living in the specified directory
#' bookdown::render_book("my_book_project")
#' }
render_book = function(
  input = ".", output_format = NULL, ..., clean = TRUE, envir = parent.frame(),
  clean_envir = !interactive(), output_dir = NULL, new_session = NA,
  preview = FALSE, config_file = '_bookdown.yml'
) {

  verify_rstudio_version()

  # select and check input file(s)
  if (length(input) == 1L && dir_exists(input)) {
    message(sprintf("Rendering book in directory '%s'", input))
    owd = setwd(input); on.exit(setwd(owd), add = TRUE)
    # if a directory is passed, we assume that index.Rmd exists
    input = get_index_file()
    # No input file to use as fallback
    if (is_empty(input)) input = NULL
  } else {
    stop_if_not_exists(input)
  }

  format = NULL  # latex or html
  if (is.list(output_format)) {
    format = output_format$bookdown_output_format
    if (!is.character(format) || !(format %in% c('latex', 'html'))) format = NULL
  } else if (is.null(output_format) || is.character(output_format)) {
    if (is.null(output_format) || identical(output_format, 'all')) {
      # formats can safely be guess when considering index.Rmd and its expected frontmatter
      # As a fallback we assumes input could have the YAML, otherwise we just use gitbook();
      # Also, when no format provided, return name of the first resolved
      output_format = get_output_formats(
        fallback_format = "bookdown::gitbook",
        first = is.null(output_format),
        fallback_index = input
      )
    }
    if (length(output_format) > 1) return(unlist(lapply(output_format, function(fmt)
      xfun::Rscript_call(render_book, list(
        input, fmt, ..., clean = clean, envir = envir, output_dir = output_dir,
        new_session = new_session, preview = preview, config_file = config_file
      ), fail = c("bookdown::render_book() failed to render the output format '", fmt, "'."))
    )))
    format = target_format(output_format)
  }

  if (!missing(clean_envir)) warning(
    "The argument 'clean_envir' has been deprecated and will be removed in future ",
    "versions of bookdown."
  )

  on.exit(opts$restore(), add = TRUE)
  opts$set(config_file = config_file)
  config = load_config()  # configurations in _bookdown.yml
  output_dir = output_dirname(output_dir, config)
  on.exit(xfun::del_empty_dir(output_dir), add = TRUE)
  if (!preview) unlink(ref_keys_path(output_dir))  # clean up reference-keys.txt
  # store output directory and the initial input Rmd name
  opts$set(
    output_dir = output_dir,
    input_rmd = xfun::relative_path(input),
    preview = preview
  )

  aux_diro = '_bookdown_files'
  # move _files and _cache from _bookdown_files to ./, then from ./ to _bookdown_files
  aux_dirs = files_cache_dirs(aux_diro)
  file_rename(aux_dirs, basename(aux_dirs))
  on.exit({
    aux_dirs = files_cache_dirs('.')
    if (length(aux_dirs)) {
      dir_create(aux_diro)
      file_rename(aux_dirs, file.path(aux_diro, basename(aux_dirs)))
    }
  }, add = TRUE)

  # you may set, e.g., new_session: yes in _bookdown.yml
  if (is.na(new_session)) {
    new_session = FALSE
    if (is.logical(config[['new_session']])) new_session = config[['new_session']]
  }

  main = book_filename()
  if (!grepl('[.][Rr]?md$', main)) main = paste0(main, if (new_session) '.md' else '.Rmd')
  delete_main = config[['delete_merged_file']]
  check_main = function() file.exists(main) && is.null(delete_main)
  if (check_main()) stop(
    'The file ', main, ' exists. Please delete it if it was automatically generated. ',
    'If you are sure it can be safely overwritten or deleted, please set the option ',
    "'delete_merged_file' to true in _bookdown.yml."
  )
  on.exit(if (check_main()) {
    message('Please delete ', main, ' after you finish debugging the error.')
  }, add = TRUE)
  opts$set(book_filename = main)  # store the book filename

  files = source_files(format, config)
  if (length(files) == 0) stop(
    'No input R Markdown files found from the current directory ', getwd(),
    ' or in the rmd_files field of _bookdown.yml'
  )
  if (new_session && any(dirname(files) != '.')) stop(
    'With new_session = TRUE, all input files must be under the root directory ',
    'of the (book) project. You might have used `rmd_files` or `rmd_subdir` to ',
    'specify input files from subdirectories, which will not work with `new_session`.'
  )

  res = if (new_session) {
    render_new_session(files, main, config, output_format, clean, envir, ...)
  } else {
    render_cur_session(files, main, config, output_format, clean, envir, ...)
  }
  if (!isFALSE(delete_main)) file.remove(main)
  res
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

render_new_session = function(files, main, config, output_format, clean, envir, ...) {

  # save a copy of render arguments in a temp file
  render_args = tempfile('render', '.', '.rds')
  on.exit(file.remove(render_args), add = TRUE)
  saveRDS(
    list(output_format = output_format, ..., clean = FALSE, envir = envir),
    render_args
  )
  # an RDS file to save all the metadata after compiling each Rmd
  render_meta = with_ext(main, '.rds')

  files_md = output_path(with_ext(files, '.md'))
  # copy pure Markdown input files to output directory; no need to render() them
  for (i in which(grepl('[.]md$', files) & files != files_md))
    file.copy(files[i], files_md[i], overwrite = TRUE)
  # if input is index.Rmd or not preview mode, compile all Rmd's
  rerun = !opts$get('preview') || opts$get('input_rmd') %in% get_index_file()
  if (!rerun) rerun = files %in% opts$get('input_rmd')
  add1 = merge_chapter_script(config, 'before')
  add2 = merge_chapter_script(config, 'after')
  on.exit(unlink(c(add1, add2)), add = TRUE)
  # compile chapters in separate R sessions
  for (f in files[rerun]) Rscript_render(f, render_args, render_meta, add1, add2)

  if (!all(dirname(files_md) == '.'))
    file.copy(files_md[!rerun], basename(files_md[!rerun]), overwrite = TRUE)

  meta = clean_meta(render_meta, files)
  move = !(unlist(meta) %in% files)  # do not move input files to output dir
  on.exit(file_rename(unlist(meta)[move], files_md[move]), add = TRUE)

  merge_chapters(unlist(meta), main, orig = files)

  knit_meta = unlist(lapply(meta, attr, 'knit_meta', exact = TRUE), recursive = FALSE)
  intermediates = unlist(lapply(meta, attr, 'intermediates', exact = TRUE))
  if (clean) on.exit(unlink(intermediates, recursive = TRUE), add = TRUE)

  rmarkdown::render(
    main, output_format, ..., clean = clean, envir = envir,
    run_pandoc = TRUE, knit_meta = knit_meta
  )

}

#' Clean up the output files and directories from the book
#'
#' After a book is rendered, there will be a series of output files and
#' directories created in the book root directory, typically including
#' \file{*_files/}, \file{*_cache/}, \file{_book/}, and some HTML/LaTeX
#' auxiliary files. These filenames depend on the book configurations. This
#' function identifies these files and directories, and delete them if desired,
#' so you can rebuild the book with a clean source.
#' @param clean Whether to delete the possible output files. If \code{FALSE},
#'   simply print out a list of files/directories that should probably be
#'   deleted. You can set the global option \code{bookdown.clean_book = TRUE} to
#'   force this function to delete files. You are recommended to take a look at
#'   the list of files at least once before actually deleting them, i.e. run
#'   \code{clean_book(FALSE)} before \code{clean_book(TRUE)}.
#' @export
clean_book = function(clean = getOption('bookdown.clean_book', FALSE)) {
  r = '_(files|cache)$'
  one = with_ext(book_filename(), '')  # the main book file
  src = with_ext(source_files(all = TRUE), '')  # input documents
  out = list.files('.', r)
  out = out[dir_exists(out)]
  out = out[gsub(r, '', out) %in% c(src, one)]  # output dirs generated from src names
  out = c(out, output_dirname(NULL, create = FALSE))  # output directory
  out = c(out, with_ext(one, c('bbl', 'html', 'tex', 'rds')))  # aux files for main file
  out = c(out, load_config()[['clean']])  # extra files specified in _bookdown.yml
  out = sort(unique(out))
  if (length(out) == 0) return(invisible())
  if (clean) unlink(out, recursive = TRUE) else {
    out = out[file.access(out) == 0]
    if (length(out) == 0) return(invisible())
    message(
      'These files/dirs can probably be removed: \n\n', paste(mark_dirs(out), collapse = '\n'),
      '\n\nYou can set options(bookdown.clean_book = TRUE) to allow this function to always clean up the book directory for you.'
    )
    invisible(out)
  }
}
