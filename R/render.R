#' Render multiple R Markdown documents into a single output file
#'
#' A wrapper function to merge all R Markdown files under the current working
#' directory, and render the merged R Markdown file. It was mainly designed to
#' be used in the RStudio IDE (specifically, the \code{knit} field in YAML).
#' @param input Ignored. All R Markdown files under the current working
#'   directory are merged as the actual input to
#'   \code{rmarkdown::\link[rmarkdown]{render}()}.
#' @param output_format,...,envir Arguments to be passed to \code{render()}.
#' @note The R Markdown files that start with an underscore \code{_} are ignored
#'   when merging all \file{.Rmd }files.
#' @export
render_book = function(
  input, output_format = NULL, ..., clean = TRUE, envir = parent.frame(),
  force_rerun = FALSE
) {

  format = NULL  # latex or html
  if (is.list(output_format)) {
    format = output_format$bookdown_output_format
    if (!is.character(format) || !(format %in% c('latex', 'html'))) format = NULL
  }

  config = load_config()  # _config.yml

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

  main = if (is.character(config[['main_md']])) {
    config[['main_md']][1]
  } else '_main.md'

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
  rerun = if (force_rerun) TRUE else {
    files_md = with_ext(files, '.md')
    !utils::file_test('-ot', files, files_md)  # Rmd not older than md
  }
  # compile chapters in separate R sessions
  for (f in files[rerun]) Rscript_render_one(f, render_args, render_meta)

  meta = clean_meta(render_meta, files)

  content = unlist(lapply(meta, function(f) {
    x = readUTF8(f)
    id = with_ext(f, '')  # base filename (without extension)
    c(x, '', paste0('<!--chapter:end:', id, '-->'), '')
  }))
  writeLines(enc2utf8(content), main, useBytes = TRUE)
  on.exit(unlink(main), add = TRUE)

  knit_meta = unlist(lapply(meta, attr, 'knit_meta', exact = TRUE), recursive = FALSE)
  intermediates = unlist(lapply(meta, attr, 'intermediates', exact = TRUE))
  if (clean) on.exit(unlink(intermediates, recursive = TRUE), add = TRUE)

  rmarkdown:::render_one(
    main, output_format, ..., clean = clean, envir = envir,
    run_pandoc = TRUE, knit_meta = knit_meta
  )
}
