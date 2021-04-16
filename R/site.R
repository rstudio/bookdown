
#' R Markdown site generator for bookdown
#'
#' Implementation of custom R Markdown site generator for bookdown.
#'
#' @inheritParams rmarkdown::render_site
#'
#' @export
bookdown_site = function(input, ...) {

  on.exit(opts$restore(), add = TRUE)

  # need to switch to the project directory for computing the config
  book_proj = find_book_proj(input)
  oldwd = setwd(book_proj)
  on.exit(setwd(oldwd), add = TRUE)

  # load the config for the input directory
  config = load_config()

  # get the name from the config (default to the directory name
  # if there is no name in the config)
  name = find_book_name(config, basename(normalizePath(input)))

  # get the book dir from the config
  book_dir = find_book_dir(config)

  # Custom render function. bookdown rendering is more complex than
  # for most formats and as a result uses a custom R script (_render.R)
  # or Makefile to define what's required to render the book.
  render = function(input_file, output_format, envir, quiet, encoding, ...) {
    # input_file indicates that caller (likely the IDE) would like to
    # build a single file of the website only
    if (is.null(input_file)) {
      in_dir(input, render_book_script(output_format, envir, quiet))
    } else {
      # Input file can be in a sub-directory  of the project. So we need to
      # to emit our own message with absolute path
      res = xfun::in_dir(
          book_proj,
          suppress_output_message(
            render_book(input_file, output_format, envir = envir, preview = TRUE)
          )
      )
      # emit our own message for IDE preview
      if (!quiet) message(paste0("\nOutput created: ", res))
      res
    }
  }

  clean = function() {
    suppressMessages(clean_book(clean = FALSE))
  }

  # return site generator
  list(
    name = name,
    output_dir = book_dir,
    render = render,
    subdirs = TRUE,
    clean = clean
  )
}

# render the book via _render.R or Makefile, or fallback to render_book()
render_book_script = function(output_format = NULL, envir = globalenv(), quiet = TRUE) {
  result = 0
  if (length(script <- existing_r('_render', TRUE))) {
    result = Rscript(c(if (quiet) '--quiet', script, shQuote(output_format)))
  } else if (file.exists('Makefile')) {
    result = system2('make')
  } else {
    render_book('index.Rmd', output_format = output_format, envir = envir)
  }
  if (result != 0) stop('Error ', result, ' attempting to render book')
}

find_book_dir = function(config) {
  d = output_dirname(NULL, config, create = FALSE)
  if (is.null(d)) '.' else d
}

find_book_name = function(config, default) {
  name = with_ext(book_filename(config, fallback = FALSE), '')
  if (is.null(name)) default else name
}

find_book_proj = function(input) {
  # if bookdown_site() is executed it is because site: has been set in index.Rmd
  rules = matrix(c(
    '^index.Rmd$', '^\\s*site:\\s*bookdown::bookdown_site\\s*$'
  ), ncol = 2, byrow = TRUE, dimnames = list(NULL, c('file', 'pattern')))
  xfun::proj_root(input, rules)
}

# This function aims at suppressing the message "\nOutput created: <path>" emitted by render()
# and used by RStudio to preview, so that another one can be emitted
suppress_output_message <- function(expr, pattern = '^\nOutput created: ') {
  withCallingHandlers(
    expr,
    message = function(e) {
      if (grepl(pattern, e$message)) {
        # muffle the message
        invokeRestart("muffleMessage")
      }
    }
  )
}
