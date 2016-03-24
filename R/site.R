
#' R Markdown site generator for bookdown
#'
#' Implementation of custom R Markdown site generator for bookdown.
#'
#' @inheritParams rmarkdown::render_site
#'
#' @export
bookdown_site = function(input, ...) {

  on.exit(opts$restore(), add = TRUE)

  # need to switch to the input directory for computing the config
  oldwd = setwd(input)
  on.exit(setwd(oldwd), add = TRUE)

  # load the config for the input directory
  config = load_config()

  # get the name from the config (default to the directory name
  # if there is no name in the config)
  name = with_ext(book_filename(config, fallback = FALSE), '')
  if (is.null(name))
    name = basename(normalizePath(input))

  # get the book dir from the config
  book_dir = output_dirname('_book', config, create = FALSE)
  if (is.null(book_dir))
    book_dir = '.'

  # Custom render function. bookdown rendering is more complex than
  # for most formats and as a result uses a custom R script (_render.R)
  # or Makefile to define what's required to render the book.
  render = function(output_format, envir, quiet, encoding, ...) {

    # switch to the input dir for the duration of render
    oldwd = setwd(input)
    on.exit(setwd(oldwd), add = TRUE)

    # perform the render
    result = 0
    if (length(script <- existing_r('_render', TRUE))) {
      result = Rscript(c(script, if (quiet) '--quiet'))
    } else if (file.exists('Makefile')) {
      result = system2('make')
    } else {
      render_book('index.Rmd', output_format = output_format, envir = envir)
    }
    if (result != 0) stop('Error ', result, ' attempting to render book')
  }

  # return site generator
  list(
    name = name,
    output_dir = book_dir,
    render = render
  )
}
