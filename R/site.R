
# Generic site generation function for all bookdown HTML formats
bookdown_site <- function(input, ...) {

  on.exit(opts$restore(), add = TRUE)

  # Custom render function. bookdown rendering is more complex than
  # for most formats and as a result uses a custom R script (_render.R)
  # or Makefile to define what's required to render the book.
  render <- function(output_format = NULL,
                     quiet = FALSE,
                     encoding = getOption("encoding"),
                     ...) {
    if (length(script <- existing_r('_render', TRUE))) {
      result = Rscript(script)
    } else if (file.exists('Makefile')) {
      result = system2('make')
    } else stop('Site rendering requires a _render.R or Makefile')
    if (result != 0) stop('Error ', result, ' attempting to render book')
  }

  # return site generator
  list(
    name = book_filename(),
    output_dir = output_dirname('_book'),
    render = render
  )
}

#' R Markdown site generators for bookdown
#'
#' Implementation of custom R Markdown site generators for bookdown.
#'
#' @inheritParams rmarkdown::render_site
#'
#' @name site_generators
#' @export
gitbook_site <- bookdown_site

#' @rdname site_generators
#' @export
html_book_site <- bookdown_site

#' @rdname site_generators
#' @export
tufte_html_book_site <- bookdown_site

