#' @export
render_book = function(input, ...) {
  # TODO: do we really want to support duplicate chunk labels?
  opts = options(knitr.duplicate.label = 'allow')
  on.exit(options(opts), add = TRUE)

  rmarkdown::render(merge_rmd(), ...)
}
