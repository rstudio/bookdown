#' Render multiple R Markdown documents into a single output file
#'
#' A wrapper function to merge all R Markdown files under the current working
#' directory, and render the merged R Markdown file. It was mainly designed to
#' be used in the RStudio IDE (specifically, the \code{knit} field in YAML).
#' @param input Ignored. All R Markdown files under the current working
#'   directory are merged as the actual input to
#'   \code{rmarkdown::\link[rmarkdown]{render}()}.
#' @param ... Other arguments to be passed to \code{render()}, such as
#'   \code{encoding}.
#' @note The R Markdown files that start with an underscore \code{_} are ignored
#'   when merging all \file{.Rmd }files.
#' @export
render_book = function(input, ...) {
  # TODO: do we really want to support duplicate chunk labels?
  opts = options(knitr.duplicate.label = 'allow')
  on.exit(options(opts), add = TRUE)

  rmarkdown::render(merge_rmd(), ...)
}
