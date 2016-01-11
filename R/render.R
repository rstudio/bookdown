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
render_book = function(input, output_format, ..., envir = parent.frame()) {
  if (is.list(output_format)) {
    format = output_format$bookdown_output_format
    if (is.character(format) && (format %in% c('latex', 'html'))) {
      main = merge_rmd(format = format)
    } else main = merge_rmd()
  } else main = merge_rmd()
  on.exit(unlink(main), add = TRUE)
  rmarkdown::render(main, output_format, ..., envir = envir)
}
