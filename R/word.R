#' @rdname html_document2
#' @export
markdown_document2 = function(
  fig_caption = TRUE, md_extensions = NULL, pandoc_args = NULL, ..., base_format
) {
  from = rmarkdown::from_rmarkdown(fig_caption, md_extensions)

  base_format = get_base_format(base_format)
  config = base_format(
    fig_caption = fig_caption, md_extensions = md_extensions, pandoc_args = pandoc_args, ...
  )
  pre = config$pre_processor
  config$pre_processor = function(metadata, input_file, ...) {
    # Pandoc does not support numbered sections for Word, so figures/tables have
    # to be numbered globally from 1 to n
    process_markdown(input_file, from, pandoc_args, TRUE)
    if (is.function(pre)) pre(metadata, input_file, ...)
  }
  post = config$post_processor
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    move_output(output)
  }
  config$bookdown_output_format = config$pandoc$to
  config = set_opts_knit(config)
  config
}

#' @rdname html_document2
#' @export
word_document2 = function(...) {
  markdown_document2(..., base_format = rmarkdown::word_document)
}

#' @rdname html_document2
#' @export
odt_document2 = function(...) {
  markdown_document2(..., base_format = rmarkdown::odt_document)
}

#' @rdname html_document2
#' @export
powerpoint_presentation2 = function(...) {
  markdown_document2(..., base_format = rmarkdown::powerpoint_presentation)
}
