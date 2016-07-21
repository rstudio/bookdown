#' @rdname html_document2
#' @export
word_document2 = function(fig_caption = TRUE, md_extensions = NULL, pandoc_args = NULL, ...) {
  from_rmarkdown = getFromNamespace('from_rmarkdown', 'rmarkdown')
  from = from_rmarkdown(fig_caption, md_extensions)

  config = rmarkdown::word_document(
    fig_caption = fig_caption, md_extensions = md_extensions, ...
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
    if (is.null(opts$get('output_dir'))) return(output)
    output2 = output_path(output)
    file.rename(output, output2)
    output2
  }
  config$bookdown_output_format = 'docx'
  config = set_opts_knit(config)
  config
}
