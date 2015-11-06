#' @export
pdf_book = function(..., toc = TRUE, number_sections = TRUE, fig_caption = TRUE) {
  config = rmarkdown::pdf_document(
    ..., toc = TRUE, number_sections = number_sections, fig_caption = fig_caption
  )
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    f = with_ext(output, '.tex')
    x = readLines(f, encoding = 'UTF-8', warn = FALSE)
    x = resolve_refs_latex(x)
    writeLines(enc2utf8(x), f, useBytes = TRUE)
    latexmk(f, config$pandoc$latex_engine)
    with_ext(output, '.pdf')
  }
  config = set_opts_knit(config)
  config
}

resolve_refs_latex = function(x) {
  x = gsub('(^| )@ref\\(([-:[:alnum:]]+)\\)', '\\1\\\\ref{\\2}', x)
  x = gsub('\\(\\\\#((fig|tab):[-[:alnum:]]+)\\)', '\\\\label{\\1}', x)
  x
}

latexmk = getFromNamespace('latexmk', 'rmarkdown')
