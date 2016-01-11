#' Convert R Markdown to a PDF book
#'
#' Convert R Markdown files to PDF while resolving the special tokens of
#' \pkg{bookdown} (e.g., the tokens for references and labels) to native LaTeX
#' commands. This function is based on
#' \code{rmarkdown::\link[rmarkdown]{pdf_document}} with better default argument
#' valuess for books.
#' @param toc,number_sections,fig_caption See
#'   \code{rmarkdown::\link[rmarkdown]{pdf_document}}.
#' @param ... Other arguments to be passed to \code{pdf_document()}.
#' @export
pdf_book = function(toc = TRUE, number_sections = TRUE, fig_caption = TRUE, ...) {
  config = rmarkdown::pdf_document(
    toc = TRUE, number_sections = number_sections, fig_caption = fig_caption, ...
  )
  config$pandoc$ext = '.tex'
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    f = with_ext(output, '.tex')
    x = resolve_refs_latex(readUTF8(f))
    writeLines(enc2utf8(x), f, useBytes = TRUE)
    latexmk(f, config$pandoc$latex_engine)
    with_ext(output, '.pdf')
  }
  config$bookdown_output_format = 'latex'
  config = set_opts_knit(config)
  config
}

resolve_refs_latex = function(x) {
  x = gsub('(^| )@ref\\(([-:[:alnum:]]+)\\)', '\\1\\\\ref{\\2}', x)
  x = gsub('\\(\\\\#((fig|tab):[-[:alnum:]]+)\\)', '\\\\label{\\1}', x)
  x
}

latexmk = getFromNamespace('latexmk', 'rmarkdown')
