#' Convert R Markdown to a PDF book
#'
#' Convert R Markdown files to PDF while resolving the special tokens of
#' \pkg{bookdown} (e.g., the tokens for references and labels) to native LaTeX
#' commands.
#'
#' This function is based on \code{rmarkdown::\link{pdf_document}} (by default)
#' with better default arguments. You can also change the default format to
#' other LaTeX/PDF format functions using the \code{base_format} argument.
#' @param toc,number_sections,fig_caption See
#'   \code{rmarkdown::\link{pdf_document}}, or the documentation of the
#'   \code{base_format} function.
#' @param ... Other arguments to be passed to \code{base_format}.
#' @param base_format An output format function to be used as the base format.
#' @export
pdf_book = function(
  toc = TRUE, number_sections = TRUE, fig_caption = TRUE, ...,
  base_format = rmarkdown::pdf_document
) {
  base_format = get_base_format(base_format)
  config = base_format(
    toc = TRUE, number_sections = number_sections, fig_caption = fig_caption, ...
  )
  config$pandoc$ext = '.tex'
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    f = with_ext(output, '.tex')
    x = resolve_refs_latex(readUTF8(f))
    writeUTF8(x, f)
    latexmk(f, config$pandoc$latex_engine)
    unlink(with_ext(output, 'bbl'))  # not sure why latexmk left a .bbl there

    output = with_ext(output, '.pdf')
    o = opts$get('output_dir')
    keep_tex = isTRUE(config$pandoc$keep_tex)
    if (!keep_tex) file.remove(f)
    if (is.null(o)) return(output)

    output2 = file.path(o, output)
    file.rename(output, output2)
    if (keep_tex) file.rename(f, file.path(o, f))
    output2
  }
  config$bookdown_output_format = 'latex'
  config = set_opts_knit(config)
  config
}

#' @rdname html_document2
#' @export
pdf_document2 = function(...) {
  pdf_book(..., base_format = rmarkdown::pdf_document)
}

#' @rdname html_document2
#' @export
tufte_handout2 = function(...) {
  pdf_book(..., base_format = tufte::tufte_handout)
}

#' @rdname html_document2
#' @export
tufte_book2 = function(...) {
  pdf_book(..., base_format = tufte::tufte_book)
}

resolve_refs_latex = function(x) {
  x = gsub(
    '(?<!\\\\textbackslash{})@ref\\(([-:[:alnum:]]+)\\)', '\\\\ref{\\1}', x,
    perl = TRUE
  )
  x = gsub('\\(\\\\#((fig|tab):[-[:alnum:]]+)\\)', '\\\\label{\\1}', x)
  x
}

latexmk = function(...) {
  FUN = getFromNamespace('latexmk', 'rmarkdown')
  FUN(...)
}
