#' Convert R Markdown to a PDF book
#'
#' Convert R Markdown files to PDF after resolving the special tokens of
#' \pkg{bookdown} (e.g., the tokens for references and labels) to native LaTeX
#' commands.
#'
#' This function is based on \code{rmarkdown::\link[rmarkdown]{pdf_document}}
#' (by default) with better default arguments. You can also change the default
#' format to other LaTeX/PDF format functions using the \code{base_format}
#' argument.
#'
#' The global R option \code{bookdown.post.latex} can be set to a function to
#' post-process the LaTeX output. This function takes the character vector of
#' the LaTeX output as its input argument, and should return a character vector
#' to be written to the \file{.tex} output file. This gives you full power to
#' post-process the LaTeX output.
#' @param toc,number_sections,fig_caption,pandoc_args See
#'   \code{rmarkdown::\link[rmarkdown]{pdf_document}}, or the documentation of
#'   the \code{base_format} function.
#' @param ... Other arguments to be passed to \code{base_format}.
#' @param base_format An output format function to be used as the base format.
#' @param toc_unnumbered Whether to add unnumbered headers to the table of
#'   contents.
#' @param toc_appendix Whether to add the appendix to the table of contents.
#' @param toc_bib Whether to add the bibliography section to the table of
#'   contents.
#' @param quote_footer If a character vector of length 2 and the quote footer
#'   starts with three dashes (\samp{---}), \code{quote_footer[1]} will be
#'   prepended to the footer, and \code{quote_footer[2]} will be appended; if
#'   \code{NULL}, the quote footer will not be processed.
#' @param highlight_bw Whether to convert colors for syntax highlighting to
#'   black-and-white (grayscale).
#' @note This output format can only be used with \code{\link{render_book}()}.
#' @export
pdf_book = function(
  toc = TRUE, number_sections = TRUE, fig_caption = TRUE, pandoc_args = NULL, ...,
  base_format = rmarkdown::pdf_document, toc_unnumbered = TRUE,
  toc_appendix = FALSE, toc_bib = FALSE, quote_footer = NULL, highlight_bw = FALSE
) {
  config = get_base_format(base_format, list(
    toc = toc, number_sections = number_sections, fig_caption = fig_caption,
    pandoc_args = pandoc_args2(pandoc_args), ...
  ))
  config$pandoc$ext = '.tex'
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    f = with_ext(output, '.tex')
    x = read_utf8(f)
    x = restore_block2(x, !number_sections)
    x = resolve_refs_latex(x)
    x = resolve_ref_links_latex(x)
    x = restore_part_latex(x)
    x = restore_appendix_latex(x, toc_appendix)
    if (!toc_unnumbered) x = remove_toc_items(x)
    if (toc_bib) x = add_toc_bib(x)
    if (!is.null(quote_footer)) {
      if (length(quote_footer) != 2 || !is.character(quote_footer)) warning(
        "The 'quote_footer' argument should be a character vector of length 2"
      ) else x = process_quote_latex(x, quote_footer)
    }
    if (highlight_bw) x = highlight_grayscale_latex(x)
    post = getOption('bookdown.post.latex')
    if (is.function(post)) x = post(x)
    write_utf8(x, f)
    tinytex::latexmk(
      f, config$pandoc$latex_engine,
      if ('--biblatex' %in% config$pandoc$args) 'biber' else 'bibtex'
    )

    output = with_ext(output, '.pdf')
    o = opts$get('output_dir')
    keep_tex = isTRUE(config$pandoc$keep_tex)
    if (!keep_tex) file.remove(f)
    if (is.null(o)) return(output)

    output2 = file.path(o, output)
    file_rename(output, output2)
    if (keep_tex) file_rename(f, file.path(o, f))
    output2
  }
  # always enable tables (use packages booktabs, longtable, ...)
  pre = config$pre_processor
  config$pre_processor = function(...) {
    c(
      if (is.function(pre)) pre(...), '--variable', 'tables=yes', '--standalone',
      if (rmarkdown::pandoc_available('2.7.1')) '-Mhas-frontmatter=false'
    )
  }
  config = common_format_config(config, 'latex')
  config
}

#' @rdname html_document2
#' @export
pdf_document2 = function(...) {
  pdf_book(..., base_format = rmarkdown::pdf_document)
}

#' @rdname html_document2
#' @export
beamer_presentation2 = function(..., number_sections = FALSE) {
  pdf_book(..., base_format = rmarkdown::beamer_presentation)
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
  # equation references \eqref{}
  x = gsub(
    '(?<!\\\\textbackslash{})@ref\\((eq:[-/:[:alnum:]]+)\\)', '\\\\eqref{\\1}', x,
    perl = TRUE
  )
  # normal references \ref{}
  x = gsub(
    '(?<!\\\\textbackslash{})@ref\\(([-/:[:alnum:]]+)\\)', '\\\\ref{\\1}', x,
    perl = TRUE
  )
  x = gsub(sprintf('\\(\\\\#((%s):[-/[:alnum:]]+)\\)', reg_label_types), '\\\\label{\\1}', x)
  x
}

resolve_ref_links_latex = function(x) {
  res = parse_ref_links(x, '^%s (.+)$')
  if (is.null(res)) return(x)
  x = res$content; txts = res$txts; i = res$matches
  # text for a tag may be wrapped into multiple lines; collect them until the
  # empty line
  for (j in seq_along(i)) {
    k = 1
    while (x[i[j] + k] != '') {
      txts[j] = paste(txts[j], x[i[j] + k], sep = '\n')
      x[i[j] + k] = ''
      k = k + 1
    }
  }
  restore_ref_links(x, '(?<!\\\\texttt{)%s', res$tags, txts, FALSE)
}

restore_part_latex = function(x) {
  r = '^\\\\(chapter|section)\\*\\{\\(PART(\\*)?\\)( |$)'
  i = grep(r, x)
  if (length(i) == 0) return(x)
  x[i] = gsub(r, '\\\\part\\2{', x[i])
  # remove (PART*) from the TOC lines for unnumbered parts
  r = '^(\\\\addcontentsline\\{toc\\}\\{)(chapter|section)(\\}\\{)\\(PART\\*\\)( |$)'
  x = gsub(r, '\\1part\\3', x)
  # for numbered parts, remove the line \addcontentsline since it is not really
  # a chapter title and should not be added to TOC
  j = grep('^\\\\addcontentsline\\{toc\\}\\{(chapter|section)\\}\\{\\(PART\\)( |$)', x)
  k = j; n = length(x)
  for (i in seq_along(j)) {
    # figure out how many lines \addcontentsline{toc} spans over (search until
    # it finds an empty line)
    l = 1
    while (j[i] + l <= n && x[j[i] + l] != '') {
      k = c(k, j[i] + l)
      l = l + 1
    }
  }
  if (length(k)) x = x[-k]
  x
}

restore_appendix_latex = function(x, toc = FALSE) {
  r = '^\\\\(chapter|section)\\*\\{\\(APPENDIX\\) .*'
  i = find_appendix_line(r, x)
  if (length(i) == 0) return(x)
  level = gsub(r, '\\1', x[i])
  brace = grepl('}}$', x[i])
  x[i] = '\\appendix'
  if (toc) x[i] = paste(
    x[i], sprintf('\\addcontentsline{toc}{%s}{\\appendixname}', level)
  )
  if (brace) x[i] = paste0(x[i], '}')  # pandoc 2.0
  if (grepl('^\\\\addcontentsline', x[i + 1])) x[i + 1] = ''
  x
}

find_appendix_line = function(r, x) {
  i = grep(r, x)
  if (length(i) > 1) stop('You must not have more than one appendix title')
  i
}

remove_toc_items = function(x) {
  r = '^\\\\addcontentsline\\{toc\\}\\{(part|chapter|section|subsection|subsubsection)\\}\\{.+\\}$'
  x[grep(r, x)] = ''
  x
}

add_toc_bib = function(x) {
  # natbib
  r = '^\\s*\\\\bibliography\\{.+\\}$'
  i = grep(r, x)
  if (length(i) != 0) {
    # natbib - add toc manually using \bibname
    # e.g adding \addcontentsline{toc}{chapter}{\bibname}
    i = i[1]
    level = if (length(grep('^\\\\chapter\\*?\\{', x))) 'chapter' else 'section'
    x[i] = sprintf('%s\n\\addcontentsline{toc}{%s}{\\bibname}', x[i], level)
  } else {
    # biblatex - add heading=bibintoc in options
    # e.g \printbibliography[title=References,heading=bibintoc]
    r = '^(\\s*\\\\printbibliography)(\\[.*\\])?$'
    i = grep(r, x)
    if (length(i) == 0) return(x)
    opts = gsub(r, "\\2", x[i])
    bibintoc = "heading=bibintoc"
    if (nzchar(opts)) {
      opts2 = gsub("^\\[(.*)\\]$", "\\1", opts)
      opts = if (!grepl("heading=", opts2)) sprintf("[%s,%s]", opts2, bibintoc)
    } else (
      opts = sprintf("[%s]", bibintoc)
    )
    x[i] = sprintf('%s%s', gsub(r, "\\1", x[i]), opts)
  }
  x
}

restore_block2 = function(x, global = FALSE) {
  i = grep('^\\\\begin\\{document\\}', x)[1]
  if (is.na(i)) return(x)
  # add the necessary definition in the preamble when block2 engine
  # (\BeginKnitrBlock) or pandoc fenced div (\begin) is used if not already
  # define. But don't do it with beamer and it defines already amsthm
  # environments.
  # An options allow external format to skip this part
  # (useful for rticles see rstudio/bookdown#1001)
  if (getOption("bookdown.theorem.preamble", TRUE) &&
      !knitr::pandoc_to("beamer") &&
      length(grep(sprintf('^\\\\(BeginKnitrBlock|begin)\\{(%s)\\}', paste(all_math_env, collapse = '|')), x)) &&
      length(grep('^\\s*\\\\newtheorem\\{theorem\\}', head(x, i))) == 0) {
    theorem_label = vapply(theorem_abbr, function(a) {
      label_prefix(a)()
    }, character(1), USE.NAMES = FALSE)
    theorem_defs = sprintf(
      '%s\\newtheorem{%s}{%s}%s', theorem_style(names(theorem_abbr)),
      names(theorem_abbr), str_trim(theorem_label),
      if (global) '' else {
        if (length(grep('^\\\\chapter[*]?', x))) '[chapter]' else '[section]'
      }
    )
    # the proof environment has already been defined by amsthm
    proof_envs = setdiff(names(label_names_math2), 'proof')
    proof_labels = vapply(proof_envs, function(a) {
      label_prefix(a, dict = label_names_math2)()
    }, character(1), USE.NAMES = FALSE)
    proof_defs = sprintf(
      '%s\\newtheorem*{%s}{%s}', theorem_style(proof_envs), proof_envs,
      gsub('^\\s+|[.]\\s*$', '', proof_labels)
    )
    x = append(x, c('\\usepackage{amsthm}', theorem_defs, proof_defs), i - 1)
  }
  # remove the empty lines around the block2 environments
  i3 = c(
    if (length(i1 <- grep(r1 <- '^(\\\\)BeginKnitrBlock(\\{)', x)))
      (i1 + 1)[x[i1 + 1] == ''],
    if (length(i2 <- grep(r2 <- '(\\\\)EndKnitrBlock(\\{[^}]+})$', x)))
      (i2 - 1)[x[i2 - 1] == '']
  )
  x[i1] = gsub(r1, '\\1begin\\2', x[i1])
  x[i2] = gsub(r2, '\\1end\\2',   x[i2])
  if (length(i3)) x = x[-i3]

  r = '^(.*\\\\begin\\{[^}]+\\})(\\\\iffalse\\{-)([-0-9]+)(-\\}\\\\fi\\{\\})(.*)$'
  if (length(i <- grep(r, x)) == 0) return(x)
  opts = sapply(strsplit(gsub(r, '\\3', x[i]), '-'), function(z) {
    intToUtf8(as.integer(z))
  }, USE.NAMES = FALSE)
  x[i] = paste0(gsub(r, '\\1', x[i]), opts, gsub(r, '\\5', x[i]))
  x
}

style_definition = c('definition', 'example', 'exercise', 'hypothesis')
style_remark = c('remark')
# which styles of theorem environments to use
theorem_style = function(env) {
  styles = character(length(env))
  styles[env %in% style_definition] = '\\theoremstyle{definition}\n'
  styles[env %in% style_remark] = '\\theoremstyle{remark}\n'
  styles
}

process_quote_latex = function(x, commands) {
  for (i in grep('^\\\\end\\{quote\\}$', x)) {
    i1 = NULL; i2 = i - 1
    k = 1
    while (k < i) {
      xk = x[i - k]
      if (grepl('^---.+', xk)) {
        i1 = i - k
        break
      }
      if (xk == '' || grepl('^\\\\begin', xk)) break
      k = k + 1
    }
    if (is.null(i1)) next
    x[i1] = paste0(commands[1], x[i1])
    x[i2] = paste0(x[i2], commands[2])
  }
  x
}

# \newenvironment{Shaded}{\begin{snugshade}}{\end{snugshade}}
# \newcommand{\KeywordTok}[1]{\textcolor[rgb]{x.xx,x.xx,x.xx}{\textbf{{#1}}}}
# \newcommand{\DataTypeTok}[1]{\textcolor[rgb]{x.xx,x.xx,x.xx}{{#1}}}
# ...
highlight_grayscale_latex = function(x) {
  i1 = grep('^\\\\newenvironment\\{Shaded\\}', x)
  if (length(i1) == 0) return(x)
  i1 = i1[1]
  r1 = '^\\\\newcommand\\{\\\\[a-zA-Z]+\\}\\[1]\\{.*\\{#1\\}.*\\}$'
  r2 = '^(.*?)([.0-9]+,[.0-9]+,[.0-9]+)(.*)$'
  i = i1 + 1
  while (grepl('^\\\\newcommand\\{.+\\}$', x[i])) {
    if (grepl(r1, x[i]) && grepl(r2, x[i])) {
      col = as.numeric(strsplit(gsub(r2, '\\2', x[i]), ',')[[1]])
      x[i] = gsub(
        r2, paste0('\\1', paste(round(rgb2gray(col), 2), collapse = ','), '\\3'),
        x[i]
      )
    }
    i = i + 1
  }
  x
}

# https://en.wikipedia.org/wiki/Grayscale
rgb2gray = function(x, maxColorValue = 1) {
  rep(sum(c(.2126, .7152, .0722) * x/maxColorValue), 3)
}
