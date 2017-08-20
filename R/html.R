#' Build book chapters into separate HTML files
#'
#' Split the HTML output into chapters while updating relative links (e.g. links
#' in TOC, footnotes, citations, figure/table cross-references, and so on).
#' Functions \code{html_book()} and \code{tufte_html_book()} are simple wrapper
#' functions of \code{html_chapter()} using a specific base output format.
#' @inheritParams pdf_book
#' @param toc,number_sections,fig_caption,lib_dir,template See
#'   \code{rmarkdown::\link{html_document}},
#'   \code{tufte::\link[tufte]{tufte_html}}, or the documentation of the
#'   \code{base_format} function.
#' @param ... Other arguments to be passed to \code{base_format}. For
#'   \code{html_book()} and \code{tufte_html_book()}, \code{...} is passed to
#'   \code{html_chapters()}.
#' @param split_by How to name the HTML output files from the book: \code{rmd}
#'   uses the base filenames of the input Rmd files to create the HTML
#'   filenames, e.g. generate \file{chapter1.html} for \file{chapter1.Rmd};
#'   \code{none} means do not split the HTML file (the book will be a single
#'   HTML file); \code{chapter} means split the file by the first-level headers;
#'   \code{section} means the second-level headers. For \code{chapter} and
#'   \code{section}, the HTML filenames will be determined by the header ID's,
#'   e.g. the filename for the first chapter with a chapter title \code{#
#'   Introduction} will be \file{introduction.html}; for \code{chapter+number}
#'   and \code{section+number}, the chapter/section numbers will be prepended to
#'   the HTML filenames, e.g. \file{1-introduction.html} and
#'   \file{2-1-literature.html}.
#' @param split_bib Whether to split the bibliography onto separate pages where
#'   the citations are actually used.
#' @param page_builder A function to combine different parts of a chapter into a
#'   page (an HTML character vector). See \code{\link{build_chapter}} for the
#'   specification of this function.
#' @note These functions are expected to be used in conjunction with
#'   \code{\link{render_book}()}. It is almost meaningless if they are used with
#'   \code{rmarkdown::render()}. Functions like \code{\link{html_document2}} are
#'   designed to work with the latter.
#'
#'   If you want to use a different template, the template must contain three
#'   pairs of HTML comments: \samp{<!--bookdown:title:start-->} and
#'   \samp{<!--bookdown:title:end-->} to mark the title section of the book
#'   (this section will be placed only on the first page of the rendered book);
#'   \samp{<!--bookdown:toc:start-->} and \samp{<!--bookdown:toc:end-->} to mark
#'   the table of contents section (it will be placed on all chapter pages);
#'   \samp{<!--bookdown:body:start-->} and \samp{<!--bookdown:body:end-->} to
#'   mark the HTML body of the book (the HTML body will be split into separate
#'   pages for chapters). You may open the default HTML template
#'   (\code{bookdown:::bookdown_file('templates/default.html')}) to see where
#'   these comments were inserted.
#' @return An R Markdown output format object to be passed to
#'   \code{bookdown::render_book()}.
#' @export
html_chapters = function(
  toc = TRUE, number_sections = TRUE, fig_caption = TRUE, lib_dir = 'libs',
  template = bookdown_file('templates/default.html'), ...,
  base_format = rmarkdown::html_document, split_bib = TRUE, page_builder = build_chapter,
  split_by = c('section+number', 'section', 'chapter+number', 'chapter', 'rmd', 'none')
) {
  base_format = get_base_format(base_format)
  config = base_format(
    toc = toc, number_sections = number_sections, fig_caption = fig_caption,
    self_contained = FALSE, lib_dir = lib_dir,
    template = template, ...
  )
  split_by = match.arg(split_by)
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    move_files_html(output, lib_dir)
    output2 = split_chapters(output, page_builder, number_sections, split_by, split_bib)
    if (file.exists(output) && !same_path(output, output2)) file.remove(output)
    output2
  }
  config$bookdown_output_format = 'html'
  config = set_opts_knit(config)
  config
}

#' @rdname html_chapters
#' @export
html_book = function(...) {
  html_chapters(..., base_format = rmarkdown::html_document)
}

#' @rdname html_chapters
#' @export
tufte_html_book = function(...) {
  html_chapters(..., base_format = tufte::tufte_html)
}

#' Output formats that allow numbering and cross-referencing
#' figures/tables/equations
#'
#' These are simple wrappers of the output format functions like
#' \code{rmarkdown::\link{html_document}()}, and they added the capability of
#' numbering figures/tables/equations/theorems and cross-referencing them. See
#' References for the syntax. Note you can also cross-reference sections by
#' their ID's using the same syntax when sections are numbered.
#' @param ...,fig_caption,md_extensions,pandoc_args Arguments to be passed to a
#'   specific output format function. For a function \code{foo2()}, its
#'   arguments are passed to \code{foo()}, e.g. \code{...} of
#'   \code{html_document2()} are passed to \code{rmarkdown::html_document()}.
#' @param number_sections Whether to number section headers: if \code{TRUE},
#'   figure/table numbers will be of the form \code{X.i}, where \code{X} is the
#'   current first-level section number, and \code{i} is an incremental number
#'   (the i-th figure/table); if \code{FALSE}, figures/tables will be numbered
#'   sequentially in the document from 1, 2, ..., and you cannot cross-reference
#'   section headers in this case.
#' @inheritParams pdf_book
#' @return An R Markdown output format object to be passed to
#'   \code{rmarkdown::\link{render}()}.
#' @note These function are expected to work with a single R Markdown document
#'   instead of multiple documents of a book, so they are to be passed to
#'   \code{rmarkdown::render()} instead of \code{bookdown::render_book()}. The
#'   functions \samp{tufte_*()} are wrappers of funtions in the \pkg{tufte}
#'   package.
#' @references \url{https://bookdown.org/yihui/bookdown/}
#' @export
html_document2 = function(
  ..., number_sections = TRUE, base_format = rmarkdown::html_document
) {
  base_format = get_base_format(base_format)
  config = base_format(..., number_sections = number_sections)
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    x = readUTF8(output)
    x = restore_appendix_html(x, remove = FALSE)
    x = restore_part_html(x, remove = FALSE)
    x = resolve_refs_html(x, global = !number_sections)
    writeUTF8(x, output)
    output
  }
  config$bookdown_output_format = 'html'
  config = set_opts_knit(config)
  config
}

#' @rdname html_document2
#' @export
tufte_html2 = function(..., number_sections = FALSE) {
  html_document2(
    ..., number_sections = number_sections, base_format = tufte::tufte_html
  )
}


#' Combine different parts of an HTML page
#'
#' Given the HTML header, body, and footer, etc, build an HTML page.
#'
#' This function is for expert use only. The \code{head} and \code{foot}
#' arguments may not be strictly the HTML header and footer. It depends on the
#' HTML comment tokens in the template (see \code{\link{html_chapters}}).
#' @param head A character vector of the HTML code before the document title.
#' @param toc A character vector of the table of contents.
#' @param chapter The body of a chapter.
#' @param link_prev,link_next The URL of the previous/next chapter (may be
#'   \code{NULL}).
#' @param rmd_cur The Rmd filename of the current chapter (may be \code{NULL}).
#' @param html_cur The HTML filename of the current chapter (may be
#'   \code{NULL}).
#' @param foot A character vector of the HTML code after the chapter body.
#' @export
#' @keywords internal
build_chapter = function(
  head, toc, chapter, link_prev, link_next, rmd_cur, html_cur, foot
) {
  # add a has-sub class to the <li> items that has sub lists
  toc = gsub('^(<li>)(.+<ul>)$', '<li class="has-sub">\\2', toc)
  paste(c(
    head,
    '<div class="row">',
    '<div class="col-sm-12">',
    toc,
    '</div>',
    '</div>',
    '<div class="row">',
    '<div class="col-sm-12">',
    chapter,
    '<p style="text-align: center;">',
    button_link(link_prev, 'Previous'),
    edit_link(rmd_cur),
    button_link(link_next, 'Next'),
    '</p>',
    '</div>',
    '</div>',
    foot
  ), collapse = '\n')
}

split_chapters = function(output, build = build_chapter, number_sections, split_by, split_bib, ...) {

  use_rmd_names = split_by == 'rmd'
  split_level = switch(
    split_by, none = 0, chapter = 1, `chapter+number` = 1,
    section = 2, `section+number` = 2, rmd = 1
  )

  if (!(split_level %in% 0:2)) stop('split_level must be 0, 1, or 2')

  x = readUTF8(output)

  i1 = find_token(x, '<!--bookdown:title:start-->')
  i2 = find_token(x, '<!--bookdown:title:end-->')
  i3 = find_token(x, '<!--bookdown:toc:start-->')
  i4 = find_token(x, '<!--bookdown:toc:end-->')
  i5 = find_token(x, '<!--bookdown:body:start-->')
  i6 = find_token(x, '<!--bookdown:body:end-->')

  r_chap = '^<!--chapter:end:(.+)-->$'
  n = length( grep(r_chap, x) )

  # Need to take care of the div tags here before restore_part_html and
  # restore_appendix_html erase the section ids of the hidden PART or APPENDIX
  # sections.
  if (split_level > 1) {
    body = x[(i5 + 1):(i6 - 1)]
    h1 = grep('^<div (id="[^"]+" )?class="section level1("| )', body) + i5
    h2 = grep('^<div (id="[^"]+" )?class="section level2("| )', body) + i5
    h12 = setNames(c(h1, h2), rep(c('h1', 'h2'), c(length(h1), length(h2))))
    if (length(h12) > 0 && h12[1] != i5 + 1) stop(
      'The document must start with a first (#) or second level (##) heading'
    )
    h12 = sort(h12)
    if (length(h12) > 1) {
      n12 = names(h12)
      # h2 that immediately follows h1
      i = h12[n12 == 'h2' & c('h2', head(n12, -1)) == 'h1'] - 1
      # close the h1 section early with </div>
      if (length(i)) x[i] = paste(x[i], '\n</div>')
      # h1 that immediately follows h2 but not the first h1
      i = n12 == 'h1' & c('h1', head(n12, -1)) == 'h2'
      if (any(i) && n12[1] == 'h2') i[which(n12 == 'h1')[1]] = FALSE
      i = h12[i] - 1
      if (tail(n12, 1) == 'h2' && any(n12 == 'h1')) i = c(i, length(x))
      for (j in i) {
        # the i-th lines should be the closing </div> for h1, or empty
        # because of the appendix h1, which will be removed
        if (!x[j] %in% c('</div>', '')) warning(
          'Something wrong with the HTML output. The line ', x[j],
          ' is supposed to be </div>'
        )
      }
      x[i] = paste('<!--', x[i], '-->')  # remove the extra </div> of h1
    }
  }

  x = add_section_ids(x)
  x = restore_part_html(x)
  x = restore_appendix_html(x)

  # no (or not enough) tokens found in the template
  if (any(c(i1, i2, i3, i4, i5, i6) == 0)) {
    x = resolve_refs_html(x, !number_sections)
    x = add_chapter_prefix(x)
    writeUTF8(x, output)
    return(output)
  }

  html_head  = x[1:(i1 - 1)]  # HTML header + includes
  html_title = x[(i1 + 1):(i2 - 1)]  # title/author/date
  html_toc   = x[(i3 + 1):(i4 - 1)]  # TOC
  html_body  = x[(i5 + 1):(i6 - 1)]  # body
  html_foot  = x[(i6 + 1):length(x)]  # HTML footer

  html_toc = add_toc_ids(html_toc)

  idx = grep(r_chap, html_body)
  nms = gsub(r_chap, '\\1', html_body[idx])  # to be used in HTML filenames
  h1 = grep('^<div (id="[^"]+" )?class="section level1("| )', x)
  if (length(h1) < length(nms)) warning(
    'You have ', length(nms), ' Rmd input file(s) but only ', length(h1),
    ' first-level heading(s). Did you forget first-level headings in certain Rmd files?'
  )

  html_body = resolve_refs_html(html_body, !number_sections)

  # do not split the HTML file
  if (split_level == 0) {
    html_body[idx] = ''  # remove chapter tokens
    html_body = add_chapter_prefix(html_body)
    writeUTF8(build(
      html_head, html_toc, c(html_title, html_body), NULL, NULL, NULL, output, html_foot, ...
    ), output)
    return(move_to_output_dir(output))
  }

  if (split_bib) {
    # parse and remove the references chapter
    res = parse_references(html_body)
    refs = res$refs; html_body = res$html; ref_title = res$title
  }
  # parse and remove footnotes (will reassign them to relevant pages later)
  res = parse_footnotes(html_body)
  fnts = res$items
  if (length(fnts)) html_body[res$range] = ''

  if (use_rmd_names) {
    html_body[idx] = ''
    nms_chaps = nms  # Rmd filenames
    if (n >= 1) {
      idx = next_nearest(idx, grep('^<div', html_body))
      idx = c(1, idx[-n])
    }
  } else {
    h1 = grep('^<div (id="[^"]+" )?class="section level1("| )', html_body)
    h2 = grep('^<div (id="[^"]+" )?class="section level2("| )', html_body)
    idx2 = if (split_level == 1) h1 else if (split_level == 2) sort(c(h1, h2))
    n = length(idx2)
    nms_chaps = if (length(idx)) {
      vapply(idx2, character(1), FUN = function(i) head(nms[idx > i], 1))
    }
    reg_id = '^<div id="([^"]+)".*$'
    reg_num = '^(<h[12]><span class="header-section-number">)([.A-Z0-9]+)(</span>.+</h[12]>).*$'
    nms = vapply(idx2, character(1), FUN = function(i) {
      x1 = html_body[i]; x2 = html_body[i + 1]
      id = if (grepl(reg_id, x1)) gsub(reg_id, '\\1', x1)
      num = if (grepl(reg_num, x2)) gsub(reg_num, '\\2', x2)
      if (is.null(id) && is.null(num)) stop(
        'The heading ', x2, ' must have at least an id or a number'
      )
      nm = if (grepl('[+]number$', split_by)) {
        paste(c(num, id), collapse = '-')
      } else id
      if (is.null(nm)) stop('The heading ', x2, ' must have an id')
      gsub('[^[:alnum:]]+', '-', nm)
    })
    if (anyDuplicated(nms)) stop(
      'Automatically generated filenames contain duplicated ones: ',
      paste(nms[duplicated(nms)], collapse = ', ')
    )
    # generate index.html if the first Rmd filename is index.Rmd
    if (identical(with_ext(head(nms_chaps, 1), ''), 'index')) nms[1] = 'index'
    html_body[idx] = ''
    idx = idx2
  }
  if (n == 0) {
    idx = 1; nms = output; n = 1
  }

  nms = basename(with_ext(nms, '.html'))  # the HTML filenames to be generated
  input = opts$get('input_rmd')
  html_body = add_chapter_prefix(html_body)
  html_toc = restore_links(html_toc, html_body, idx, nms)
  for (i in seq_len(n)) {
    # skip writing the chapter.html if the current Rmd name is not in the vector
    # of Rmd names passed to render_book() (only this vector of Rmd's should be
    # rendered for preview purposes)
    if (isTRUE(opts$get('preview')) && !(nms_chaps[i] %in% input)) {
      if (!file.exists(output_path(nms[i]))) file.create(nms[i])
      next
    }
    i1 = idx[i]
    i2 = if (i == n) length(html_body) else idx[i + 1] - 1
    html = c(if (i == 1) html_title, html_body[i1:i2])
    a_targets = parse_a_targets(html)
    if (split_bib) {
      html = relocate_references(html, refs, ref_title, a_targets)
    }
    html = relocate_footnotes(html, fnts, a_targets)
    html = restore_links(html, html_body, idx, nms)
    html = build(
      html_head, html_toc, html,
      if (i > 1) nms[i - 1],
      if (i < n) nms[i + 1],
      if (length(nms_chaps)) nms_chaps[i],
      nms[i], html_foot, ...
    )
    writeUTF8(html, nms[i])
  }
  nms = move_to_output_dir(nms)

  # find the HTML output file corresponding to the Rmd file passed to render_book()
  if (is.null(input) || length(nms_chaps) == 0) j = 1 else {
    if (is.na(j <- match(input[1], nms_chaps))) j = 1
  }
  nms[j]
}

# move files to output dir if specified
move_to_output_dir = function(files) {
  files2 = output_path(files)
  i = file.exists(files) & (files != files2)
  file.rename(files[i], files2[i])
  files2
}

find_token = function(x, token) {
  i = which(x == token)
  n = length(i)
  if (n == 1) return(i)
  if (n == 0) return(0)
  stop("Cannot find the unique token '", token, "'")
}

button_link = function(target, text) {
  if (length(target) == 0) return()
  sprintf(
    '<a href="%s"><button class="btn btn-default">%s</button></a>', target, text
  )
}

edit_link = function(target) {
  if (length(target) == 0) return()
  setting = edit_setting()
  if (is.null(setting)) return()
  button_link(sprintf(setting$link, target), setting$text)
}

edit_setting = function(config) {
  config_default = load_config()[['edit']]
  if (missing(config) || is.null(config)) config = config_default
  if (is.null(config)) return()
  if (is.character(config)) config = list(link = config, text = NULL)
  if (!is.character(link <- config[['link']])) return()
  if (!grepl('%s', link)) stop('The edit link must contain %s')
  if (!is.character(text <- config[['text']])) text = ui_language('edit')
  list(link = link, text = text)
}

resolve_refs_html = function(content, global = FALSE) {
  content = resolve_ref_links_html(content)

  res = parse_fig_labels(content, global)
  content = res$content
  ref_table = c(res$ref_table, parse_section_labels(content))

  # look for @ref(label) and resolve to actual figure/table/section numbers
  m = gregexpr('(?<!\\\\)@ref\\(([-:[:alnum:]]+)\\)', content, perl = TRUE)
  refs = regmatches(content, m)
  for (i in seq_along(refs)) {
    if (length(refs[[i]]) == 0) next
    # strip off html tags when resolve numbers in <img>'s alt attribute because
    # the numbers may contain double quotes, e.g. <img alt="<a
    # href="#foo">1.2</a>"" width=...
    ref = ref_to_number(refs[[i]], ref_table, FALSE)
    if (is_img_line(content[i])) ref = strip_html(ref)
    refs[[i]] = ref
  }
  regmatches(content, m) = refs
  content
}

is_img_line = function(x) grepl('^<img src=".* alt="', x)

ref_to_number = function(ref, ref_table, backslash) {
  if (length(ref) == 0) return(ref)
  lab = gsub(if (backslash) '^\\\\@ref\\(|\\)$' else '^@ref\\(|\\)$', '', ref)
  # TODO: deprecate the prefix ex: for Examples (use exm: instead)
  if (length(i <- grep('^ex:.+', lab))) {
    warning(
      'Please change the prefix ex: to exm: in label(s) ', knitr::combine_words(lab[i]),
      call. = FALSE
    )
    lab[i] = sub('^ex:', 'exm:', lab[i])
  }
  ref = prefix_section_labels(lab)
  num = ref_table[ref]
  i = is.na(num)
  if (any(i)) {
    if (!isTRUE(opts$get('preview')))
      warning('The label(s) ', paste(lab[i], collapse = ', '), ' not found', call. = FALSE)
    num[i] = '<strong>??</strong>'
  }
  # equation references should include paratheses
  i = grepl('^eq:', ref)
  num[i] = paste0('(', num[i], ')')
  res = sprintf('<a href="#%s">%s</a>', ref, num)
  # do not add relative links to equation numbers in ePub/Word (not implemented)
  ifelse(backslash & i, num, res)
}

# add prefix to section id's
prefix_section_labels = function(labels) {
  prefix = knitr::opts_knit$get("rmarkdown.pandoc.id_prefix")
  is_sec = !grepl(sprintf("^(%s):", reg_label_types), labels)
  labels[is_sec] = paste0(prefix, labels[is_sec])
  labels
}

reg_chap = '^(<h1><span class="header-section-number">)([A-Z0-9]+)(</span>.+</h1>)$'

# default names for labels
label_names = list(fig = 'Figure ', tab = 'Table ', eq = 'Equation ')
# prefixes for theorem environments
theorem_abbr = c(
  theorem = 'thm', lemma = 'lem', definition = 'def', corollary = 'cor',
  proposition = 'prp', example = 'exm', exercise = 'exr'
)
# numbered math environments
label_names_math = setNames(list(
  'Theorem ', 'Lemma ', 'Definition ', 'Corollary ', 'Proposition ', 'Example ', 'Exercise '
), theorem_abbr)
# unnumbered math environments
label_names_math2 = list(proof = 'Proof. ', remark = 'Remark. ', solution = 'Solution. ')
all_math_env = c(names(theorem_abbr), names(label_names_math2))

label_names = c(label_names, label_names_math)

# types of labels currently supported, e.g. \(#fig:foo), \(#tab:bar)
label_types = names(label_names)
reg_label_types = paste(label_types, collapse = '|')
# compatibility with bookdown <= 0.4.7: ex was the prefix for Example; now it's exm
reg_label_types = paste(reg_label_types, 'ex', sep = '|')

# parse figure/table labels, and number them either by section numbers (Figure
# 1.1, 1.2, ..., 2.1, ...), or globally (Figure 1, 2, ...)
parse_fig_labels = function(content, global = FALSE) {
  lines = grep(reg_chap, content)
  chaps = gsub(reg_chap, '\\2', content[lines])  # chapter numbers
  if (length(chaps) == 0) global = TRUE  # no chapter titles or no numbered chapters
  arry = character()  # an array of the form c(label = number, ...)
  if (global) chaps = '0'  # Chapter 0 (could be an arbitrary number)

  content = restore_math_labels(content)

  # look for (#fig:label) or (#tab:label) and replace them with Figure/Table x.x
  m = gregexpr(sprintf('\\(#((%s):[-/[:alnum:]]+)\\)', reg_label_types), content)
  labs = regmatches(content, m)
  cntr = new_counters(label_types, chaps)  # chapter counters
  figs = grep('^<div class="figure', content)
  eqns = grep('<span class="math display">', content)

  for (i in seq_along(labs)) {
    lab = labs[[i]]
    if (length(lab) == 0) next
    if (length(lab) > 1)
      stop('There are multiple labels on one line: ', paste(lab, collapse = ', '))

    j = if (global) chaps else tail(chaps[lines <= i], 1)
    lab = gsub('^\\(#|\\)$', '', lab)
    type = gsub('^([^:]+):.*', '\\1', lab)
    num = arry[lab]
    if (is.na(num)) {
      num = cntr$inc(type, j)  # increment number only if the label has not been used
      if (!global) num = paste0(j, '.', num)  # Figure X.x
    }
    arry = c(arry, setNames(num, lab))

    switch(type, fig = {
      if (length(grep('^<p class="caption', content[i - 0:1])) == 0) {
        # remove these labels, because there must be a caption on this or
        # previous line (possible negative case: the label appears in the alt
        # text of <img>)
        labs[[i]] = character(length(lab))
        next
      }
      labs[[i]] = paste0(label_prefix(type), num, ': ')
      k = max(figs[figs <= i])
      content[k] = paste0(content[k], sprintf('<span id="%s"></span>', lab))
    }, tab = {
      if (length(grep('^<caption', content[i - 0:1])) == 0) next
      labs[[i]] = sprintf(
        '<span id="%s">%s</span>', lab, paste0(label_prefix(type), num, ': ')
      )
    }, eq = {
      labs[[i]] = sprintf('\\tag{%s}', num)
      k = max(eqns[eqns <= i])
      content[k] = sub(
        '(<span class="math display")', sprintf('\\1 id="%s"', lab), content[k]
      )
    }, {
      labs[[i]] = paste0(label_prefix(type), num, ' ')
    })
  }

  regmatches(content, m) = labs

  # remove labels in figure alt text (it will contain \ like (\#fig:label))
  content = gsub('"\\(\\\\#(fig:[-/[:alnum:]]+)\\)', '"', content)

  list(content = content, ref_table = arry)
}


# given a label, e.g. fig:foo, figure out the appropriate prefix
label_prefix = function(type, dict = label_names) i18n('label', type, dict)

ui_names = list(edit = 'Edit', chapter_name = '')
ui_language = function(key, dict = ui_names) i18n('ui', key, ui_names)

i18n = function(group, key, dict = list()) {
  labels = load_config()[['language']][[group]]
  if (is.null(labels[[key]])) dict[[key]] else labels[[key]]
}

sec_num = '^<h[1-6]><span class="header-section-number">([.A-Z0-9]+)</span>.+</h[1-6]>$'

# parse section numbers and labels (id's)
parse_section_labels = function(content) {
  arry = character()
  sec_ids = '^<div id="([^"]+)" class="section .+$'
  for (i in grep(sec_num, content)) {
    if (!grepl(sec_ids, content[i - 1])) next  # no section id
    # extract section number and id, store in the array
    arry = c(arry, setNames(
      sub(sec_num, '\\1', content[i]),
      sub(sec_ids, '\\1', content[i - 1])
    ))
  }
  arry
}

reg_ref_links = '(\\(ref:[-/[:alnum:]]+\\))'
# parse "reference links" of the form "(ref:foo) text", and replace (ref:foo) in
# the content with `text`; this is for figure/table captions that are
# complicated in the sense that they contain special LaTeX/HTML characters (e.g.
# _), or special Markdown syntax (e.g. citations); we can just use (ref:foo) in
# the captions, and write the actual captions elsewhere using (ref:foo) text
resolve_ref_links_html = function(x) {
  res = parse_ref_links(x, '^<p>%s (.+)</p>$')
  if (is.null(res)) return(x)
  restore_ref_links(res$content, '(?<!code>)%s', res$tags, res$txts, TRUE)
}

parse_ref_links = function(x, regexp) {
  r = sprintf(regexp, reg_ref_links)
  if (length(i <- grep(r, x)) == 0) return()
  tags = gsub(r, '\\1', x[i])
  txts = gsub(r, '\\2', x[i])
  if (any(k <- duplicated(tags))) {
    warning('Possibly duplicated text reference labels: ', paste(tags[k], collapse = ', '))
    k = !k
    tags = tags[k]
    txts = txts[k]
    i = i[k]
  }
  x[i] = ''
  list(content = x, tags = tags, txts = txts, matches = i)
}

restore_ref_links = function(x, regexp, tags, txts, alt = TRUE) {
  r = sprintf(regexp, reg_ref_links)
  m = gregexpr(r, x, perl = TRUE)
  tagm = regmatches(x, m)
  for (i in seq_along(tagm)) {
    tag = tagm[[i]]
    if (length(tag) == 0) next
    k = match(tag, tags)
    tag[!is.na(k)] = txts[na.omit(k)]
    if (alt && is_img_line(x[i])) tag = strip_html(tag)
    tagm[[i]] = tag
  }
  regmatches(x, m) = tagm
  x
}

# add automatic identifiers to those section headings without ID's
add_section_ids = function(content) {
  r = '^(<div)( class="section level[1-6].+)$'
  for (i in grep(r, content)) {
    if (grepl('id=".+"', content[i])) next  # the id exists
    h = content[i + 1]
    # use section number as ID if section is numbered, otherwise use the raw
    # values of the heading text
    if (grepl(sec_num, h)) {
      id = gsub(sec_num, 'section-\\1', h)
    } else {
      id = as.character(charToRaw(gsub('^<h[1-6]>|</h[1-6]>$', '', h)))
      id = paste(id, collapse = '')
    }
    content[i] = gsub(r, paste0('\\1 id="', id, '"\\2'), content[i])
  }
  content
}

# add identifiers to TOC
add_toc_ids = function(toc) {
  # use section numbers as ID's
  r = '^(<li><a)(><span class="toc-section-number">)([.A-Z0-9]+)(</span>.+</a>.*)$'
  for (i in grep(r, toc)) {
    toc[i] = gsub(r, '\\1 href="#section-\\3"\\2\\3\\4', toc[i])
  }
  # use raw vectors as ID's
  r = '^(<li><a)(>)(.+)(</a>.*)$'
  for (i in grep(r, toc)) {
    id = as.character(charToRaw(gsub(r, '\\3', toc[i])))
    id = paste(id, collapse = '')
    toc[i] = gsub(r, paste0('\\1 href="#', id, '"\\2\\3\\4'), toc[i])
  }
  toc
}

add_chapter_prefix = function(content) {
  config = load_config()
  chapter_name = config[['chapter_name']] %n% ui_language('chapter_name')
  if (is.null(chapter_name) || identical(chapter_name, '')) return(content)
  chapter_fun = if (is.character(chapter_name)) {
    function(i) switch(
      length(chapter_name), paste0(chapter_name, i),
      paste0(chapter_name[1], i, chapter_name[2]),
      stop('chapter_name must be of length 1 or 2')
    )
  } else if (is.function(chapter_name)) chapter_name else {
    stop('chapter_name in _bookdown.yml must be a character string or function')
  }
  r_chap = '^(<h1><span class="header-section-number">)([0-9]+)(</span>.+</h1>.*)$'
  for (i in grep(r_chap, content)) {
    h = content[i]
    x1 = gsub(r_chap, '\\1', h)
    x2 = gsub(r_chap, '\\2', h)
    x3 = gsub(r_chap, '\\3', h)
    content[i] = paste0(x1, chapter_fun(x2), x3)
  }
  content
}

restore_links = function(segment, full, lines, filenames) {
  # if there is only one chapter in the HTML in total, no need to restore links
  if (length(lines) <= 1) return(segment)
  r = '<a href="#([^"]+)"'
  m = gregexpr(r, segment)
  regmatches(segment, m) = lapply(regmatches(segment, m), function(x) {
    if (length(x) == 0) return(x)
    links = gsub(r, '\\1', x)
    for (i in seq_along(links)) {
      a = grep(sprintf(' id="%s"', links[i]), full, fixed = TRUE)
      if (length(a) == 0) next
      a = a[1]
      x[i] = sprintf(
        '<a href="%s#%s"', filenames[which.max(lines[lines <= a])], links[i]
      )
    }
    x
  })
  segment
}

restore_part_html = function(x, remove = TRUE) {
  i = grep('^<h1>\\(PART\\*?\\) .+</h1>$', x)
  if (length(i) == 0) return(x)
  i = i[grep('^<div .*class=".*unnumbered.*">$', x[i - 1])]
  if (remove) {
    x[i] = x[i - 1] = x[i + 1] = ''
  } else {
    k = grepl('^<h1>\\(PART\\*\\) .+</h1>$', x[i])  # unnumbered parts
    i1 = i[k]; i2 = i[!k]
    x[i1] = gsub('<h1>\\(PART\\*\\)', '<h1>', x[i1])
    x[i2] = mapply(
      gsub, '<h1>\\(PART\\)', x = x[i2],
      sprintf('<h1><span class="header-section-number">%s</span>', as.roman(seq_along(i2)))
    )
  }

  r = '^<li><a href="[^"]*">\\(PART\\*\\) (.+)</a>(.+)$'
  i = grep(r, x)
  x[i] = gsub(r, '<li class="part"><span><b>\\1</b></span>\\2', x[i])

  r = '^<li><a href="[^"]*">\\(PART\\) (.+)</a>(.+)$'
  i = grep(r, x)
  if (length(i) == 0) return(x)
  x[i] = mapply(
    gsub, r, x = x[i],
    sprintf('<li class="part"><span><b>%s \\1</b></span>\\2', as.roman(seq_along(i)))
  )
  x
}

# process the appendix "chapter" in the body, and change the numbering style in
# the appendices (also change in TOC), e.g. A.1, A.2, B.1, ...
restore_appendix_html = function(x, remove = TRUE) {
  r = '^(<h1>)\\(APPENDIX\\) (.+)(</h1>)$'
  i = find_appendix_line(r, x)
  if (length(i) == 0) return(x)
  # keep or remove the appendix heading in body
  if (remove) {
    x[i] = x[i - 1] = x[i + 1] = ''
  } else x[i] = gsub(r, '\\1\\2\\3', x[i])
  x = number_appendix(x, i + 1, length(x), 'header')
  r = '^<li><a href="[^"]*">\\(APPENDIX\\) (.+)</a>(.+)$'
  i = find_appendix_line(r, x)
  if (length(i) == 0) return(x)
  # remove link on (APPENDIX) in the TOC item
  x[i] = gsub(r, '<li class="appendix"><span><b>\\1</b></span>\\2', x[i])
  x = number_appendix(x, i + 1, next_nearest(i, which(x == '</div>')), 'toc')
  x
}

# parse reference items so we can move them back to the chapter where they were used
parse_references = function(x) {
  i = which(x == '<div id="refs" class="references">')
  if (length(i) != 1) return(list(refs = character(), html = x))
  r = '^<div id="(ref-[^"]+)">$'
  k = grep(r, x)
  k = k[k > i]
  n = length(k)
  if (n == 0) return(list(refs = character(), html = x))

  ids = gsub(r, '\\1', x[k])
  ref = x[k + 1]
  # replace 3 em-dashes with author names
  dashes = paste0('^<p>', intToUtf8(rep(8212, 3)), '[.]')
  for (j in grep(dashes, ref)) {
    ref[j] = sub(dashes, sub('^([^.]+[.])( .+)$', '\\1', ref[j - 1]), ref[j])
  }
  ref = paste(x[k], ref, x[k + 2], sep = '\n')  # add <div id=ref-...></div>
  title = if (grepl('^<h1[^>]*>', x[i - 2]) && grepl('^<div ', x[i - 3]))
    gsub('<span class="header-section-number">[.0-9]+</span>', '', x[i - 2])
  x[k] = '<div>'  # remove the div id's

  list(refs = setNames(ref, ids), html = x, title = title)
}

# move references back to the relevant chapter
relocate_references = function(x, refs, title, ids) {
  if (length(refs) == 0) return(x)
  ids = intersect(ids, names(refs))
  if (length(ids) == 0) return(x)
  title = if (is.null(title)) '<h3>References</h3>' else gsub('h1>', 'h3>', title)
  c(x, title, '<div id="refs" class="references">', refs[ids], '</div>')
}

# extract relative links from text
parse_a_targets = function(x) {
  r = '<a href="#([^"]+)"'
  unlist(lapply(regmatches(x, gregexpr(r, x)), function(target) {
    if (length(target) == 0) return()
    gsub(r, '\\1', target)
  }))
}

# parse footnotes in the div of class "footnotes"; each footnote is one <li>
# with id fnX and a link back to the text
parse_footnotes = function(x) {
  i = which(x == '<div class="footnotes">')
  if (length(i) == 0) return(list(items = character(), range = integer()))
  j = which(x == '</div>')
  j = min(j[j > i])
  n = length(x)
  r = '<li id="fn([0-9]+)"><p>.+?<a href="#fnref\\1">.</a></p></li>'
  s = paste(x[i:n], collapse = '')
  items = unlist(regmatches(s, gregexpr(r, s)))
  list(items = setNames(items, gsub(r, 'fn\\1', items)), range = i:j)
}

# move footnotes to the relevant page
relocate_footnotes = function(x, notes, ids) {
  if (length(notes) == 0) return(x)
  ids = intersect(ids, names(notes))
  if (length(ids) == 0) return(x)
  c(
    x, '<div class="footnotes">', '<hr />',
    sprintf('<ol start="%s">', gsub('^fn', '', ids[1])), notes[ids],
    '</ol>', '</div>'
  )
}

number_appendix = function(x, i1, i2, type = c('toc', 'header')) {
  r = sprintf(
    '^(<%s>.*<span class="%s-section-number">)([.0-9]+)(</span>.+)',
    if (type == 'toc') 'li' else 'h[1-6]', type
  )
  d = list()  # a dictionary e.g. list(12 = 'A', 13 = 'B', ...)
  i = i1:i2
  for (j in i[grep(r, x[i])]) {
    s1 = gsub(r, '\\1', x[j])
    s2 = gsub(r, '\\2', x[j])
    s3 = gsub(r, '\\3', x[j])
    s = strsplit(s2, '[.]')[[1]]  # section numbers
    if (is.null(d[[s[1]]])) d[[s[1]]] = LETTERS[length(d) + 1]
    s[1] = d[[s[1]]]
    if (is.na(s[1])) stop('Too many chapters in the appendix (more than 26)')
    x[j] = paste0(s1, paste(s, collapse = '.'), s3)
  }
  x
}

# detect and move files to the output directory (if specified)
move_files_html = function(output, lib_dir) {
  if (is.null(o <- opts$get('output_dir'))) return()
  x = readUTF8(output)
  # detect local resources used in HTML
  r = '[- ](src|href)="([^"]+)"'
  m = gregexpr(r, x)
  f = unlist(lapply(regmatches(x, m), function(z) {
    if (length(z) == 0) z else gsub(r, '\\2', z)
  }))
  f = c(f, parse_cover_image(x))
  f = local_resources(unique(f[file.exists(f)]))
  # detect resources in CSS
  css = lapply(grep('[.]css$', f, ignore.case = TRUE, value = TRUE), function(z) {
    d = dirname(z)
    z = readUTF8(z)
    r = 'url\\("?([^")]+)"?\\)'
    lapply(regmatches(z, gregexpr(r, z)), function(s) {
      s = local_resources(gsub(r, '\\1', s))
      file.path(d, s)
    })
  })
  f = c(f, unlist(css))
  f = gsub('[?#].+$', '', f)  # strip the #/? part in links, e.g. a.html#foo
  f = gsub('^[.]/', '', f)  # strip the initial ./, e.g. ./foo.png -> foo.png
  f = f[f != '']
  f = f[!knitr:::is_abs_path(f)]
  if (getOption('bookdown.js.debug', FALSE)) f = c(f, js_min_sources(f))
  # add leaflet images if any used
  f = c(f, grep('png$', list.files(
    list.files(lib_dir, '^leaflet', full.names = TRUE),
    full.names = TRUE, recursive = TRUE
  ), value = TRUE))
  f = unique(f[file_test('-f', f)])
  lapply(file.path(o, setdiff(dirname(f), '.')), dir_create)
  f2 = file.path(o, f)
  i = !same_path(f, f2, mustWork = FALSE)
  if (any(i)) file.copy(f[i], f2[i], overwrite = TRUE)
  # should not need the lib dir any more
  if (length(lib_dir) == 1 && is.character(lib_dir))
    unlink(lib_dir, recursive = TRUE)
}

# parse the cover image from HTML meta
parse_cover_image = function(x) {
  r = '^\\s*<meta property="og:url" content="([^"]+)" />\\s*$'
  i = grep(r, x)
  if (length(i) == 0) return()
  u = gsub(r, '\\1', x[i[1]])  # URL
  r = '^\\s*<meta property="og:image" content="([^"]+)" />\\s*$'
  i = grep(r, x)
  if (length(i) == 0) return()
  m = gsub(r, '\\1', x[i[1]])  # cover image
  m = gsub(u, '', m, fixed = TRUE)
  m
}

# source js files and map files
js_min_sources = function(x) {
  r = '[.]min[.]js$'
  x = grep(r, x, value = TRUE)
  c(gsub(r, '.js', x), gsub(r, '.min.map', x))
}

# only parse equation labels (\#eq:label) in math environments; this needs
# special treatment because the backslash \ before # is preserved in equation
# environments in HTML output, whereas it is removed in normal paragraphs
restore_math_labels = function(x) {
  i1 = grep('^(<[a-z]+>)?<span class="math display">\\\\\\[', x)
  i2 = grep('\\\\\\]</span>(</[a-z]+>)?$', x)
  n1 = length(i1); n2 = length(i2)
  if (n1 * n2 == 0) return(x)
  i2 = next_nearest(i1, i2, TRUE)
  if (any(is.na(i2))) {
    # retry without assuming the closing \]</span> is only followed by an optional </tag>
    i2 = grep('\\\\\\]</span>(</[a-z]+>)?', x)
    i2 = next_nearest(i1, i2, TRUE)
    if (any(is.na(i2))) {
      warning(
        "There seems to be problems with math expressions of the display style. ",
        "Labels of these expressions will not be processed."
      )
      return(x)
    }
  }
  i = unlist(mapply(seq, i1, i2, SIMPLIFY = FALSE))
  # remove \ before #
  x[i] = gsub('\\(\\\\(#eq:[-/[:alnum:]]+)\\)', '(\\1)', x[i])
  x
}
