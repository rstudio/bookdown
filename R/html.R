#' Build book chapters into separate HTML files
#'
#' Split the HTML output into chapters while updating relative links (e.g. links
#' in TOC, footnotes, citations, figure/table cross-references, and so on).
#' Functions \code{html_book()} and \code{tufte_html_book()} are simple wrapper
#' functions of \code{html_chapter()} using a specific base output format.
#' @param toc,number_sections,fig_caption,lib_dir,template See
#'   \code{rmarkdown::\link{html_document}}, \code{tufte::\link{tufte_html}}, or
#'   the documentation of the \code{base_format} function.
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
#'   \samp{<!--bookdown:toc:start-->} and \samp{<!--bookdown:toc:end-->} to mar
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
  base_format = rmarkdown::html_document, page_builder = build_chapter,
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
    output2 = split_chapters(output, page_builder, split_by)
    if (!same_path(output, output2)) file.remove(output)
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

#' Output formats that allow numbering and cross-referencing figures/tables
#'
#' These are simple wrappers of the output format functions like
#' \code{rmarkdown::\link{html_document}()}, and they added the capability of
#' numbering figures/tables and cross-referencing them. See References for the
#' syntax. Note you can also cross-reference sections by their ID's using the
#' same syntax as figures/tables.
#' @param ... Arguments to be passed to a specific output format function. For a
#'   function \code{foo2()}, its arguments are passed to \code{foo()}, e.g.
#'   \code{...} of \code{html_document2()} are passed to \code{html_document()}.
#' @param number_sections Whether to number section headers: if \code{TRUE},
#'   figure/table numbers will be of the form \code{X.i}, where \code{X} is the
#'   current first-level section number, and \code{i} is an incremental number
#'   (the i-th figure/table); if \code{FALSE}, figures/tables will be numbered
#'   sequentially in the document from 1, 2, ..., and you cannot cross-reference
#'   section headers in this case.
#' @return An R Markdown output format object to be passed to
#'   \code{rmarkdown::\link{render}()}.
#' @note These function are expected to work with a single R Markdown document
#'   instead of multiple documents of a book, so they are to be passed to
#'   \code{rmarkdown::render()} instead of \code{bookdown::render_book()}. The
#'   functions \samp{tufte_*()} are wrappers of funtions in the \pkg{tufte}
#'   package.
#' @references \url{http://rstudio.github.io/bookdown/figures.html}
#' @export
html_document2 = function(..., number_sections = TRUE) {
  html_document_alt(
    ..., number_sections = number_sections, base_format = rmarkdown::html_document
  )
}

#' @rdname html_document2
#' @export
tufte_html2 = function(..., number_sections = FALSE) {
  html_document_alt(
    ..., number_sections = number_sections, base_format = tufte::tufte_html
  )
}

html_document_alt = function(
  ..., number_sections = TRUE, base_format = rmarkdown::html_document
) {
  config = base_format(..., number_sections = number_sections)
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    x = readUTF8(output)
    writeUTF8(resolve_refs_html(x, global = !number_sections), output)
    output
  }
  config$bookdown_output_format = 'html'
  config = set_opts_knit(config)
  config
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
build_chapter = function(head, toc, chapter, link_prev, link_next, rmd_cur, html_cur, foot) {
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

split_chapters = function(output, build = build_chapter, split_by, ...) {
  x = readUTF8(output)

  i1 = find_token(x, '<!--bookdown:title:start-->')
  i2 = find_token(x, '<!--bookdown:title:end-->')
  i3 = find_token(x, '<!--bookdown:toc:start-->')
  i4 = find_token(x, '<!--bookdown:toc:end-->')
  i5 = find_token(x, '<!--bookdown:body:start-->')
  i6 = find_token(x, '<!--bookdown:body:end-->')

  # no (or not enough) tokens found in the template
  if (any(c(i1, i2, i3, i4, i5, i6) == 0)) {
    x = resolve_refs_html(x)
    x = add_chapter_prefix(x)
    writeUTF8(x, output)
    return(output)
  }

  use_rmd_names = split_by == 'rmd'
  split_level = switch(
    split_by, none = 0, chapter = 1, `chapter+number` = 1,
    section = 2, `section+number` = 2, rmd = 1
  )

  html_head  = x[1:(i1 - 1)]  # HTML header + includes
  html_title = x[(i1 + 1):(i2 - 1)]  # title/author/date
  html_toc   = x[(i3 + 1):(i4 - 1)]  # TOC
  html_body  = x[(i5 + 1):(i6 - 1)]  # body
  html_foot  = x[(i6 + 1):length(x)]  # HTML footer

  r_chap = '^<!--chapter:end:(.+)-->$'
  idx = grep(r_chap, html_body)
  nms = gsub(r_chap, '\\1', html_body[idx])  # to be used in HTML filenames
  n = length(idx)

  html_body = resolve_refs_html(html_body)

  if (!(split_level %in% 0:2)) stop('split_level must be 0, 1, or 2')
  # do not split the HTML file
  if (split_level == 0) {
    html_body[idx] = ''  # remove chapter tokens
    html_body = add_chapter_prefix(html_body)
    writeUTF8(build(
      html_head, html_toc, c(html_title, html_body), NULL, NULL, NULL, output, html_foot, ...
    ), output)
    return(output)
  }
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
    idx2 = if (split_level == 1) h1 else if (split_level == 2) {
      h12 = setNames(c(h1, h2), rep(c('h1', 'h2'), c(length(h1), length(h2))))
      if (length(h12) > 0 && h12[1] != 1) stop(
        'The document must start with a first (#) or second level (##) heading'
      )
      h12 = sort(h12)
      if (length(h12) > 1) {
        n12 = names(h12)
        # h2 that immediately follows h1
        i = h12[n12 == 'h2' & c('h2', head(n12, -1)) == 'h1'] - 1
        # close the h1 section early with </div>
        if (length(i)) html_body[i] = paste(html_body[i], '\n</div>')
        # h1 that immediately follows h2 but not the first h1
        i = n12 == 'h1' & c('h1', head(n12, -1)) == 'h2'
        if (any(i) && n12[1] == 'h2') i[which(n12 == 'h1')[1]] = FALSE
        i = h12[i] - 1
        if (tail(n12, 1) == 'h2' && any(n12 == 'h1')) i = c(i, length(html_body))
        for (j in i) {
          if (html_body[j] != '</div>') warning(
            'Something wrong with the HTML output. The line ', html_body[j],
            ' is supposed to be </div>'
          )
        }
        html_body[i] = paste('<!--', html_body[i], '-->')  # remove the extra </div> of h1
      }
      unname(h12)
    }
    n = length(idx2)
    nms_chaps = if (length(idx)) {
      vapply(idx2, character(1), FUN = function(i) head(nms[idx > i], 1))
    }
    reg_id = '^<div id="([^"]+)".*$'
    reg_num = '^(<h[12]><span class="header-section-number">)([0-9.]+)(</span>.+</h[12]>).*$'
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
  # move HTML files to output dir
  nms2 = output_path(nms)
  i = file.exists(nms) & (nms != nms2)
  file.rename(nms[i], nms2[i])
  nms = nms2

  # find the HTML output file corresponding to the Rmd file passed to render_book()
  if (is.null(input) || length(nms_chaps) == 0) j = 1 else {
    if (is.na(j <- match(input[1], nms_chaps))) j = 1
  }
  nms[j]
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

edit_setting = function() {
  config = load_config()[['edit']]
  if (!is.character(link <- config[['link']])) return()
  if (!grepl('%s', link)) stop('The edit link must contain %s')
  if (!is.character(text <- config[['text']])) text = 'Edit'
  list(link = link, text = text)
}

resolve_refs_html = function(content, global = FALSE) {
  res = parse_fig_labels(content, global)
  content = res$content
  ref_table = c(res$ref_table, parse_section_labels(content))

  # look for @ref(label) and resolve to actual figure/table/section numbers
  m = gregexpr(' @ref\\(([-:[:alnum:]]+)\\)', content)
  refs = regmatches(content, m)
  regmatches(content, m) = lapply(refs, function(ref) {
    if (length(ref) == 0) return(ref)
    ref = gsub('^ @ref\\(|\\)$', '', ref)
    num = ref_table[ref]
    if (any(i <- is.na(num))) {
      warning('The label(s) ', paste(ref[i], collapse = ', '), ' not found', call. = FALSE)
      num[i] = '<strong>??</strong>'
    }
    sprintf(' <a href="#%s">%s</a>', ref, num)
  })
  content
}

reg_chap = '^(<h1><span class="header-section-number">)([0-9]+)(</span>.+</h1>)$'

# parse figure/table labels, and number them either by section numbers (Figure
# 1.1, 1.2, ..., 2.1, ...), or globally (Figure 1, 2, ...)
parse_fig_labels = function(content, global = FALSE) {
  lines = grep(reg_chap, content)
  chaps = gsub(reg_chap, '\\2', content[lines])  # chapter numbers
  arry = character()  # an array of the form c(label = number, ...)
  if (global) chaps = '0'  # Chapter 0 (could be an arbitrary number)
  if (length(chaps) == 0) return(list(content = content, ref_table = arry))

  # look for (#fig:label) or (#tab:label) and replace them with Figure/Table x.x
  m = gregexpr('\\(#((fig|tab):[-[:alnum:]]+)\\)', content)
  labs = regmatches(content, m)
  cntr = new_counters(c('Figure', 'Table'), chaps)  # chapter counters
  figs = grep('^<div class="figure', content)

  for (i in seq_along(labs)) {
    lab = labs[[i]]
    if (length(lab) == 0) next
    if (length(lab) > 1)
      stop('There are multiple labels on one line: ', paste(lab, collapse = ', '))

    j = if (global) chaps else tail(chaps[lines <= i], 1)
    lab = gsub('^\\(#|\\)$', '', lab)
    type = ifelse(grepl('^fig:', lab), 'Figure', 'Table')
    num = cntr$inc(type, j)
    if (!global) num = paste0(j, '.', num)  # Figure X.x
    arry = c(arry, setNames(num, lab))

    if (type == 'Figure') {
      if (length(grep('^<p class="caption', content[i - 0:1])) == 0) {
        # remove these labels, because there must be a caption on this or
        # previous line (possible negative case: the label appears in the alt
        # text of <img>)
        labs[[i]] = character(length(lab))
        next
      }
      labs[[i]] = paste0(type, ' ', num, ': ')
      k = max(figs[figs <= i])
      content[k] = paste0(content[k], sprintf('<span id="%s"></span>', lab))
    } else {
      if (length(grep('^<caption>', content[i - 0:1])) == 0) next
      labs[[i]] = sprintf('<span id="%s">%s</span>', lab, paste0(type, ' ', num, ': '))
    }
  }

  regmatches(content, m) = labs

  # remove labels in figure alt text (it will contain \ like (\#fig:label))
  content = gsub('"\\(\\\\#(fig:[-[:alnum:]]+)\\)', '"', content)

  list(content = content, ref_table = arry)
}

# parse section numbers and labels (id's)
parse_section_labels = function(content) {
  arry = character()
  sec_num = '^<h[1-6]><span class="header-section-number">([.0-9]+)</span>.+</h[1-6]>$'
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

add_chapter_prefix = function(content) {
  config = load_config()
  chapter_name = config[['chapter_name']]
  if (is.null(chapter_name)) return(content)
  chapter_fun = if (is.character(chapter_name)) {
    function(i) paste0(chapter_name, i)
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

# detect and move files to the output directory (if specified)
move_files_html = function(output, lib_dir) {
  if (is.null(o <- opts$get('output_dir'))) return()
  x = readUTF8(output)
  # detect local resources used in HTML
  r = ' (src|href)="([^"]+)"'
  m = gregexpr(r, x)
  f = unlist(lapply(regmatches(x, m), function(z) {
    if (length(z) == 0) z else gsub(r, '\\2', z)
  }))
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
  f = unique(f[file.exists(f)])
  lapply(file.path(o, setdiff(dirname(f), '.')), dir_create)
  file.copy(f, file.path(o, f), overwrite = TRUE)
  # should not need the lib dir any more
  if (length(lib_dir) == 1 && is.character(lib_dir))
    unlink(lib_dir, recursive = TRUE)
}
