#' Build book chapters into separate HTML files
#'
#' Split the HTML output into chapters while updating relative links (e.g. links
#' in TOC, footnotes, citations, figure/table cross-references, and so on). This
#' function is expected to be used in conjunction with
#' \code{\link{render_book}()}. It is almost meaningless if it is used with
#' \code{rmarkdown::render()}.
#' @param toc,number_sections,fig_caption,lib_dir,template See
#'   \code{rmarkdown::\link{html_document}}, or the documentation of the
#'   \code{base_format} function.
#' @inheritParams pdf_book
#' @param page_builder A function to combine different parts of a chapter into a
#'   page (an HTML character string); it has arguments \code{head} (page
#'   header), \code{toc} (table of contents), \code{chapter} (the chapter body),
#'   \code{link_prev}/ \code{link_next}, (the HTML filename of the previous/next
#'   chapter), \code{rmd_cur} (the current Rmd filename), and \code{foot} (page
#'   footer). See \code{bookdown:::build_chapter} for an example.
#' @note If you want to use a different template, the template must contain
#'   three pairs of HTML comments: \samp{<!--token:title:start-->} and
#'   \samp{<!--token:title:end-->} to mark the title section of the book (this
#'   section will be placed only on the first page of the rendered book);
#'   \samp{<!--token:toc:start-->} and \samp{<!--token:toc:end-->} to mar the
#'   table of contents section (it will be placed on all chapter pages);
#'   \samp{<!--token:body:start-->} and \samp{<!--token:body:end-->} to mark the
#'   HTML body of the book (the HTML body will be split into separate pages for
#'   chapters). You may open the default HTML template
#'   (\code{bookdown:::bookdown_file('templates/default.html')}) to see where
#'   these comments were inserted.
#' @export
html_chapters = function(
  toc = TRUE, number_sections = TRUE, fig_caption = TRUE, lib_dir = 'libs',
  template = bookdown_file('templates/default.html'), ...,
  base_format = rmarkdown::html_document, page_builder
) {
  base_format = get_base_format(base_format)
  if (missing(page_builder)) page_builder = build_chapter
  config = base_format(
    toc = toc, number_sections = number_sections, fig_caption = fig_caption,
    self_contained = FALSE, lib_dir = lib_dir,
    template = template, ...
  )
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    split_chapters(output)
  }
  config$bookdown_output_format = 'html'
  config = set_opts_knit(config)
  config
}

build_chapter = function(head, toc, chapter, link_prev, link_next, rmd_cur, foot) {
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

split_chapters = function(output) {
  x = readUTF8(output)

  i1 = find_token(x, '<!--token:title:start-->')
  i2 = find_token(x, '<!--token:title:end-->')
  i3 = find_token(x, '<!--token:toc:start-->')
  i4 = find_token(x, '<!--token:toc:end-->')
  i5 = find_token(x, '<!--token:body:start-->')
  i6 = find_token(x, '<!--token:body:end-->')

  html_head  = x[1:(i1 - 1)]  # HTML header + includes
  html_title = x[(i1 + 1):(i2 - 1)]  # title/author/date
  html_toc   = x[(i3 + 1):(i4 - 1)]  # TOC
  html_body  = x[(i5 + 1):(i6 - 1)]  # body
  html_foot  = x[(i6 + 1):length(x)]  # HTML footer


  r_chap = '^<!--chapter:end:(.+)-->$'
  idx = grep(r_chap, html_body)
  nms = gsub(r_chap, '\\1', html_body[idx])
  n = length(idx)
  if (n == 0) return(output)  # no chapters

  html_body[idx] = ''  # remove chapter tokens

  idx = next_nearest(idx, grep('^<div', html_body))
  idx = c(1, idx[-n])

  html_body = resolve_refs_html(html_body, idx)
  html_body = add_chapter_prefix(html_body)
  html_toc = restore_links(html_toc, html_body, idx, nms)

  build_chapters = function() {
    if (n == 1) return(setNames(list(html_body), nms))

    res = list()
    for (i in seq_len(n)) {
      i1 = idx[i]
      i2 = if (i == n) length(html_body) else idx[i + 1] - 1
      html = c(if (i == 1) html_title, html_body[i1:i2])
      html = restore_links(html, html_body, idx, nms)
      res[[length(res) + 1]] = build_chapter(
        html_head, html_toc, html,
        sprintf('%s.html', if (i > 1) nms[i - 1]),
        sprintf('%s.html', if (i < n) nms[i + 1]),
        paste0(nms[i], '.Rmd'), html_foot
      )
    }
    setNames(res, nms)
  }

  chapters = build_chapters()
  for (i in names(chapters)) {
    writeLines(enc2utf8(chapters[[i]]), paste0(i, '.html'), useBytes = TRUE)
  }

  # return the first chapter (TODO: in theory should return the current chapter)
  paste0(names(chapters)[1], '.html')
}

find_token = function(x, token) {
  i = which(x == token)
  if (length(i) != 1) stop("Cannot find the unique token '", token, "'")
  i
}

button_link = function(target, text) {
  if (length(target) == 0) target = '#'
  sprintf(
    '<button class="btn btn-default"%s><a href="%s">%s</a></button>',
    if (target == '#') ' disabled' else '', target, text
  )
}

edit_link = function(target) {
  config = load_config()[['edit']]
  if (!is.character(link <- config[['link']])) return()
  if (!grepl('%s', link)) stop('The edit link must contain %s')
  if (!is.character(text <- config[['text']])) text = 'Edit'
  button_link(sprintf(link, target), text)
}

resolve_refs_html = function(content, lines) {
  # look for (#fig:label) or (#tab:label) and replace them with Figure/Table x.x
  m = gregexpr('\\(#((fig|tab):[-[:alnum:]]+)\\)', content)
  labs = regmatches(content, m)
  arry = character()  # an array of the form c(label = number, ...)
  cntr = new_counters(c('Figure', 'Table'), length(lines))  # chapter counters
  figs = grep('^<div class="figure', content)

  for (i in seq_along(labs)) {
    lab = labs[[i]]
    if (length(lab) == 0) next
    if (length(lab) > 1)
      stop('There are multiple labels on one line: ', paste(lab, collapse = ', '))

    j = which.max(lines[lines <= i])
    lab = gsub('^\\(#|\\)$', '', lab)
    type = ifelse(grepl('^fig:', lab), 'Figure', 'Table')
    num = paste0(j, '.', cntr$inc(type, j))
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

  # parse section numbers and labels (id's)
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

  # look for @ref(label) and resolve to actual figure/table/section numbers
  m = gregexpr(' @ref\\(([-:[:alnum:]]+)\\)', content)
  refs = regmatches(content, m)
  regmatches(content, m) = lapply(refs, function(ref) {
    if (length(ref) == 0) return(ref)
    ref = gsub('^ @ref\\(|\\)$', '', ref)
    num = arry[ref]
    if (any(i <- is.na(num))) {
      warning('The label(s) ', paste(ref[i], collapse = ', '), ' not found', call. = FALSE)
      num[i] = '<strong>??</strong>'
    }
    sprintf(' <a href="#%s">%s</a>', ref, num)
  })

  content
}

add_chapter_prefix = function(content) {
  config = load_config()
  chapter_name = config[['chapter_name']]
  if (is.null(chapter_name)) chapter_name = 'Chapter '
  chapter_fun = if (is.character(chapter_name)) {
    function(i) paste0(chapter_name, i)
  } else if (is.function(chapter_name)) chapter_name else {
    stop('chapter_name in _config.yml must be a character string or function')
  }
  r_chap = '^(<h1><span class="header-section-number">)([0-9]+)(</span>.+</h1>)$'
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
        '<a href="%s.html#%s"', filenames[which.max(lines[lines <= a])], links[i]
      )
    }
    x
  })
  segment
}
