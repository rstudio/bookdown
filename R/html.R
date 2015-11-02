merge_rmd = function(files = list.files('.', '[.]Rmd$', ignore.case = TRUE)) {
  files = grep('^[^_]', files, value = TRUE)  # exclude those start with _
  index = match('index', with_ext(files, ''))
  # if there is a index.Rmd, put it in the beginning
  if (!is.na(index)) files = c(files[index], files[-index])
  content = unlist(lapply(files, function(f) {
    x = readLines(f, warn = FALSE, encoding = 'UTF-8')
    id = with_ext(f, '')  # base filename (without extension)
    c(x, '', paste0('<!--chapter:end:', id, '-->'), '')
  }))
  main = '_full_book.Rmd'
  writeLines(enc2utf8(content), main, useBytes = TRUE)
  main
}

#' Build book chapters into separate HTML files
#'
#' Merge individual R Markdown documents (assuming each document is one chapter)
#' into one main document, render it into HTML, and split it into chapters while
#' updating relative links (e.g. links in TOC, footnotes, citations,
#' figure/table cross-references, and so on).
#' @param main The main R Markdown document (by default, a document that is the
#'   combination of all R Markdown documents under the current working
#'   directory).
#' @param toc,number_sections,lib_dir See
#'   \code{rmarkdown::\link[rmarkdown]{html_document}}.
#' @export
build_html_chapters = function(
  main = merge_rmd(), toc = TRUE, number_sections = TRUE, lib_dir = 'libs'
) {
  # TODO: do we really want to support duplicate chunk labels?
  opts = options(knitr.duplicate.label = 'allow')
  on.exit(options(opts), add = TRUE)
  out = rmarkdown::render(
    main, rmarkdown::html_document(
      toc = toc, number_sections = number_sections, self_contained = FALSE,
      lib_dir = lib_dir, template = bookdown_file('templates/default.html')
    ),
    encoding = 'UTF-8', envir = globalenv()
  )

  x = readLines(out, encoding = 'UTF-8')

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

  one_chapter = function(chapter, chap_prev, chap_next) {
    paste(c(
      html_head,
      '<div class="row">',
      '<div class="col-sm-4 well">',
      html_toc,
      '</div>',
      '<div class="col-sm-8">',
      chapter,
      '<div style="text-align: center;">',
      button_link(chap_prev, 'Previous'),
      button_link(chap_next, 'Next'),
      '</div>',
      '</div>',
      '</div>',
      html_foot
    ), collapse = '\n')
  }

  r_chap = '^<!--chapter:end:(.+)-->$'
  idx = grep(r_chap, html_body)
  nms = gsub(r_chap, '\\1', html_body[idx])
  n = length(idx)

  html_body[idx] = ''  # remove chapter tokens

  idx = next_nearest(idx, grep('^<div', html_body))
  idx = c(1, idx[-n])

  html_toc = restore_links(html_toc, html_body, idx, nms)

  build_chapters = function() {
    if (n == 1) return(setNames(list(html_body), nms))

    res = list()
    for (i in seq_len(n)) {
      i1 = idx[i]
      i2 = if (i == n) length(html_body) else idx[i + 1] - 1
      html = c(if (i == 1) html_title, html_body[i1:i2])
      html = restore_links(html, html_body, idx, nms)
      res[[length(res) + 1]] = one_chapter(
        html,
        if (i > 1) nms[i - 1], if (i < n) nms[i + 1]
      )
    }
    setNames(res, nms)
  }

  chapters = build_chapters()
  for (i in names(chapters)) {
    writeLines(enc2utf8(chapters[[i]]), paste0(i, '.html'), useBytes = TRUE)
  }
}

find_token = function(x, token) {
  i = which(x == token)
  if (length(i) != 1) stop("Cannot find the unique token '", token, "'")
  i
}

button_link = function(target, text) {
  target = if (is.null(target)) '#' else paste0(target, '.html')
  sprintf(
    '<a href="%s" class="btn btn-default"%s>%s</a>',
    target, if (target == '#') ' disabled' else '', text
  )
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
