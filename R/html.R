merge_rmd = function(files = list.files('.', '[.]Rmd$', ignore.case = TRUE)) {

  config = list()
  if (file.exists('_config.yml')) config = yaml::yaml.load_file('_config.yml')

  if (is.character(config[['rmd_files']])) {
    files = config[['rmd_files']]
  } else {
    files = grep('^[^_]', files, value = TRUE)  # exclude those start with _
    index = match('index', with_ext(files, ''))
    # if there is a index.Rmd, put it in the beginning
    if (!is.na(index)) files = c(files[index], files[-index])
  }

  main = if (is.character(config[['main_rmd']])) {
    config[['main_rmd']][1]
  } else '_main.Rmd'
  files = setdiff(files, main)

  before_chapter = insert_chapter_script(config, 'before')
  after_chapter = insert_chapter_script(config, 'after')

  content = unlist(lapply(files, function(f) {
    x = c(before_chapter, readUTF8(f), after_chapter)
    id = with_ext(f, '')  # base filename (without extension)
    c(x, '', paste0('<!--chapter:end:', id, '-->'), '')
  }))
  writeLines(enc2utf8(content), main, useBytes = TRUE)
  main
}

insert_chapter_script = function(config, where = 'before') {
  script = config[[sprintf('%s_chapter_script', where)]]
  if (is.character(script)) {
    c('```{r include=FALSE}', unlist(lapply(script, readUTF8)), '```')
  }
}

#' Build book chapters into separate HTML files
#'
#' Merge individual R Markdown documents (assuming each document is one chapter)
#' into one main document, render it into HTML, and split it into chapters while
#' updating relative links (e.g. links in TOC, footnotes, citations,
#' figure/table cross-references, and so on). This function is expected to be
#' used in conjunction with \code{\link{render_book}()}. It is almost
#' meaningless if it is used with \code{rmarkdown::render()}.
#' @param toc,number_sections,fig_caption,lib_dir See
#'   \code{rmarkdown::\link[rmarkdown]{html_document}}.
#' @param ... Other arguments to be passed to \code{html_document()}.
#' @export
html_chapters = function(
  toc = TRUE, number_sections = TRUE, fig_caption = TRUE, lib_dir = 'libs', ...
) {
  config = rmarkdown::html_document(
    toc = toc, number_sections = number_sections, fig_caption = fig_caption,
    self_contained = FALSE, lib_dir = lib_dir,
    template = bookdown_file('templates/default.html'), ...
  )
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    split_chapters(output)
  }
  config = set_opts_knit(config)
  config
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
  if (n == 0) return(output)  # no chapters

  html_body[idx] = ''  # remove chapter tokens

  idx = next_nearest(idx, grep('^<div', html_body))
  idx = c(1, idx[-n])

  html_body = resolve_refs_html(html_body, idx, nms)
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

  # return the first chapter (TODO: in theory should return the current chapter)
  paste0(names(chapters)[1], '.html')
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

resolve_refs_html = function(content, lines, filenames) {
  # look for (#fig:label) or (#tab:label) and replace them with Figure/Table x.x
  m = gregexpr('\\(#((fig|tab):[-[:alnum:]]+)\\)', content)
  labs = regmatches(content, m)
  arry = character()  # an array of the form c(label = number, ...)
  cntr = new_counters(c('Figure', 'Table'), length(lines))  # chapter counters
  figs = grep('^<div class="figure"', content)

  for (i in seq_along(labs)) {
    lab = labs[[i]]
    if (length(lab) == 0) next

    j = which.max(lines[lines <= i])
    lab = gsub('^\\(#|\\)$', '', lab)
    type = ifelse(grepl('^fig:', lab), 'Figure', 'Table')
    num = paste0(j, '.', cntr$inc(type, j))
    arry = c(arry, setNames(num, lab))

    if (type == 'Figure') {
      if (length(grep('^<p class="caption">', content[i - 0:1])) == 0) {
        labs[[i]] = character(length(lab))  # remove these labels
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
