#' The GitBook output format
#'
#' This output format function ported a style provided by GitBook
#' (\url{https://www.gitbook.com}) for R Markdown.
#' @inheritParams html_chapters
#' @param fig_caption,lib_dir,... Arguments to be passed to
#'   \code{rmarkdown::\link{html_document}()} (\code{...} not including
#'   \code{toc}, \code{number_sections}, \code{self_contained}, \code{theme},
#'   and \code{template}).
#' @export
gitbook = function(
  fig_caption = TRUE, lib_dir = 'libs', ..., use_rmd_names = FALSE, split_level = 2
) {
  html_document2 = function(..., extra_dependencies = list()) {
    rmarkdown::html_document(
      ..., extra_dependencies = c(extra_dependencies, gitbook_dependency())
    )
  }
  config = html_document2(
    toc = TRUE, number_sections = TRUE, fig_caption = fig_caption,
    self_contained = FALSE, lib_dir = lib_dir, theme = NULL,
    template = bookdown_file('templates', 'gitbook.html'), ...
  )
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    on.exit(write_search_data(), add = TRUE)
    move_files_html(output, lib_dir)
    split_chapters(output, gitbook_page, use_rmd_names, split_level)
  }
  config$bookdown_output_format = 'html'
  config = set_opts_knit(config)
  config
}

gitbook_search = local({
  data = NULL
  list(
    get = function() data,
    collect = function(...) data <<- c(data, ...),
    empty = function() data <<- NULL
  )
})

write_search_data = function(x) {
  x = gitbook_search$get()
  if (length(x) == 0) return()
  gitbook_search$empty()
  x = matrix(json_string(strip_html(x)), nrow = 3)
  x = apply(x, 2, paste, collapse = ',')
  x = paste0('[\n', paste0('[', x, ']', collapse = ',\n'), '\n]')
  json_file = 'search_index.json'
  writeUTF8(x, json_file)
  if (!is.null(o <- opts$get('output_dir')))
    file.rename(json_file, file.path(o, json_file))
}

gitbook_dependency = function() {
  assets = bookdown_file('templates', 'gitbook')
  owd = setwd(assets); on.exit(setwd(owd), add = TRUE)
  list(htmltools::htmlDependency(
    'gitbook', '2.6.7', src = assets,
    stylesheet = file.path('css', c(
      'style.css', 'plugin-highlight.css', 'plugin-search.css',
      'plugin-fontsettings.css'
    )),
    script = file.path('js', c(
      'app.js', 'lunr.js', 'plugin-search.js', 'plugin-sharing.js',
      'plugin-fontsettings.js', 'plugin-bookdown.js'
    ))
  ))
}

gitbook_page = function(head, toc, chapter, link_prev, link_next, rmd_cur, html_cur, foot) {
  toc = gitbook_toc(toc, rmd_cur)

  has_prev = length(link_prev) > 0
  has_next = length(link_next) > 0
  a_prev = if (has_prev) sprintf(
    '<a href="%s" class="navigation navigation-prev %s" aria-label="Previous page"><i class="fa fa-angle-left"></i></a>',
    link_prev, if (has_next) '' else 'navigation-unique'
  ) else ''
  a_next = if (has_next) sprintf(
    '<a href="%s" class="navigation navigation-next %s" aria-label="Next page""><i class="fa fa-angle-right"></i></a>',
    link_next, if (has_prev) '' else 'navigation-unique'
  ) else ''
  foot = sub('<!--bookdown:link_prev-->', a_prev, foot)
  foot = sub('<!--bookdown:link_next-->', a_next, foot)

  l_prev = if (has_prev) sprintf('<link rel="prev" href="%s">', link_prev) else ''
  l_next = if (has_next) sprintf('<link rel="next" href="%s">', link_next) else ''
  head = sub('<!--bookdown:link_prev-->', l_prev, head)
  head = sub('<!--bookdown:link_next-->', l_next, head)

  # gitbook JS scripts only work after the DOM has been loaded, so move them
  # from head to foot
  i = grep('^\\s*<script src=".+/gitbook([^/]+)?/js/[a-z-]+[.]js"></script>\\s*$', head)
  s = head[i]; head[i] = ''
  j = grep('^\\s*<script>\\s*$', foot)[1]
  foot[j] = paste(c(s, foot[j]), collapse = '\n')

  titles = paste(grep('^<(h[12])(>| ).+</\\1>.*$', chapter, value = TRUE), collapse = ' ')
  gitbook_search$collect(html_cur, titles, paste(chapter, collapse = ' '))

  e_link = '/[*] bookdown:edit:link [*]/'
  e_text = '/[*] bookdown:edit:text [*]/'

  if (length(rmd_cur) && is.list(setting <- edit_setting())) {
    foot = sub(e_link, json_string(sprintf(setting$link, rmd_cur)), foot)
    foot = sub(e_text, json_string(setting$text), foot)
  } else {
    foot = sub(e_link, 'null', foot)
    foot = sub(e_text, 'null', foot)
  }

  c(head, toc, chapter, foot)
}

gitbook_toc = function(x, cur) {
  i1 = find_token(x, '<!--bookdown:toc2:start-->')
  i2 = find_token(x, '<!--bookdown:toc2:end-->')
  x[i1] = ''; x[i2] = ''
  if (i2 - i1 < 2) return(x)
  toc = x[(i1 + 1):(i2 - 1)]
  if (toc[1] == '<ul>') {
    toc[1] = '<ul class="summary">'
    if (!is.null(extra <- gitbook_toc_extra('before'))) {
      toc[1] = paste(c(toc[1], extra, '<li class="divider"></li>'), collapse = '\n')
    }
  }
  n = length(toc)
  if (toc[n] == '</ul>') {
    if (!is.null(extra <- gitbook_toc_extra('after'))) {
      toc[n] = paste(c('<li class="divider"></li>', extra, toc[n]), collapse = '\n')
    }
  }
  r = '^<li><a href="([^#]*)(#[^"]+)"><span class="toc-section-number">([0-9.]+)</span>([^<]+)(</a>.*)$'
  i = grep(r, toc)
  toc[i] = gsub(
    r,
    '<li class="chapter" data-level="\\3" data-path="\\1"><a href="\\1\\2"><i class="fa fa-check"></i><b>\\3</b>\\4\\5',
    toc[i]
  )
  toc[i] = sub(' data-path="">', paste0(' data-path="', with_ext(cur, '.html'), '">'), toc[i])
  x[(i1 + 1):(i2 - 1)] = toc
  x
}

gitbook_toc_extra = function(which = c('before', 'after')) {
  which = match.arg(which)
  config = load_config()
  config[[sprintf('gitbook_toc_%s', which)]]
}
