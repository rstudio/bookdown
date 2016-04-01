library(htmltools)
library(xml2)
options(stringsAsFactors = FALSE)

exclude_urls = c(
  'https://bookdown.org/jjallaire/bookdown/',
  'https://bookdown.org/jjallaire/site/',
  'https://bookdown.org/yihui/bookdown-demo2/',
  'https://bookdown.org/yihui/bookdown-demo3/',
  'https://bookdown.org/yihui/homepage/'
)

book_panels = function(ncol = 3) {
  sitemap = as_list(read_xml('https://bookdown.org/sitemap.xml'))
  meta = lapply(sitemap, function(site) {
    if (length(site) < 2) return()
    loc = unlist(site$loc)
    lastmod = unlist(site$lastmod)
    if (length(loc) * length(lastmod) != 1) return()
    data.frame(url = loc, lastmod = as.POSIXct(strptime(lastmod, '%Y-%m-%dT%H:%M:%SZ', 'UTC')))
  })
  meta = do.call(rbind, meta)
  meta = meta[order(meta$lastmod, decreasing = TRUE), ]

  books = mapply(meta$url, meta$lastmod, FUN = function(url, date) {

    if (url %in% exclude_urls) return()
    if (grepl('/bookdown-demo/$', url) && !grepl('/yihui/', url)) return()

    # cache the scraped data
    if (file.exists('_book_meta.rds')) {
      panels = readRDS('_book_meta.rds')
      if (identical(panels[[url]][['date']], date)) {
        return(panels[[url]][['panel']])
      }
    } else panels = list()

    panel = book_panel(url, date)

    res = list(div(class = paste0('col-sm-', 12 / ncol), panel))
    panels[[url]] = list(date = date, panel = res)
    saveRDS(panels, '_book_meta.rds')
    res

  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  books = unlist(books, recursive = FALSE)
  arrange_panels(books, ncol)
}

# arrange elements in rows
arrange_panels = function(elements, ncol = 3) {
  n = length(elements)
  nrow = ceiling(n / ncol)
  elements = append(elements, rep(list(NULL), nrow * ncol - n))
  rows = list()
  for (i in seq_len(nrow)) {
    rows[[i]] = div(class = 'row row-eq-height', elements[(i -1) * ncol + 1:3])
  }
  tagList(rows)
}

xml_find = function(x, xpath, all = FALSE) {
  FUN = if (all) xml_find_all else xml_find_one
  tryCatch(FUN(x, xpath), error = function(e) NULL)
}

# a single book panel
book_panel = function(url, date) {

  html = read_html(url, encoding = 'UTF-8')
  title = xml_find(html, './/title')
  if (is.null(title)) return()
  title = xml_text(title)
  if (title == '') return()

  cover = xml_find(html, './/meta[@property="og:image"]')
  if (is.null(cover)) {
    description = xml_find(html, './/meta[@name="description"]')
    if (!is.null(description)) description = xml_attr(description, 'content')
  } else {
    description = img(src = xml_attr(cover, 'content'))
  }

  repo = xml_find(html, './/meta[@name="github-repo"]')
  if (!is.null(repo)) repo = xml_attr(repo, 'content')

  author = xml_find(html, './/meta[@name="author"]', all = TRUE)
  if (is.null(author) || length(author) == 0) {
    author = unlist(strsplit(url, '/'))  # https://bookdown.org/user/book
    author = author[length(author) - 1]
  } else {
    author = xml_attr(author, 'content')
    author = paste(author, collapse = ', ')
  }

  div(
    class = 'panel panel-default',
    div(class = 'panel-heading', a(href = url, title)),
    div(
      class = 'panel-body',
      p(github_buttons(repo)),
      p(description)
    ),
    div(
      class = 'panel-footer',
      tags$i(class = 'fa fa-user'), author,
      tags$i(class="fa fa-calendar"), as.Date(date)
    )
  )
}

github_buttons = function(repo = NULL) {
  if (is.null(repo)) return()
  a(
    class = "github-button", href = sprintf("https://github.com/%s", repo),
    `data-count-api` = sprintf("/repos/%s#stargazers_count", repo),
    `data-count-aria-label` = "# stargazers on GitHub",
    `aria-label` = "Star on GitHub", "Star"
  )
  res = strsplit(repo, '/')[[1]]
  if (length(res) != 2) return()
  tags$iframe(
    src = sprintf("https://ghbtns.com/github-btn.html?user=%s&repo=%s&type=star&count=true", res[1], res[2]),
    frameborder = "0", scrolling = "0", width = "120px", height = "20px"
  )
}

