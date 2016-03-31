library(htmltools)
library(xml2)
options(stringsAsFactors = FALSE)

exclude_urls = c(
  'https://bookdown.org/jjallaire/bookdown/',
  'https://bookdown.org/jjallaire/site/',
  'https://bookdown.org/yihui/homepage/'
)

book_listing = function() {
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

  # function to yield the next color class (we rotate among 3 colors)
  next_color <- 1
  next_color_class <- function() {
    class <- paste0("color", next_color)
    next_color <<- next_color + 1
    if (next_color > 3) next_color <<- 1
    class
  }
  
  # function to produce book html
  book_html = function(url, date) {
    html = read_html(url, encoding = 'UTF-8')
    title = xml_find(html, './/title')
    if (is.null(title)) return()
    title = xml_text(title)
    if (title == '') return()
    
    description = xml_find(html, './/meta[@name="description"]')
    if (!is.null(description)) description = xml_attr(description, 'content')
    cover = xml_find(html, './/meta[@property="og:image"]')
    if (!is.null(cover)) 
      cover = xml_attr(cover, 'content')
    
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
    
    # build the cover
    if (!is.null(cover)) {
      # <div class="bookImage" style="background-image:url(content/cover.jpg)"></div>
      coverDiv <- div(class = "bookImage", 
                      style = paste0("background-image:url(", cover ,")"))
    } else {
      coverDiv <- div(class=paste("bookImage", next_color_class()),
                      div(class = "title", title),
                      div(class = "author", author))
    }
    
    div(class = "book",
        a(href = url,
          coverDiv,
          div(class = "bookMeta",
              github_buttons(repo),
              div(class = "title", a(href = url, title)),
              div(class = "author", author),
              div(class = "date", as.Date(date)),
              div(class = "overview", description)
          )
        )    
    )
  } 
  
  
  
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
    
    panel = book_html(url, date)
    
    panels[[url]] = list(date = date, panel = panel)
    saveRDS(panels, '_book_meta.rds')
    panel
    
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  tagList(books)
}

xml_find = function(x, xpath, all = FALSE) {
  FUN = if (all) xml_find_all else xml_find_one
  tryCatch(FUN(x, xpath), error = function(e) NULL)
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

