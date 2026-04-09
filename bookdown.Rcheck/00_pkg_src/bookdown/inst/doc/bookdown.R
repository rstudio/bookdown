## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----results='asis', echo = FALSE, eval = FALSE-------------------------------
# # run this to update the content below
# xfun::pkg_attach2("xml2")
# html <- read_html("https://yihui.org/bookdown/")
# chapters <- xml_find_all(html, "//li[@class='chapter']")
# first_level <- chapters[which(purrr::map_lgl(xml_attr(chapters, 'data-level'), ~ grepl('^\\d+$', .x)))]
# titles <- xml_text(xml_find_all(first_level, "a"))
# titles <- gsub("^(\\d+)", "\\1.", titles)
# titles <- gsub("^(.*) \\([*])$", "\\1", titles)
# url <- file.path("https://yihui.org/bookdown", xml_attr(first_level, "data-path"))
# formatted <- sprintf("* [%s](%s)", titles, url)
# cat(formatted, sep = "\n")

