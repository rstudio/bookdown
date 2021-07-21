# All tests below require Pandoc
skip_if_not_pandoc()
# test for bs4_book() needs to be run only if deps are installed
skip_if_bs4_book_deps_missing()

test_that("bs4_book() repo specification works - default case", {
  skip_if_bs4_book_deps_missing()
  book <- local_bs4_book(
    output_options = list(
      repo = "https://github.com/hadley/ggplot2-book"
    )
  )
  html <- xml2::read_html(file.path(book, "_book", "index.html"))

  repo_a <- xml2::xml_find_first(html, "//a[@id='book-repo']")

  expect_equal(
    xml2::xml_attr(xml2::xml_child(repo_a), "class"),
    "fab fa-github"
  )

  expect_equal(
    xml2::xml_attr(repo_a, "href"),
    "https://github.com/hadley/ggplot2-book"
  )

  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(html, "//a[@id='book-edit']"), "href"),
    "https://github.com/hadley/ggplot2-book/edit/master/index.Rmd"
  )
  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(html, "//a[@id='book-source']"), "href"),
    "https://github.com/hadley/ggplot2-book/blob/master/index.Rmd"
  )

})

test_that("bs4_book() repo specification works - branch and subdir", {
  skip_if_bs4_book_deps_missing()
  book <- local_bs4_book(
    output_options = list(
      repo = list(
        base = "https://github.com/hadley/ggplot2-book",
        branch = "main",
        subdir = "book"
      )
    )
  )
  html <- xml2::read_html(file.path(book, "_book", "index.html"))

  expect_equal(
    xml2::xml_attr(xml2::xml_child(xml2::xml_find_first(html, "//a[@id='book-repo']")), "class"),
    "fab fa-github"
  )

  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(html, "//a[@id='book-repo']"), "href"),
    "https://github.com/hadley/ggplot2-book"
  )
  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(html, "//a[@id='book-edit']"), "href"),
    "https://github.com/hadley/ggplot2-book/edit/main/book/index.Rmd"
  )
  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(html, "//a[@id='book-source']"), "href"),
    "https://github.com/hadley/ggplot2-book/blob/main/book/index.Rmd"
  )
})

test_that("bs4_book() repo specification works - GitLab", {
  skip_if_bs4_book_deps_missing()
  book <- local_bs4_book(
    output_options = list(
      repo = "https://gitlab.com/hadley/ggplot2-book"
    )
  )
  html <- xml2::read_html(file.path(book, "_book", "index.html"))

  expect_equal(
    xml2::xml_attr(xml2::xml_child(xml2::xml_find_first(html, "//a[@id='book-repo']")), "class"),
    "fab fa-gitlab"
  )
})

test_that("bs4_book() repo specification works - custom icon", {
  skip_if_bs4_book_deps_missing()
  book <- local_bs4_book(
    output_options = list(
      repo = list(
        base = "https://gitlab.com/hadley/ggplot2-book",
        icon = "fas fa-air-freshener"
      )
    )
  )
  html <- xml2::read_html(file.path(book, "_book", "index.html"))

  expect_equal(
    xml2::xml_attr(xml2::xml_child(xml2::xml_find_first(html, "//a[@id='book-repo']")), "class"),
    "fas fa-air-freshener"
  )
})

test_that("bs4_book() repo specification works - custom icon GitHub", {
  skip_if_bs4_book_deps_missing()
  book <- local_bs4_book(
    output_options = list(
      repo = list(
        base = "https://github.com/hadley/ggplot2-book",
        icon = "fas fa-air-freshener"
      )
    )
  )
  html <- xml2::read_html(file.path(book, "_book", "index.html"))

  expect_equal(
    xml2::xml_attr(xml2::xml_child(xml2::xml_find_first(html, "//a[@id='book-repo']")), "class"),
    "fas fa-air-freshener"
  )
})

test_that("bs4_book() metadata tweaking works -- index", {
  skip_if_bs4_book_deps_missing()
  book <- local_bs4_book(description = "A very nice book.", url = 'https://example.com/')
  html <- xml2::read_html(file.path(book, "_book", "index.html"))

  generator <- xml2::xml_find_first(html, '//meta[@name="generator"]')
  # No test for the whole string as it contains bookdown version
  expect_match(get_meta_content(generator), "bs4_book")

  url <- xml_find_meta_property(html, 'og:url')
  expect_equal(get_meta_content(url), "https://example.com/")

  description <- xml_find_meta_property(html, 'og:description')
  expect_equal(
    get_meta_content(description),
    "A very nice book."
  )

  twitter_description <- xml_find_meta_name(html, 'twitter:description')
  expect_equal(
    get_meta_content(twitter_description),
    "A very nice book."
  )

  description <- xml_find_meta_name(html, 'description')
  expect_equal(
    get_meta_content(description),
    "A very nice book."
  )

})


test_that("bs4_book() metadata tweaking works -- not index", {
  skip_if_bs4_book_deps_missing()
  book <- local_bs4_book(description = "A very nice book.", url = 'https://example.com/')
  html <- xml2::read_html(file.path(book, "_book", "introduction.html"))

  generator <- xml2::xml_find_first(html, '//meta[@name="generator"]')
  # No test for the whole string as it contains bookdown version
  expect_match(get_meta_content(generator), "bs4_book")

  url <- xml_find_meta_property(html, 'og:url')
  expect_equal(get_meta_content(url), "https://example.com/introduction.html")

  og_description <- xml_find_meta_property(html, 'og:description')
  expect_equal(
    get_meta_content(og_description),
    "0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7..."
  )

  twitter_description <- xml_find_meta_name(html, 'twitter:description')
  expect_equal(
    get_meta_content(twitter_description),
    "0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7..."
  )

  description <- xml_find_meta_name(html, 'description')
  expect_equal(
    get_meta_content(description),
    "0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7..."
  )

})

test_that("bs4_book() and 404 -- page is tweaked", {
  skip_if_bs4_book_deps_missing()
  book <- local_bs4_book()
  withr::local_dir(book)
  expect_true(file.exists("_book/404.html"))
  html <- xml2::read_html("_book/404.html")
  expect_length(xml2::xml_find_all(html, ".//nav[@id = 'toc']"), 0L)
})

test_that("bs4_book() and 404 -- custom 404 page", {
  skip_if_bs4_book_deps_missing()
  book <- local_bs4_book()
  withr::local_dir(book)
  xfun::write_utf8(c("# Page not found", "", "I am created with _404.Rmd"), "_404.Rmd")
  suppressMessages(render_book(".", "bookdown::bs4_book", quiet = TRUE))
  expect_true(file.exists("_book/404.html"))
  html <- xml2::read_html("_book/404.html")
  expect_match(xml2::xml_text(xml2::xml_find_first(html, ".//main/div/p")), "_404.Rmd", fixed = TRUE)
  unlink("_404.Rmd")
  xfun::write_utf8(c("# Page not found", "", "I am created with _404.md"), "_404.md")
  suppressMessages(render_book(".", "bookdown::bs4_book", quiet = TRUE))
  expect_true(file.exists("_book/404.html"))
  html <- xml2::read_html("_book/404.html")
  expect_match(xml2::xml_text(xml2::xml_find_first(html, ".//main/div/p")), "_404.md", fixed = TRUE)
})
