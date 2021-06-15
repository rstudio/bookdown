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

