test_that("bs4_book() repo specification works", {
  temp_path <- withr::local_tempdir()
  book <- create_minimal_bs_book(
    path = temp_path,
    output_options = list(
      repo = "https://github.com/hadley/ggplot2-book"
    )
  )
  html <- xml2::read_html(file.path(temp_path, "_book", "index.html"))

  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(html, "//a[@id='book-repo']"), "href"),
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


  temp_path <- withr::local_tempdir()
  book <- create_minimal_bs_book(
    path = temp_path,
    output_options = list(
      repo = list(
        base = "https://github.com/hadley/ggplot2-book",
        branch = "main",
        subdir = "book"
      )
    )
  )
  html <- xml2::read_html(file.path(temp_path, "_book", "index.html"))

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
