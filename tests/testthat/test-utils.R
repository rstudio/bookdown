test_that("stop_if_not_exists()", {
  expect_error(stop_if_not_exists("dummy"))
  f <- withr::local_tempfile()
  file.create(f)
  expect_error(stop_if_not_exists(f), regexp = NA)
  expect_error(stop_if_not_exists(c(f, "dummy")))
})

test_that("target_formats() detects type from string", {
  expect_identical(target_format("html"), "html")
  expect_identical(target_format("bookdown::gitbook"), "html")
  expect_identical(target_format("bookdown::bs4_book"), "html")
  expect_identical(target_format("bookdown::pdf_book"), "latex")
  expect_identical(target_format("bookdown::tufte_book2"), "latex")
  expect_identical(target_format("bookdown::tufte_handout2"), "latex")
  expect_identical(target_format("bookdown::tufte_html_book"), "html")
  expect_identical(target_format("bookdown::epub_book"), "epub")
  expect_identical(target_format("bookdown::word_document2"), "docx")
  expect_identical(target_format("bookdown::powerpoint_presentation2"), "pptx")
  expect_identical(target_format("bookdown::beamer_presentation2"), "latex")
})

test_that("Correctly get index file", {
  withr::local_dir(withr::local_tempdir())
  expect_equal(get_index_file(), character())
  file.create(c("test.Rmd"))
  expect_equal(get_index_file(), character())
  file.create("index.Rmd")
  expect_equal(get_index_file(), "index.Rmd")
  unlink("index.Rmd");file.create("index.rmd")
  expect_equal(get_index_file(), "index.rmd")
  skip_if_not(xfun::is_linux())
  file.create("index.Rmd")
  expect_warning(index_file <- get_index_file())
  expect_length(index_file, 1L)
})

test_that("Correctly guess output format in various situations", {
  book <- local_book()
  withr::local_dir(book)
  # With default index.Rmd
  expect_equal(
    get_output_formats(),
    c("bookdown::gitbook", "bookdown::pdf_book", "bookdown::epub_book",
      "bookdown::bs4_book")
  )
  expect_equal(get_output_formats(first = TRUE), "bookdown::gitbook")
  expect_equal(
    get_output_formats(filter = function(f) grep('bs4', f, value = TRUE)),
    "bookdown::bs4_book"
  )
  # with index.rmd
  file.rename("index.Rmd", "index.rmd")
  expect_equal(
    get_output_formats(),
    c("bookdown::gitbook", "bookdown::pdf_book", "bookdown::epub_book",
      "bookdown::bs4_book")
  )
  expect_equal(get_output_formats(first = TRUE), "bookdown::gitbook")
  expect_equal(
    get_output_formats(filter = function(f) grep('bs4', f, value = TRUE)),
    "bookdown::bs4_book"
  )
  # With another Rmd file
  file.rename("index.rmd", "main.Rmd")
  expect_equal(
    get_output_formats(fallback_index = "main.Rmd"),
    c("bookdown::gitbook", "bookdown::pdf_book", "bookdown::epub_book",
      "bookdown::bs4_book")
  )
  expect_equal(
    get_output_formats(fallback_format = "bookdown::pdf_book"),
    "bookdown::pdf_book"
  )
  unlink("main.Rmd")
  expect_equal(
    get_output_formats(fallback_format = "bookdown::pdf_book"),
    "bookdown::pdf_book"
  )
})

test_that("first_html_format correctly found the format", {
  withr::local_dir(local_book())
  expect_equal(first_html_format(), "bookdown::gitbook")
  skip_if_not_installed("yaml")
  yaml <- yaml::read_yaml("_output.yml")
  yaml["bookdown::gitbook"] <- NULL
  yaml::write_yaml(yaml, "_output.yml")
  expect_equal(first_html_format(), "bookdown::bs4_book")
  yaml["bookdown::bs4_book"] <- NULL
  yaml::write_yaml(yaml, "_output.yml")
  expect_equal(first_html_format(), "bookdown::gitbook")
})
