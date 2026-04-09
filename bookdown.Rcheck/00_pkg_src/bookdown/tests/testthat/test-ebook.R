test_that("epub_book() correctly renders math without warning", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("jsonlite")
  book <- local_book()
  # add complex math
  xfun::in_dir(
    book,
    xfun::write_utf8(c(
      "# Methods",
      "",
      "Inserting Math",
      "",
      "$$",
      "SE = \\sqrt(\\frac{p(1-p)}{n}) \\approx \\sqrt{\\frac{1/3 (1 - 1/3)} {300}} = 0.027",
      "$$"
    ), "03-Methods.Rmd")
  )
  file.create(tmp_file <- withr::local_tempfile(pattern = "pandoc", fileext = ".log"))
  res <- .render_book_quiet(book, output_format = epub_book(pandoc_args = c(sprintf("--log=%s", tmp_file), "--quiet")))
  expect_false("CouldNotConvertTeXMath" %in% jsonlite::fromJSON(tmp_file)$type)
})
