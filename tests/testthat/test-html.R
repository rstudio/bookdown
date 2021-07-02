test_that("build_404_page creates correct 404 page", {
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(tmp_dir)
  pieces <- c("HEAD", "TOC", "FOOT")
  pieces <- as.list(setNames(sprintf("<!-- %s -->", pieces), pieces))
  fun <- function(...) paste(c(...), collapse = "\n")

  # default page
  expect_false(file.exists("404.html"))
  build_404_page(pieces$HEAD, pieces$TOC, pieces$FOOT, fun)
  expect_true(file.exists("404.html"))

  # do nothing if one exist
  expect_null(build_404_page(pieces$HEAD, pieces$TOC, pieces$FOOT, fun))
  unlink("404.html")

  # custom pages
  xfun::write_utf8(c("# Page not found", "", "I am created with _404.Rmd"), "_404.Rmd")
  build_404_page(pieces$HEAD, pieces$TOC, pieces$FOOT, fun)
  expect_true(file.exists("404.html"))
  expect_match(xfun::file_string("404.html"), "I am created with _404.Rmd", fixed = TRUE)
  unlink(c("404.html", "_404.Rmd"))

  xfun::write_utf8(c("# Page not found", "", "I am created with _404.md"), "_404.md")
  build_404_page(pieces$HEAD, pieces$TOC, pieces$FOOT, fun)
  expect_true(file.exists("404.html"))
  expect_match(xfun::file_string("404.html"), "I am created with _404.md", fixed = TRUE)
})
