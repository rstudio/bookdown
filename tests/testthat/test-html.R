test_that("build_404 creates correct 404 page", {
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(tmp_dir)

  # default page
  expect_false(file.exists("404.html"))
  expect_null(build_404()$rmd_cur)
  expect_false(file.exists("404.html"))

  # custom pages
  xfun::write_utf8(c("# Page not found", "", "I am created with _404.Rmd"), "_404.Rmd")
  build_404()
  expect_true(file.exists("404.html"))
  expect_match(xfun::file_string("404.html"), "I am created with _404.Rmd", fixed = TRUE)

  # do nothing if one exist
  expect_null(build_404()$html)

  unlink(c("404.html", "_404.Rmd"))

  xfun::write_utf8(c("# Page not found", "", "I am created with _404.md"), "_404.md")
  build_404()
  expect_true(file.exists("404.html"))
  expect_match(xfun::file_string("404.html"), "I am created with _404.md", fixed = TRUE)
})
