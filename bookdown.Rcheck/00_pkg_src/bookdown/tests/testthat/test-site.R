test_that("find_book_proj() correction found the root dir", {
  book_dir <- xfun::normalize_path(local_book())
  withr::local_dir(book_dir)
  expect_equal(find_book_proj("."), book_dir)
  expect_equal(find_book_proj("01-Introduction.Rmd"), book_dir)
  gsub("^(site:.*)$", "\\1 # any comment", xfun::read_utf8("index.Rmd"))
  xfun::gsub_file("index.Rmd", pattern = "^(site:.*)$", replacement = "\\1 # any comment")
  expect_equal(find_book_proj("."), book_dir)
})
