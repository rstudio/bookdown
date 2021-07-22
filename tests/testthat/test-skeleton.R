test_that("Get resources files for formats", {
  expect_identical(
    bookdown_skeleton_get_files(),
    list.files(bookdown_skeleton_get_dir(), recursive = TRUE)
  )
  expect_identical(
    bookdown_skeleton_get_files("gitbook", relative = FALSE),
    list.files(bookdown_skeleton_get_dir("gitbook"), recursive = TRUE, full.names = TRUE)
  )
})

test_that("bookdown_skeleton_insert_yml()", {
  dir <- withr::local_tempdir()
  book_skeleton("dummybook", "for test", "CD", chapters = NULL, path = dir)
  withr::local_dir(dir)
  xfun::gsub_file("index.Rmd", pattern = "^(title:.*)$", replacement = "\\1\nyaml: placeholder")
  xfun::write_utf8(c("name: doe", "job: none"), "child.yml")
  bookdown_skeleton_insert_yml("index.Rmd", "child.yml", "yaml: placeholder")
  expect_false(file.exists("child.yml"))
  content <- xfun::read_utf8("index.Rmd")
  pos <- grep("title:", content)
  expect_match(content[pos + 1], "name: doe")
  expect_match(content[pos + 2], "job: none")
})

test_that("bookdown_skeleton_append_yml()", {
  dir <- withr::local_tempdir()
  book_skeleton("dummybook", "for test", "CD", chapters = NULL, path = dir)
  withr::local_dir(dir)
  xfun::write_utf8(c("name: doe", "job: none"), "child.yml")
  bookdown_skeleton_append_yml("_bookdown.yml", "child.yml")
  expect_false(file.exists("child.yml"))
  content <- xfun::read_utf8("_bookdown.yml")
  expect_match(content[1], "name: doe")
  expect_match(content[2], "job: none")
  expect_match(content[3], "^book_filename")
})

test_that("bookdown_skeleton_remove_blocks()", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  content1 <- c("to keep1", "<!--gitbook:start-->", "to remove", "<!--gitbook:end-->")
  xfun::write_utf8(content1, "test.Rmd")
  bookdown_skeleton_remove_blocks(".", "bs4_book")
  expect_identical(xfun::read_utf8("test.Rmd"), "to keep1")
  content2 <- c("to keep2", "<!--bs4_book:start-->", "to keep3", "<!--bs4_book:end-->")
  xfun::write_utf8(c(content1, content2), "test.Rmd")
  bookdown_skeleton_remove_blocks(".", "bs4_book")
  expect_identical(xfun::read_utf8("test.Rmd"), paste0("to keep", 1:3))
  xfun::write_utf8(c(content1, content2), "test.Rmd")
  bookdown_skeleton_remove_blocks(".", "notinfile")
  expect_identical(xfun::read_utf8("test.Rmd"), paste0("to keep", 1:2))
  xfun::write_utf8(paste0("to keep", 1:2), "test.Rmd")
  bookdown_skeleton_remove_blocks(".", "gitbook")
  expect_identical(xfun::read_utf8("test.Rmd"), paste0("to keep", 1:2))
})

test_that("Created gitbook template works", {
  skip_on_cran()
  skip_if_not_pandoc()
  dir <- withr::local_tempdir()
  create_gitbook(dir)
  expect_error(suppressMessages(render_book(dir, quiet = TRUE)), NA)
})

test_that("Created bs4_book template works", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_bs4_book_deps_missing()
  dir <- withr::local_tempdir()
  create_bs4_book(dir)
  res <- suppressMessages(render_book(dir, quiet = TRUE))
  expect_true(file.exists(res))
})
