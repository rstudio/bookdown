# TODO: Replace if helper in testthat gets vectorised
# https://github.com/r-lib/testthat/issues/1398
skip_if_bs4_book_deps_missing <- function() {
  check <- vapply(bs4_book_deps(), skip_if_not_installed, logical(1L), USE.NAMES = FALSE)
  invisible(check)
}

local_bs4_book <- function(name = "book",
                           title = "Awesome Cookbook",
                           author = "Yoda",
                           output_options = NULL,
                           description = NULL,
                           url = NULL,
                           verbose = FALSE,
                           env = parent.frame()) {

  # don't run test using this book skeleton when Pandoc is not available
  skip_if_not_pandoc()

  path <- withr::local_tempdir(.local_envir = env)

  book_skeleton(
    name = name,
    title = title,
    author = author,
    path = path,
    description = description,
    url = url
  )

  # Add text to Introduction

  intro <- readLines(file.path(path, "01-Introduction.Rmd"))
  writeLines(
    c(intro, paste0(rep(0:9, 42), collapse = " ")),
    file.path(path, "01-Introduction.Rmd")
  )

  fun <- if (verbose) {
    identity
  } else {
    suppressMessages
  }

  fun(
    render_book(
      path,
      output_format = "bookdown::bs4_book",
      output_options = output_options,
      quiet = TRUE
    )
  )

  return(path)
}

get_meta_content <- function(node) {
  xml2::xml_attr(node, "content")
}
