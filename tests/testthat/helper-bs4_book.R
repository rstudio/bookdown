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
  skip_if_not_pandoc('2.0')

  path <- local_book(name = name, title = title, author = author,
    description = description, url = url, verbose = verbose, env = env)

  suppressMessages(
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
