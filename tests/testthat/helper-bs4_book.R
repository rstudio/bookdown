skip_if_bs4_book_deps_missing <- function() {
  purrr::map_lgl(c(bs4_book_deps(), "withr"), testthat::skip_if_not_installed)
}

create_minimal_bs_book <- function(path,
                                   name = "book",
                                   title = "Awesome Cookbook",
                                   author = "Yoda",
                                   metadata = NULL,
                                   output_options = NULL) {
  book_skeleton(
    name = name,
    title = title,
    author = author,
    index_metadata = metadata,
    path = path
  )

  suppressMessages(
    render_book(
      path,
      output_format = "bs4_book",
      output_options = output_options,
      quiet = TRUE
    )
  )
}