# From rmarkdown helper.R

# Use to test pandoc availability or version lower than
skip_if_not_pandoc <- function(ver = NULL) {
  if (!rmarkdown::pandoc_available(ver)) {
    msg <- if (is.null(ver)) {
      "Pandoc is not available"
    } else {
      sprintf("Version of Pandoc is lower than %s.", ver)
    }
    skip(msg)
  }
}

# Use to test version greater than
skip_if_pandoc <- function(ver = NULL) {
  if (rmarkdown::pandoc_available(ver)) {
    msg <- if (is.null(ver)) {
      "Pandoc is available"
    } else {
      sprintf("Version of Pandoc is greater than %s.", ver)
    }
    skip(msg)
  }
}

local_rmd_file <- function(..., .env = parent.frame()) {
  path <- withr::local_tempfile(.local_envir = .env, fileext = ".Rmd")
  xfun::write_utf8(c(...), path)
  path
}

local_render <- function(input, ..., .env = parent.frame()) {
  skip_if_not_pandoc()
  output_file <- withr::local_tempfile(.local_envir = .env)
  rmarkdown::render(input, output_file = output_file, quiet = TRUE, ...)
}

.render_and_read <- function(input, ...) {
  skip_if_not_pandoc()
  res <- local_render(input, ...)
  xfun::read_utf8(res)
}

local_book <- function(name = "book",
                       title = "Awesome Cookbook",
                       author = "Yoda",
                       description = NULL,
                       url = NULL,
                       verbose = FALSE,
                       env = parent.frame()) {

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

  return(path)
}
