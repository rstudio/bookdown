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
