#' Publish a book to the web
#'
#' Publish a book to the web. Note that you should be sure to render all versions
#' of the book using \code{\link{render_book}} before publishing.
#'
#' @param name Name of the book (this will be used in the URL path of the published book).
#'  Defaults to the \code{book_filename} in \code{_bookdown.yml} if not specified.
#' @param book_dir Directory where the book's contents are located. Defaults to
#'  \code{output_dir} in \code{_bookdown.yml} if not specified.
#' @param account Account name to publish to. Will default to any previously published
#'  to account or any single account already associated with \code{server}.
#' @param server Server to publish to (by default beta.rstudioconnect.com
#' but any RStudio Connect server can be published to).
#'
#' @export
publish_book <- function(name = NULL,
                         book_dir = NULL,
                         account = NULL,
                         server = "beta.rstudioconnect.com") {

  # load the config
  config <- load_config()

  # get the name from the config if necessary
  if (is.null(name)) {
    if (is.character(config[['book_filename']])) {
      name <- config[['book_filename']][1]
    } else {
      stop("You must specify a name for the book ",
           "(or set book_filename in _bookdown.yml")
    }
  }

  # get the book dir from the config
  if (is.null(book_dir)) {
    if (is.character(config[['output_dir']])) {
      book_dir <- config[['output_dir']][1]
    }
  }

  # check whether we already have an account registered on the bookdown
  # server (if we don't then offer to create one)
  if (!is.null(server) && !length(rsconnect::accounts(server))) {

    # see if they want to configure an account
    message("You don't currently have a publishing account configured on this system.")
    result <- readline("Would you like to configure one now? [Y/n]: ")
    if (tolower(result) == "n")
      return (invisible())

    # configure the account
    rsconnect::connectUser(server = server)
  }

  # publish
  rsconnect::deployApp(appName = name,
                       appDir = book_dir,
                       appSourceDoc = "index.Rmd",
                       contentCategory = "book",
                       account = account,
                       server = server,
                       lint = FALSE)
}


