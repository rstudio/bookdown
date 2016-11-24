#' Publish a book to the web
#'
#' Publish a book to the web. Note that you should be sure to render all
#' versions of the book before publishing, unless you have specified
#' \code{render = TRUE}.
#'
#' @inheritParams rsconnect::deploySite
#'
#' @param name Name of the book (this will be used in the URL path of the
#'   published book). Defaults to the \code{book_filename} in
#'   \code{_bookdown.yml} if not specified.
#' @param account Account name to publish to. Will default to any previously
#'   published to account or any single account already associated with
#'   \code{server}.
#' @param server Server to publish to (by default beta.rstudioconnect.com but
#'   any RStudio Connect server can be published to).
#'
#' @export
publish_book = function(
  name = NULL, account = NULL, server = NULL, render = c("none", "local", "server")
) {

  # if there are no RS Connect accounts setup on this machine
  # then offer to add one for bookdown.org
  accounts <- rsconnect::accounts()
  accounts <- subset(accounts, server != "shinyapps.io")
  if (is.null(accounts) || nrow(accounts) == 0) {

    # add the server if we need to
    servers = rsconnect::servers()
    if (nrow(subset(servers, name == 'bookdown.org')) == 0)
      rsconnect::addServer("https://bookdown.org/__api__", 'bookdown.org')

    # see if they want to configure an account (bail if they don't)
    message('You do not currently have a bookdown.org publishing account ',
            'configured on this system.')
    result = readline('Would you like to configure one now? [Y/n]: ')
    if (tolower(result) == 'n') return(invisible())

    # configure the account
    rsconnect::connectUser(server = 'bookdown.org')
  }

  # deploy the book
  rsconnect::deploySite(
    siteDir = getwd(), siteName = name, account = account, server = server,
    render = render, logLevel = 'normal'
  )
}
