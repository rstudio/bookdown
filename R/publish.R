#' Publish a book to a Posit Connect server
#'
#' Publish a book to a Connect Server. By default, you should render the book
#' locally before publishing.
#' @param name Name of the book (this will be used in the URL path of the
#'   published book). Defaults to the \code{book_filename} in
#'   \code{_bookdown.yml} if not specified.
#' @param ... Other arguments to be passed to [rsconnect::deploySite()].
#' @note Previously the default server was bookdown.org, which will be sunset.
#'   You are no longer recommended to publish to bookdown.org.
#' @export
publish_book = function(name = NULL, ...) {
  # delete local records of bookdown.org
  accounts = rsconnect::accounts()
  x1 = 'bookdown.org' %in% accounts$server
  x2 = 'bookdown.org' %in% rsconnect::servers()$name
  if (x1 || x2) {
    warning(
      'bookdown.org will be sunset on January 31, 2026. Please consider ',
      'publishing to https://connect.posit.cloud instead.'
    )
    if (readline('Do you want to remove the bookdown.org server now? Your book will _not_ be removed. (y/n) ') == 'y') {
      if (x1) rsconnect::removeAccount(server = 'bookdown.org')
      if (x2) rsconnect::removeServer('bookdown.org')
    }
    if (readline('Do you want to delete local records of the Connect deployment? Your book will _not_ be deleted (y/n) ') == 'y') {
      rsconnect::forgetDeployment()
    }
  }
  # if no accounts other than shinyapps.io and bookdown.org have been set up on
  # this machine, offer to add one for connect.posit.cloud
  if (length(setdiff(accounts$server), c('shinyapps.io', 'bookdown.org')) == 0) {
    if (readline('Do you want to connect to connect.posit.cloud? (y/n)') == 'y')
      rsconnect::connectCloudUser()
  }

  # deploy the book
  rsconnect::deploySite(siteDir = getwd(), siteName = name, ...)
}
