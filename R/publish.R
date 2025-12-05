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
    if (file.exists(d <- 'rsconnect/documents/index.Rmd/bookdown.org') &&
        readline('Do you want to delete local records of the bookdown.org deployment? Your book will _not_ be deleted (y/n) ') == 'y') {
      unlink(d, recursive = TRUE)
    }
  }
  # if no accounts other than shinyapps.io and bookdown.org have been set up on
  # this machine, offer to add one for connect.posit.cloud
  if (length(setdiff(accounts$server, c('shinyapps.io', 'bookdown.org'))) == 0) {
    if (readline('Do you want to connect to connect.posit.cloud? (y/n)') == 'y') {
      # due to https://github.com/rstudio/rsconnect/pull/1266 we need to install RCurl for rsconnect <= 1.6.2
      if (!xfun::pkg_available("rsconnect", "1.6.3") && !xfun::pkg_available("RCurl")) {
        msg = 'The RCurl package is not installed but required for Connect Cloud auth flow.'
        if (readline(
          paste0(msg, '\nDo you want to install RCurl now? (y/n) ')) == 'y') {
          install.packages('RCurl')
        } else {
          stop(msg, ' Please install the RCurl package. Then run rsconnect::connectCloudUser().')
        }
      }
      rsconnect::connectCloudUser()
    }
  }

  # deploy the book
  rsconnect::deploySite(siteDir = getwd(), siteName = name, ...)
}
