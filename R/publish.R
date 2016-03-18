#' Publish a book to the web
#'
#' Publish a book to the web. Note that you should be sure to render all
#' versions of the book before publishing, unless you have specified
#' \code{render = TRUE}.
#'
#' @param name Name of the book (this will be used in the URL path of the
#'   published book). Defaults to the \code{book_filename} in
#'   \code{_bookdown.yml} if not specified.
#' @param account Account name to publish to. Will default to any previously
#'   published to account or any single account already associated with
#'   \code{server}.
#' @param server Server to publish to (by default beta.rstudioconnect.com but
#'   any RStudio Connect server can be published to).
#' @param render \code{TRUE} to render all formats prior to publishing (defaults
#'   to \code{FALSE}, however, this can be modified via the
#'   \code{bookdown.render_on_publish} option). Note that this requires the use
#'   of either an R script \file{_render.R} (or a \file{Makefile} if
#'   \file{_render.R} is not found) to provide the implementaiton of rendering
#'   all formats that you want to publish.
#'
#' @export
publish_book = function(
  name = NULL, account = NULL, server = NULL,
  render = getOption('bookdown.render_on_publish', FALSE)
) {

  on.exit(opts$restore(), add = TRUE)

  # render if requested
  if (isTRUE(render)) {
    if (length(script <- existing_r('_render', TRUE))) {
      result = Rscript(script)
    } else if (file.exists('Makefile')) {
      result = system2('make')
    } else stop('Rendering before publishing requires a _render.R or Makefile')
    if (result != 0) stop('Error ', result, ' attempting to render book')
  }

  # see if we have a single existing deployment that matches the values
  # passed. if we do then use that deployment's name, account, and server.
  # this allows users to deploy with a set of explicit parmaeters the first
  # time then be able to deploy with no arguments thereafter.
  deployments = rsconnect::deployments(
    'index.Rmd', nameFilter = name, accountFilter = account, serverFilter = server
  )
  if (nrow(deployments) == 1) {
    name = deployments$name
    account = deployments$account
    server = deployments$server
  }

  # load the config
  config = load_config()

  # get the book dir from the config
  book_dir = output_dirname('_book', config, create = FALSE)
  if (is.null(book_dir)) book_dir = '.'
  if (!file.exists(file.path(book_dir, 'index.html'))) warning(
    'There is not an index.html in ', book_dir, '.'
  )

  # get the name from the config if necessary
  if (is.null(name)) name = with_ext(book_filename(config, fallback = FALSE), '')
  if (is.null(name)) stop(
    'You must specify a name for the book or set book_filename in _bookdown.yml'
  )

  # if the server is null then default to beta.rstudioconnect.com
  # (note: change this to bookdown.org when it's up and running)
  if (is.null(server)) server = 'beta.rstudioconnect.com'

  # check whether we already have an account registered on the bookdown
  # server (if we don't then offer to create one)
  if (!length(rsconnect::accounts(server))) {

    # see if they want to configure an account
    message('You do not currently have a publishing account configured on this system.')
    result = readline('Would you like to configure one now? [Y/n]: ')
    if (tolower(result) == 'n') return(invisible())

    # configure the account
    rsconnect::connectUser(server = server)
  }

  # publish
  rsconnect::deployApp(
    appName = name, appDir = book_dir, appSourceDoc = 'index.Rmd',
    contentCategory = 'book', account = account, server = server, lint = FALSE
  )
}
