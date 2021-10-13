quiet = "--quiet" %in% commandArgs(FALSE)
formats = commandArgs(TRUE)
# travis is still use for github page deployment
# TODO: Remove github page deployment
ghpages = identical(Sys.getenv("DEPLOY_GH_PAGES", NA), "true")


src = (function() {
  attr(body(sys.function()), 'srcfile')
})()$filename
if (is.null(src) || src == '') src = '.'
owd = setwd(dirname(src))

# provide default formats if necessary
if (length(formats) == 0) formats = c(
  'bookdown::pdf_book', 'bookdown::epub_book', 'bookdown::gitbook', 'bookdown::bs4_book'
)
# render the book to all formats unless they are specified via command-line args
message(">> Building Books:")
for (fmt in formats) {
  message("  -- Building format: ", fmt)
  # change output_dir for bs4_book as we don't want to publish it
  output_dir = if (fmt == 'bookdown::bs4_book') "'_bs4_book'" else "NULL"
  cmd = sprintf("bookdown::render_book('index.Rmd', '%s', quiet = %s, output_dir = %s)", fmt, quiet, output_dir)
  res = xfun::Rscript(c('-e', shQuote(cmd)))
  if (res != 0) stop('Failed to compile the book to ', fmt)
  if (fmt == 'bookdown::epub_book') bookdown::calibre('_book/bookdown.epub', 'mobi')
}
unlink('bookdown.log')

# tweak the generated html files
message(">> Tweaking HTML gitbook")
for (f in list.files('_book', '[.]html$', full.names = TRUE)) {
  x = readLines(f)

  if (length(i <- grep('^\\s*<body>\\s*$', x)) == 0) next
  # patch HTML files when published to gh-pages to redirect to bookdown.org
  if (ghpages) x[i[1]] = '<body onload="window.location = \'https://bookdown.org/yihui\'+location.pathname">'
  i = grep('<i class="fa fa-circle-o-notch fa-spin"></i><a href="./">.+</a>', x)[1]
  # shorter title on the toolbar
  if (!is.na(i)) x[i] = gsub('bookdown: ', '', x[i], fixed = TRUE)
  # Remove some lines when published to gh-pages
  if (ghpages) {
    i = c(
      grep('&lt;bytecode: 0x[0-9a-f]+&gt;$', x),
      grep('^\\s*<meta name="generator" content="bookdown [.0-9]+ and GitBook [.0-9]+" />$', x),
      grep('^<meta name="date" content="[-0-9]+" />$', x)
    )
    if (length(i)) x = x[-i]
  }

  writeLines(x, f)
}

# When several format are rendered, usually when make all is called,
# then we publish everything to bookdown.org
if (length(formats) > 1 && !ghpages) {
  message(">> Publishing Books")
  if (!is.na(Sys.getenv("CI", NA))) {
    # On CI connect to server, using API KEY and deploy using appId
    rsconnect::addConnectServer('https://bookdown.org', 'bookdown.org')
    rsconnect::connectApiUser(
      account = 'GHA', server = 'bookdown.org',
      apiKey = Sys.getenv('CONNECT_API_KEY')
    )
    rsconnect::deploySite(
      appId = Sys.getenv('CONTENT_ID'),
      server = 'bookdown.org',
      render = 'none', logLevel = 'verbose',
      forceUpdate = TRUE)
  } else {
    # for local deployment when rsconnect/ is available
    bookdown::publish_book('bookdown', server = 'bookdown.org', render = 'none')
  }
}

setwd(owd)
