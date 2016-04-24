quiet = "--quiet" %in% commandArgs(FALSE)
formats = commandArgs(TRUE)
travis = !is.na(Sys.getenv('CI', NA))

# provide default formats if necessary
if (length(formats) == 0) formats = c(
  'bookdown::pdf_book', 'bookdown::epub_book', 'bookdown::gitbook'
)
# render the book to all formats unless they are specified via command-line args
for (fmt in formats) {
  cmd = sprintf("bookdown::render_book('index.Rmd', '%s', quiet = %s)", fmt, quiet)
  res = bookdown:::Rscript(c('-e', shQuote(cmd)))
  if (res != 0) stop('Failed to compile the book to ', fmt)
  if (!travis && fmt == 'bookdown::epub_book')
    bookdown::calibre('_book/bookdown.epub', 'mobi')
}

# patch HTML files in gh-pages if built on Travis
if (travis) {
  r = '<body onload="window.location = \'https://bookdown.org/yihui\'+location.pathname">'
  for (f in list.files('_book', '[.]html$', full.names = TRUE)) {
    x = readLines(f)
    if (length(i <- grep('^\\s*<body>\\s*$', x)) == 0) next
    x[i[1]] = r
    writeLines(x, f)
  }
}
