quiet = "--quiet" %in% commandArgs(FALSE)
formats = commandArgs(TRUE)
travis = !is.na(Sys.getenv('CI', NA))

src = (function() {
  attr(body(sys.function()), 'srcfile')
})()$filename
if (is.null(src) || src == '') src = '.'
owd = setwd(dirname(src))

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

r = '<body onload="window.location = \'https://bookdown.org/yihui\'+location.pathname">'
for (f in list.files('_book', '[.]html$', full.names = TRUE)) {
  x = readLines(f)
  if (length(i <- grep('^\\s*<body>\\s*$', x)) == 0) next
  # patch HTML files in gh-pages if built on Travis, to redirect to bookdown.org
  if (travis) x[i[1]] = r
  i = grep('<i class="fa fa-circle-o-notch fa-spin"></i><a href="./">.+</a>', x)[1]
  # shorter title on the toolbar
  if (!is.na(i)) x[i] = gsub('bookdown: ', '', x[i], fixed = TRUE)
  i = c(
    grep('^\\s*<meta name="generator" content="bookdown [.0-9]+ and GitBook [.0-9]+">$', x),
    grep('^<meta name="date" content="[-0-9]+">$', x)
  )
  if (travis && length(i)) x = x[-i]
  writeLines(x, f)
}

if (length(formats) > 1) bookdown::publish_book()

setwd(owd)
