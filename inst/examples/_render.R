if (Sys.which('kindlegen') == '') {
  if (.Platform$OS.type == 'windows' || Sys.info()[['sysname']] == 'Darwin')
    stop('This script does not support Windows or Mac OS X')
  bookdown:::in_dir(tempdir(), {
    download.file('http://kindlegen.s3.amazonaws.com/kindlegen_linux_2.6_i386_v2_9.tar.gz', 'kindle.tar.gz', mode = 'wb')
    system2('tar', c('zxf', 'kindle.tar.gz'))
    dir.create('~/bin', showWarnings = FALSE)
    file.rename('kindlegen', '~/bin/kindlegen')
    if (Sys.which('kindlegen') == '')
      Sys.setenv(PATH = paste0('~/bin:', Sys.getenv('PATH')))
  })
}

quiet = "--quiet" %in% commandArgs(FALSE)
formats = commandArgs(TRUE)

# provide default formats if necessary
if (length(formats) == 0) formats = c(
  'bookdown::pdf_book', 'bookdown::epub_book', 'bookdown::gitbook'
)
# render the book to all formats unless they are specified via command-line args
for (fmt in formats) {
  cmd = sprintf("bookdown::render_book('index.Rmd', '%s', quiet = %s)", fmt, quiet)
  res = bookdown:::Rscript(c('-e', shQuote(cmd)))
  if (res != 0) stop('Failed to compile the book to ', fmt)
  if (fmt == 'bookdown::epub_book') bookdown::kindlegen()
}

# patch HTML files in gh-pages if built on Travis
if (!is.na(Sys.getenv('CI', NA))) {
  r = '<body onload="window.location = \'https://bookdown.org/yihui\'+location.pathname">'
  for (f in list.files('_book', '[.]html$', full.names = TRUE)) {
    x = readLines(f)
    if (length(i <- grep('^\\s*<body>\\s*$', x)) == 0) next
    x[i[1]] = r
    writeLines(x, f)
  }
}
