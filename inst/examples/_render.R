quiet = "--quiet" %in% commandArgs(FALSE)
formats = commandArgs(TRUE)

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
  i = grep('<i class="fa fa-circle-o-notch fa-spin"></i><a href="./">.+</a>', x)[1]
  # shorter title on the toolbar
  if (!is.na(i)) x[i] = gsub('bookdown: ', '', x[i], fixed = TRUE)

  writeLines(x, f)
}

setwd(owd)
