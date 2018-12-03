# this script is used by bookdown::serve_book() to continuously render the book
local({
  args = commandArgs(TRUE)
  if (length(args) < 4) return()
  quiet = args[4] == 'TRUE'
  res = bookdown::render_book(
    args[-(1:4)], args[1], output_dir = args[2], envir = globalenv(),
    preview = args[3] == 'TRUE', quiet = quiet
  )
  if (quiet) invisible(res) else res
})
