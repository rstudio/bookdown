# this script is used by bookdown::serve_book() to continuously render the book
local({
  args = commandArgs(TRUE)
  if (length(args) >= 3) bookdown::render_book(
    args[-(1:2)], output_dir = args[1], envir = globalenv(), preview = args[2] == 'TRUE'
  )
})
