# this script is used by bookdown::serve_book() to continuously render the book
local({
  args = commandArgs(TRUE)
  if (length(args) >= 3) bookdown::render_book(
    args[-(1:3)], args[1], output_dir = args[2], envir = globalenv(),
    preview = args[3] == 'TRUE'
  )
})
