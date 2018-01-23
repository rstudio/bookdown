# compile an Rmd file without running Pandoc; arguments are passed from Rscript;
# the three arguments are 1) the Rmd filename; 2) the render arguments; 3) the
# filename to save the returned value of render()
local({
  args = commandArgs(TRUE)
  out = do.call(
    rmarkdown::render, c(args[1], readRDS(args[2]), list(run_pandoc = FALSE, encoding = 'UTF-8'))
  )
  out_expected = xfun::with_ext(args[1], '.md')
  if (out != out_expected) {
    file.rename(out, out_expected)
    attributes(out_expected) = attributes(out)
    out = out_expected
  }
  if (file.exists(args[3])) {
    res = readRDS(args[3])
    res[[args[1]]] = out
    saveRDS(res, args[3])
  } else saveRDS(setNames(list(out), args[1]), args[3])
})
