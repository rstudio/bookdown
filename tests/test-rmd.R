# only run this when NOT_CRAN is true (e.g., on Travis CI)
if (Sys.getenv('NOT_CRAN') == 'true') local({
  all_files = function() {
    list.files('rmd', all.files = TRUE, full.names = TRUE)
  }
  files1 = all_files()
  on.exit(unlink(setdiff(all_files(), files1), recursive = TRUE), add = TRUE)

  for (f in list.files('rmd', '[.]Rmd$', full.names = TRUE)) {
    rmarkdown::render(f, envir = globalenv(), quiet = TRUE)
  }
  if (!any(readLines('rmd/equation-label.html') == 'y \\in \\mathbb{Z} \\tag{2}')) {
    stop('Failed to render the equation label in equation-label.Rmd')
  }
})
