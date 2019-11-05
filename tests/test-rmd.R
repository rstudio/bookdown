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

  # footnotes are parsed and moved correctly
  ## deleted from last section
  if (any(readLines("rmd/subsection-footnotes-2.html") == '<div class="footnotes">')) {
    stop('Failed to parse and delete the footnotes in parse_footnotes.Rmd')
  }
  ## footnote one is moved to first section
  if (!any(readLines('rmd/test-footnote.html') == '<div class="footnotes">') ||
      !any(grepl('id="fn1"', readLines('rmd/test-footnote.html')))) {
    stop('Failed to move the footnotes back to subsection 1 in parse_footnotes.Rmd')
  }
  ## footnote two is moved to second section
  if (!any(readLines('rmd/subsection-footnotes-1.html') == '<div class="footnotes">') ||
      !any(grepl('id="fn2"', readLines('rmd/subsection-footnotes-1.html')))) {
    stop('Failed to move the footnotes back to subsection 1 in parse_footnotes.Rmd')
  }
})
