source('./testthat/helper-validate_html.R')
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
 
  html_issues = simplify_html_validation(
    validate_html(list.files("rmd", ".html$", full.names = TRUE))
  )
  if(nrow(html_issues) > 0)
    stop("HTML issues detected in ", paste(html_issues$file, collapse = ', '))

  # split by section works correctly
  ## id is used for html file name
  sections_files = c(
    "section-1.html", "subsection-1.html",
    "section-2.html", "sub2.html", "subsection-22.html",
    "section-3.html", "subsection-3.html"
  )

  if (any(!file.exists(file.path("rmd", sections_files))))
    stop("Failed to generate sections files")
  ## reference is working correctly (see #787)
  if (!any(xfun::read_utf8("rmd/subsection-1.html") == '<p>See chapter 2 now at <a href="section-2.html#section-2">2</a></p>'))
    stop('Failed to reference section when split by sections')
  if (!any(xfun::read_utf8("rmd/sub2.html") == '<p>See figure <a href="sub2.html#fig:iris-plot">2.1</a></p>'))
    stop('Failed to reference figure when split by sections')

  # Check equation label are working
  if (!any(readLines('rmd/equation-label.html') == 'y \\in \\mathbb{Z} \\tag{2}')) {
    stop('Failed to render the equation label in equation-label.Rmd')
  }

  # footnotes are parsed and moved correctly
  ## deleted from last section
  if (any(grepl('<div class="footnotes[^"]*">', readLines("rmd/subsection-footnotes-2.html")))) {
    stop('Failed to parse and delete the footnotes in parse-footnotes.Rmd')
  }
  ## footnote one is moved to first section
  if (!any(readLines('rmd/test-footnote.html') == '<div class="footnotes">') ||
      !any(grepl('id="fn1"', readLines('rmd/test-footnote.html')))) {
    stop('Failed to move the footnotes back to subsection 1 in parse-footnotes.Rmd')
  }
  ## footnote two is moved to second section
  if (!any(readLines('rmd/subsection-footnotes-1.html') == '<div class="footnotes">') ||
      !any(grepl('id="fn2"', readLines('rmd/subsection-footnotes-1.html')))) {
    stop('Failed to move the footnotes back to subsection 1 in parse-footnotes.Rmd')
  }
  # multiline footnote is also moved
  if (!any(readLines('rmd/subsection-footnotes-1.html') == '<div class="footnotes">') ||
      !any(grepl('id="fn3"', readLines('rmd/subsection-footnotes-1.html')))) {
    stop('Failed to move the footnotes back to subsection 1 in parse-footnotes.Rmd')
  }

  # number sections now works in markdown_document2
  content = readLines("rmd/number-sections.md")
  if (!any(grepl("(## )?1.1 subsection 1", content)) ||
      !any(grepl("<a href=.*>2.1</a>", content))) {
    stop("Something wrong in number-sections.Rmd")
  }

  # lua filter for custom environment
  local({
    reg_env = sprintf('<div class="(%s)">', paste(bookdown:::all_math_env, collapse = "|"))
    if (!any(grepl(reg_env, readLines("rmd/custom-environments.html"))))
      stop("Lua filter for custom environment fails to create divs")
    i18n = xfun::in_dir("rmd", bookdown:::load_config()$language$label$solution)
    reg_span = sprintf('<span .* class="solution"><em>%s</em>\\. </span>', i18n)
    if (!any(grepl(reg_span, readLines("rmd/custom-environments.html"))))
      stop("Lua filter for custom environment fails to apply correct translation")
  })

  # tests also some specific format
  rmarkdown::render(
    "rmd/custom-environments.Rmd", output_format = "bookdown::pdf_document2",
    envir = globalenv(), quiet = TRUE
  )
  if (!file.exists("rmd/custom-environments.pdf"))
    stop("Failed to render custom-environments for pdf document")

})
