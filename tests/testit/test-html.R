library(testit)

assert("parse figure reference correctly", {
  res = parse_fig_labels("<caption>(#tab:foo) caption </caption>")
  (names(res$ref_table) %==% "tab:foo")
  (grepl("Table 1", res$content, fixed = TRUE))
  (!grepl("(#tab:foo)", res$content, fixed = TRUE))
  # works with indented too
  # https://github.com/rstudio/gt/issues/719
  res = parse_fig_labels("  <caption>(#tab:foo) caption </caption>")
  (names(res$ref_table) %==% "tab:foo")
  (grepl("Table 1", res$content, fixed = TRUE))
  (!grepl("(#tab:foo)", res$content, fixed = TRUE))
})

assert("biblio references section is correcly found", {
  # with a csl like https://www.zotero.org/styles/nature
  # reference div have more attributes
  html = c('<div id="refs" class="references csl-bib-body" line-spacing="2">',
           '<div id="ref-item1" class="csl-entry">',
           '<div class="csl-left-margin">1. </div><div class="csl-right-inline">Doe, J. <em>First book</em>. (Cambridge University Press, 2005).</div>',
           '</div>')
  (parse_references(html)$div %==% html[[1]])
  (length(parse_references(html)$refs) == 1)
  html[1] = '<div id="refs" class="references csl-bib-body">'
  (parse_references(html)$div %==% html[[1]])
  (length(parse_references(html)$refs) == 1)
  html[1] = '<div id="refs" class="references">'
  (parse_references(html)$div %==% html[[1]])
  (length(parse_references(html)$refs) == 1)
})
