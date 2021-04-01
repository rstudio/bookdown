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
  html = c('<div id="references" class="section level1 unnumbered">',
           '<h1>References</h1>',
           '',
           '<div id="refs" class="references csl-bib-body" line-spacing="2">',
           '<div id="ref-item1" class="csl-entry">',
           '<div class="csl-left-margin">1. </div><div class="csl-right-inline">Doe, J. <em>First book</em>. (Cambridge University Press, 2005).</div>',
           '</div>')
  (parse_references(html)$div %==% html[[4]])
  (length(parse_references(html)$refs) == 1)
  html[4] = '<div id="refs" class="references csl-bib-body">'
  (parse_references(html)$div %==% html[[4]])
  (length(parse_references(html)$refs) == 1)
  html[4] = '<div id="refs" class="references">'
  (parse_references(html)$div %==% html[[4]])
  (length(parse_references(html)$refs) == 1)
})

assert("i18n config can be retrieved ", {
  opts$set(config = list())
  # default
  (i18n("label", "tab", label_names) %==% "Table ")
  (i18n("ui", "chapter_name", ui_names) %==% "")
  (i18n("ui", "dummy", ui_names) %==% NULL)
  # config set
  opts$set(config = list(language = list(
    label = list(tab = "TABLE "),
    ui = list(chapter_name = "CHAPTER "))
  ))
  (i18n("label", "tab") %==% "TABLE ")
  (i18n("ui", "chapter_name") %==% "CHAPTER ")
  opts$set(config = list())
})

assert("label_prefix retrieves correct config", {
  fun = function(i) paste0("TAB-", i)
  opts$set(config = list(language = list(label = list(tab = fun))))
  (label_prefix("tab") %==% fun)
  (is.function(label_prefix("fig")))
  (label_prefix("fig")(1) %==% "Figure 1")
  (label_prefix("fig", sep = ":")(1) %==% "Figure 1:")
  opts$set(config = list())
})
