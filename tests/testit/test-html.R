library(testit)

assert("parse reference correctly", {
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
