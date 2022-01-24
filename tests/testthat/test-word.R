test_that("process_markdown() correctly resolves reference", {
  skip_if_not_pandoc()
  # testing using markdown_document2() for snapshotting easier
  rmd <- local_rmd_file(
    "# Theory {#theory}", "", "see \\@ref(label1)", "",
    "## Some other header {#label1}", "", "Content"
  )
  out <- local_render(rmd, output_format = markdown_document2())
  expect_snapshot_file(out, "md-resolve-ref.md")
})
