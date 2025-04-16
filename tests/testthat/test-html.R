test_that("PART feature correctly works in HTML without anchor sections", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")
  rmd <- local_rmd_file(c("---", "title: test", "---", "",
                          "# (PART) T1 {-}", "", "# CHAP1", "",
                          "# (PART\\*) T2 {-}", "", "# CHAP2"))
  res <- local_render_book(rmd, output_format = gitbook(anchor_sections = FALSE))
  content <- xml2::read_html(res)
  TOC <- xml2::xml_find_all(content, "//div[@class='book-summary']/nav/ul/li")
  expect_equal(xml2::xml_attr(TOC, "class"), c("part", "chapter", "part", "chapter"))
  expect_equal(xml2::xml_text(TOC), c("I T1", "1 CHAP1", "T2", "2 CHAP2"))
})

test_that("PART feature correctly works in HTML with anchor sections", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")
  rmd <- local_rmd_file(c("---", "title: test", "---", "",
                          "# (PART) T1 {-}", "", "# CHAP1", "",
                          "# (PART\\*) T2 {-}", "", "# CHAP2"))
  res <- local_render_book(rmd, output_format = gitbook(anchor_sections = TRUE))
  content <- xml2::read_html(res)
  TOC <- xml2::xml_find_all(content, "//div[@class='book-summary']/nav/ul/li")
  expect_equal(xml2::xml_attr(TOC, "class"), c("part", "chapter", "part", "chapter"))
  expect_equal(xml2::xml_text(TOC), c("I T1", "1 CHAP1", "T2", "2 CHAP2"))
})

test_that("build_404 creates correct 404 page", {
  skip_on_cran()
  skip_if_not_pandoc()

  tmp_dir <- withr::local_tempdir()
  withr::local_dir(tmp_dir)

  # default page
  expect_false(file.exists("404.html"))
  expect_null(build_404()$rmd_cur)
  expect_false(file.exists("404.html"))

  # custom pages
  xfun::write_utf8(c("# Page not found", "", "I am created with _404.Rmd"), "_404.Rmd")
  build_404()
  expect_true(file.exists("404.html"))
  expect_match(xfun::file_string("404.html"), "I am created with _404.Rmd", fixed = TRUE)

  # do nothing if one exist
  expect_null(build_404()$html)

  unlink(c("404.html", "_404.Rmd"))

  xfun::write_utf8(c("# Page not found", "", "I am created with _404.md"), "_404.md")
  build_404()
  expect_true(file.exists("404.html"))
  expect_match(xfun::file_string("404.html"), "I am created with _404.md", fixed = TRUE)
})

test_that("add_toc_class() adds the class on correct toc element", {
  toc <- "<li><a href=\"06-share.html#sharing-your-book\"><span class=\"toc-section-number\">7</span> Sharing your book</a><ul>"
  expect_match(add_toc_class(toc), '^<li class="has-sub">')
  toc <- "<li><a href=\"06-share.html#sharing-your-book\"><span class=\"toc-section-number\">7</span> Sharing your book</a>"
  expect_match(add_toc_class(toc), '^<li class="has-sub">')
  toc <- "<li><a href=\"03-parts.html#parts\"><span class=\"toc-section-number\">4</span> Parts</a></li>"
  expect_no_match(add_toc_class(toc), '^<li class="has-sub">')
})

test_that("add_toc_class() works for all pandoc versions", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")
  md <- withr::local_tempfile(fileext = ".md")
  html <- withr::local_tempfile(fileext = ".html")
  xfun::write_utf8(c("# h1", "## h2", "# h12", "# h13", "## h34"), md)
  rmarkdown::pandoc_convert(md, to = "html4", from = "markdown",
                            options = c("--toc", "-s", rmarkdown::pandoc_metadata_arg("title", "test")),
                            output = html)
  res <- add_toc_class(xfun::read_utf8(html))
  content <- xml2::read_html(paste(res, collapse = "\n"))
  TOC <- xml2::xml_find_all(content, "//div[@id = 'TOC']/ul/li")
  expect_equal(xml2::xml_attr(TOC, "class"), c("has-sub", NA, "has-sub"))
})


test_that("Figure are numbered correctly", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")

  dir <- withr::local_tempdir("bookdown-test")
  rmd_file <- "figures.Rmd"
  file.copy(test_path("resources", rmd_file), dir)
  file.copy(test_path("resources", "md.png"), dir)
  withr::local_dir(dir)
  out <- rmarkdown::render(rmd_file, output_format = "bookdown::html_document2")

  content <- xml2::read_html(out)
  xpath <- if (rmarkdown::pandoc_version() >= "3") {
    "//div[@class='figcaption']"
  } else {
    "//p[@class='caption']"
  }
  figure <- xml2::xml_find_first(content, xpath)
  expect_match(xml2::xml_text(figure), "Figure 1.1", fixed = TRUE)

})
