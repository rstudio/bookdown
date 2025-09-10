test_that("gitbook_toc correctly process pandoc html without anchor section", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")
  rmd <- local_rmd_file(c("---", "title: test", "---", "",
                          "# T1", "", "## CHAP1", "",
                          "# T2 {-}", "", "## CHAP2"))
  res <- local_render_book(rmd, output_format = gitbook(anchor_sections = FALSE))
  content <- xml2::read_html(res)
  TOC <- xml2::xml_find_all(content, "//div[@class='book-summary']/nav/ul/li")
  expect_equal(xml2::xml_attr(TOC, "class"), c("chapter", "chapter"))
  expect_equal(xml2::xml_attr(TOC, "data-level"), c("1", ""))
  expect_equal(xml2::xml_attr(TOC, "data-path"), c("t1.html", "t2.html"))
  H1 <- xml2::xml_find_all(TOC, "a")
  expect_equal(xml2::xml_text(H1), c("1 T1", "T2"))
  H2 <- xml2::xml_find_all(TOC, ".//li/a")
  expect_equal(xml2::xml_text(H2), c("1.1 CHAP1", "1.2 CHAP2"))
})

test_that("gitbook_toc correctly process pandoc html with anchor section", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")
  rmd <- local_rmd_file(c("---", "title: test", "---", "",
                          "# T1", "", "## CHAP1", "",
                          "# T2 {-}", "", "## CHAP2"))
  res <- local_render_book(rmd, output_format = gitbook(anchor_sections = TRUE))
  content <- xml2::read_html(res)
  TOC <- xml2::xml_find_all(content, "//div[@class='book-summary']/nav/ul/li")
  expect_equal(xml2::xml_attr(TOC, "class"), c("chapter", "chapter"))
  expect_equal(xml2::xml_attr(TOC, "data-level"), c("1", ""))
  expect_equal(xml2::xml_attr(TOC, "data-path"), c("t1.html", "t2.html"))
  H1 <- xml2::xml_find_all(TOC, "a")
  expect_equal(xml2::xml_text(H1), c("1 T1", "T2"))
  H2 <- xml2::xml_find_all(TOC, ".//li/a")
  expect_equal(xml2::xml_text(H2), c("1.1 CHAP1", "1.2 CHAP2"))
  # no empty spans https://github.com/rstudio/bookdown/issues/1326
  expect_true(all(xml2::xml_find_lgl(TOC, "not(boolean(./a/span[count(node()) = 0]))")))
})

test_that("gitbook_toc correctly process pandoc html with non-ascii character", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")
  book <- local_book()
  # add a file with non-ascii
  xfun::in_dir(book, xfun::write_utf8(c("# Entraîneur", "", "some content", "", "## été", "", "Content"), "03-entraineur.Rmd"))
  res <- .render_book_quiet(book, output_format = gitbook())
  content <- xml2::read_html(res)
  TOC <- xml2::xml_find_all(content, "//div[@class='book-summary']/nav/ul/li")
  expect_equal(xml2::xml_attr(TOC, "class"), rep("chapter", 4))
  expect_equal(xml2::xml_attr(TOC, "data-path"), c("index.html", "introduction.html", "references.html", "entraîneur.html"))
  H1 <- xml2::xml_find_all(TOC, "a")
  expect_equal(xml2::xml_text(H1), c("Preface", "1 Introduction", "# References", "2 Entraîneur"))
  expect_equal(xml2::xml_attr(H1, "href"), c("index.html", "introduction.html", "references.html", "entraîneur.html"))
  H2 <- xml2::xml_find_all(TOC, ".//li/a")
  if (rmarkdown::pandoc_available("3.2.1")) {
    expect_equal(xml2::xml_attr(H2, "href"), "entraîneur.html#%C3%A9t%C3%A9")
  } else {
    expect_equal(xml2::xml_attr(H2, "href"), "entraîneur.html#été")
  }
  expect_equal(xml2::xml_text(H2), "2.1 été")
})

# https://github.com/rstudio/bookdown/pull/1408
# https://github.com/rstudio/bookdown/issues/1101
test_that("gitbook() correctly handles extra_dependency after its own", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")
  book <- local_book()
  res <- .render_book_quiet(
    book,
    output_format = gitbook(extra_dependencies = list(rmarkdown::html_dependency_font_awesome())),
  )
  content <- xml2::read_html(res)
  gitbook_css <- xml2::xml_find_first(content, "//head/link[contains(@href, 'gitbook')][last()]")
  extra_css <- xml2::xml_find_all(gitbook_css, "./following-sibling::link[contains(@href, 'font-awesome')]")
  expect_gt(length(extra_css), 0L)
})
