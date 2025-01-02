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

test_that("gitbook() correctly splits with a specified numeric", {
      
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")
 
  rmd <- local_rmd_file(
    c("---", "title: test split_by as numeric", "---", "",
    "# CHAPTER 1", "## SECTION 1", "### SUBSECTION 1", 
    "#### SUBSUBSECTION 1",
    "", "# CHAPTER 2", "## SECTION 2")
  )
  res <- local_render_book(rmd, output_format = gitbook(split_by = "4", toc_depth = 4))
  content <- xml2::read_html(res)
  
  TOC <- xml2::xml_find_all(content, "//div[@class='book-summary']/nav/ul//li")
  
  expect_equal(
    xml2::xml_attr(TOC, "data-level"), 
    c("1", "1.1", "1.1.1", "1.1.1.1", "2", "2.1")
  )
  expect_equal(
    xml2::xml_attr(TOC, "data-path"), 
    c("chapter-1.html", "section-1.html", "subsection-1.html", 
      "subsubsection-1.html", "chapter-2.html", "section-2.html")
  )
  
})

test_that("gitbook() split by section is equivalent of split by 2", {
      
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")
      
  rmd <- local_rmd_file(
    c("---", "title: test split_by section", "---", "",
      "# CHAPTER 1", "## SECTION 1", "### SUBSECTION 1",
      "# CHAPTER 2", "## SECTION 2")
  )

  resSection <- local_render_book(rmd, 
    output_format = gitbook(split_by = "section"))
  contentSection <- xml2::read_html(resSection)

  res2 <- local_render_book(rmd, 
    output_format = gitbook(split_by = "2"))
  content2 <- xml2::read_html(res2)

  expect_equal(object = content2, expected = contentSection)
  
})
