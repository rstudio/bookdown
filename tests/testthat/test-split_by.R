test_that("invalid split_by produces error", {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")

  rmd <- local_rmd_file(
    c("---", "title: test split_by as numeric", "---", "",
      "# CHAPTER 1", "## SECTION 1", "### SUBSECTION 1",
      "#### SUBSUBSECTION 1",
      "", "# CHAPTER 2", "## SECTION 2",
      "##### LEVEL 5", "###### LEVEL 6"
    )
  )
  for (split_by in list(letters, 8, 'numbers+1', -1)){
    expect_error(local_render_book(rmd, output_format = gitbook(split_by = split_by)))
  }
})

test_that("gitbook() correctly splits with a specified level", {

  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")

  rmd <- local_rmd_file(
    c("---", "title: test split_by as numeric", "---", "",
      "# CHAPTER 1", "## SECTION 1", "### SUBSECTION 1",
      "#### SUBSUBSECTION 1",
      "", "# CHAPTER 2", "## SECTION 2",
      "##### LEVEL 5", "###### LEVEL 6"
    )
  )
  expected <- list(
    "0" = list(
      "toc-numbers" = c("1", "2"),
      "html-names-default" = c("", ""),
      "html-names-number" = c("", "")
    ),
    "1" = list(
      "toc-numbers" = c("1", "2"),
      "html-names-default" = c("chapter-1.html", "chapter-2.html"),
      "html-names-number" =  c("1-chapter-1.html", "2-chapter-2.html")
    ),
    "2" = list(
      "toc-numbers" = c("1", "1.1", "2", "2.1"),
      "html-names-default" = c("chapter-1.html", "section-1.html",
                               "chapter-2.html", "section-2.html"),
      "html-names-number" =  c("1-chapter-1.html", "1-1-section-1.html",
                               "2-chapter-2.html", "2-1-section-2.html")
    ),
    "3" = list(
      "toc-numbers" = c("1", "1.1", "1.1.1", "2", "2.1"),
      "html-names-default" = c("chapter-1.html", "section-1.html",
                               "subsection-1.html",
                               "chapter-2.html", "section-2.html"),
      "html-names-number" =  c("1-chapter-1.html", "1-1-section-1.html",
                               "1-1-1-subsection-1.html",
                               "2-chapter-2.html", "2-1-section-2.html")
    ),
    "4" = list(
      "toc-numbers" = c("1", "1.1", "1.1.1", "1.1.1.1", "2", "2.1"),
      "html-names-default" = c("chapter-1.html", "section-1.html",
                               "subsection-1.html", "subsubsection-1.html",
                               "chapter-2.html", "section-2.html"),
      "html-names-number" =  c("1-chapter-1.html", "1-1-section-1.html",
                               "1-1-1-subsection-1.html", "1-1-1-1-subsubsection-1.html",
                               "2-chapter-2.html", "2-1-section-2.html")
    ),
    "5" = list(
      "toc-numbers" = c("1", "1.1", "1.1.1", "1.1.1.1", "2", "2.1", "2.1.0.0.1"),
      "html-names-default" = c("chapter-1.html", "section-1.html",
                               "subsection-1.html", "subsubsection-1.html",
                               "chapter-2.html", "section-2.html",
                               "level-5.html"),
      "html-names-number" =  c("1-chapter-1.html", "1-1-section-1.html",
                               "1-1-1-subsection-1.html", "1-1-1-1-subsubsection-1.html",
                               "2-chapter-2.html", "2-1-section-2.html",
                               "2-1-1-1-1-level-5.html")
    ),
    "6" = list(
      "toc-numbers" = c("1", "1.1", "1.1.1", "1.1.1.1", "2", "2.1", "2.1.0.0.1", "2.1.0.0.1.1"),
      "html-names-default" = c("chapter-1.html", "section-1.html",
                               "subsection-1.html", "subsubsection-1.html",
                               "chapter-2.html", "section-2.html",
                               "level-5.html", "level-6.html"),
      "html-names-number" =  c("1-chapter-1.html", "1-1-section-1.html",
                               "1-1-1-subsection-1.html", "1-1-1-1-subsubsection-1.html",
                               "2-chapter-2.html", "2-1-section-2.html",
                               "2-1-0-0-1-level-5.html", "2-1-0-0-1-1-level-6.html")
    )
  )

  for (split_by in names(expected)) {
    for (toc_depth in seq_len(as.numeric(split_by))) {
      for (with_num in ""){
        html_path <- local_render_book(
          rmd,
          output_format = gitbook(split_by = paste0(split_by, with_num), toc_depth = toc_depth)
        )
        TOC <- xml2::xml_find_all(xml2::read_html(html_path), "//div[@class='book-summary']/nav/ul//li")
        expect_equal(
          xml2::xml_attr(TOC, "data-level"),
          intersect(
            expected[[split_by]][["toc-numbers"]],
            expected[[as.character(toc_depth)]][["toc-numbers"]]
          )
        )
        expect_equal(
          xml2::xml_attr(TOC, "data-path"),
          intersect(
            expected[[split_by]][["html-names-default"]],
            expected[[as.character(toc_depth)]][["html-names-default"]]
          )
        )
      }
    }
  }

})

test_that('try to write a temp file', {
  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")

  rmd <- local_rmd_file(
    c("---", "title: test split_by as numeric", "---", "",
      "# CHAPTER 1", "## SECTION 1", "### SUBSECTION 1",
      "#### SUBSUBSECTION 1",
      "", "# CHAPTER 2", "## SECTION 2",
      "##### LEVEL 5", "###### LEVEL 6"
    )
  )
  rmd <- local_rmd_file(
    c("---", "title: test split_by as numeric", "---", "",
      "# CHAPTER 1", "## SECTION 1", "### SUBSECTION 1",
      "#### SUBSUBSECTION 1",
      "", "# CHAPTER 2", "## SECTION 2",
      "##### LEVEL 5", "###### LEVEL 6"
    )
  )
  expect_true(file.exists(rmd))
  html_path <- local_render_book(rmd, output_format=gitbook(split_by='none'))
  expect_true(file.exists(html_path))
})

test_that("gitbook() split_by equivalents produce equivalent output", {

  skip_on_cran()
  skip_if_not_pandoc()
  skip_if_not_installed("xml2")

  equivalents <- list(
    list('none', '0', 0),
    list('chapter', '1', 1),
    list('section', '2', 2)
  )

  rmd<- local_rmd_file(
    c("---", "title: test split_by section", "---", "",
      "# CHAPTER 1", "## SECTION 1", "### SUBSECTION 1",
      "# CHAPTER 2", "## SECTION 2")
  )

  for (i in seq_along(equivalents)) {
    for (j in seq_along(equivalents[[i]])){
      html_path <- local_render_book(
        rmd,
        output_format = gitbook(split_by = equivalents[[i]][[j]], toc_depth = 1)
      )
      # using the first content as a baseline, check if each subsequent content matches it
      if (j == 1) content_baseline <- xml2::read_html(html_path) else
        expect_equal(content_baseline, xml2::read_html(html_path))
    }
  }
})