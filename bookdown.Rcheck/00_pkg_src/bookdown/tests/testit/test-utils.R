library(testit)

assert('next_nearest() works', {
  (next_nearest(c(1, 4, 8), 1:9) %==% c(2L, 5L, 9L))
})

assert('clean_meta_tags() cleans HTML inside <meta>', {
  (clean_meta_tags('<meta name="foo" content="hi text">') %==%
     '<meta name="foo" content="hi text">')
  (clean_meta_tags('<meta name="foo" content="hi <strong>HTML</strong>">') %==%
    '<meta name="foo" content="hi HTML">')
})

assert('prepend_chapter_title() adds the chapter title to the page title', {
  h = '<head></head>'
  (prepend_chapter_title(h, '')  %==% h)

  h = '<title>asdf qwer</title>'
  (prepend_chapter_title(h, '')  %==% h)

  h = '<title>asdf qwer</title>'
  (prepend_chapter_title(h, '<h1>chapter one</h1>')  %==% '<title>chapter one | asdf qwer</title>')

  h = '<title>asdf qwer</title>'
  (prepend_chapter_title(h, '<h1>asdf qwer</h1>')  %==% '<title>asdf qwer</title>')

  h = '<title>asdf qwer</title><meta property="og:title" content="asdf qwer" />'
  (prepend_chapter_title(h, '<h1>chapter one</h1>')  %==%
      '<title>chapter one | asdf qwer</title><meta property="og:title" content="chapter one | asdf qwer" />')
})

assert('source_files() handles several configurations correctly', {
  get_files = function(files = NULL, dirs = NULL, md = NULL, ...) {
    source_files(config = list(rmd_files = files, rmd_subdir = dirs, include_md = md), ...)
  }

  # create dummy project
  dir.create(project <- tempfile())
  old = setwd(project)
  files = c(
    'index.Rmd', '_ignored.Rmd', '01-first.Rmd',
    'subdir/other.Rmd', 'subdir/_ignore.Rmd', 'subdir2/last.Rmd',
    'abc/def.Rmd', 'abc/ghi.Rmd', 'abc/jkl.md'
  )
  lapply(unique(dirname(files)), dir.create, FALSE, recursive = TRUE)
  file.create(files)

  # default behavior is all in root dir except _*.Rmd
  (get_files() %==% files[c(1, 3)])

  # using rmd_files allow to change default (_*.Rmd is always ignored, and
  # index.Rmd is always the first)
  (get_files(files[1]) %==% files[1])
  (get_files(files[1:2]) %==% files[1])
  (get_files(files[3:1]) %==% files[c(1, 3)])
  (get_files(files[4:1]) %==% files[c(1, 4, 3)])

  # format allows to filter selected files
  (get_files(list(html = 'index.Rmd'), NULL, NULL, 'html') %==% files[1])

  # rmd_subdir allows subdir contents and root Rmds
  (get_files(, TRUE) %==% files[c(1, 3, 7:8, 4, 6)])
  (get_files(, dirname(files[4])) %==% files[c(1, 3, 4)])
  (get_files(, dirname(files[c(4, 6)])) %==% files[c(1, 3, 4, 6)])
  (get_files(, dirname(files[c(4, 6, 7)])) %==% files[c(1, 3, 4, 6, 7:8)])

  # using rmd_files with subdir adds to subdir content
  (get_files(files[3], dirname(files[6])) %==% files[c(3, 6)])
  (get_files(files[3], TRUE) %==% files[c(3, 7:8, 4, 6)])
  (get_files(files[3], dirname(files[c(4, 6)])) %==% files[c(3, 4, 6)])
  (get_files(files[3], dirname(files[c(4, 6, 7)])) %==% files[c(3, 4, 6, 7:8)])

  # include_md toggles inclusion of md files
  (get_files(files[3], dirname(files[c(4, 6, 7)]), FALSE) %==% files[c(3, 4, 6, 7:8)])
  (get_files(files[3], dirname(files[c(4, 6, 7)]), TRUE) %==% files[c(3, 4, 6, 7:9)])

  # clean tests
  unlink(project, recursive = TRUE); rm(project)
  setwd(old); rm(old)

  TRUE
})

assert('lua_filter() works as expected', {
  (basename(lua_filter("custom-environment.lua")) %==% "custom-environment.lua")
})

if (pandoc2.0()) assert("bookdown_yml_arg() passes _bookdown.yml to Pandoc as the 'bookdown' field", {
  p = tempfile(); d = list(book_filename = 'cool'); a = bookdown_yml_arg(d, p)
  ("--metadata-file" %in% a)
  (yaml::read_yaml(p) %==% list(bookdown = d))
  unlink(p)
})

assert('fence_theorems() converts the knitr engine syntax to fenced Divs', {
  old = c(
    "```{theorem, label = \"thm\", name = \"My Theorem\"}",
    "Some text",
    "```",
    "",
    "# A header",
    "",
    "```{remark, name = \"My Remark\"}",
    "Some text",
    "```",
    "",
    "```{lemma, my-lem}",
    "Some text",
    "```",
    "```{proof, label = \"my-proof\", name = \"A proof\" , eval = TRUE}",
    "Some text",
    "```",
    "```{solution my-sol, name = \"My Solution\"}",
    "Some text",
    "```")
  new = c(
    "::: {.theorem #thm name=\"My Theorem\"}",
    "Some text",
    ":::",
    "",
    "# A header",
    "",
    "::: {.remark name=\"My Remark\"}",
    "Some text",
    ":::",
    "",
    "::: {.lemma #my-lem}",
    "Some text",
    ":::",
    "::: {.proof #my-proof name=\"A proof\" eval=TRUE}",
    "Some text",
    ":::",
    "::: {.solution #my-sol name=\"My Solution\"}",
    "Some text",
    ":::")

  res = fence_theorems(text = old)
  (unclass(res) %==% new)

  old = "# A header\n\nSome text"
  res = fence_theorems(text = old)
  (unclass(res) %==% old)

  # other chunk are not changed
  old = c("```{r, lab, echo=FALSE}", "1+1", "```")
  res = fence_theorems(text = old)
  (unclass(res) %==% old)
})
