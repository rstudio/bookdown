library(testit)

1 %==% 1

assert(
  'next_nearest() works',
  next_nearest(c(1, 4, 8), 1:9) %==% c(2L, 5L, 9L)
)

assert(
  'with_ext() works',
  with_ext(NULL, 'a') %==% NULL,
  with_ext('a', NULL) %==% 'a',
  with_ext('a', 'bcd') %==% 'a.bcd',
  with_ext('a.html', '') %==% 'a',
  with_ext('a.html', 'tex') %==% 'a.tex',
  with_ext('a.html', '.tex') %==% 'a.tex',
  with_ext(c('a', 'b', 'c'), 'css') %==% c('a.css', 'b.css', 'c.css'),
  with_ext(c('a.html', 'b', 'c.js'), 'css') %==% c('a.css', 'b.css', 'c.css'),
  with_ext(c('a.html', 'b', 'c'), '.css') %==% c('a.css', 'b.css', 'c.css'),
  with_ext('a', c('css', '.html')) %==% c('a.css', 'a.html'),
  with_ext(c('a.doc', 'b.gz', 'c'), c('css', '.tar', '.png')) %==% c('a.css', 'b.tar', 'c.png')
)

assert(
  'with_ext() signals an error when length(x) != length(ext)',
  has_error(with_ext(c('a', 'b.css'), c('foo', 'bar', 'ham')))
)

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

assert('source_files() handles several configurations correcly', {
  get_files = function(files = NULL, dirs = NULL, ...) {
    source_files(config = list(rmd_files = files, rmd_subdir = dirs), ...)
  }

  # create dummy projet
  dir.create(project <- tempfile())
  old = setwd(project)
  files = c(
    'index.Rmd', '_ignored.Rmd', '01-first.Rmd',
    'subdir/other.Rmd', 'subdir/_ignore.Rmd', 'subdir2/last.Rmd',
    'abc/def.Rmd', 'abc/ghi.Rmd'
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
  (get_files(list(html = 'index.Rmd'), NULL, 'html') %==% files[1])

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
