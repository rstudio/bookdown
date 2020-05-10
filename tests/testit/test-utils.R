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

assert('correctly clean empty dir if required', {
  # do nothing is NULL (#857)
  (clean_empty_dir(NULL) %==% NULL)
  # remove if empty
  dir.create(temp_dir <- tempfile())
  clean_empty_dir(temp_dir)
  (dir_exists(temp_dir) %==% FALSE)
  # do not remove if not empty
  dir.create(temp_dir <- tempfile())
  writeLines('test', tempfile(tmpdir = temp_dir))
  (clean_empty_dir(temp_dir) %==% NULL)
  (dir_exists(temp_dir) %==% TRUE)
  unlink(temp_dir, recursive = TRUE)
})

assert('source_files() handles several configurations correcly', {
  # create dummy projet
  dir.create(project <- tempfile())
  old <- setwd(project)
  file.create(c("index.Rmd", "_ignored.Rmd", "01-first.Rmd"))
  dir.create("subdir")
  dir.create("subdir2")
  file.create(c("subdir/other.Rmd", "subdir2/last.Rmd"))
  # default behavior is all in root dir except _*.Rmd
  (source_files(format = NULL, config = list(), all = FALSE) %==%
    c("index.Rmd", "01-first.Rmd"))
  # using rmd_files allow to change default
  (source_files(format = NULL,
                config = list(rmd_files = "index.Rmd"),
                all = FALSE) %==%
    c("index.Rmd"))
  # format allows to filter selected files
  (source_files(format = 'html',
                config = list(rmd_files = list(html = "index.Rmd")),
                all = FALSE) %==%
    c("index.Rmd"))
  # rmd_subdir allows subdir contents and root Rmds
  (source_files(format = NULL,
                config = list(rmd_subdir = TRUE),
                all = FALSE) %==%
    c("index.Rmd", "01-first.Rmd", "subdir/other.Rmd", "subdir2/last.Rmd"))
  (source_files(format = NULL,
                config = list(rmd_subdir = "subdir"),
                all = FALSE) %==%
    c("index.Rmd", "01-first.Rmd", "subdir/other.Rmd"))
  # using rmd_files with subdir adds to subdir content
  (source_files(format = NULL,
                config = list(rmd_subdir = "subdir",
                              rmd_files = "01-first.Rmd"),
                all = FALSE) %==%
      c("01-first.Rmd", "subdir/other.Rmd"))
  (source_files(format = NULL,
                config = list(rmd_subdir = TRUE,
                              rmd_files = "01-first.Rmd"),
                all = FALSE) %==%
      c("01-first.Rmd", "subdir/other.Rmd", "subdir2/last.Rmd"))
  # clean tests
  unlink(project, recursive = TRUE); rm(project)
  setwd(old); rm(old)
})
