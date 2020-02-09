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

assert("correctly clean empty dir if required", {
       # do nothing is NULL (#857)
       (clean_empty_dir(NULL) %==% NULL)
       # remove if empty
       dir.create(temp_dir <- tempfile())
       clean_empty_dir(temp_dir)
       (dir_exists(temp_dir) %==% FALSE)
       # do no remove if not empty
       dir.create(temp_dir <- tempfile())
       writeLines("test", tempfile(tmpdir = temp_dir))
       (clean_empty_dir(temp_dir) %==% NULL)
       (dir_exists(temp_dir) %==% TRUE)
       unlink(temp_dir, recursive = TRUE)
       })
