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
