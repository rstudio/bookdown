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

assert('number_appendix inserts the prefix and counters', {
  html = c(
    '<ul>',
    '<li><span class=\"toc-section-number\">1</span> One',
    '<li><span class=\"toc-section-number\">1.1</span> One subsection',
    '<li><span class=\"toc-section-number\">2</span> Two',
    '</ul>'
  )
  (number_appendix(html, 2, 5, 'toc', prefix = 'APP ', counters = letters) %==%
    c(
      '<ul>',
      '<li><span class=\"toc-section-number\">APP a</span> One',
      '<li><span class=\"toc-section-number\">a.1</span> One subsection',
      '<li><span class=\"toc-section-number\">APP b</span> Two',
      '</ul>'
    ))
  eng_ints <- function(n) {
    eng = c("one", "two", "three")[n[1]]
    rest = paste0(n[-1], collapse = ".")
    trimws(paste(eng, rest))
  }
  (number_appendix(html, 2, 5, 'toc', prefix = 'APP ', counters = eng_ints) %==%
    c(
      '<ul>',
      '<li><span class=\"toc-section-number\">APP one</span> One',
      '<li><span class=\"toc-section-number\">one 1</span> One subsection',
      '<li><span class=\"toc-section-number\">APP two</span> Two',
      '</ul>'
    ))
})
