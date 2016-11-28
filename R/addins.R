jquery_dependency = function() {
  htmltools::htmlDependency(
    'jquery', '2.2.3', bookdown_file('resources', 'jquery'),
    script = 'jquery.min.js'
  )
}

mathquill = function() {
  sys.source(bookdown_file('scripts', 'mathquill.R'), new.env())
}
