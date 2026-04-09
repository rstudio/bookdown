jquery_dependency = function() {
  jquerylib::jquery_core()
}

mathquill = function() {
  sys.source(bookdown_file('scripts', 'mathquill.R'), new.env())
}
