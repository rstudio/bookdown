uglifyjs = function(file, args = NULL) {
  if (Sys.which('uglifyjs') == '') return(1)
  owd = setwd(dirname(file)); on.exit(setwd(owd), add = TRUE)
  file = basename(file)
  out = shQuote(xfun::with_ext(file, c('.min.js', '.min.map')))
  system2('uglifyjs', c(shQuote(file), '-o', out[1], '--source-map', out[2], args))
}

xfun::in_dir('inst/resources/gitbook/js', uglifyjs('app.js'))
