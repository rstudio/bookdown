uglifyjs = function(file, args = NULL) {
  if (Sys.which('uglifyjs') == '') return(1)
  owd = setwd(dirname(file)); on.exit(setwd(owd), add = TRUE)
  file = basename(file)
  out = shQuote(xfun::with_ext(file, '.min.js'))
  system2('uglifyjs', c(shQuote(file), '--source-map', '-o', out, args))
}

xfun::in_dir('inst/resources/gitbook/js', uglifyjs('app.js'))
