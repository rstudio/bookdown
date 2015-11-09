bookdown_file = function(...) {
  system.file(..., package = 'bookdown')
}

# find the y[j] closest to x[i] with y[j] > x[i]
next_nearest = function(x, y) {
  n = length(x); m = length(y); z = integer(n)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      if (y[j] > x[i]) {
        z[i] = y[j]
        break
      }
    }
  }
  z
}

# change the filename extension
with_ext = function(x, ext) {
  sub('[.][[:alnum:]]+$', ext, x)
}

# counters for figures/tables
new_counters = function(type, len) {
  base = matrix(0L, nrow = len, ncol = length(type), dimnames = list(NULL, type))
  list(
    inc = function(type, which) {
      base[which, type] <<- base[which, type] + 1L
    }
  )
}

# set some internal knitr options
set_opts_knit = function(config) {
  # use labels of the form (\#label) in knitr
  config$knitr$opts_knit$bookdown.internal.label = TRUE
  # when the output is LaTeX, force LaTeX tables instead of default Pandoc tables
  # http://tex.stackexchange.com/q/276699/9128
  config$knitr$opts_knit$bookdown.table.latex = TRUE
  config
}

readUTF8 = function(input) {
  readLines(input, encoding = 'UTF-8', warn = FALSE)
}

load_config = function() {
  if (file.exists('_config.yml')) yaml::yaml.load_file('_config.yml') else list()
}
