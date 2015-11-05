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
