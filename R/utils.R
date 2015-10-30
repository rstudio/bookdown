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
