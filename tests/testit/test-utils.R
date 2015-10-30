library(testit)

assert(
  'next_nearest() works',
  identical(next_nearest(c(1, 4, 8), 1:9), c(2L, 5L, 9L))
)