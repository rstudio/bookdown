library(testit)
if (!exists('%==%', mode = 'function')) `%==%` = function(x, y) identical(x, y)
test_pkg('bookdown')
