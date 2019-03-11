lambda <- pryr::f

`%.%` <- pryr::`%.%`

`%+%` <- paste0

`%++%` <- append

is_empty <- lambda(identical(x, list()))

carStr <- lambda(list(substring(x, 1, 1), substring(x, 2)))

`%<-%` <- function(x, y) {
  x <- Map(deparse, substitute(x))[-1]
  if (is_empty(y)) {
    assign(x[[1]], NULL, envir = parent.frame())
    assign(x[[2]], list(), envir = parent.frame())
  } else {
    assign(x[[1]], y[[1]], envir = parent.frame())
    assign(x[[2]], y[[2]], envir = parent.frame())
  }
}
