`%++%` <- append

is_empty <- function(x) identical(x, list())

carStr <- function(x) {
  list(substring(x, 1, 1), substring(x, 2))
}

if_else <- function(test, yes, no) {
  if (test) yes else no
}
