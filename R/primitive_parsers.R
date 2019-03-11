success <- function(v) {
  function(inp) list(list(v, inp))
}

fail <- function(inp) list()

#' Satisfy parser constructor
#' @param predicate A predicate function.
#' @export
satisfy <- function(predicate) {
  function(inp) {
    if (is_empty(inp)) return(fail())

    y <- carStr(inp)
    x <- if_else(is_empty(y), NULL, y[[1]])
    xs <- if_else(is_empty(y), list(), y[[2]])
    if (predicate(x)) {
      return(success(x)(xs))
    } else {
      return(fail())
    }
  }
}

#' Literal parser
#' @description Match any single character.
#' @param ch A character.
#' @export
literal <- function(ch) {
  f <- function(v) v == ch
  satisfy(f)
}
