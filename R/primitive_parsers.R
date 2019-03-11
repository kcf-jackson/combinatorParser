success <- lambda(v, lambda(p, list(list(v, p))))

fail <- lambda(inp, list())

#' Satisfy parser constructor
#' @param predicate A predicate function.
#' @export
satisfy <- function(predicate) {
  function(inp) {
    if (is_empty(inp)) return(fail())

    list(x, xs) %<-% carStr(inp)
    if (predicate(x)) {
      return(success(x)(xs))
    } else {
      return(fail())
    }
  }
}

#' Literal parser
#' @description Match any single character.
#' @param predicate A predicate function.
#' @export
literal <- function(x) satisfy(lambda(v, v == x))
