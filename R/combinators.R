#' Or combinator (with multiple parsing results)
#' @description Success if any of the parsers returns success.
#' @param p1 A parser.
#' @param p2 A parser.
#' @export
`%alt%` <- function(p1, p2) lambda(p1(inp) %++% p2(inp))

#' Or combinator (with single parsing result)
`%|%` <- function(p, q) {
  function(cs) {
    x <- (p %alt% q)(cs)
    if (is_empty(x)) {
      return(x)
    } else {
      return(x[1])
    }
  }
}

#' And(-then) combinator and variants
#' @description Success if both parsers return success.
#' @param p1 A parser.
#' @param p2 A parser.
#' @name then-combinator
#' @export
`%then%` <- function(p1, p2) {
  function(inp) {
    res <- list()
    for (x in p1(inp)) {
      list(v1, out1) %<-% x
      for (y in p2(out1)) {
        list(v2, out2) %<-% y
        res[[length(res) + 1]] <- list(list(v1, v2), out2)
      }
    }
    res
  }
}

#' @rdname then-combinator
#' @examples
#' p12 <- xthen(string("extract"), string("THIS"))
#' p12("extractTHISpart")  # success:  list("THIS", "part"))
#' p12("BBCDE")  #   fail :  list()
#' @export
`%xthen%` <- function(p1, p2) (p1 %then% p2) %using% lambda(x[[2]])

#' @rdname then-combinator
#' @examples
#' p12 <- thenx(string("keepthis"), string("IGNORE"))
#' p12("keepthisIGNORE")          # success:  list("keepthis", ""))
#' p12("keepthisIGNOREcontinue")  # success:  list("keepthis", "continue"))
#' p12("BBCDE")  # fail :  list()
#' @export
`%thenx%` <- function(p1, p2) (p1 %then% p2) %using% lambda(x[[1]])

#' Constructor combinator
#' @description Allows chaining with a constructor function that parses the
#' matched results into an AST
#' @param p A parser.
#' @param f A function; typically a constructor of a datatype.
#' @export
`%using%` <- function(p, f) {
  function(inp) {
    Map(p(inp), f = function(x) {
      list(v, out) %<-% x
      list(f(v), out)
    })
  }
}
