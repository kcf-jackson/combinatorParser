#' Or combinator (with multiple parsing results)
#' @description Success if any of the parsers returns success.
#' @param p1 A parser.
#' @param p2 A parser.
#' @export
`%alt%` <- function(p1, p2) {
  function(inp) {
    append(p1(inp), p2(inp))
  }
}

#' Or combinator (with single parsing result)
#' @name or-combinator
#' @description Success if any of the parsers returns success.
#' @param p1 A parser.
#' @param p2 A parser.
#' @export
`%|%` <- function(p1, p2) {
  function(inp) {
    x <- p1(inp)
    if (is_empty(x)) {
      y <- p2(inp)
      if (is_empty(y)) {
        return(y)
      } else {
        return(y[1])
      }
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
`%&%` <- `%then%` <- function(p1, p2) {
  function(inp) {
    res <- list()
    for (x in p1(inp)) {
      v1 <- if_else(is_empty(x), NULL, x[[1]])
      out1 <- if_else(is_empty(x), list(), x[[2]])
      for (y in p2(out1)) {
        v2 <- if_else(is_empty(y), NULL, y[[1]])
        out2 <- if_else(is_empty(y), list(), y[[2]])
        res[[length(res) + 1]] <- list(list(v1, v2), out2)
      }
    }
    res
  }
}

#' Constructor combinator
#' @description Allows chaining with a constructor function that parses the
#' matched results into an AST
#' @param p A parser.
#' @param f A function; typically a constructor of a datatype.
#' @export
`%using%` <- function(p, f) {
  function(inp) {
    Map(p(inp), f = function(x) {
      v <- if_else(is_empty(x), NULL, x[[1]])
      out <- if_else(is_empty(x), list(), x[[2]])
      list(f(v), out)
    })
  }
}

#' @rdname then-combinator
#' @export
`%join%` <- function(p1, p2) {
  (p1 %then% p2) %using% join
}

#' @rdname then-combinator
#' @examples
#' p12 <- string("extract") %xthen% string("THIS")
#' p12("extractTHISpart")  # success:  list("THIS", "part"))
#' p12("BBCDE")  #   fail :  list()
#' @export
`%xthen%` <- function(p1, p2) {
  f <- function(x) x[[2]]
  (p1 %then% p2) %using% f
}

#' @rdname then-combinator
#' @examples
#' p12 <- string("keepthis") %thenx% string("IGNORE")
#' p12("keepthisIGNORE")          # success:  list("keepthis", ""))
#' p12("keepthisIGNOREcontinue")  # success:  list("keepthis", "continue"))
#' p12("BBCDE")  # fail :  list()
#' @export
`%thenx%` <- function(p1, p2) {
  f <- function(x) x[[1]]
  (p1 %then% p2) %using% f
}
