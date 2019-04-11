join <- function(x) paste(unlist(x), collapse = "")

#' Zero-or-more combinator
#' @param p A parser.
#' @param f A function for combining two inputs.
#' @examples
#' p9 <- zero_or_more(literal("A"))
#' p9("AAABC")  # success:  list("AAA", "BC"))
#' p9("BBCDE")  # success:  list("", "BBCDE"))
#' @export
zero_or_more <- function(p, f = join) {
  ((p %then% zero_or_more(p, f)) %using% f) %|% success("")
}

#' One-or-more combinator
#' @param p A parser.
#' @param f A function for combining two inputs.
#' @examples
#' p10 <- one_or_more(literal("A"))
#' p10("AAABC")  # success:  list("AAA", "BC"))
#' p10("BBCDE")  #   fail :  list()
#' @export
one_or_more <- function(p, f = join) {
  (p %then% zero_or_more(p, f)) %using% f
}

#' Zero-or-one combinator
#' @param p A parser.
#' @examples
#' p11 <- zero_or_one(literal("A"))
#' p11("AAABC")  # success:  list("A", "AABC"))
#' p11("BBCDE")  # success:  list("", "BBCDE"))
#' @export
zero_or_one <- function(p) p %|% success("")


# Parser constructors (from strings and from character vector)
#' String parser constructor
#' @description Match a string.
#' @param str0 A character string.
#' @export
string <- function(str0) {
  if (str0 == "") return(success(""))
  y <- car_str(str0)
  x <- if_else(is_empty(y), NULL, y[[1]])
  xs <- if_else(is_empty(y), list(), y[[2]])
  (literal(x) %then% string(xs)) %using% join
}

#' One-of / Any-of combinator
#' @description Match any element in a vector.
#' @param cs A character vector.
#' @examples
#' p6 <- one_of(c("A", "B", "C"))
#' p6("ABCDE")  # success:  list("A", "BCDE"))
#' p6("BBCDE")  # success:  list("B", "BCDE"))
#' p6("CBCDE")  # success:  list("C", "BCDE"))
#' p6("DBCDE")  #   fail :  list()
#' @export
one_of <- function(cs) {
  Reduce(`%|%`, Map(string, cs))
}


#' #' High-order combinator
#' #' Choice combinator
#' #' @param list_of_parsers A list of parsers.
#' #' @examples
#' #' p5 <- choice(list(literal("A"), literal("B"), literal("C")))
#' #' p5("ABCDE")  # success:  list("A", "BCDE")
#' #' p5("BBCDE")  # success:  list("B", "BCDE"))
#' #' p5("CBCDE")  # success:  list("C", "BCDE"))
#' #' p5("DBCDE")  #   fail :  list())
#' #' @export
#' choice <- function(list_of_parsers) {
#'   Reduce(or_else, list_of_parsers)
#' }

#' #' Sequence combinator
#' #' @param list_of_parsers A list of parser.
#' #' @examples
#' #' p7 <- sequence(list(literal("A"), literal("B"), literal("C")))
#' #' p7("ABCDE")  # success:  list("ABC", "DE"))
#' #' p7("BBCDE")  #   fail :  list()
#' #' @export
#' sequence <- function(...) {
#'   Reduce(`%then%`, list(...))
#' }


#' Between combinator
#' @param p1 A parser.
#' @param p2 A parser.
#' @param p3 A parser.
#' @examples
#' p1 <- between(literal("("), string("hi!"), literal(")"))
#' p1("(hi!)")   # success:  list("hi!", "")
#' p1("BBCDE")   #   fail :  list()
#' @export
between <- function(p1, p2, p3) {
  p1 %xthen% p2 %thenx% p3
}

#' Separated-by combinator
#' @param p A parser.
#' @param sep A parser.
#' @examples
#' p14 <- sep_by(one_of(as.character(0:9)), literal(","))
#' p14("1,")        # success:  list("1", ","))
#' p14("1,2,")      # success:  list("12", ","))
#' p14("1,2,;")     # success:  list("12", ",;"))
#' p14("1,2,3,;")   # success:  list("123", ",;"))
#' p14("1;2,;")     # success:  list("1", ";2,;"))
#' p14(",1,2,3,;")  #   fail :  list()
#' @export
sep_by <- function(p, sep) {
  p %then% zero_or_more(sep %xthen% p, list)
}

#' Chained-by combinator
#' @param p A parser.
#' @param op A parser.
#' @examples
#' p15 <- one_of(as.character(0:9)) %chain_by% literal("+")
#' p15("1+")        # success:  list("1", "+"))
#' p15("1+2+")      # success:  list("1+2", "+"))
#' p15("1+2+;")     # success:  list("1+2", "+;"))
#' p15("1+2+3+;")   # success:  list("1+2+3", "+;"))
#' p15("1;2+;")     # success:  list("1", ";2+;"))
#' p15("+1+2+3+;")  #   fail :  list()
#' @export
`%chain_by%` <- function(p, op) {
  p %then% zero_or_more(op %then% p, list)
}
