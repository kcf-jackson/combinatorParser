join <- function(x) paste(unlist(x), collapse = "")

#' Zero-or-more combinator
#' @param p A parser.
#' @examples
#' p9 <- zero_or_more(literal("A"))
#' p9("AAABC")  # success:  list("AAA", "BC"))
#' p9("BBCDE")  # success:  list("", "BBCDE"))
#' @export
zero_or_more <- function(p) {
  ((p %then% zero_or_more(p)) %using% join) %|% success("")
}

#' One-or-more combinator
#' @param p A parser.
#' @examples
#' p10 <- one_or_more(literal("A"))
#' p10("AAABC")  # success:  list("AAA", "BC"))
#' p10("BBCDE")  #   fail :  list()
#' @export
one_or_more <- function(p) {
  (p %then% zero_or_more(p)) %using% join
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
  y <- carStr(str0)
  x <- if_else(is_empty(y), NULL, y[[1]])
  xs <- if_else(is_empty(y), list(), y[[2]])
  (literal(x) %then% string(xs)) %using% join
}

#' Character vector parser constructor
#' @description Match any element in a vector.
#' @param cs A character vector.
#' @export
one_of <- function(cs) {
  f <- function(x) x %in% cs
  satisfy(f)
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


#' #' Any-of combinator
#' #' @param char_vec0 A vector of characters.
#' #' @examples
#' #' p6 <- any_of(c("A", "B", "C"))
#' #' p6("ABCDE")  # success:  list("A", "BCDE"))
#' #' p6("BBCDE")  # success:  list("B", "BCDE"))
#' #' p6("CBCDE")  # success:  list("C", "BCDE"))
#' #' p6("DBCDE")  #   fail :  list()
#' #' @export
#' any_of <- function(char_vec0) {
#'   choice(purrr::map(char_vec0, literal))
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



#'
#' #' Between combinator
#' #' @param p1 A parser.
#' #' @param p2 A parser.
#' #' @param p3 A parser.
#' #' @examples
#' #' p0 <- zero_or_more(any_of(c(letters, " ", ".")))
#' #' p1 <- between(string("KEY-1"), p0, string("KEY-2"))
#' #'
#' #' input_str <- "KEY-1 this is content. KEY-2"
#' #' p1(input_str)   # success:  list(" this is content. ", "")
#' #' p1("BBCDE")     #   fail :  list()
#' #' @export
#' between <- function(p1, p2, p3) {
#'   thenx(xthen(p1, p2), p3) #' throw away p1, p3 results
#' }
#'
#'
#' #' Separated-by-at-least-one combinator
#' #' @param p A parser.
#' #' @param sep A character.
#' #' @examples
#' #' p14 <- sep_by_1(any_of(as.character(0:9)), literal(","))
#' #' p14("1,")        # success:  list("1", ","))
#' #' p14("1,2,")      # success:  list("12", ","))
#' #' p14("1,2,;")     # success:  list("12", ",;"))
#' #' p14("1,2,3,;")   # success:  list("123", ",;"))
#' #' p14("1;2,;")     # success:  list("1", ";2,;"))
#' #' p14(",1,2,3,;")  #   fail :  list()
#' #' @export
#' sep_by_1 <- function(p, sep) {
#'   sep_then_p <- xthen(sep, p)
#'   and_then(p, zero_or_more(sep_then_p))
#' }
#'
#'
#' #' Separated-by combinator
#' #' @param p A parser.
#' #' @param sep A character.
#' #' @examples
#' #' p15 <- sep_by(any_of(as.character(0:9)), literal(","))
#' #' p15("1,")        # success:  list("1", ","))
#' #' p15("1,2,")      # success:  list("12", ","))
#' #' p15("1,2,;")     # success:  list("12", ",;"))
#' #' p15("1,2,3,;")   # success:  list("123", ",;"))
#' #' p15(",1,2,3,;")  # success:  list("", ",1,2,3,;"))
#' #' @export
#' sep_by <- function(p, sep) {
#'   or_else(sep_by_1(p, sep), function(input0) { success("", input0) })
#' }
