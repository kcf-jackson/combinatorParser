# Primitives
success <- function(x, remaining) {
  list(x, remaining)
}


fail <- function(msg) {
  if (!missing(msg)) print(msg)
  list()
}


literal_def <- function(char0, input0) {
  if (char0 == "") return(fail("No more input"))

  x <- substr(input0, 1, 1)
  remaining <- substring(input0, 2)
  if (x == char0) return(success(char0, remaining))

  fail(glue::glue("Expecting {char0}. Got {x} instead."))
}


#' Literal parser
#' @description Make an single-character-matching parser
#' @param char0 A character.
#' @examples
#' parser0 <- literal("A")
#' parser0("ABC")  # succuess: list("A", "BC")
#'
#' parser0 <- literal("B")
#' parser0("ABC")  #   fail : list()
#' @export
literal <- curry::Curry(literal_def)


# Combinators
is_fail <- purrr::is_empty
is_success <- purrr::compose(`!`, is_fail)


#' And-then combinator
#' @param p1 A parser.
#' @param p2 A parser.
#' @param join_fun The function to join the results from the two parsers.
#' @examples
#' p3 <- and_then(literal("A"), literal("B"))
#' p3("ABCDE")   # success:  list("AB", "CDE")
#' p3("ACCDE")   #   fail :  list()
#'
#' p3 <- and_then(literal("A"), literal("B"), list)
#' p3("ABCDE")   # success:  list(list("A", "B"), "CDE"))
#' p3("ACCDE")   #   fail :  list()
#' @export
and_then <- function(p1, p2, join_fun = paste0) {
  function(input0) {
    res <- p1(input0)
    if (is_fail(res)) return(res)

    res2 <- p2(res[[2]])
    if (is_fail(res2)) return(res2)

    success(join_fun(res[[1]], res2[[1]]), res2[[2]])
  }
}


#' Or-else combinator
#' @param p1 A parser.
#' @param p2 A parser.
#' @examples
#' p4 <- or_else(literal("A"), literal("B"))
#' p4("ABCDE")  # success:  list("A", "BCDE"))
#' p4("BCCDE")  # success:  list("B", "CCDE"))
#' p4("CCCDE")  #   fail :  list()
#' @export
or_else <- function(p1, p2) {
  function(input0) {
    res <- p1(input0)
    if (is_success(res)) return(res)
    p2(input0)
  }
}


#' Map combinator
#' @param f A function to be applied to the parsed output.
#' @param p1 A parser.
#' @examples
#' parser0 <- literal("A")
#' add_prefix <- function(x) { paste0("PREFIX-", x) }
#' parser1 <- p_map(add_prefix, parser0)
#'
#' parser1("ABCDE")  # success:  list("PREFIX-A", "BCDE")
#' parser1("BBDED")  #  fail  :  list()
#' @export
p_map <- function(f, p1) {
  function(input0) {
    res <- p1(input0)
    if (is_fail(res)) return(res)
    success(f(res[[1]]), res[[2]])
  }
}


#' High-order combinator
#' Choice combinator
#' @param list_of_parsers A list of parsers.
#' @examples
#' p5 <- choice(list(literal("A"), literal("B"), literal("C")))
#' p5("ABCDE")  # success:  list("A", "BCDE")
#' p5("BBCDE")  # success:  list("B", "BCDE"))
#' p5("CBCDE")  # success:  list("C", "BCDE"))
#' p5("DBCDE")  #   fail :  list())
#' @export
choice <- function(list_of_parsers) {
  Reduce(or_else, list_of_parsers)
}


#' Any-of combinator
#' @param char_vec0 A vector of characters.
#' @examples
#' p6 <- any_of(c("A", "B", "C"))
#' p6("ABCDE")  # success:  list("A", "BCDE"))
#' p6("BBCDE")  # success:  list("B", "BCDE"))
#' p6("CBCDE")  # success:  list("C", "BCDE"))
#' p6("DBCDE")  #   fail :  list()
#' @export
any_of <- function(char_vec0) {
  choice(purrr::map(char_vec0, literal))
}


#' Sequence combinator
#' @param list_of_parsers A list of parser.
#' @examples
#' p7 <- sequence(list(literal("A"), literal("B"), literal("C")))
#' p7("ABCDE")  # success:  list("ABC", "DE"))
#' p7("BBCDE")  #   fail :  list()
#' @export
sequence <- function(list_of_parsers) {
  Reduce(and_then, list_of_parsers)
}


#' String combinator
#' @param str0 A character string.
#' @examples
#' p8 <- string("ABC")
#' p8("ABCDE")  # success:  list("ABC", "DE"))
#' p8("BBCDE")  #   fail :  list()
#' @export
string <- function(str0) {
  x <- unlist(strsplit(str0, ""))
  sequence(purrr::map(x, literal))
}


#' Zero-or-more combinator
#' @param p A parser.
#' @examples
#' p9 <- zero_or_more(literal("A"))
#' p9("AAABC")  # success:  list("AAA", "BC"))
#' p9("BBCDE")  # success:  list("", "BBCDE"))
#' @export
zero_or_more <- function(p) {
  or_else(
    and_then(p, zero_or_more(p)),
    function(input0) { success("", input0) }
  )
}


#' One-or-more combinator
#' @param p A parser.
#' @examples
#' p10 <- one_or_more(literal("A"))
#' p10("AAABC")  # success:  list("AAA", "BC"))
#' p10("BBCDE")  #   fail :  list()
#' @export
one_or_more <- function(p) { and_then(p, zero_or_more(p)) }


#' Zero-or-one combinator
#' @param p A parser.
#' @examples
#' p11 <- zero_or_one(literal("A"))
#' p11("AAABC")  # success:  list("A", "AABC"))
#' p11("BBCDE")  # success:  list("", "BBCDE"))
#' @export
zero_or_one <- function(p) {
  or_else(p, function(input0) { success("", input0) })
}


#' Xthen combinator
#' @param p1 A parser.
#' @param p2 A parser.
#' @examples
#' p12 <- xthen(string("extract"), string("THIS"))
#' p12("extractTHISpart")  # success:  list("THIS", "part"))
#' p12("BBCDE")  #   fail :  list()
#' @export
xthen <- function(p1, p2) {
  p_map(function(x) { x[[2]] }, and_then(p1, p2, list))
}


#' ThenX combinator
#' @param p1 A parser.
#' @param p2 A parser.
#' @examples
#' p12 <- thenx(string("keepthis"), string("IGNORE"))
#' p12("keepthisIGNORE")          # success:  list("keepthis", ""))
#' p12("keepthisIGNOREcontinue")  # success:  list("keepthis", "continue"))
#' p12("BBCDE")  # fail :  list()
#' @export
thenx <- function(p1, p2) {
  p_map(function(x) { x[[1]] }, and_then(p1, p2, list))
}


#' Between combinator
#' @param p1 A parser.
#' @param p2 A parser.
#' @param p3 A parser.
#' @examples
#' p0 <- zero_or_more(any_of(c(letters, " ", ".")))
#' p1 <- between(string("KEY-1"), p0, string("KEY-2"))
#'
#' input_str <- "KEY-1 this is content. KEY-2"
#' p1(input_str)   # success:  list(" this is content. ", "")
#' p1("BBCDE")     #   fail :  list()
#' @export
between <- function(p1, p2, p3) {
  thenx(xthen(p1, p2), p3) #' throw away p1, p3 results
}


#' Separated-by-at-least-one combinator
#' @param p A parser.
#' @param sep A character.
#' @examples
#' p14 <- sep_by_1(any_of(as.character(0:9)), literal(","))
#' p14("1,")        # success:  list("1", ","))
#' p14("1,2,")      # success:  list("12", ","))
#' p14("1,2,;")     # success:  list("12", ",;"))
#' p14("1,2,3,;")   # success:  list("123", ",;"))
#' p14("1;2,;")     # success:  list("1", ";2,;"))
#' p14(",1,2,3,;")  #   fail :  list()
#' @export
sep_by_1 <- function(p, sep) {
  sep_then_p <- xthen(sep, p)
  and_then(p, zero_or_more(sep_then_p))
}


#' Separated-by combinator
#' @param p A parser.
#' @param sep A character.
#' @examples
#' p15 <- sep_by(any_of(as.character(0:9)), literal(","))
#' p15("1,")        # success:  list("1", ","))
#' p15("1,2,")      # success:  list("12", ","))
#' p15("1,2,;")     # success:  list("12", ",;"))
#' p15("1,2,3,;")   # success:  list("123", ",;"))
#' p15(",1,2,3,;")  # success:  list("", ",1,2,3,;"))
#' @export
sep_by <- function(p, sep) {
  or_else(sep_by_1(p, sep), function(input0) { success("", input0) })
}
