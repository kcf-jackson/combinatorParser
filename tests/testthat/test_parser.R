# Turn example into unit test
unit_test <- function(expr, output) {
  res <- eval(quote(expr))
  # print(res)
  testthat::expect_equal(res, output)
}


testthat::test_that("literal", {
  unit_test(literal("A")("ABC"), list(list("A", "BC")))
  unit_test(literal("C")("ABC"), list())
})


testthat::test_that("then", {
  p3 <- literal("A") %then% literal("B")
  unit_test(p3("ABCDE"), list(list(list("A", "B"), "CDE")))
  unit_test(p3("ACCDE"), list())
})


p4 <- literal("A") %|% literal("B")
unit_test(p4("ABCDE"), list(list("A", "BCDE")))
unit_test(p4("BCCDE"), list(list("B", "CCDE")))
unit_test(p4("CCCDE"), list())


p8 <- string("ABC")
unit_test(p8("ABCDE"), list(list("ABC", "DE")))
unit_test(p8("BBCDE"), list())


p9 <- zero_or_more(literal("A"))
unit_test(p9("AAABC"), list(list("AAA", "BC")))
unit_test(p9("BBCDE"), list(list("", "BBCDE")))


p10 <- one_or_more(literal("A"))
unit_test(p10("AAABC"), list(list("AAA", "BC")))
unit_test(p10("BBCDE"), list())


p11 <- zero_or_one(literal("A"))
unit_test(p11("AAABC"), list(list("A", "AABC")))
unit_test(p11("BBCDE"), list(list("", "BBCDE")))


p12 <- string("extract") %xthen% string("THIS")
unit_test(p12("extractTHISpart"), list(list("THIS", "part")))
unit_test(p12("BBCDE"), list())


p12 <- string("keepthis") %thenx% string("IGNORE")
unit_test(p12("keepthisIGNORE"), list(list("keepthis", "")))
unit_test(p12("keepthisIGNOREcontinue"), list(list("keepthis", "continue")))
unit_test(p12("BBCDE"), list())


# p13 <- between(string("for"), string("{i=1:10}"), string("next"))
# unit_test(p13("for{i=1:10}next"), list("{i=1:10}", ""))
# unit_test(p13("BBCDE"), list())
#
#
# p14 <- sep_by_1(any_of(as.character(0:9)), literal(","))
# unit_test(p14("1,"), list("1", ","))
# unit_test(p14("1,2,"), list("12", ","))
# unit_test(p14("1,2,;"), list("12", ",;"))
# unit_test(p14("1,2,3,;"), list("123", ",;"))
# unit_test(p14("1;2,;"), list("1", ";2,;"))
# unit_test(p14(",1,2,3,;"), list())
#
#
# p15 <- sep_by(any_of(as.character(0:9)), literal(","))
# unit_test(p15("1,"), list("1", ","))
# unit_test(p15("1,2,"), list("12", ","))
# unit_test(p15("1,2,;"), list("12", ",;"))
# unit_test(p15("1,2,3,;"), list("123", ",;"))
# unit_test(p15(",1,2,3,;"), list("", ",1,2,3,;"))
#
