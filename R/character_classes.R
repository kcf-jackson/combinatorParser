# Classes of characters

#' Digits
#' @export
digit <- as.character(0:9)

#' Lower-case letters
#' @export
lower <- letters

#' Upper-case letters
#' @export
upper <- LETTERS

#' Punctuation
#' @export
punct <- c("|", " ", "!", "#", "$", "%", "&", "(", ")", "*", "+", ",",
           "-", ".", "/", ":", ";", ">", "=", "<", "?", "@", "[", "\\",
           "]", "^", "_", "`", "{", "}", "~")

#' Whitespaces
#' @export
blank <- c(" ", "\t", "\n")

#' Alphabet
#' @export
alpha <- c(lower, upper)

#' Alpha-numeric
#' @export
alnum <- c(alpha, digit)

#' Symbols (Alpha-numeric and Punctuation)
#' @export
symbs <- c(alnum, punct)
