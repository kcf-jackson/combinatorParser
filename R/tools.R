#' Report the input and output of a function
#' @param f A function.
#' @examples report(max)(3, 4, 5)
#' @export
report <- function(f) {
  fname <- deparse(substitute(f))
  function(...) {
    cat("========== The function '", fname, "' is called ==========\n", sep = "")
    cat("Input:\n")
    print(...)
    cat("\n")
    res <- do.call(f, list(...))
    cat("Output:\n")
    print(res)
    cat("\n")
    invisible(res)
  }
}


#' Memoisation of a parser
#' @description This is used to tackle (infinite) left recursion in parsing,
#' and it is done by memoising the input so that if the input is called twice
#' with the same parser without evaluation, then an infinite recursion is
#' registered.
#' @param p A parser.
#' @examples
#' \dontrun{
#' term_s <- literal("s")
#' term_s("s")
#' sum_s <- (sum_s %then% sum_s) %|% term_s
#' sum_s("ss")   # error: infinite left recursion
#' msum_s <- memoise(msum_s %then% msum_s) %|% term_s
#' msum_s("ss")
#' }
memoise <- function(p) {
  table0 <- list()
  attempt_table0 <- list()
  function(inp) {
    if (inp == "") return(list())
    computed <- !is.null(table0[[inp]])
    if (computed) {
      return(table0[[inp]])
    } else {
      deja_vu <- !is.null(attempt_table0[[inp]])
      if (deja_vu) {
        # this is only reached if it has been called, but not computed
        # print("Deja Vu happened!")
        # print(inp)
        return(list())
      } else {
        attempt_table0[[inp]] <<- TRUE
        res <- p(inp)
        table0[[inp]] <<- res
        return(res)
      }
    }
  }
}
