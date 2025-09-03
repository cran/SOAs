#' Another utility for array creation (Shi and Tang)
#' @rdname createABCcols
#'
#' @param k integer; determines the run size: the resulting array will have s^k runs
#'
#' @return \code{createABCcols} returns a list of Yates matrix column numbers to
#' be used for A, B, AB and C for the Shi and Tang (2020) family 1 construction
#'
#' @keywords internal
## function for the Shi and Tang (2020) family 1 construction
createABCcols <- function(k){
  if (k==5)
    return(list(Acols=  c( 1,2,4,8,16,  15,19,21,22),
            Bcols=  c(24,28,3,5,   10,   6,12,14,11),
            ABcols= c(25, 30, 7, 13, 26, 9,31,27,29),
            ## Ccols figured out by me
            ## using Theorem 1 of the JASA manuscript (by Shi and Xu)
            Ccols = c(6, 5, 10, 20, 7, 12, 7, 18, 17)))
  # columns for A and B that were previously used
  # with a suitable C column, but worse S pattern
  # return(list(Acols=  c( 1,  2, 4,  8, 16,  7, 11, 19, 29),
  #            Bcols=  c(24, 20, 9,  6,  5, 27, 17, 12, 3),
  #            ABcols= c(25, 22, 13,14, 21, 28, 26, 31, 30),
  ##    Ccols figured out by me
  ##     using Theorem 1 of the JASA manuscript (by Shi and Xu)
  #     Ccols = c(20, 24, 27, 17, 9,  9, 13, 5, 12)))
  if (k%%2==0){
    ## even k, start vector for k=4
    Acols <- c(1, 2, 4, 8, 15)
    Bcols <- c(12, 9, 3, 6, 5)
    ABcols <- c( 13, 11, 7, 14, 10)
    Ccols <- c(11, 7, 9, 5, 12)
    times <- (k - 4)/2
    startlen <- 4
  }else{
    ## odd k, start vector for k=7
    Acols <- c(1, 2, 4, 8, 15, 17, 18, 20, 24, 31, 33, 34, 36, 40, 47, 49, 50,
               52, 56, 63, 65, 66, 68, 72, 79, 81, 82, 84, 88, 95,
               97, 98, 100, 104, 111, 113, 114, 116, 120, 127)
    Bcols <- c(42, 37, 25, 3, 117, 74, 41, 10, 14, 102, 92, 69, 23, 6, 83, 90,
               73, 71, 21, 86, 54, 28, 7, 5, 57, 61, 44, 26, 19,
               53, 60, 12, 9, 13, 58, 55, 62, 35, 27, 38)
    ABcols <- c(43, 39, 29, 11, 122, 91, 59, 30, 22, 121, 125, 103,
                51, 46, 124, 107, 123, 115, 45, 105, 119, 94, 67, 77, 118,
                108, 126, 78, 75, 106, 93, 110, 109, 101, 85, 70, 76, 87,
                99, 89)
    Ccols <- c(60, 27,90,69,54,61,119,71,59,108,
               75,105,77,85,53,76,78,125,59,106,
               61,101,122,107,53,42,119,109,46,102,
               74,9,3,6,73,10,69,42,5,21)
    times <- (k-7)/2
    startlen <- 7
  }
  if (times > 0){
    for (i in 1:times){
      ## construction mechanism for recursion
      ## as Yates column numbers
      Acols <- c(Acols,
                 Acols + 2^(startlen),
                 Acols + 2^(startlen + 1),
                 Acols + 2^(startlen) + 2^(startlen + 1))
      Bcols <- c(Bcols,
                 Bcols + 2^(startlen + 1),
                 Bcols + 2^(startlen) + 2^(startlen + 1),
                 Bcols + 2^(startlen))
      ABcols <- c(ABcols,
                  ABcols + 2^(startlen) + 2^(startlen + 1),
                  ABcols + 2^(startlen),
                  ABcols + 2^(startlen + 1))
      Ccols <- c(Ccols,
                 Ccols + 2^(startlen + 1),
                 Ccols + 2^(startlen) + 2^(startlen + 1),
                 Ccols + 2^(startlen))
      startlen <- startlen + 2
    }
  }
  list(Acols=Acols, Bcols=Bcols, ABcols=ABcols, Ccols=Ccols)
}
