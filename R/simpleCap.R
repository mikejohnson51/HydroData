#' A function for capitilizing all words in a string
#'
#' @examples
#' simpleCap("santa barbara")
#'
#' @author
#' Mike Johnson

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
  }

