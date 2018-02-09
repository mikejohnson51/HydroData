#' A function for capitilizing all words in a string
#'
#' @param x a string to be parsed.
#'
#' @examples
#' simpleCap("santa barbara")
#'
#' @export
#' @author
#' Mike Johnson

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
  }



HydroDataProj = sp::CRS('+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs')


