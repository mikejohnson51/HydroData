#' Download, unzip and read shapefile from URL
#'
#' Internal HydroData function
#'
#' @param URL path to data
#' @param type description of data being downloaded

#'
#' @examples
#' \dontrun{
#' download.shp(URL, type = "flowlines")
#' }
#'
#'
#' @export
#' @author
#' Mike Johnson


download.shp = function(URL, type){

  temp <- tempfile(fileext = ".zip")
  td <- tempdir()

  message("Trying URL ... \n")

  download.file(URL, destfile =  temp, quiet = TRUE)
  unzip(temp, exdir = td, overwrite = TRUE)

  sp = suppressWarnings(rgdal::readOGR(list.files(td, pattern = '.shp$', full.names = TRUE), stringsAsFactors = FALSE, verbose = FALSE))
  message("All ", type, " loaded: ", formatC(dim(sp)[1], format="d", big.mark=","), " in total.\n")

  unlink(dir(td))

  return(sp)

}



