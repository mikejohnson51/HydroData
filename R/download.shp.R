#' Download, unzip and read shapefile from URL
#'
#' @param URL path to data
#' @param type description of data being downloaded used for messaging
#'
#' @family HydroData 'helper' function
#'
#' @examples
#' \dontrun{
#' download.shp(URL, type = "flowlines")
#' }
#'
#' @export
#' @author
#' Mike Johnson


download.shp = function(URL, type) {

  td   <- tempfile()
  temp <- tempfile(pattern = type, fileext = ".zip")

  message("Trying URL ... ")

  download.file(URL, destfile =  temp, quiet = F)
  unzip(temp, exdir = td, overwrite = TRUE)

  sp = suppressWarnings(rgdal::readOGR(
    list.files(td, pattern = '.shp$', full.names = TRUE),
    stringsAsFactors = FALSE,
    verbose = FALSE
  ))

  if (dim(sp)[1] == 0) {
    stop ("No ", type, " found for this AOI.")
  } else {
    message("All ",
            type,
            " loaded: ",
            formatC(dim(sp)[1], format = "d", big.mark = ","),
            " in total.\n")
  }

  unlink(temp, recursive=TRUE, force = TRUE)

  return(sp)

}

