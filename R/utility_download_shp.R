#' Download, unzip and read shapefile from URL
#'
#' @param URL path to data
#' @param type description of data being downloaded used for messaging#'
#'
#' @family HydroData 'utility' function
#'
#' @examples
#' \dontrun{
#' download.shp(URL, type = "flowlines")
#' }
#'
#' @return a \code{Spatial} object retrived from URL and projected to "+init=epsg:4326"
#' @export
#' @author
#' Mike Johnson


download_shp = function(URL, type) {

  td   <- tempfile()
  temp <- tempfile(pattern = type, fileext = ".zip")

  message("Trying URL ... ")

  download.file(URL, destfile =  temp, quiet = T)
  unzip(temp, exdir = td, overwrite = TRUE)

  sp <- tryCatch({suppressWarnings(rgdal::readOGR(
        list.files(td, pattern = '.shp$', full.names = TRUE),
        stringsAsFactors = FALSE,
        verbose = FALSE,
        pointDropZ = TRUE))},
        error = function(e){
          return(NULL)
        }
      )


  if(is.null(sp)){ stop(paste0("0 ", type, " found"))} else {

  message("All ", type," loaded: ",
            formatC(
              dim(sp)[1],
              format = "d", big.mark = ","),
            " in total.\n")

    sp = sp::spTransform(sp, AOI::aoiProj)
    }



  unlink(temp, recursive=TRUE, force = TRUE)

  return(sp)

}


