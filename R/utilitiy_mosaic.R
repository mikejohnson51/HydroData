#' Mosaic
#'
#' Interal function for mosaicing a list of rasters
#'
#' @param input raster stack
#' @author Mike Johnson
#' @export


mosaic.hd = function(input){
  if(length(input) > 1){
    message("Mosaicing raster...")
    utils::flush.console()
    input$fun <- max
    input$na.rm <- TRUE
    mos = do.call(raster::mosaic, input)
    gc()
  } else {
    mos = input[[1]]
  }
  return(mos)
}
