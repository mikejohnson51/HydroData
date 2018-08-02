mosaic.hd = function(input){
  if(length(input) > 1){
    message("Mosaicing raster...")
    utils::flush.console()
    input$fun <- max
    input$na.rm <- TRUE
    mos = do.call(mosaic, input)
    gc()
  } else {
    mos = input
  }
  return(mos)
}
