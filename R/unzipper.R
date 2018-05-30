unzip_crop = function(AOI = NULL, path, file.type = "img"){

  file <- tempfile()
  if(!file.exists(path)){stop("Zip folder does not exist")}

  if (!dir.create(file)) { stop("failed to create a temporary directory") }

  fname = utils::unzip(zipfile = path,  list = T)$Name
  f = fname[grepl(paste0(".",file.type,"$"), fname)]
  utils::unzip(path, files=f, exdir=file, overwrite=TRUE)

  data <- list.files(file, full.names = TRUE, recursive = F)

  data <- raster::raster(data)

  AOI = AOI %>% spTransform(data@crs)

  if(!is.null(intersect(extent(data),extent(AOI)))){
    data <- crop(data, AOI, snap = "out")
    message("Raster Cropped.")
  } else {
    message("Raster not needed")
  }

  data <- data * 1

  unlink(file, recursive = TRUE)

  return(data)
}


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
