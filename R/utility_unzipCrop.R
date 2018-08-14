unzip_crop = function(AOI = NULL, path, file.type = "img"){

  file <- tempfile()

  if(!file.exists(path)){stop("Zip folder does not exist")}
  if (!dir.create(file)) { stop("failed to create a temporary directory") }

  fname = utils::unzip(zipfile = path,  list = T)$Name
  f = fname[grepl(paste0(".",file.type,"$"), fname)]
  utils::unzip(path, files=f, exdir=file, overwrite=TRUE)

  dat <- list.files(file, full.names = TRUE, recursive = F)

  dat <- raster::raster(dat)

  AOI = sp::spTransform(AOI, dat@crs)

  if(!is.null(raster::intersect(raster::extent(dat),raster::extent(AOI)))){
    dat <- raster::crop(dat, AOI, snap = "out")
    #message("Raster Cropped.")
  } else {
    message("Raster not needed")
  }

  unlink(file, recursive = TRUE)

  return(dat)
}


