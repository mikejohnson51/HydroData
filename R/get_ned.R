get_ned = function(state = NULL, county = NULL, clip_unit = NULL, res = 1, keep.boundary = TRUE){
  
######### 1. Define Area of Interst #########
  
  shp = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basmap = FALSE)
  bb = matrix(shp@bbox , ncol = 2)
  
  # Defined by upper left Coordinates
  
  lon = head(sprintf("%03d", abs(seq(floor(bb[1,1]), ceiling(bb[1,2]), by = 1))), -1)
  lat = head(seq(ceiling(bb[2,2]), floor(bb[2,1]), by = -1), -1)
  mat = expand.grid(lat,lon)
    message("AOI defined, and shapefile determined.\n")

########## 2. Download Data ##########

  urls = vector()
  
  for(i in 1:dim(mat)[1]){
  urls[i] = paste0('https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/', res, '/IMG/n', mat[i,1],"w", mat[i,2],".zip")
  }
  
  if(length(urls) > 1){ 
    verb = 'are'
    noun = 'rasters'
  } else {
    verb = 'is'
    noun = 'raster'
  }

    message(paste("There", verb, length(urls), "NED", noun, "in this scene."))

  temp = tempfile()
  tempd = tempdir()
  
  for(i in 1:length(urls)){
    message(paste0("Downloading raster ", i, " of ", length(urls)))
    download.file(url = urls[i], destfile = temp, quiet = TRUE )
    unzip(temp, exdir = tempd, overwrite = TRUE)
    message(paste0("Finished downloading raster ", i, " of ", length(urls)))
  }

########## 3. Process Data ##########

  input.rasters <- lapply(list.files(tempd, pattern = ".img", full.names = TRUE), raster)
  bounds = spTransform(shp, input.rasters[[1]]@crs)
  
  for(j in 1:length(input.rasters)){
    if(!is.null(intersect(extent(input.rasters[[j]]),extent(bounds)))){
    input.rasters[[j]] <- crop(input.rasters[[j]], bounds)
    message("Raster Cropped.")
  } else {
    message("Raster ", j, " not needed")
  }
  }
  
  if(length(input.rasters) > 1){
    message("Mosaicing raster...")
    utils::flush.console()
    
    input.rasters$fun <- max
    input.rasters$na.rm <- TRUE
    
    mos = do.call(mosaic, input.rasters)
    
    gc()
  } else {
    mos = input.rasters[[1]]
  }
  
  unlink(temp)
  unlink(tempd)
  
  if(keep.boundary == TRUE){
    message("Returned object contains elevation raster and boundary shapefile")
    return(list(elev = mos, boundary = bounds))
  }else{
    message("Returned object contains elevevation raster")
    return(mos)
  }
 
}

test.ned = get_ned(state = "TX", county = "Harris", res = 1, keep.boundary = TRUE)
test.nlcd = get_nlcd_multi(state = "TX", county = "Harris", year = c(2001, 2006, 2011), keep.boundary = TRUE)

par(mfrow=c(1,2))

plot(test.ned$elev, col = col_elev)
plot(test.ned$boundary, add = T, lwd = 5)

plot(test.nlcd$lc$lc_2011, col = col_lc)
plot(test.nlcd$boundary, add = T, lwd = 5)
