get_nlcd_multi = function(state = NULL, county = NULL, clip_unit = NULL, year = NULL, keep.boundary = TRUE){

AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basmap = FALSE)

message("AOI defined, and shapefile determined.")

  USAbb = c(48,66,24,123)
  bb = matrix(AOI@bbox, ncol = 2)

  lon.bb = c(abs(ceiling(bb[1,2])), abs(floor(bb[1,1])))

  lon = seq(USAbb[2], USAbb[4], 3)
    i = which.max(1/(lon.bb[1] - lon))
    j = sum((lon.bb[2] - lon) > 0) #   which.max(1/(lon.bb[2] - lon))
    k = seq(i,j,1)
  lon.f = sprintf('%03d', lon[k])

  lat.bb = c(floor(bb[2,1]), ceiling(bb[2,2]))

  lat = seq(USAbb[3], USAbb[1], 3)
    i = which.max(1/(lat.bb[1] - lat))
    j = sum((lat.bb[2] - lat) > 0)
    k = seq(i,j,1)
  lat.f = lat[k]

mat = expand.grid(lat.f,lon.f)

urls = vector()

for(i in 1:dim(mat)[1]){
  for(j in 1:length(year)){
  urls = append(urls, paste0('https://s3-us-west-2.amazonaws.com/prd-tnm/StagedProducts/NLCD/data/',
                   year[j],
                   '/landcover/3x3/NLCD',
                   year[j],
                   "_LC_N",
                   mat[i,1],
                   "W",
                   mat[i,2],
                   ".zip"))
  }
}

if(length(urls) > 1){
  verb = 'are'
  noun = 'rasters'
} else {
  verb = 'is'
  noun = 'raster'
}

message(paste("There", verb, length(urls), "NLCD", noun, "in this scene."))

temp = tempfile()
tempd = tempdir()

for(i in 1:length(urls)){
  message(paste0("Downloading raster ", i, " of ", length(urls)))
  download.file(url = urls[i], destfile = temp, quiet = TRUE)
  suppressWarnings(unzip(temp, exdir = tempd, overwrite = TRUE))
  message(paste0("Finished downloading raster ", i, " of ", length(urls)))
}

all.files = list.files(tempd, pattern = ".tif$", full.names = TRUE)

####### Start Loop #########
nlcd = list()
bounds = spTransform(AOI, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80
+towgs84=0,0,0"))

for(i in 1:length(year)){

xxx = grep(year[i], all.files, value=TRUE)

input.rasters <- lapply(xxx, raster)

for(j in 1:length(input.rasters)){
  message("Cropping Raster...")
  input.rasters[[j]] <- crop(input.rasters[[j]], bounds)
  message("Raster Cropped.")
}

if(length(input.rasters) > 1){
  message("Mosaicing raster for ", year[i])
  utils::flush.console()

  input.rasters$fun <- max
  input.rasters$na.rm <- TRUE

  mos = do.call(mosaic, input.rasters)

  nlcd[[paste0("lc_", year[i])]] = mos

  gc()

} else {
  nlcd[[paste0("lc_", year[i])]] = mos
}
}


unlink(temp)
unlink(tempd)
#do.call(file.remove, list(list.files(tempd, full.names = TRUE)))
#do.call(file.remove, list(list.files(temp, full.names = TRUE)))

if(keep.boundary == TRUE){
  return(list(lc = nlcd, boundary = bounds))
}else{
  return(nlcd)
}

}





