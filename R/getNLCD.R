#' Get Multi-Year National Land Cover data for an Area of Interest
#'
#' Function to download land cover data for an area of interest from 2001, 2006, 2011
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with station data in a list
#' @param year year of land cover desired. Options include 2001, 2006, and 2011. Multiple years can be given as a vector.
#'
#' @examples
#' \dontrun{
#' el.paso.lc = get_nlcd_multi(state = 'CO', county = 'El Paso', year = c(2001, 2006, 2011))
#'
#' plot(el.paso.lc$lc$lc_2001, col = col_lc$color, main = "2001")
#' plot(el.paso.lc$boundary, add = TRUE, lwd = 4)
#' legend('topright',col_lc$description,fill=col_lc$color, cex = .5)
#'
#' plot(el.paso.lc$lc$lc_2006, col = col_lc$color, main = "2006")
#' plot(el.paso.lc$boundary, add = TRUE, lwd = 5)
#' legend('topright',col_lc$description,fill=col_lc$color, cex = .5)
#'
#' plot(el.paso.lc$lc$lc_2011, col = col_lc$color, main = "2011")
#' plot(el.paso.lc$boundary, add = TRUE, lwd = 5)
#' legend('topright',col_lc$description,fill=col_lc$color, cex = .5)
#' }
#'
#' @author
#' Mike Johnson

getNLCD = function(state = NULL, county = NULL, clip_unit = NULL, year = NULL, keep.boundary = FALSE){

if((sum(year != 2001) == length(year) && sum(year != 2006) && length(year) && sum(year != 2011) == length(year))){
  stop("NLCD only avaiable for 2001, 2006, and 2011. Please only use these values in declaring year.")}

AOI = getAOI(state = state, county = county, clip_unit = clip_unit)

message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading NLCD data for ", paste0(year, collapse = ", "),".")

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

message(paste("There", verb, length(urls)/length(year), "NLCD", noun, "in this scene per year.", length(urls), " tiles reuquested..."))

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

for(i in 1:length(year)){

xxx = grep(year[i], all.files, value=TRUE)
input.rasters <- lapply(xxx, raster)
bounds = spTransform(AOI, input.rasters[[1]]@crs)

for(j in 1:length(input.rasters)){
  if(!is.null(intersect(extent(input.rasters[[j]]),extent(bounds)))){
    input.rasters[[j]] <- crop(input.rasters[[j]], bounds)
    message("Raster number ", j," Cropped.")
  } else {
    message("Raster ", j, " not needed")
  }
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
  nlcd[[paste0("lc_", year[i])]] = input.rasters[[1]]
}
}

unlink(temp)
unlink(tempd)

if(keep.boundary == TRUE){
  return(list(nlcd, boundary = bounds))
}else{
  return(nlcd)
}
}





