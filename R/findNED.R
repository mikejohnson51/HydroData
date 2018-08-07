#' @title Get NED data for an Area of Interest
#' @description Function to download elevation data for an area of interest
#' @param AOI a character string. Can be full name or state abbriviation
#' @param res resolution of NED data. 1 equals 1 arc second, 13 equals 1/3 arc second.
#' @export
#' @examples
#' \dontrun{
#' #Get 1 arc second elevation for El Paso County Colorado Springs
#'
#' el.paso.elev = get_ned(state = "CO", county = "El Paso", res = 1, keep.boundary = TRUE)
#' plot(el.paso.elev, col = col_elev)
#'}
#' @author
#' Mike Johnson

findNED = function(AOI = NULL, res = 1){

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  if(!(res %in% c(1,13))){stop("Resoultion must be either 1 (1 arc second) or 13 (1/3 arc second).")}

  bb = AOI$AOI@bbox

  lon = head(sprintf("%03d", abs(seq(floor(bb[1,1]), ceiling(bb[1,2]), by = 1))), -1)
  lat = head(seq(ceiling(bb[2,2]), floor(bb[2,1]), by = -1), -1)
  mat = expand.grid(lat,lon)

  urls = vector()

  for(i in 1:dim(mat)[1]){
    urls[i] = paste0('https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/',
                     res,
                     '/IMG/n',
                     mat[i,1],
                     "w",
                     mat[i,2],
                     ".zip")}

  if(length(urls) > 1){
    verb = 'are'
    noun = 'rasters'
  } else {
    verb = 'is'
    noun = 'raster'
  }

  message(paste("There", verb, length(urls), "NED", noun, "in this AOI."))
  all.rast = list()

  for(i in seq_along(urls)){
    message(paste0("Downloading raster ", i, " of ", length(urls)))
      check = download.url(url = urls[i], mode = "binary")
      message(paste0("Finished downloading raster ", i, " of ", length(urls)))
      rast = unzip_crop(AOI = AOI$AOI, path = check$destfile)
    all.rast[[i]] = rast
  }

  fin = mosaic.hd(all.rast)

  AOI[["NED"]] = fin[[1]]

  message(paste0("Returned object contains ", res," arc sec elevation raster"))

  class(AOI) = "HydroData"
  return(AOI)
}

