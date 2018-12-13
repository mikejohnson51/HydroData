#' @title Find National Elevation Data (NED)
#' @description \code{findNED} returns \code{Raster} elevation data from the National Elevation Dataset (\href{https://nationalmap.gov/elevation.html}{NED}) for an AOI.
#' Data comes the the USA National Map.
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param res resolution of NED data. 1 equals 1 arc second, 13 equals 1/3 arc second.
#' @return a list() of minimum length 2: AOI and NED
#' @examples
#' \dontrun{
#'  el.paso.elev = getAOI(state = "CO", county = "El Paso") %>% findNED(res = 1)
#' }
#' @author Mike Johnson
#' @export



findNED = function(AOI = NULL, res = 1){

  `%+%` = crayon::`%+%`

  if(!(class(AOI) %in% c("list", "HydroData"))){AOI = list(AOI = AOI)}

  if(!(res %in% c(1,13))){stop("Resoultion must be either 1 (1 arc second) or 13 (1/3 arc second).")}

  bb = AOI$AOI@bbox

  lon = head(sprintf("%03d", abs(seq(floor(bb[1,1]), ceiling(bb[1,2]), by = 1))), -1)
  lat = head(seq(ceiling(bb[2,2]), floor(bb[2,1]), by = -1), -1)
  mat = expand.grid(lat,lon)

  urls = vector()
  urls.bu = vector()

  for(i in 1:dim(mat)[1]){
    urls[i] = paste0('https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/',
                     res,
                     '/IMG/USGS_NED_',
                     res,
                     '_n',
                     mat[i,1],
                     "w",
                     mat[i,2],
                     "_IMG.zip")}

  for(i in 1:dim(mat)[1]){
    urls.bu[i] = paste0('https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/',
                     res,
                     '/IMG/USGS_NED_',res, "n",
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

  #message(paste("There", verb, length(urls), "NED", noun, "in this AOI."))
  all.rast = list()

  for(i in seq_along(urls)){
    cat(crayon::white(paste0("Downloading (", i, "/", length(urls), "): ")) %+% crayon::yellow(basename(urls[i])), "\n")
      check <- download.url(url = urls.bu[i])
      if(check$code != 200){
       check = download.url(url = urls[i])
      }
      rast = unzip_crop(AOI = AOI$AOI, path = check$destfile)
      all.rast[[i]] = rast
  }

  fin = mosaic.hd(all.rast)

  AOI[["NED"]] = fin[[1]]

  cat(crayon::white("Returned object contains: ") %+% crayon::green("cropped", res, "arc sec elevation raster\n"))

  class(AOI) = "HydroData"
  return(AOI)
}


file.remove(tempdir(), recursive = T)


