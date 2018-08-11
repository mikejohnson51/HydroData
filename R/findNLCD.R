#' @title Find National Land Cover Products (NLCD)
#' @description \code{findNLCD} returns \code{Raster} land cover data from the National Land Cover Dataset (\href{https://www.mrlc.gov}{NED}) for an AOI.
#' Data comes the the USA National Map and is avaialble for years 2001, 2006, 2011.
#' In additon to landcover, users can get data reflecting impervious surface and conaopy cover.
#' @param AOI A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param year the year(s) to download. Options include 2001, 2006, 2011. Default = 2011
#' @param type the type of data to downlaod. Options include landcover, canopy, and impervious. Default = landcover
#' @return a list() of minimum length 2: AOI and NLCD
#' @examples
#' \dontrun{
#'  dt = getAOI(clip = list("Devil Tower")) %>% findNLDC(2006, 'landcover')
#'  dt = getAOI(clip = list("Devil Tower")) %>% findNLDC(2011, 'canopy')
#'  dt = getAOI(clip = list("Devil Tower")) %>% findNLDC(2011, 'impervious')
#' }
#' @author Mike Johnson
#' @export

findNLCD = function(AOI = NULL, year = 2011, type = "landcover"){


if(!(type %in% c('canopy', 'impervious', "landcover"))){stop(paste0(type, " is not a valid NLCD layer."))}
if(type == "landcover") {ext = "_LC_" }
if(type == "canopy") {ext = "_CAN_" }
if(type == "impervious") {ext = "_IMP_" }

if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

all.rast = list()

if(!any(year %in% c(2001,2006,2011))){ stop("NLCD only avaiable for 2001, 2006, and 2011.")}

  USAbb = c(48,66,24,123)
  bb = matrix(AOI$AOI@bbox, ncol = 2)

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
  urls = append(urls, paste0('https://prd-tnm.s3.amazonaws.com/StagedProducts/NLCD/data/',
                   year[j],
                   '/',
                   type,
                   "/3x3/NLCD",
                   year[j],
                   ext,
                   "N",
                   mat[i,1],
                   "W",
                   mat[i,2],
                   ".zip"))
  }
}

if(length(urls)/length(year) > 1){
  verb = 'are'
  noun = 'rasters'
} else {
  verb = 'is'
  noun = 'raster'
}

message(paste("There", verb, length(urls)/length(year), "NLCD", noun, "in this AOI per year."))

for(j in seq_along(year)){

xxx = grep(year[j], urls, value=TRUE)

for(i in seq_along(xxx)){
  message(paste0("Downloading ", year[j], " raster (", i, "/", length(xxx), ")"))
  check = download.url(url = xxx[i])
  message(paste0("Finished downloading raster ", i, " of ", length(xxx)))
  rast = unzip_crop(AOI = AOI$AOI, path = check$destfile, file.type = "tif")
  all.rast[[i]] = rast
}

dat = mosaic.hd(all.rast)
#dat = raster::projectRaster(dat, crs = AOI::aoiProj, method = 'ngb')
AOI[[paste0("nlcd", year[j])]] = dat

}

report = paste0("Returned object contains land cover rasters for", paste(year, collapse = ","))

class(AOI) = "HydroData"
return(AOI)
}








