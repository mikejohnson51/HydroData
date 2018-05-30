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
#'


year = c(2006, 2011)
state = NULL
county = NULL
clip_unit = list("UCSB", 10,10)

getNLCD = function(state = NULL, county = NULL, clip_unit = NULL, year = 2011, boundary = FALSE, basemap = FALSE){

fin = list()
all.rast = list()
s =stack()

  if(!any(year %in% c(2001,2006,2011))){ stop("NLCD only avaiable for 2001, 2006, and 2011.")}

AOI = getAOI(state, county, clip_unit)

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
  urls = append(urls, paste0('https://prd-tnm.s3.amazonaws.com/StagedProducts/NLCD/data/',
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

if(length(urls)/length(year) > 1){
  verb = 'are'
  noun = 'rasters'
} else {
  verb = 'is'
  noun = 'raster'
}

message(paste("There", verb, length(urls)/length(year), "NLCD", noun, "in this scene per year."))


for(j in seq_along(year)){

xxx = grep(year[j], urls, value=TRUE)

for(i in seq_along(xxx)){
  message(paste0("Downloading ", year[j], " raster (", i, "/", length(xxx), ")"))
  check = download.url(url = xxx[i])
  if(check$code != 200){stop("Download Failed, please try again")}
  message(paste0("Finished downloading raster ", i, " of ", length(xxx)))
  rast = unzip_crop(AOI = AOI, path = check$destfile, file.type = "tif")
  all.rast[[i]] = rast
}

s = addLayer(s, mosaic.hd(all.rast))

}

names(s) = paste0("yr_", year)

items = list( name = nameAOI(state, county, clip_unit),
              source = "USGS National Land Cover Dataset",
              proj = s[[1]]@crs,
              nlcd = s)

report = paste0("Returned object contains land cover raster")

items = return.what(items = items, report = report, AOI = AOI, basemap = basemap, boundary = boundary, clip_unit= clip_unit, ids = NULL)

class(items) = "HydroData"
return(items)
}





