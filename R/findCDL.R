#' @title Find Cropland Land Cover Data from the Cropand Data Layer
#' @description \code{findCLD} returns \code{Raster} land cover data from the USDA Cropland Data Layer Dataset (\href{https://nassgeodata.gmu.edu/CropScape/}{Cropscape}) for an AOI.
#' @param AOI A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param year the year(s) to download. Options include 2008-2017 for most locations. Default = 2017
#' @return a list() of minimum length 2: AOI and cld
#' @examples
#' \dontrun{
#'  dt = getAOI(clip = list("Devil Tower")) %>% findCDL()
#'  dt = getAOI(clip = list("Devil Tower")) %>% findCDL(2011:2016)
#' }
#' @author Mike Johnson
#' @export

findCDL = function(AOI, year = 2017){

   `%+%` = crayon::`%+%`

   if(!(class(AOI) %in% c("list", "HydroData"))){AOI = list(AOI = AOI)}

   if(year > 2017 || year < 2008){stop("USDA CropScape Data only avaiable between 2008 and 2017. Please select a year within this range.")}

   cld.proj = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

   shp.p = sf::st_transform(sf::st_as_sf(AOI$AOI), cld.proj) %>% AOI::bbox_st()

   bb = paste(shp.p$xmin,shp.p$ymin, shp.p$xmax,shp.p$ymax, sep = ',')

   urls = paste0("https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=",year,"&bbox=", bb)

   for(j in seq_along(urls)){
     cat(crayon::white(paste0("Downloading (", j, "/", length(urls), "): ")) %+% crayon::yellow(year[j]), "\n")
     file = gsub(".*<returnURL>|</returnURL>.*", "", readLines(urls[j], warn = F))
     check = download.url(url = file)
     tmp = raster::raster(check$destfile)
     tmp = raster::projectRaster(tmp, crs = AOI::aoiProj, method = 'ngb')
     tmp = raster::crop(tmp, AOI$AOI)
     raster::colortable(tmp) <- col_crops$color
     AOI[[paste0("cld", year[j])]] = tmp
   }

   cat(crayon::white("Returned object contains: ") %+% crayon::green("cropped crop lands raster for" , paste(year, collapse = ", "),"\n"))

   class(AOI) = "HydroData"
   return(AOI)
}

