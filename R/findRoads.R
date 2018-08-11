#' @title Find US Census Bureau TIGER Road Networks
#'
#' @description \code{findRoads} returns a \code{SpatialLinesDataFrame} of TIGER road networks cropped to an Area of Interest.
#' This dataset is accessed through the US Census geo portal and contains the following attributes:
#' \itemize{
#' \item 'LINEARID'   : \code{character}  Linear feature identifier
#' \item 'FULLNAME'   : \code{character}  Feature Name
#' \item 'RTTYP': \code{character}  Route Type Code
#' \item 'MTFCC'   : \code{character}  Road Classification
#' \itemize{
#' \item S1100   Primary Roads
#' \item R1011   Railroad Feature (Main, Spur, or Yard)
#' \item R1051   Carline, Streetcar Track, Monorail, Other Mass Transit Rail)
#' \item R1052   Cog Rail Line, Incline Rail Line, Tram
#' \item S1100   Primary Road
#' \item S1200   Secondary Road
#' }
#' }
#' @param AOI A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @return a list() of minimum length 2: AOI and tiger
#' @examples
#' \dontrun{
#' AOI  = getAOI(clip = list("UCSB", 10, 10)) %>% findRoads()
#' }
#' @author Mike Johnson
#' @export
#'

findRoads = function(AOI = FALSE) {


  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  FIP = sprintf("%05d", as.numeric(AOI::counties[AOI$AOI,]$geoid))

  urls = paste0("https://www2.census.gov/geo/tiger/TIGER2017/ROADS/tl_2017_",FIP, "_roads.zip")

  if (length(urls) > 1) { verb = 'are'; noun = 'files' } else { verb = 'is'; noun = 'file' }

  message(paste("There", verb, length(urls), "TIGER", noun, "in this AOI"))

  input.shp = list()

  sl <- for(i in seq_along(urls)){
    input.shp[[i]] = download_shp(URL = urls[1], type = paste('TIGER', i))
    input.shp[[i]] = input.shp[[i]][AOI$AOI,]
  }

  sl = do.call(rbind, input.shp)

  AOI[["tiger"]] <- raster::crop(x = sl,  y = raster::extent(AOI$AOI))

  report = "Returned list includes: Cropped TIGER roads shapefile"

  return(AOI)
}

