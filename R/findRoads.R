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

  `%+%` = crayon::`%+%`

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  FIP = sprintf("%05d", as.numeric(AOI::counties[AOI$AOI,]$geoid))

  urls = paste0("https://www2.census.gov/geo/tiger/TIGER2017/ROADS/tl_2017_",FIP, "_roads.zip")

  if (length(urls) > 1) { verb = 'are'; noun = 'files' } else { verb = 'is'; noun = 'file' }

  input.shp = list()

  sl <- for(i in seq_along(urls)){
    cat(crayon::white(paste0("Downloading (", i, "/", length(urls), "): ")) %+% crayon::yellow(basename(urls[i]), "\n"))
    x = download.url(urls[i])
    unzip(x$destfile, exdir = tempdir(), overwrite = TRUE)
    input.shp[[i]] = sf::read_sf(list.files(tempdir(), pattern = ".shp$", full.names = T))
    input.shp[[i]] = sf::st_transform(input.shp[[i]], as.character(AOI$AOI@proj4string))
    input.shp[[i]] = suppressMessages(suppressWarnings( sf::st_intersection(input.shp[[i]], sf::st_as_sf(AOI$AOI))))
    input.shp[[i]] = sf::as_Spatial(input.shp[[i]])
  }

  sl = do.call(rbind, input.shp)

  AOI[["tiger"]] <- raster::crop(x = sl,  y = raster::extent(AOI$AOI))

  cat(crayon::white("Returned object contains: ") %+% crayon::green("cropped TIGER road network\n"))

  return(AOI)
}

