#' @title Find Watershed Boundary Geometries (WBD/HUC)
#' @description The United States Geological Survey maintains a hierarchical system of hydrologic units each assigned a
#' unique code (HUC). The hierarchical level is described by the number of digits in the code. A two-digit code (eg. HUC 2) is the coarsest unit of aggregation while the HUC 12 is the finest resolution.
#' The spatial geometries of these units are stored in the Watershed Boundary Dataset with coverage of the United States.
#' \code{findWBD} returns a \code{SpatialPolygonsDataFrame*} of WBD boundaries for the specified level within an AOI. Pending the query,
#' data comes from the USGS CIDA server or the USGS staged products FTP.\cr
#'
#' Below you can see the general factors for each HUC level:\cr
#' \tabular{ccccc}{
#'   Name \tab Digits \tab Average Size (sqmiles) \tab Example Name \tab Example Code \cr
#'   Region \tab 2 \tab 177,560 \tab Pacific Northwest \tab 17 \cr
#'   Subregion \tab 4 \tab  16,800  \tab Lower Snake \tab 1706\cr
#'   Basin \tab 6 \tab 10,596  \tab Lower Snake \tab 170601 \cr
#'   Subbasin \tab 8 \tab 700  \tab Imnaha River \tab 17060102 \cr
#'   Watershed \tab 10 \tab 227 \tab Upper Imnaha River \tab 1706010201 \cr
#'   Subwatershed \tab 12 \tab 40 \tab North Fork Imnaha River \tab 170601020101 \cr
#' }
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param level defines the HUC level of interest (default = 8)
#' @param subbasins If TRUE, all subbasins of the supplied level will be joined to retuned list
#' @param crop If TRUE, all objects are cropped to the AOI boundaries (default = \code{TRUE})
#' @param ids  If TRUE,  a vector of finest resolution HUC codes is added to returned list (default = \code{FALSE})
#' @export
#' @examples
#' \dontrun{
#' # Get Cropped HUC8s for AOI
#'  getAOI(list("UCSB", 10, 10)) %>% findWBD()
#'
#'# Get Cropped HUC10s for AOI
#'  getAOI(list("UCSB", 10, 10)) %>% findWBD(level = 10)
#'
#'# Get Cropped HUC8s, HUC10s and HUC12s for AOI
#'  getAOI(clip = list("UCSB", 10, 10)) %>% findWBD(level = 8, subbasins = TRUE)
#'
#'# Get uncropped HUC10s for AOI
#'  getAOI(clip = list("UCSB", 10, 10)) %>% findWBD(level = 10, crop = FALSE)
#' }
#' @author
#' Mike Johnson

findWBD = function(AOI,
                   level = 8,
                   subbasins = FALSE,
                   crop = TRUE,
                   ids = FALSE){

  `%+%` = crayon::`%+%`
  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}
  if(subbasins) { level =  seq(level, 12, 2) }

  a.proj = sp::spTransform(AOI$AOI, '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs')
  bb = bbox_st(a.proj)
  bb.ordered =  paste(bb$xmin,bb$ymin,bb$xmax,bb$ymax, sep = "%2C")

  ref = data.frame(huc = seq(2,12,2), endpoint = 1:6)

  for(i in 1:length(level)){

    cat(crayon::white(paste0("Downloading (", i, "/", length(level), "): ")) %+%
          crayon::yellow(paste0("HUC", level[i])), "\n")

    endpoint = ref[which(ref$huc == level[i]),]$endpoint

    url = paste0('https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/',
                 endpoint,
                 '/query?',
                 '&geometry=',
                 bb.ordered,
                 '&geometryType=esriGeometryEnvelope',
                 '&outFields=*',
                 '&returnGeometry=true',
                 '&returnZ=false',
                 '&returnM=false',
                 '&returnExtentOnly=false',
                 '&f=geoJSON')

    shp = sf::read_sf(url)
    shp = sf::st_transform(shp, as.character(AOI::aoiProj))

    shp = if(crop){
      suppressWarnings(suppressMessages( sf::st_intersection(shp, sf::st_as_sf(AOI$AOI)))) } else {
        suppressWarnings(suppressMessages(shp[sf::st_as_sf(AOI$AOI), ]))
      }

    AOI[[paste0("huc", level[i])]] = sf::as_Spatial(shp)
  }

  tmp = names(AOI)[grep("huc", names(AOI))]

  if(length(tmp) > 1){
    tmp = tmp[which.max(as.numeric(gsub("huc", "", tmp)))]
  }

  report = paste0( if(crop){"Cropped "} else {"Full "},   paste0("HUC", level, collapse = ", "), " boundaries")
  AOI = HydroData::return.what(AOI, type = tmp, report, vals = if(ids){toupper(tmp)})

  return(AOI)
}

