#' @title Find National Hydrography Dataset Waterbodies
#' @description \code{findWaterbodies} returns a \code{SpatialPolgonsDataframe} of all NHDwaterbodies within an AOI.
#' Data comes from the USGS CIDA server and contain 23 attributes, perhaps most notably:
#' \itemize{
#' \item 'objectid'   : \code{integer}  Integer value that uniquely identifies the waterbody of each feature in the NHD
#' \item 'comid'   : \code{character}  The COMID draining into the feature
#' \item 'fdate': \code{POSITct}  Date of last feature modification
#' \item 'gnis_id'   : \code{character}    Unique identifier assigned by GNIS
#' \item 'gnis_name'   : \code{character}    Proper name, term, or expression by which a particular geographic entity is known
#' \item 'meandepth'    : \code{numeric}     Mean depth of the waterbody
#' \item 'lakevolume'    : \code{numeric}    Total waterbody volume
#' \item 'maxdepth'   : \code{character}     Maximum depth of waterbody
#' \item 'meanused'   : \code{integer}  The average amount of water used
#' }
#' @param AOI A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids If TRUE, a vector of waterbody IDs is added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and waterboies
#' @export
#' @author Mike Johnson
#' @examples
#' \dontrun{
#' getAOI(clip = "Tuscaloosa") %>% findWaterbodies()
#' }
#'

findWaterbodies = function(AOI = NULL, area = NULL) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}
  f= NULL
  if(!is.null(area)){

    f = paste0('<ogc:And>',
               '<ogc:PropertyIsGreaterThan>',
               '<ogc:PropertyName>areasqkm</ogc:PropertyName>',
               '<ogc:Literal>',area,'</ogc:Literal>',
               '</ogc:PropertyIsGreaterThan>'
    )
  }

    sl = query_cida(AOI$AOI, type = "waterbodies", spatial = TRUE, filter = f)

    if(!is.null(sl)){

    AOI[["waterbodies"]] = sl

    report = paste(length(sl),  "NHD waterbodies")

    ids = FALSE
    AOI = return.what(AOI, type = 'waterbodies', report, vals = if(ids){"objectid"})
   }
    return(AOI)
  }
