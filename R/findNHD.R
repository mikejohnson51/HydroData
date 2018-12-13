#' @title Find National Hydrography River Networks
#' @description
#' \code{findNHD} returns a \code{SpatialLinesDataframe} of all NHDFlowlines reaches within an AOI.
#' Data comes from the USGS CIDA server and contain 90 attributes, perhaps most notably:
#' \itemize{
#' \item 'comid'   : \code{integer}  Integer value that uniquely identifies the occurrence of each feature in the NHD
#' \item 'reachcode'   : \code{character}  Unique identifier for a ‘reach’. The first eight numbers are the WBD_HUC8
#' \item 'fdate': \code{POSITct}  Date of last feature modification
#' \item 'gnis_id'   : \code{character}    Unique identifier assigned by GNIS
#' \item 'gnis_name'   : \code{character}    Proper name, term, or expression by which a particular geographic entity is known
#' \item 'lengthkm'    : \code{numeric}    Length of linear feature based on Albers Equal Area
#' \item 'areasqkm'    : \code{numeric}    Area of areal feature based on Albers Equal Area,
#' \item 'flowdir'   : \code{character}     Direction of flow relative to coordinate order.
#' \item 'wbareacomi'   : \code{integer}  The COMID of the waterbody through which the flowline flows.
#' \item 'ftype': \code{character}  unique identifier of a feature type
#' }
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids  If TRUE,  a vector of NHD COMIDs is added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and nhd
#' @examples
#' \dontrun{
#' nhd  = getAOI(clip = list("Tuscaloosa, AL", 10, 10)) %>% findNHD()
#' }
#' @author Mike Johnson
#' @export

findNHD = function(AOI = NULL, comid = NULL, nwis = NULL, streamorder = NULL, crop = T) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  f = NULL
  df = NULL

  if(!is.null(nwis)){
    url = paste0( "https://cida.usgs.gov/nldi/nwissite/", 'USGS-', nwis)
    mat = NULL
    for(i in 1:length(url)){
      c <- rawToChar(httr::RETRY("GET", url[i], times = 10, pause_cap = 240)$content)
      f.comid = jsonlite::fromJSON(c, simplifyVector = TRUE)
      comid = f.comid$features$properties$comid
      mat = rbind(mat, c(substrRight(url[i], 8), comid))
    }

    df = data.frame(nwis = mat[,1], comid = mat[,2], stringsAsFactors = FALSE)
    comid = df$comid
  }


  if(!is.null(streamorder)){

    f = paste0('<ogc:And>',
               '<ogc:PropertyIsGreaterThan>',
               '<ogc:PropertyName>streamorde</ogc:PropertyName>',
               '<ogc:Literal>',streamorder - 1,'</ogc:Literal>',
               '</ogc:PropertyIsGreaterThan>'
               )
  }


  if(!is.null(comid)){
    siteText <- ""

    for(i in comid){
      siteText <- paste0(siteText,'<ogc:PropertyIsEqualTo  matchCase="true">',
                         '<ogc:PropertyName>comid</ogc:PropertyName>',
                         '<ogc:Literal>',i,'</ogc:Literal>',
                         '</ogc:PropertyIsEqualTo>')
    }

    f = paste0('<ogc:Or>',
               siteText,
               '</ogc:Or>')
  }

  sl = query_cida(AOI = AOI$AOI, type = 'nhd', filter  = f, spatial = F)

  if(!is.null(df)) { sl = merge(sl, df, "comid")}

  if(!is.null(sl)){

    if(!is.null(AOI$AOI)){sl = sf::st_transform(sl, as.character(AOI::aoiProj)) %>%
      cropHUC(AOI = AOI$AOI, crop = crop)
    }

    AOI[["nhd"]] <- if(any(class(sl) %in% "sf")){ sf::as_Spatial(sl) } else { sl }

    report = paste(length(AOI$nhd), "nhd flowlines")

    ids = FALSE
    AOI = return.what(AOI, type = 'nhd', report, vals = if(ids){"comid"})
    AOI[sapply(AOI, is.null)] <- NULL
  }

  return(AOI)
}
