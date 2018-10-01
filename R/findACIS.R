#' @title Find Meterologic Stations from the Applied Climate Information System
#' @description Query station information from the applied climate information system (ACIS) API. \code{findACIS} returns a \code{SpatailPointsDataframe} of all
#' stations within an AOI. Some stations are a part of multiple networks. As such each SpatialPoint is marked by all networks and unique station ids within each.
#' Data comes from the \href{http://www.rcc-acis.org/index.html}{ACIS} and includes the following attributes:
#' \itemize{
#' \item 'uid'   : \code{character}  Unique station ID
#' \item 'NAME'   : \code{character}  Station name
#' \item 'state'    : \code{integer}    USA state
#' \item 'elev'    : \code{integer}    Station Elevation (feet)
#' \item 'staidX'   : \code{character}   Unique station id within network
#' \item 'network_typeX'    : \code{integer}   Station network
#' \item 'minDate'    : \code{integer}    Year of first observation
#' \item 'maxDate'    : \code{integer}    Year of last observation
#' }\cr
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids  If TRUE,  a vector of unique acis station IDs is added to retuned list (default = \code{FALSE})
#' @param param what observation type to search for: options include:
#' \itemize{
#' \item 'maxt'  :  Maximum temperature (Fahrenheit)
#' \item 'mint'  :  Maximum temperature (Fahrenheit)
#' \item 'avgt'  :  Maximum temperature (Fahrenheit)
#' \item 'prcpn' :  Precipitation       (inches)
#' \item 'snwd'  :  Snow depth          (inches)
#' \item 'snow'  :  Snowfall            (inches)
#' }
#' @return a list() of minimum length 2: AOI and and acis
#' @examples
#' \dontrun{
#' sta = getAOI(state = "CO", county = "El Paso") %>% findACIS()
#' }
#' @author Mike Johnson
#' @export


findACIS <- function (AOI, ids = FALSE, param = NULL) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  meta = HydroData::meta

  bbox = AOI$AOI %>% AOI::bbox_st()

  good.parm = c("maxt", "mint", "avgt", "pcpn", "snwd", "snow")

  if (is.null(param)) { param <-good.parm }

  bad.param = param[!(param %in% good.parm)]

  if(length(bad.param) >=1) {
   cat(crayon::red(bad.param, "is not a valid parameter\n"))
   cat(crayon::white("Use: ", paste0(meta$element$code[1:7], collapse = ", "), "\n"))
  }

  URL = paste0( "http://data.rcc-acis.org/StnMeta",
                "?elems=", paste(param, collapse = ","),
                "&bbox=",  paste(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, sep = ","),
                "&meta=",  paste('uid', 'name', 'state', 'll', 'elev', 'valid_daterange', 'sids', sep= ",")
                )

  base <- jsonlite::fromJSON(URL)

  dat = data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = F)

    names(dat) = c("uid", "name", "lon", "lat", "state", "elev")

    for(i in 1:NROW(base$meta)){

      tmp = as.data.frame(t(unlist(base$meta[i,])), stringsAsFactors = F)

      dat[[i, "uid"]] = tmp$uid
      dat[[i, "name"]] = tmp$name
      dat[[i, "lon"]] = tmp$ll1
      dat[[i, "lat"]] = tmp$ll2
      dat[[i, "state"]] = tmp$state
      dat[[i, "elev"]] = tmp$elev

      sids = tmp[grepl("sids", names(tmp))]

      for(j in seq_along(sids)){
        val = unlist(strsplit(as.character(sids[j]), " "))
        dat[i, paste0("staid", j)] = val[1]
        dat[i, paste0("network", j)] = meta$stationIdType$description[meta$stationIdType$code == as.numeric(val[2])]
      }

    dat[i, "minDate"] = min(unlist(tmp[grepl("date", names(tmp))]))
    dat[i, "maxDate"] = as.Date(max(unlist(tmp[grepl("date", names(tmp))])), "%Y-%m-%d")
    }

    dat$lon = as.numeric(dat$lon)
    dat$lat = as.numeric(dat$lat)
    dat$minDate = as.Date(dat$minDate, format = "%Y-%m-%d")
    dat$maxDate = as.Date(dat$maxDate, format = "%Y-%m-%d")
    #dat[is.na(dat)] <-  NULL

    dat = sf::st_as_sf(x = dat, coords = c("lon", "lat"), crs = as.character(AOI$AOI@proj4string))

    AOI[["acis"]] = sf::as_Spatial(dat)

    report = paste(length(dat$uid), "ACIS station(s)")

    AOI = return.what(AOI, type = 'acis', report, vals = if(ids){"uid"}else{NULL})

    return(AOI)

}




