#' @title Find USGS NWIS Stream Gages
#' @description \code{findNWIS} returns a \code{SpatialPointsDataFrame}
#' of all USGS NWIS gages for an Area of Interest. This dataset is accessed through the NWIS web portal and contains the following attributes:
#' \itemize{
#' \item 'OBJECTID'   : \code{character}  Unique ID in dataset
#' \item 'feature_id'   : \code{character}  NHD COMID for reach
#' \item 'site_no': \code{character}  NWIS ID number
#' \item 'site_name'   : \code{character}  Name of site
#' \item 'da_sqkm'   : \code{character}   Area drainign to gage in square kilometers
#' \item 'lat_reachCent'    : \code{numeric}    Latitude of the reach center, decimil degrees
#' \item 'lon_reachCent'    : \code{numeric}    Longitude of the reach center, decimil degrees
#' }
#' @param AOI A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids If TRUE, a vector of NIWS gage IDs are added to retuned list (default = \code{FALSE})
#' @param comids f TRUE, a vector of NHD COMIDs IDs are added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and nwis
#' @examples
#' \dontrun{
#' co = getAOI(state = "CO") %>% findNWIS()
#' }
#' @author Mike Johnson
#' @export
#'


findNWIS = function(AOI = NULL, siteType = "ST", paramCode = "00060", startDate = NULL, endDate = NULL, active = TRUE){

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  if(all(!is.null(startDate), is.null(endDate))){ endDate = Sys.Date() - 1}
  bb = round(AOI::bbox_st(AOI$AOI), 7)

  url = paste0("https://waterservices.usgs.gov/nwis/site/?format=mapper&bBox=",
               bb$xmin, ",", bb$ymin, ",", bb$xmax, ",", bb$ymax,
               if(!is.null(siteType)) { paste0("&siteType=", paste(siteType, collapse = ","))},
               if(full) {"&seriesCatalogOutput=true"},
               if(!is.null(paramCode)) { paste0("&parameterCd=", paste(paramCode, collapse = ","))},
               if(!is.null(startDate)) { paste0("&startDT=", startDate)},
               if(!is.null(endDate))   { paste0("&endDT=", endDate)},
               "&siteStatus=", ifelse(active, "active", "all"))

  dest = file.path(tempdir(), "tmp.xml")
  httr::GET(url, httr::write_disk(dest, overwrite=T), httr::add_headers('--header="Accept-Encoding: gzip"'))

  y          <- xml2::read_xml(dest)
  doc        <- xml2::xml_root(y)
  sc         <- xml2::xml_children(doc)
  sites      <- xml2::xml_children(sc)
  site_no    <- xml2::xml_attr(sites, "sno")
  station_nm <- xml2::xml_attr(sites, "sna")
  site_type  <- xml2::xml_attr(sites, "cat")
  lat        <- as.numeric(xml2::xml_attr(sites, "lat"))
  lon        <- as.numeric(xml2::xml_attr(sites, "lng"))
  agency_cd  <- xml2::xml_attr(sites, "agc")

  df <- data.frame(agency_cd, site_no, station_nm, site_type,
                   lat, lon, stringsAsFactors=FALSE)


  sp = sf::st_as_sf(x = df,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj)) %>% sf::as_Spatial()

  AOI[["nwis"]] = sp

  report = paste(length(unique(sp$site_no)), "USGS NWIS", paste0(siteType, collapse = ", "), "stations")
  ids = FALSE
  AOI = return.what(AOI, type = 'nwis', report, vals = if(ids){"site_no"} else {NULL})
  return(AOI)
}
