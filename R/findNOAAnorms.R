#' @title Find stations from NOAAs 1981–2010 US Climate Normals
#' @description Query station information from the Query station information from NOAA 1981-2010 cliamte normats. \code{findNOAAnorms} returns a \code{SpatailPointsDataframe} of all
#' stations within an AOI. Data comes from the \href{ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/}{NOAA} and includes the following attributes:
#' \itemize{
#' \item 'ID'   : \code{character}  is the station identification code. The first two characters denote the FIPS country code, the third character is a network code identifiNG the station numbering systemused, and the remaining eight characters contain the actual
# station ID.
#' \item 'ELEV'   : Station name
#' \item 'ST.ABB'    : USA state abbriviation (USA station only)
#' \item 'NAME'    : Elevation of the station (in meters, missing = -999.9).
#' \item 'GSNFLAG'   : ndicates whether the station is part of the GCOS Surface Network (GSN)
#' \item 'HCNFLAG'    : Indicates whether the station is part of the U.S. Historical Climatology Network (HCN).
#' \item 'WMOID'    : World Meteorological Organization (WMO) station number
#' \item 'METHOD'    : Indicates whether a "traditional" or a "pseudonormals" approach was utilized for temperature or precipitation
#' }\cr
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids  If TRUE,  a vector of unique acis station IDs is added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and and acis
#' @examples
#' \dontrun{
#' sta = getAOI(state = "CO", county = "El Paso") %>% findNOAAnorms()
#' }
#' @author Mike Johnson
#' @export

findNOAAnorms = function(AOI, ids = FALSE){

  ## index: ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  x = readLines("ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/station-inventories/allstations.txt")

  mydata <- data.frame(ID      = substr(x, 1,  11),
                       LAT     = as.numeric(substr(x, 13, 20)),
                       LON     = as.numeric(substr(x, 22, 30)),
                       ELEV    = as.numeric(substr(x, 32, 37)),
                       ST.ABB  = substr(x, 39, 40),
                       NAME    = substr(x, 42, 71),
                       GSNFLAG = substr(x, 73, 75),
                       HCNFLAG = substr(x, 77, 79),
                       WMOID   = substr(x, 81, 85),
                       METHOD   = substr(x, 87, 99),
                       stringsAsFactors = FALSE
  )

  sp = sf::st_as_sf(x= mydata, coords = c("LON", "LAT"))
  sf::st_crs(sp) <- 4269
  sp = suppressMessages( sp[sf::st_as_sf(AOI$AOI), ])

if(!is.null(sp)){

  AOI[["nnorms"]] = as_Spatil(sp)

  report = paste(length(sp), "NOAA Normals stations")

  AOI = return.what(AOI, type = 'nnorms', report, vals = if(ids){"ID"})
}
return(AOI)
}


