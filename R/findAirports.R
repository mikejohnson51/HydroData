#' Find Closest Airports
#'
#' @description
#' \code{findClosestAirports} returns a list and \code{SpatialPointsDataFrame*} of airports closest to a defined location\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}} and ID values (\code{ids = TRUE}) allow for Weather Underground data access via \code{getWU}.\cr\cr
#'
#' @param location given as name or a lon, lat pair
#' @param n Prescribe how many airports should be returned?
#'
#' @seealso  \code{\link{findLatLong}}
#' @seealso  \code{\link{findAirports}}
#' @seealso  \code{\link{getWU}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' A table of Airport data including identification codes, names, and distance from prescribed location
#'
#'@examples
#'\dontrun{
#' # Find Airport nearest you
#'
#' ap = findClosestAirports(location = findLatLong(), n = 1)
#'
#' # Find 5 closest Airports to Cal Poly SLO
#'
#' ap = findClosestAirports(location = 'Cal Poly SLO', n =5)
#'
#' explore(ap)
#'}
#'
#' @export
#' @author
#' Mike Johnson


findClosestAirports = function(location = NULL, n = 5){

  ap = HydroData::ap
  coords = cbind(ap[,7], ap[,6])
  air = SpatialPointsDataFrame(coords, ap)


  if(class(location) == 'numeric') { point = SpatialPoints(cbind(location[1], location[2]))
  } else if (class(location) == 'data.frame') {
    point = SpatialPoints(cbind(location$lon, location$lat))
  } else { x = dismo::geocode(location)
  point = SpatialPoints(cbind(x$longitude, x$latitude))
  }

  air@proj4string = CRS("+init=epsg:4326")
  point@proj4string = CRS("+init=epsg:4326")

  dist = spDistsN1(air, point, longlat = T)

  ndx = cbind(ap[(order(dist)[1:n]), 1:7],  dist[(order(dist)[1:n])])
  names(ndx) = c("Name", "City", "Country", "Digit3", "Digit4", "Lat", "Long", "Distance_km")

  return(list(table = ndx, airports = SpatialPointsDataFrame(cbind(ndx$Long, ndx$Lat), ndx)))
}









#' Find Airports
#'
#' @description
#' \code{findAirports} returns a list of \code{Spatial*} Objects cropped to an Area of Interest.\cr\cr
#' To better understand defining an AOI using '\emph{state}', '\emph{county}' and '\emph{clip_unit}' see \code{getAOI} and \code{getClipUnit}.\cr\cr
#' Returned \code{list} can be interactivly explored via \code{\link{explore}} and ID values (\code{ids = TRUE}) allow for Weather Underground data access via \code{getWU}.\cr\cr
#' All outputs are projected to \code{CRS '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+no_defs'}
#'
#' @param state    Full name(s) or two character abbriviation(s). Not case senstive
#' @param county    County name(s). Requires \code{state} input.
#' @param clip_unit SpatialObject* or list. For details see \code{getClipUnit}
#' @param boundary  If TRUE, the AOI \code{SpatialPolygon(s)} will be joined to returned list
#' @param basemap   If TRUE, a basemap will be joined to returned list
#'
#'  If a user wants greater control over basemap apperance replace TRUE with either:
#' \itemize{
#' \item't':  google terrain basemap
#' \item's':  google sattilite imagery basemap
#' \item'h':  google hybrid basemap
#' \item'r':  google roads basemap
#' }
#'
#' @param ids  If TRUE, returns a list of road names in AOI
#' @param save If TRUE, data is written to a HydroData folder in users working directory.
#'
#' @seealso  \code{\link{getAOI}}
#' @seealso  \code{\link{getWU}}
#' @seealso  \code{\link{explore}}
#'
#' @family HydroData 'find' functions
#'
#' @return
#' \code{findAirports} returns a named list of minimum length 1:
#'
#' \enumerate{
#' \item 'airports': A \code{SpatialPointsFrame*}\cr
#'
#' Pending parameterization, \code{findAirports} can also return:
#'
#' \item 'basemap':   A \code{RasterLayer*} basemap if \code{basemap = TRUE}
#' \item 'boundry':   A \code{SpatialPolygon*} of AOI if \code{boundary = TRUE}
#' \item 'fiat':      A \code{SpatialPolygon*} of intersected county boundaries if \code{boundary = TRUE}
#' \item 'ids':       A vector of 4 digit Airport Codes if \code{ids = TRUE}
#' }
#'
#' @examples
#'\dontrun{
#' # Find Airports in El Paso County, Colorado
#'
#' ap = findAirports(state = "CO", county = "El Paso", basemap = T, boundary = T)
#'
#' # Static Mapping
#'
#' plot(ap$basemap)
#' plot(ap$boundary, add = T, lwd = 3)
#' plot(ap$airports, add = T, pch = 16, cex = 2)
#'
#' # Generate Interactive Map
#'
#' explore(ap)
#'}
#'
#' @export
#' @author
#' Mike Johnson


findAirports = function(state = NULL, county = NULL, clip_unit = NULL, boundary = FALSE, basemap = FALSE, ids = FALSE, save = FALSE){

  ap = HydroData::ap
  coords = cbind(ap[,7], ap[,6])

  ap = ap[,1:7]
  names(ap) = c("Name", "City", "Country", "Digit3", "Digit4", "Lat", "Long")

  air = SpatialPointsDataFrame(coords, ap)
  air@proj4string = HydroDataProj

  items =  list()
  report = vector(mode = 'character')

  AOI = getAOI(state = state, county = county, clip_unit = clip_unit)
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ".\nShapefile determined.\nLoading Aiport Database")

  sp = air[AOI, ]

  message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " airports found within ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  items[['airports']] = sp ; report = append(report, "Returned list includes: airport shapefile")

  if (!(basemap == FALSE))  {
    if (basemap == TRUE) {
      type = 't'
      name = 'terrain'
    } else {
      type = basemap
    }

    if (type == 't') { name = 'terrain'   }
    if (type == 'h') { name = 'hybrid'    }
    if (type == 's') { name = 'satellite' }
    if (type == 'r') { name = 'roads'   }

    items[['basemap']] = getBasemap(AOI = AOI, type = type)
    report = append(report, paste(name, "basemap"))
  }


  if (boundary) { items[['boundary']] = AOI
  report = append(report, "AOI boundary")

  if (!is.null(clip_unit)) { items[['fiat']] = getFiatBoundary(clip_unit = AOI)
  report = append(report, "fiat boundary")
  }
  }

  if (ids) { items[['ids']] = sp$Digit4
  report = append(report, "four digit airport identifiers")
  }

  if (length(report) > 1) { report[length(report)] = paste("and",  tail(report, n = 1)) }
  message(paste(report, collapse = ", "))

  if(save){
    save.file(data    = items,
              state   = state,
              county  = county,
              clip_unit = clip_unit,
              agency  = 'NCAR',
              source  = "NCAR",
              dataset = "airports",
              other   = NULL )
  }

  return(items)
}


