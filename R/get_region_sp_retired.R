#' A function for finding County and State shapefiles
#'
#' Generates a shapefile for a county or state from a user query complete with FIP code, and identifying names.
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string.
#'
#' @examples
#' get_region_sp(state = "CA")
#' get_region_sp(state = c("CA","Utah","Nevada"))
#' get_region_sp(state = "CA", county = "San Luis Obispo")
#' get_region_sp(state = "CA", county = c("San Luis Obispo", "Santa Barbara", "Ventura")
#'
#' @return
#'
#' SpatialPolygonDataFrame
#'
#' @author
#' Mike Johnson
#' @export
#'

get_region_sp_retired = function(state = NULL, county = NULL){

  if(is.null(state)){stop("State must be provided!")}
  state = toupper(state)
  states = list()
  regions = vector()

  for(i in 1:length(state)){ if(nchar(state[i]) > 2){states[[i]] <- state[i] }else{ states[[i]] <-setNames(state.name, state.abb)[state[i]] }}

  if(is.null(county)){for(i in 1:length(states)){regions = append(regions,paste0(tolower(states[i]),","))}
  }else{for(i in 1:length(states)){regions = append(regions,paste0(tolower(states[i]),",",paste0(tolower(county),"$")))}}

  shp <- maps::map("county",  fill = TRUE, plot = FALSE)
  shp <- maptools::map2SpatialPolygons(shp, IDs=shp$names, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  fips = maps::county.fips %>% tidyr::separate(polyname, into = c("state", "county"), sep = ',', remove = FALSE) %>% mutate(state = sapply(state, simpleCap) , county = sapply(county, simpleCap))

  colnames(fips) = c("FIPS", 'ID', "state", "county")

  shp = SpatialPolygonsDataFrame(shp, fips, match = FALSE)


  shp = shp[unique(grep(paste0(regions, collapse="|"), shp$ID, value = FALSE,)),]

  return(shp)

}




