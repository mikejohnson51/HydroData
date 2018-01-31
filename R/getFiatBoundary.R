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

getFiatBoundary = function(state = NULL, county = NULL){

  if(is.null(state)){stop("State must be provided!")}
     state <- toupper(state)

  for(i in 1:length(state)){ if(nchar(state[i]) > 2){
      state[i] <- simpleCap(tolower(state[i]))
    }else{
      state[i] <-setNames(state.name, state.abb)[state[i]] }
  }

  map <- readRDS('data/countymaps.rds')
  map <- map[map$STATE %in% state,]

  if(is.null(county)){
    map <- gBuffer(map, byid=TRUE, width=0)
    map <-  maptools::unionSpatialPolygons(map, ID = map$STATEFP)
    return(map)
  }else{
      county.map <- vector(mode= 'character')
    for(i in 1:length(county)){county.map <- append(county.map, simpleCap(tolower(county[i])))}
      map <- map[map$NAME %in% county.map, ]
      return(map)
  }
}


