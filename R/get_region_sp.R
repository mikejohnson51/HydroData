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

get_region_sp = function(state = NULL, county = NULL){

if(is.null(state)){stop("State must be provided!")}
state = toupper(state)

for(i in 1:length(state)){ if(nchar(state[i]) > 2){state[i] <- simpleCap(tolower(state[i]))}else{ state[i] <-setNames(state.name, state.abb)[state[i]] }}

map = readRDS('data/countymaps.rds')
map = map[map$STATE %in% state,]

if(is.null(county)){return(map)
  }else{
    county = simpleCap(tolower(county))
    map = map[map$NAME %in% county, ]
    return(map)
}
}

