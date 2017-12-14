#' A function for finding County and State shapefiles
#'
#' This function utilizes the Maptools package to generate a shapefile for a county or state given a user query.
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param proj a projction string. Default is a EPSG:4326
#'
#' @examples
#' get_region_sp(state = "CA", county = "San Luis Obispo")
#'
#' @return
#'
#' A character string
#'
#' @author
#' Mike Johnson
#' @export

get_region_sp = function(state = NULL, county = NULL, proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"){

  if(is.null(state)){

    stop("State must be provided!")

  }else if(nchar(state) > 2){

    state <- state

  }else{

    state <-setNames(state.name, state.abb)[state]

  }

region = paste0(tolower(state),",",tolower(county))

shp <- maps::map("county", region = region, fill = TRUE, plot = FALSE)

shp <- maptools::map2SpatialPolygons(shp, IDs=shp$names, proj4string = CRS(proj))


if(!is.null(county)){
if(length(shp) > 1){
t = vector()
for(i in 1:length(shp)){
  t[i] = (shp@polygons[[i]]@ID == region)
}

shp  = shp[which(t == TRUE),]

}
}

return(shp)

}




