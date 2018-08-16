#' @title Convert Spatial HydroData Objects to Simple Features
#' @description A function to convert all Spatial* HydroData objects to simple feature geometries.
#' Non-Spatial objects (eg raster and list components) will be skipped over.
#' @param hydro.data.object a HydroData object with Spatial components
#' @return a list with the same length as the input
#' @examples
#' \dontrun{
#' AOI = getAOI(clip = 'UCSB') %>% findNED %>% findNHD %>% findNWIS %>% to_sf
#' }
#' @export
#' @author Mike Johnson

to_sf = function(hydro.data.object = NULL){

  `%+%` = crayon::`%+%`

  b = vector()
  for(i in seq_along(hydro.data.object)){ b = append(b, grepl("Spatial", class(hydro.data.object[[i]]))) }
  sf = c(hydro.data.object[!b], lapply(hydro.data.object[b], sf::st_as_sf))

  cat(crayon::white('\nConverted to simple features: ') %+% crayon::cyan(paste(names(hydro.data.object[b]), collapse = ", ")), "\n")

  return(sf)
}


