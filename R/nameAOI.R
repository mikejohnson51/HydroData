#' A function for returning a text string name of AOI
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#'
#' @examples
#' \dontrun{
#' nameAOI(state = "CA")
#'}
#' @return
#'
#' A character string
#'
#' @export
#'
#' @author
#' Mike Johnson

nameAOI = function(state = NULL, county = NULL, clip_unit = NULL){

if(!is.null(clip_unit)){unit = "supplied shapefile"}

if(!is.null(state)){
  if(is.null(county)){
    if(nchar(state == 2)){
      unit = paste0("boundary of ", setNames(datasets::state.name, datasets::state.abb)[state])
    }else{
      unit = paste0("boundary of ", state)
      }
    }
  }

if(!is.null(state) && !is.null(county)){
  county.map = vector(mode= 'character')
  for(i in 1:length(county)){county.map = append(county.map, simpleCap(tolower(county[i])))}
  if(length(county.map) > 1) {county.map[length(county.map)] = paste("and", tail(county.map, n = 1))}
  county.map = paste(county.map, collapse = ', ')

    if(nchar(state == 2)){
        unit = paste0("boundary of ", county.map ," County, ", setNames(datasets::state.name, datasets::state.abb)[state])
      }else{
        unit = paste0("boundary of ", county.map ," County, ", state)
    }
}
  return(unit)
}

