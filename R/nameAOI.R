#' A function for returning a text string name of AOI
#'
#' @examples
#' nameAOI(state = "CA")
#'
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
      unit = paste0("boundary of ", setNames(state.name, state.abb)[state])
    }else{
      unit = paste0("boundary of ", state)
      }
    }
  }

if(!is.null(state) && !is.null(county)){
    if(nchar(state == 2)){
        unit = paste0("boundary of ", simpleCap(county) ," County, ", setNames(state.name, state.abb)[state])
      }else{
        unit = paste0("boundary of ", simpleCap(county) ," County, ", state)
    }
}
  return(unit)
}

