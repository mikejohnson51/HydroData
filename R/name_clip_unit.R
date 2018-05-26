#' Interprete the English description of a clip unit input
#'
#' @param clip_unit a user supplied clip_unit
#'
#' @return a string describing the clipt unit in plain english
#' @family HydroData 'helper' functions
#' @author Mike Johnson
#' @export



name.clip.unit = function(clip_unit){

test = define.clip.unit(clip_unit)

if(class(test$location) == 'numeric'){
 test$location =  paste(paste(round(test$location,2), collapse = "/"), "(lat/lon)")
}

if(test$o == 'center'){
   test$o = "centered"
   name = paste0("A ", test$h, " mile tall by ", test$w, " mile wide region ", test$o, " on (the) ", test$location)
} else{

if(test$o == "lowerright"){ test$o = "lower right corner"}
if(test$o == "lowerleft"){ test$o = "lower left corner"}
if(test$o == "upperright"){ test$o = "upper right corner"}
if(test$o == "upperleft"){ test$o = " upper right corner"}

name = paste0("A ", test$h, " mile tall by ", test$w, " mile wide region with (the) ",  test$location, " in the ", test$o)


return(name)

}
}
