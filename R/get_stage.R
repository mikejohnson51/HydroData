#' Convert Discharge to Stage via HAND synthetic rating curves
#'
#' Converts data, which can be found via \code{\link{readNWM}} to stage values
#' using the sythetic rating curves assoicated with the HAND methodology. These rating curves can be generated via
#' the \code{\link{get.rating}}.
#'
#' @param flows dataframe of flows generated from readNWM \code{\link{readNWM}}
#' @param rating_curves dataframe of synthetic rating curves \code{\link{get.rating}}
#'
#' @example
#' stage = get_stage(al.flows, al.ratingCurves)
#'
#' @export
#' @author
#'
#' Mike Johnson
#'
#'

get_stage = function(flows, rating_curves){

  if(length(unique(rating_curves$comid)) > length(unique(flows$comid))){
    index = rating_curves$comid %in% flows$comid
    rating_curves = rating_curves[index,]
  }else if(length(unique(rating_curves$comid)) < length(unique(flows$comid))){
    index = flows$comid %in% rating_curves$comid
    flows = flows[index,]}

comids = unique(flows$comid)
stage = NULL

  for(i in 1:length(comids)){
      flow = flows[flows$comid == comids[i],]
      curve = rating_curves[rating_curves$comid == comids[i],]

    for(j in 1:dim(flow)[1]){
      test = curve$stage[which.min(abs(curve$cms - flow$cms[j]))]
        if(length(test) > 0){test = test} else {test = NA}
        stage = rbind(stage, data.frame(flow[j, ], stage = test))
    }

  }

return(stage)

}



