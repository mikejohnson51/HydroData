mapFlood = function(stage = NULL, catchment = NULL, HAND = NULL){



  stage_hourly = list()
  stage_raster_hourly = list()
  flood_raster        = list()
  flood_polygon       = list()

  stage = melt(stage, c("dateTime", "comid"), "stage") %>%  dcast(comid ~ dateTime, fun.aggregate=NULL)

  ggplot() + geom_line(data = nwc.stage, aes(x = dateTime, y = stage, group = comid, color = comid))

  for(i in 2:dim(stage)[2]){ stage_hourly[[i-1]] = cbind(stage[,1], stage[,i])}

  recl_hourly =  flood.classification(catchment_raster = catchment, stage_hourly = stage_hourly)

  for ( i in 1:length(recl_hourly)){ stage_raster_hourly[[i]] = reclassify(catchment, rcl = recl_hourly[[i]]) }

  for (i in 1: length(stage_raster_hourly)){
    flood_raster[[i]] = stage_raster_hourly[[i]] - HAND
    values(flood_raster[[i]])[values(flood_raster[[i]]) < 0] = NA
    values(flood_raster[[i]])[values(flood_raster[[i]]) > 0] = 1
  }

  return(flood_raster)
}


flood.classification = function(catchment_raster, stage_hourly){

  recl_hourly = list()
  vals = unique(values(catchment_raster))

  for( i in 1:length(stage_hourly)){
    recl_hourly[[i]] = matrix(c(vals, rep(0, length(vals))), ncol = 2)
    for( j in 1:length(vals)){
      if(!length(which(stage_hourly[[i]][,1] == recl_hourly[[i]][j,1], arr.ind = TRUE))){
        recl_hourly[[i]][j,2] = NA
      }else{
        recl_hourly[[i]][j,2] = stage_hourly[[i]][which(stage_hourly[[i]][,1] == recl_hourly[[i]][j,1], arr.ind = TRUE),2]
      }}}

  return(recl_hourly)
}
