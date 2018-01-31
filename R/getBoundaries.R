getBasemap = function(AOI = NULL, type = NULL){
  if (is.null(type)){ type = 't'}
  if (!(type %in% c('s', 'h', 't'))){ stop("'type' must be 's' (satellite), 'h' (hybrid), or 't' (terrain)")}
  if (type == 't') { type = 'terrain'}
  if (type == 'h') { type = 'hybrid'}
  if (type == 's') { type = 'satellite'}

  bmap = dismo::gmap (AOI, lonlat = TRUE, type = type, scale = 1.5)
  return (bmap)
}


getBoundary = function(AOI = NULL){
  map <- readRDS('data/countymaps.rds')
  map <- map[AOI, ]
  return(map)
}

