findNearestWaterbodies = function(point = NULL, n = 1, ids = FALSE, AOI = FALSE){

  h = 10
  w = 10
  lines = data.frame()
  pt = definePoint(point)

  while(dim(lines)[1] < n){
    lines <-  suppressWarnings(query_cida(AOI = suppressMessages(
      AOI::getAOI(clip = list(pt$coords$lat, pt$coord$lon, h, w))),
      type = 'nhdwaterbody',
      spatial = FALSE))

    if(is.null(lines)){lines = data.frame()}
    h = h + 2
    w = w + 2
  }

  dist = data.frame(objectid = lines$objectid, Distance_km = sf::st_distance(x = lines, y = pt$geo))
  dist = dist[order(dist$Distance_km)[1:n],]
  fin = merge(lines, dist, by = "objectid")

  if (any(AOI, ids)) {
    fin = list(closest_wb = fin)
    if (AOI) {
      fin[["AOI"]] = AOI::getBoundingBox(fin$closest_wb)
    }
    if (ids) {
      fin[["objectid"]] = unique(fin$closest_wb$objectid)
    }
  }

  return(fin)

}


