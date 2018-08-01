definePoint = function(point){

  if(any(class(point) == "sf")){
    pts = sf::st_coordinates(point)
    colnames(pts) = c("lon", "lat")
    pts = data.frame(pts)
  }else if(class(point) == "character"){
    pts = AOI::getPoint(point)
    point = sf::st_as_sf(x = pts, coords = c("lon", "lat"), crs = as.character(AOI::aoiProj))
  }else{
    pts = data.frame(lat = point[1], lon = point[2])
    point = sf::st_as_sf(x = pts, coords = c("lon", "lat"), crs = as.character(AOI::aoiProj))
  }

  return(list(coords = pts, geo = point))
}
