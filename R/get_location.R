get_clip_unit = function(location = "UCSB", width = 10, height = NULL){

if(class(location) == "numeric"){ location = location }
  
if(class(location) == "character"){
  location = dismo::geocode(location)
  location = c(location$latitude, location$longitude)
}
  
  df = (height/2)/69
  dl = ((width/2)/69) / cos(location[1] * pi/180)
  
  south = location[1] - df
  north = location[1] + df
  west  = location[2] - dl
  east  = location[2] + dl

  coords = matrix(c(west, south,
                    east, south,
                    east, north,
                    west, north,
                    west, south), 
                    ncol = 2, byrow = TRUE) 
  
  P1 = Polygon(coords)
  
  shp = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  

return(shp)  
  
}


