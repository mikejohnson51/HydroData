get_flowlines = function(location = NULL, AOI.height = 10, AOI.width = 10, clip_unit = NULL){

  if(!is.null(clip_unit)){

    URL = paste0("https://cida.usgs.gov/nwc/geoserver/nhdplus/ows?service=WFS&version=2.0.0&request=GetFeature&typeNames=nhdplus:nhdflowline_network&srsName=EPSG:4326&bbox=",
                 min(clip_unit@bbox[2,]), ",",
                 min(clip_unit@bbox[1,]),  ",",
                 max(clip_unit@bbox[2,]), ",",
                 max(clip_unit@bbox[1,]),
                 "&outputFormat=SHAPE-ZIP")

    temp = tempfile()
    temp2 = tempfile()

    download.file(url = URL, destfile = temp, quiet = TRUE )
    unzip(temp, exdir =temp2)

    flowlines = readOGR(paste0(temp2,'/nhdflowline_network.shp'), verbose = FALSE) %>% spTransform(clip_unit@proj4string)

    test = flowlines[clip_unit,]
    unlink(temp); unlink(temp2)

    return(test)

  }

  if(!is.null(location)){latlon = get_location(location)}

  df = (AOI.height/2)/69
  dl = ((AOI.width/2)/69) / cos(location[2] * pi/180)

  south = location[2] - df
  north = location[2] + df
  west  = location[1] - dl
  east  = location[1] + dl

  URL = paste0("https://cida.usgs.gov/nwc/geoserver/nhdplus/ows?service=WFS&version=2.0.0&request=GetFeature&typeNames=nhdplus:nhdflowline_network&srsName=EPSG:4326&bbox=",
               south, ",",
               west,  ",",
               north, ",",
               east,
               "&outputFormat=SHAPE-ZIP")

  temp = tempfile()
  temp2 = tempfile()

  download.file(url = URL, destfile = temp, quiet = TRUE )
  unzip(temp, exdir =temp2)

  flowlines = readOGR(paste0(temp2,'/nhdflowline_network.shp'), verbose = FALSE)
  flowlines_84 = spTransform(flowlines, CRS("+proj=longlat +datum=WGS84"))

  return(flowlines84)


}





