get_reservoir = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE ){

#Define AOI
  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basmap = FALSE)
  message("AOI defined!")

# Get Data
  URL = 'https://water.usgs.gov/GIS/dsdl/reservoir_shp.zip'
  temp  = tempfile()
  temp1 = tempfile()
  download.file(url = URL, destfile = temp, quiet = TRUE)
  unzip(temp, exdir = temp1)
  sp = readOGR(paste0(temp1, "/reservoir_shp.shp"), verbose = FALSE) %>% spTransform(AOI@proj4string)
    unlink(temp)
    unlink(temp1)
    message("Data downloaded!")

# Clip Data
  sp = sp[AOI, ]
  message("Data Clipped!")
  message(paste(length(sp), "reservoirs found in Area of Interest"))

  if(keep.boundary == TRUE){
      return(list(boundary = AOI, reservoirs = sp))
  }else{
      return(sp)
  }
}

