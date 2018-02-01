
tigerRoads = function(state = NULL, county = NULL, clip_unit = NULL){

  if(!is.null(clip_unit)){
    AOI = getFiatBoundary(clip_unit = clip_unit)
  } else {
    AOI = getAOI(state = state, county = county)
  }

fips = sprintf("%05d", AOI$FIPS)
    URL = paste0("https://www2.census.gov/geo/tiger/TIGER2015/ROADS/tl_2015_",fips, "_roads.zip")
     zip = tempfile()
       dir = tempdir()
         download.file(URL, zip)
         unzip(zip, exdir =  dir)
   unlink(dir)
         path = list.files(dir, pattern = ".shp$", full.names = TRUE)

   roads = readOGR(path)

 return(roads)

}

