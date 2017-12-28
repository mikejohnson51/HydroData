
tigerRoads = function(state = NULL, county = NULL, clip_unit = NULL){

AOI = define_AOI(state = state, county = county, clip_unit = clip_unit)
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
