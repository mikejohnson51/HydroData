#' Find all WBD boundaries by location
#'
#' Function to locate all WBD basins at all nested levels. Input can be a know HUC code or a location within a basin of interest.
#' The nested leve of interest should be supplied by the level parameter. The subbassins parameter gets all nested levels beneth the levels specification.
#'
#' @param HUC a character string.
#' @param location a character string.
#' @param level specify whay HUC level you are interested in. Options include, 2,4,6,8,10,12.
#' @param subbasins logical. If TRUE, a shapefile of nested basin within specified level retuned as list object
#' @param keep.basemap logical. If TRUE, a google basemap will be returned with HUC shapefiles
#'
#' @examples
#'\dontrun{
#' harris.huc = getWBD(location = "Harris County, Texas", level = 6,
#'                     subbasins = T, keep.basemap = T)
#' plot(harris.huc$map)
#' plot(harris.huc$huc$huc6, add = T, lwd = 6)
#' plot(harris.huc$huc$huc8, add = T, pch = 16, lwd = 3)
#' plot(harris.huc$huc$huc10, add = T,  lwd = 1, border = 'red')
#'}
#' @export
#' @author
#' Mike Johnson

findWBD = function(HUC = NULL, location = NULL, level = 10, subbasins = FALSE, keep.basemap = FALSE){

    if(class(location) == "numeric"){

      # Check 1: Ensure location is length of 2
      if(length(location) != 2){stop("Location entered must be numeric vector of length 2: c(lat, lon) or a character string")}

      # Check 2: Ensure latitude is correct
      if(!(-14.4246950943  <= location[1] && location[1] <= 71.4395725902)){
        stop("Latitude must be vector element 1 and within: (-14.4246950943 <= x <= 71.4395725902)")}

      # Check 3: Ensure longitude is correct
      if( !(-179.229655487 <= location[2] && location[2] <= 179.856674735)){
        stop("Longitude must be vector element 2 and within: (-179.229655487 <= x <= 179.856674735)")}

        latlon = location

    } else if(class(location) == "character"){
      location = geocode(location)
      latlon = c(location$lat, location$lon)
    } else {
      stop
      print("Location must be of type 'numeric' or 'character'. \nNumeric values should be entered as a vector of c(lat,lon). \nPlace names should be surronded by qoutes. \nIf you are looking for your current location consider using the get_ip_loc() function.")
    }

  rm(location)

  if(!is.null(HUC)){

  HUC2 = substr(HUC,1,2)

    url = paste0('ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Hydrography/WBD/HU2/Shape/WBD_',HUC2,'_Shape.zip')

   # 'http://prd-tnm.s3-website-us-west-2.amazonaws.com/?prefix=StagedProducts/Hydrography/NHD/HU8/HighResolution/'

    temp <- tempfile()
    temp2 <- tempfile()

    download.file(url, temp)

    unzip(zipfile = temp, exdir = temp2, overwrite = TRUE)

    shp = rgdal::readOGR(paste0(temp2,'/Shape/WBDHU',nchar(HUC),".shp"))%>%
      spTransform(CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))


    ii = which(shp@data$HUC == HUC)
    shp = shp[ii,]

    unlink(temp)
    unlink(temp2)

  }

  if(!is.null(latlon)){

    sp = SpatialPoints(cbind(latlon[2], latlon[1]) , CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 "))

    #bb width and height = 1

    df = (3/2)/69
    dl = ((3/2)/69) / cos(latlon[1] * pi/180)

    south = latlon[1] - df
    north = latlon[1] + df
    west  = latlon[2] - dl
    east  = latlon[2] + dl

    URL = paste0("https://cida.usgs.gov/nwc/geoserver/nhdplus/ows?service=WFS&version=2.0.0&request=GetFeature&typeNames=nhdplus:nhdflowline_network&srsName=EPSG:4326&bbox=",
                 south, ",",
                 west,  ",",
                 north, ",",
                 east,
                 "&outputFormat=SHAPE-ZIP")

    temp <- tempfile()
    temp2 <- tempfile()

    download.file(URL, temp, quiet = TRUE)

    unzip(zipfile = temp, exdir = temp2)

    shp = suppressWarnings(rgdal::readOGR(paste0(temp2,'/nhdflowline_network.shp'), verbose = FALSE))

    HUC8 = unique(substr(shp$reachcode, 1, 8))

     URL = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_", HUC8 ,"_Shape.zip")

     temp3 <- tempfile()
     temp4 <- tempfile()

     download.file(URL, temp3, quiet = TRUE)
     unzip(zipfile = temp3, exdir = temp4)

if(subbasins == FALSE){
       shp = suppressWarnings(rgdal::readOGR(paste0(temp4,'/Shape/WBDHU', level ,'.shp'), verbose = FALSE)) %>%
         spTransform(CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

       if(level >= 8){
         shp = shp[sp,]
        } else {
         shp = shp}
     }else{
      shp = list()
     list = seq(level,12, 2)

     test = suppressWarnings(rgdal::readOGR(paste0(temp4,'/Shape/WBDHU', list[1] ,'.shp'), verbose = FALSE))%>%
       spTransform(CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

     shp[[paste0("huc",list[1])]] = test[sp,]

     value = as.character(shp[[1]][[paste0("HUC",list[1])]])

     for(i in 2:length(list)){
       test = suppressWarnings(rgdal::readOGR(paste0(temp4,'/Shape/WBDHU', list[i] ,'.shp'), verbose = FALSE))%>%
         spTransform(CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

        toMatch = grepl(value, as.character(test[[paste0("HUC",list[i])]]))

        shp[[paste0("huc",list[i])]] = test[toMatch, ]

     }
    }
  }
     unlink(temp3)
     unlink(temp4)

  if(keep.basemap){
    if(length (shp) > 1){map = shp[[1]]} else {map = shp}
      bmap = suppressWarnings(dismo::gmap(map, lonlat = TRUE))
    return(list (huc = shp, basemap = bmap))
  }else{ return (shp) }
}


