get_WBD = function(HUC = NULL, location = NULL, level = 8, subbasins = FALSE){

  # WBD Spatal Extent
  #West_Bounding_Coordinate: -179.229655487
  #East_Bounding_Coordinate: 179.856674735
  #North_Bounding_Coordinate: 71.4395725902
  #South_Bounding_Coordinate: -14.4246950943

    if(class(location) == "numeric"){

      # Check 1: Ensure location is length of 2
      if(length(location) != 2){stop("Location entered must be numeric vector of length 2: c(lat, lon).")}


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

  if(!is.null(HUC)){

  HUC2 = substr(HUC,1,2)

    url = paste0('ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Hydrography/WBD/HU2/Shape/WBD_',HUC2,'_Shape.zip')

   # 'http://prd-tnm.s3-website-us-west-2.amazonaws.com/?prefix=StagedProducts/Hydrography/NHD/HU8/HighResolution/'

    temp <- tempfile()
    temp2 <- tempfile()

    download.file(url, temp)

    unzip(zipfile = temp, exdir = temp2)

    shp = readOGR(paste0(temp2,'/Shape/WBDHU',nchar(HUC),".shp"))%>%
      spTransform(CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))


    ii = which(shp@data$HUC == HUC)
    shp = shp[ii,]

    unlink(temp)
    unlink(temp2)

  }

  if(!is.null(latlon)){

    #bb width and height = 1

    df = (1/2)/69
    dl = ((1/2)/69) / cos(latlon[1] * pi/180)

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

    shp = suppressWarnings(readOGR(paste0(temp2,'/nhdflowline_network.shp'), verbose = FALSE))

    HUC8 = unique(substr(shp$reachcode, 1, 8))

     URL = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_", HUC8 ,"_Shape.zip")

     temp3 <- tempfile()
     temp4 <- tempfile()

     download.file(URL, temp3, quiet = TRUE)
     unzip(zipfile = temp3, exdir = temp4)

     if(subbasins == FALSE){
       shp = suppressWarnings(readOGR(paste0(temp4,'/Shape/WBDHU', level ,'.shp'), verbose = FALSE)) %>%
         spTransform(CRS("+proj=longlat +datum=WGS84"))

       sp = SpatialPoints(cbind(latlon[2], latlon[1]))
       proj4string(sp) =  CRS("+proj=longlat +datum=WGS84")

       if(level >= 8){
         shp = shp[sp,]
        } else{
         shp = shp
        }

     }else{
     shp = list()
     list = seq(level,12, 2)

     for(i in 1:length(list)){
       shp[[paste0("huc",list[i])]] = suppressWarnings(readOGR(paste0(temp4,'/Shape/WBDHU', list[i] ,'.shp'), verbose = FALSE))

     }
     }

     unlink(temp3)
     unlink(temp4)

  }

  return(shp)
}
