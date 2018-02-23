getCLD = function(state = NULL, county = NULL, clip_unit = NULL, year = NULL, keep.boundary = FALSE, save = FALSE){

      urls = vector()
      tempd = tempdir()

      bad.years = vector()
      crops = list()

   if(year > 2017 || year < 2008){stop("USDA CropScape Data only avaiable between 2008 and 2017. Please only use these values in declaring year.")}

   AOI = getAOI(state = state, county = county, clip_unit = clip_unit)
      message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Now loading CropScape data for ", paste0(year, collapse = ", "),".")

   p = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

   shp.p = spTransform(AOI, p)

   bb = paste(shp.p@bbox[1,1],shp.p@bbox[2,1], shp.p@bbox[1,2],shp.p@bbox[2,2], sep = ',')

   for(i in 1:length(year)){ urls = append(urls, paste0("https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=",year[i],"&bbox=", bb)) }

   for(j in 1:length(urls)){

     URL = RCurl::getURL(urls[j])

    if(grepl('fault', URL)){
        for(i in 2008:2017){
          if(grepl('fault', RCurl::getURL(paste0("https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=",i,"&bbox=",bb)))){
              bad.years = append(bad.years, i)
          }else{
              stop(paste('Crop data only avaliable for', paste0(setdiff(c(2008:2017), bad.years), collapse = ", "), "within the", nameAOI(state = state, county = county, clip_unit = clip_unit)))
          }
       }
      }else{
        src = as.character(XML::xmlToDataFrame(URL)$text)
        path = paste0(tempd,"/", year[j], ".tif")

        utils::download.file(url = src , destfile = path, quiet = FALSE, mode = 'wb')

        message("CropScape data downloaded for ", year[j])
      }
   }

    all.files = list.files(tempd, pattern = ".tif$", full.names = TRUE)

    for(i in 1:length(all.files)){
      crops[[paste0('CLD_', year[i])]] = raster(all.files[i])
    }

    b = brick(crops)
    b = projectRaster(b, crs = HydroDataProj, method = 'ngb')
    b = crop(b, AOI)
    b = stack(b)

    for(i in 1:dim(b)[3]){
      colortable(b[[i]]) <- col_crops$color
    }

    unlink(tempd, recursive = TRUE)

      if(save){
        save.file(data = crops,
                  state = state,
                  county = county,
                  clip_unit = clip_unit,
                  agency  = 'USDA',
                  source  = "Cropscape",
                  dataset = "crops",
                  other   = year )
      }

    list = unstack(b)
    names(list) <- names(crops)

      return(list)
}


