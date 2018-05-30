getCLD = function(state = NULL, county = NULL, clip_unit = NULL, year = NULL, boundary = FALSE, basemap = FALSE, save = FALSE){

    crops = list()
    all.files = NULL

   if(year > 2017 || year < 2008){stop("USDA CropScape Data only avaiable between 2008 and 2017. Please select a year within this range.")}

   AOI = getAOI(state, county, clip_unit)

   p = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

   shp.p = spTransform(AOI, p)

   bb = paste(shp.p@bbox[1,1],shp.p@bbox[2,1], shp.p@bbox[1,2],shp.p@bbox[2,2], sep = ',')

   urls = paste0("https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=",year,"&bbox=", bb)

   for(j in seq_along(year)){

     res = curl::curl_fetch_memory(urls[j])

        src = strsplit(rawToChar(res$content), "returnURL>")
        src2 = grep("https", src[[1]], value = T)
        src3 = gsub("(tif).*","\\1",src2)

        check = download.url(url = src3, mode = "binary")
        if(check$code != 200){stop("Download Failed, please try again")}
        message("CropScape data downloaded for ", year[j])
        all.files = append(all.files, check$destfile)
   }



    for(i in 1:length(all.files)){
      crops[[paste0('cld_', year[i])]] = raster::raster(all.files[i])
    }

    b = raster::brick(crops) %>% raster::projectRaster(crs = HydroDataProj, method = 'ngb') %>% raster::crop(AOI) %>% raster::stack()

    for(i in 1:dim(b)[3]){
      raster::colortable(b[[i]]) <- col_crops$color
    }

   items = list( name = nameAOI(state, county, clip_unit),
                                 source = "USDA Cropland Data Layers",
                                 proj = b@crs,
                                 cld = b)

   report = paste0("Returned list includes: ", paste(year, collapse = ", "), " Croplands Data Layer")

   items = return.what(sp , items = items, report, AOI = AOI, basemap = basemap, boundary = boundary, clip_unit= clip_unit, ids = NULL)

    #unlink(tempd, recursive = TRUE)

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

    class(items) = "HydroData"

    return(items)
}


