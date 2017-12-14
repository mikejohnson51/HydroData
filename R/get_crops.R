get_crop = function(state = NULL, county = NULL, clip_unit = NULL, year = NULL, keep.boundary = FALSE){

urls = vector()
tempd = tempdir()
bad.years = vector()
crops = list()

if(year > 2016 || year < 1997){stop("USDA CropScape Data only avaiable between 1997 and 2016. Please only use these values in declaring year.")}

AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = FALSE)
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading CropScape data for ", paste0(year, collapse = ", "),".")

p = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  shp.p = spTransform(AOI, p)
  bb = paste(shp.p@bbox[1,1],shp.p@bbox[2,1], shp.p@bbox[1,2],shp.p@bbox[2,2], sep = ',')

for(i in 1:length(year)){ urls = append(urls, paste0("https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=",year[i],"&bbox=", bb)) }

for(j in 1:length(urls)){

  URL = getURL(urls[j])

if(grepl('fault', URL)){
    for(i in 1997:2016){
        if(grepl('fault', getURL(paste0("https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?year=",2010,"&bbox=",bb)))){
          bad.years = append(bad.years, i)
        }else{
          stop(paste('Crop data only avaliable for', paste0(setdiff(c(1997:2016), bad.years), collapse = ", "), "within the", nameAOI(state = state, county = county, clip_unit = clip_unit)))
        }
    }
}else{
  download.file(url = as.character(xmlToDataFrame(URL)$text) , destfile = paste0(tempd,"/", year[j], ".tif"), quiet = TRUE)
  message("CropScape data downloaded for ", year[j])
  }
}

all.files = list.files(tempd, pattern = ".tif$", full.names = TRUE)

for(i in 1:length(all.files)){    crops[[paste(year[i])]] = raster(all.files[i])    }

unlink(tempd)

if(keep.boundary == TRUE){return(list(crops = crops, boundary = shp.p))
}else{return(crops)}
}

