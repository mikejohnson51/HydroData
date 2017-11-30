get_nid = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE){

  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
    }

  # Ensure parameters are present
  if(is.null(state) && is.null(clip_unit)){
    stop("A character string state name or a spatial clipping unit must be provided.")
  }


  # Ensure state argument is given with county
  if(!is.null(county) && is.null(state)){
    stop("A state must be proveded with county name. Use state = ")
  }

  # Ensure state and clip_unit are not both given
  if(!is.null(state) && !is.null(clip_unit)){
    stop("Either a clipping unit OR a state and/or state county combination can be used")
  }

  library(dams)
  data(nid_cleaned)
  dams = nid_cleaned[nid_cleaned$State != 'AK', ]
  dams = dams[dams$State != 'HI', ]
  dams = dams[dams$State != 'GU', ]
  dams = dams[dams$State != 'PR', ]
  dams = dams %>% drop_na(Longitude) %>% drop_na(Latitude)

  coords = cbind(dams$Longitude, dams$Latitude)
  sp = SpatialPointsDataFrame(coords, data = dams, proj4string = CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))


  if(!is.null(clip_unit)){
    unit = "supplied shapefile"
    shp = clip_unit}

  if(!is.null(state)){
    if(is.null(county)){
      unit = paste0("boundary of ", setNames(state.name, state.abb)[state])
    } else {
      unit = paste0("boundary of ", county ," County, ", setNames(state.name, state.abb)[state])
    shp = get_region_sp(state = state, county = county) }
    }

  message(paste("Shapefile Defined by", unit))


  sp = sp[shp, ]

  if(keep.boundary == TRUE){
    return(list(boundary = shp, dams = sp))
  }else{
    return(sp)
  }
}



