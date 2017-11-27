find_USGS_stations = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE){
  
  #Load usgsStation Data
  load('/Users/mikejohnson/Library/Mobile Documents/com~apple~CloudDocs/RandomFunctions/usgsStations.Rdata')
  #Convert to Spatial Points Dataframe  
  coords = cbind(usgsStations$lon_reachCent, usgsStations$lat_reachCent)
  sp = SpatialPoints(coords)
  sp = SpatialPointsDataFrame(sp, usgsStations) 
  # Remove file
  rm(usgsStations)
  
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
  
  #Use correct shp to clip usgs station data
  if(is.null(state) && !is.null(clip_unit)){
    
      sp@proj4string = clip_unit@proj4string
      shp = sp[clip_unit,]
      
      return(shp)
    
    } else {
    
      area = get_region_sp(state = state, county = county)
      sp@proj4string = area@proj4string
      
      shp = sp[area,]
      
      if(keep.boundary == TRUE){
        return(list(boundary = area, stations = shp))
      }else{
        return(shp)
    }
    }
}

