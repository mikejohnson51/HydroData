
find_waterbodies = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE){

  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = keep.basemap)
  if(is.null(clip_unit)){AOI = AOI} else {AOI = AOI$map}

  meta = data.frame(STATE = state, COUNTY = county, CLIP = clip_unit, type = 'water.bodies', ACCESS = Sys.time())

  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading North American water bodies database...")

  # Get Data

  URL = 'http://open-esrifederal.opendata.arcgis.com/datasets/9dff3cf646704abd9e74265f02abeb09_0.zip'

  sp = download.shp( URL, "water bodies")

  message("Subsetting to ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  if(keep.basemap){
    sp = spTransform(sp, AOI$shp@proj4string)
    shp = sp[AOI$shp,]
  }else{
    sp = spTransform(sp, AOI@proj4string)
    shp = sp[AOI,]
  }

  message(formatC(as.numeric(length(shp)), format="d", big.mark=","), " water bodies found.")

  if(keep.boundary && keep.basemap){
    message("Shapefiles of water bodies,", nameAOI(state = state, county = county, clip_unit = clip_unit), ", and raster basemap returned")
    return(list(waterbodies = shp, boundary = AOI$shp, basmap = AOI$bmap, meta = meta))

  }else if(!keep.boundary && keep.basemap ){
    message("Shapefile of water bodies and basemap returned")
    return(list(waterbodies = shp, basmap = AOI$bmap, meta = meta))

  }else if(keep.boundary && !keep.basemap ){
    message("Water bodies, and ", nameAOI(state = state, county = county, clip_unit = clip_unit), " shapefile returned")
    return(list(waterbodies = shp, boundary = AOI, meta = meta))

  }else{
    message("Water bodies shapefile returned")
    return(shp, meta = meta)
  }
}

find_waterbodies(state = 'TX', county = "harris")
