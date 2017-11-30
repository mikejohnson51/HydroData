get_nlcd = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = TRUE, crop = FALSE){

  if(!is.null(clip_unit)){
    unit = "supplied shapefile"
    shp = clip_unit}

  if(!is.null(state)){
    if(is.null(county)){ xxx = NULL} else {xxx = simpleCap(county)}
    unit = paste0("boundary of ", simpleCap(county)," County, ", setNames(state.name, state.abb)[state])
    shp = get_region_sp(state = state, county = county) }

message(paste("Area of Interest Defined by", unit))

#  <ows:BoundingBox crs="urn:ogc:def:crs:EPSG::3857">
#  <ows:LowerCorner>-14497453.9106248 2480608.8817100283 </ows:LowerCorner>
#  <ows:UpperCorner>-7087933.9106248002 6960328.8817100283 </ows:UpperCorner>
#  </ows:BoundingBox>

grid.origin = c(-14497438.9106248, 6960313.8817100283) # appears to be upper left
grid.offset = c(30, -30)
grid.extent = c(-7087933.9106248002, 2480608.8817100283)

#min x, min y, max x, max y
latlon1 = shp@bbox[,1]
latlon2 = shp@bbox[,2]

grid.origin[1] = grid.origin[1] + .5 * grid.offset[1]
grid.origin[2] = grid.origin[2] + .5 * grid.offset[2]

x.len = (grid.extent[1] - grid.origin[1])/ grid.offset[1]
y.len = (grid.extent[2] - grid.origin[2])/ grid.offset[2]

grid.extent[1] = grid.origin[1] + grid.offset[1] * x.len
grid.extent[2] = grid.origin[2] + grid.offset[2] * y.len

p= CRS("+init=epsg:3857")

shp.p = spTransform(shp, p)
p1 = shp.p@bbox[,1]
p2 = shp.p@bbox[,2]

xi1 = floor((p1[1]   - grid.origin[1])/grid.offset[1]) * grid.offset[1] + grid.origin[1]
xi2 = ceiling((p2[1] - grid.origin[1])/grid.offset[1]) * grid.offset[1] + grid.origin[1]
yi1 = floor((p1[2]   - grid.origin[2])/grid.offset[2]) * grid.offset[2] + grid.origin[2]
yi2 = ceiling((p2[2] - grid.origin[2])/grid.offset[2]) * grid.offset[2] + grid.origin[2]

URL = paste0("https://raster.nationalmap.gov/arcgis/services/LandCover/USGS_EROS_LandCover_NLCD/MapServer/WCSServer?request=GetCoverage&service=WCS&Version=1.1.1&Identifier=2&boundingBox=",
             xi1, ",",
             yi1, ",",
             xi2, ",",
             yi2,",",
             "urn:ogc:def:crs:EPSG::3857&gridBaseCRS=urn:ogc:def:crs:EPSG::3857&gridOffsets=30,-30&format=image/tiff&store=true&InterpolationType=nearest")

URL = getURL(URL)

message("Coverage Defined!")

data = xmlToList(xmlParse(URL))

temp= tempfile()
message("Downloading Land use raster...")
download.file(url = data$Coverage$Reference[['href']], destfile = temp, quiet = TRUE )
message("Raster downloaded!")

LU = brick(temp)
shp = spTransform(shp, LU@crs)

if(crop == TRUE){
  print("Cropping Land use to shapefile...")
  LU = mask(LU, shp)
  message("Crop complete!")}

if(keep.boundary == TRUE) {
  message("List of boundary and land use raster returned. Use plotRGB() with land use.")
  return(list(shp = shp, LU = LU))
  } else {
    message("Land use raster returned. Use plotRGB().")
    return(LU)}
}


