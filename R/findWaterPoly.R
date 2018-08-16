#' @title Find Water Polygon
#' @description A function to join all NHD water featrues into a single polygon.
#' Flowlines are sized according to there Strahler stream order multiplied by 10 m.
#' Useful for viualization and masking operations where a water surface is needed.
#' @param AOI an AOI
#' @return a single polygon of all NHD water features.
#' @author Mike Johnson
#' @examples
#' \dontrun{
#' getAOI(state = 'CO', county = 'El Paso') %>% findWaterPoly()
#' }
#' @export

findWaterPoly = function(AOI){

if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

val = AOI$AOI %>% findNHD() %>% findWaterbodies() %>% to_sf()

river = val$nhd
wb = val$waterbodies
wb = wb[!is.na(wb$lakearea),]

river.p = sf::st_transform(river, "+init=epsg:3395")
wb.p = sf::st_transform(wb, "+init=epsg:3395")

rivers = list()

st.order = unique(river.p$streamorde)
st.order = sort(st.order[st.order !=0])

for( i in st.order ){
  tmp = river.p[river.p$streamorde == i,]
  rivers[[i]] = st_buffer(tmp, dist=10 * i)
}


rivers[sapply(rivers, is.null)] <- NULL

r = do.call(rbind, rivers)
r = sf::st_union(r)

wb.j = sf::st_union(wb.p)
t = sf::st_union(r, wb.j)

t = sf::st_transform(t, as.character(AOI::aoiProj))

AOI[["waterPoly"]] = t

return(AOI)
}

