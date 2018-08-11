#' Title
#'
#' @param AOI and
#'
#' @return
#' @export
#'
#' @examples
#'
findWaterPoly = function(AOI){

if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

val = AOI$AOI %>% findNHD() %>% findWaterbodies()

river = val$nhd
wb = val$waterbodies
wb = wb[!is.na(wb$lakearea),]



river.p = spTransform(river, CRS("+init=epsg:3395"))
wb.p = spTransform(wb, CRS("+init=epsg:3395"))

rivers = list()

st.order = unique(river.p$streamorde)
st.order = sort(st.order[st.order !=0])

for( i in st.order ){
  tmp = river.p[river.p$streamorde == i,]
  rivers[[i]] = rgeos::gBuffer(tmp, width=10 * i, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=0.01, byid = T)
}

rivers[sapply(rivers, is.null)] <- NULL

r = do.call(rbind, rivers)
r = rgeos::gUnaryUnion(r)

wb.j = rgeos::gUnaryUnion(wb.p)

t = rbind(r, wb.j, makeUniqueIDs = TRUE)
t = rgeos::gUnaryUnion(t)

t = sp::spTransform(t, AOI::aoiProj)

AOI[["waterPoly"]] = t

return(AOI)
}

