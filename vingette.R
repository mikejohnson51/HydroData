# Define AOI ------------------------------------------------------

AOI = list("National Water Center", 20, 20)

#Download/process NHD Flowlines, HAND data, and Rating curves

nwc.hand     = get_HAND(clip_unit = AOI, keep.flowlines = TRUE)
ratingCurves = get.rating(nwc.hand$flowlines)

# Step 3: Download/Access Flow Data and convert to stage ---------------------------------------

nwc.flows    = readNWM(comids = nwc.hand$flowlines, path = "/Users/mikejohnson/Library/Mobile Documents/com~apple~CloudDocs/FloodMapping/RetroData_12.25.2015", startDate = "2015-12-25 00:00:00", interval.hr = 1)
nwc.stage    = get_stage(flows = nwc.flows, rating_curves = ratingCurves)

ggplot() + geom_line(data = nwc.stage, aes(x = dateTime, y = cms, group = comid, color = comid))
ggplot() + geom_line(data = nwc.stage, aes(x = dateTime, y = stage, group = comid, color = comid))

# Generate Flood Polygons -----------------------------------------

flood_forecast = mapFlood(stage = nwc.stage, catchment = nwc.hand$catch, HAND = nwc.hand$hand)


leaflet() %>% addTiles() %>% addRasterImage(flood_forecast[[2]], colors = blues9, opacity = 0.5)


