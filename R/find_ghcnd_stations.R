
find_ghcnd_stations = function(shapefile){
  

  
bb = shapefile@bbox

url = 'https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt'

stations = read.table(url, fill = TRUE, header = FALSE, stringsAsFactors = FALSE)
stations = as.data.frame(stations)

test = stations %>% dplyr::select(V1:V3) %>% magrittr::set_names(c("ID", "LAT", "LONG")) %>% na.omit() %>% mutate(LAT = (as.numeric(LAT))) %>% mutate(LONG = (as.numeric(LONG))) %>% 
  filter(LAT  <= bb[2,2]) %>% 
  filter(LAT  >= bb[2,1]) %>% 
  filter(LONG >= bb[1,1]) %>% 
  filter(LONG <= bb[1,2])

return(test)
}

