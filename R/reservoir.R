##Build Reservoir fills

library(sharpshootR)
library(ggplot2)

start = '1960-01-01'
end   = '2017-10-31'

ORO = CDECquery(id = 'oro', sensor = 15, interval = 'M', start = start, end = end)
CCH = CDECquery(id = 'cch', sensor = 15, interval = 'M', start = start, end = end)
SHA = CDECquery(id = 'sha', sensor = 15, interval = 'M', start = start, end = end)
FOL = CDECquery(id = 'fol', sensor = 15, interval = 'M', start = start, end = end)

p1 = ggplot(data = ORO, aes(x = datetime, y = value)) + geom_line() + labs(x = "Date", y = "Storage in AF", title = paste("Lake Oroville Levels:", start,"-", end)) + theme_classic() + geom_smooth(method = "lm", color = "blue", se = TRUE, aes(group =1) )
p2 = ggplot(data = CCH, aes(x = datetime, y = value)) + geom_line() + labs(x = "Date", y = "Storage in AF", title = paste("Lake Cachuma Levels:", start,"-", end))  + theme_classic() + geom_smooth(method = "lm", color = "red", se = TRUE, aes(group =1))
p3 = ggplot(data = SHA, aes(x = datetime, y = value)) + geom_line() + labs(x = "Date", y = "Storage in AF", title = paste("Lake Shasta Levels:", start,"-", end))   + theme_classic() + geom_smooth(method = "lm", color = "darkgreen", se = TRUE, aes(group =1))
p4 = ggplot(data = FOL, aes(x = datetime, y = value)) + geom_line() + labs(x = "Date", y = "Storage in AF", title = paste("Lake Folsom Levels:", start,"-", end))   + theme_classic() + geom_smooth(method = "lm", color = "orange", se = TRUE, aes(group =1))

gridExtra::grid.arrange(p1,p2,p3,p4, ncol = 2)

res = readOGR("/Users/mikejohnson/Downloads/Reservoir/Reservoir.shp")
  spTransform(res, ws@proj4string)
plot(res)




res = readOGR("/Users/mikejohnson/Downloads/hydrom020/hydrogl020.shp")

res[ws,]


URL = 'https://prd-tnm.s3.amazonaws.com/StagedProducts/Small-scale/data/Hydrography/hydrogm020_nt00015.tar.gz'

tmp  = tempfile()
tmp1 = tempfile()

download.file(url = URL, destfile = tmp)
untar(tmp, exdir = tmp1)

shp = readOGR(paste0(tmp1, "/hydrogl020.shp"))


####################
damsdata(nid_cleaned)

lon = nid_cleaned$Longitude
lat = nid_cleaned$Latitude

coords = na.omit(cbind(lon, lat))
sp = SpatialPoints(coords, nid_cleaned)
