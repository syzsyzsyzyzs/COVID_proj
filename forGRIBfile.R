install.packages('raster')
install.packages('rgdal')
require(raster)
require(rgdal)
getwd()
setwd('F:\\stat6110_proj\\Data')
raw_data <- readGDAL('GlobTemp.grib')

data1 <- brick(raw_data)
data2 <- as.array(data1)

a <- data1[[1]]

raw_data <- brick('ned.nc')
plot(raw_data)
raw_data1 <- as.array(raw_data)

extract(hadcm, SpatialPoints())
GRIB <- as.array(hadcm)

e <- extent(125,130,-25,-20)
a <- extract(hadcm, e)
a <- rasterToPoints(hadcm)
plot(a)


View(hadcm)
sp.pts <- SpatialPoints(cbind(128,-23))
v.hadcm <- as.vector(extract(hadcm, sp.pts))
plot(v.hadcm, type='b')


getValues(hadcm)

