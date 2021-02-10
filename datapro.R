# install.packages('sp')
# install.packages('raster')
# install.packages('rgdal')
options("rgdal_show_exportToProj4_warnings"="none")
require(raster)
require(rgdal)
require(ncdf4)
require(maptools)
# require(psych)
require(ggplot2)
# setwd('/Users/Jayson/Documents/Git/Covid_proj/')
setwd('F:\\COVID_proj')
raw_data <- read.csv(file='https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')
bdy_data <- rgdal::readOGR('./Data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp')
weather_data <- raster::brick('./Data/globalWeather.grib')

crs <- weather_data@crs
crs(bdy_data) <- crs
weather_data_1 <- rotate(weather_data)

# Generate out dataFrame
raw_data_1 <- subset(raw_data, select=c(iso_code:total_cases, total_deaths, total_cases_per_million,
                                      population_density:gdp_per_capita, stringency_index, population,
                                      handwashing_facilities:human_development_index))
raw_data_1$Days_100th = 0
raw_data_1 = raw_data_1[,c(ncol(raw_data_1),1:(ncol(raw_data_1)-1))]
View(subset(raw_data_1, total_cases>100, select=Days_100th))
is.na(raw_data_1[raw_data_1$total_cases>100,"Days_100th"])
View(head(raw_data_1, n=50))

# ————————————————————————
ctry_data <- read.csv('./Data/out.csv')
ctry_bdy <- readShapePoly('./Data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp')
raw_data_0 <- brick(readGDAL('/Users/Jayson/Documents/python_practice/stat6110_proj/Data/grib/ClimateData.grib'))

crs <- raw_data_0@crs
crs(ctry_bdy) <- crs # VERY IMPORTANT!!!!!
raw_data <- rotate(raw_data_0)

# Correct the ISO code for countries.
levels(ctry_bdy@data$ISO_A3) <- c(levels(ctry_bdy@data$ISO_A3),'FRA', 'NOR', 'OWID_KOS')
ctry_bdy[ctry_bdy$NAME == 'France',"ISO_A3"] <- 'FRA'
ctry_bdy[ctry_bdy$NAME == 'Norway',"ISO_A3"] <- 'NOR'
ctry_bdy[ctry_bdy$NAME == 'Kosovo',"ISO_A3"] <- 'OWID_KOS'

name <- vector()
for (i in 1:7){
  for (j in c('EWind','NWind','dewP','AirTemp','SkinTemp','radia','precip')){
    name <- append(name,sprintf('%s_%d',j,i))
  }
}
names(raw_data) <- name

ctry_data_1week <- ctry_data[ctry_data$Days_100th==30,]
ctry_list <- ctry_data_1week$iso_code

ctry_mean_data <- matrix(NA, 0, 6)
ctry_col <- vector()
counter <- 0
for (i in ctry_list){
  # print(class(i))
  # i <- ctry_list[42]
  temp_ctry_bdy <- ctry_bdy[ctry_bdy$ISO_A3 == i,]
  before_month <- ctry_data_1week[ctry_data_1week$iso_code==i,'before_month']
  if ((length(temp_ctry_bdy) != 0) & (before_month < 8)){
    climate_ctry_info <- extract(raw_data, temp_ctry_bdy, layer = before_month*7-6, nl= 7)
  } else{
    print(sprintf('%s FAILED!!!', i))
    next
  }
  climate_ctry_info_0 <- na.omit(climate_ctry_info[[1]])
  Wind <- sqrt((climate_ctry_info_0[,1])^2+(climate_ctry_info_0[,2]^2))
  climate_ctry_info_1 <- cbind(Wind,matrix(climate_ctry_info_0[,3:7], ncol=5))
  temp_ctry_meaninfo <- matrix(apply(climate_ctry_info_1, MARGIN = 2, mean),1,6)
  ctry_mean_data <- rbind(ctry_mean_data,temp_ctry_meaninfo)
  ctry_col <- append(ctry_col, i)
  counter <- counter + 1
  print(sprintf('%s is extracted! - %d/180', i, counter))
}
ctry_mean_info <- cbind(data.frame(ctry_col),data.frame(ctry_mean_data))
ctry_mean_info <- ctry_mean_info[!is.nan(ctry_mean_info$X1),]
colnames(ctry_mean_info) <- c('iso_A3','Wind','dew_T','Air_T','Skin_T','radia','precip')
# View(ctry_mean_info)
# View(ctry_data_1week)
# View(ctry_info_1week)
# View(ctry_info_1week_1)
ctry_info_1week <- merge(x=ctry_data_1week, y=ctry_mean_info, by.x='iso_code', by.y='iso_A3')
ctry_info_1week_1 <- ctry_info_1week[,-16]
total_cases_density <- ctry_info_1week_1$total_cases/ctry_info_1week_1$population_density
total_deaths_density <- ctry_info_1week_1$total_deaths/ctry_info_1week_1$population_density
ctry_info_1week_2 <- cbind(ctry_info_1week_1,total_cases_density,total_deaths_density)
View(ctry_info_1week_2)

names(ctry_info_1week_1)
# write.csv(ctry_info_1week_1,'ctry_1week_data.csv', row.names = FALSE)
write.csv(ctry_info_1week_1,'ctry_30d_data.csv', row.names = FALSE)
# write.csv(ctry_info_1week_2,'ctry_1week_data.csv', row.names = FALSE)
# Descriptive analysis --------------------------------------------------------------------------------------------------
require(ggplot2)
getwd()
setwd('/Users/Jayson/Documents/python_practice/stat6110_proj')
ctry_1week_data <- read.csv('ctry_1week_data.csv')
View(names(ctry_1week_data))
View(ctry_1week_data)
hist(ctry_1week_data$gdp_per_capita, breaks = 50)
low_in <- subset(ctry_1week_data,gdp_per_capita<=4000)
mid_in <- subset(ctry_1week_data,gdp_per_capita > 4000&gdp_per_capita<=13000)
high_in <- subset(ctry_1week_data,gdp_per_capita>13000)

plot_data <- ctry_1week_data
pop_dens <- (plot_data$population_density)^0.3
plot1 <- ggplot(data=plot_data, 
                aes(x=gdp_per_capita, y=total_cases_per_million, color = pop_dens)) +
  scale_color_gradient(low="deepgreen", high="red") + 
  stat_smooth(method = 'lm') + 
  ylim(0,500) + 
  geom_point(size=(plot_data$gdp_per_capita)^0.5/70, alpha=0.9);plot1
plot1 <- plot1 + xlim(0,2);plot1
hist((ctry_1week_data$gdp_per_capita)^0.5)
hist((ctry_1week_data$population_density)^0.2)


name_list <- c(5,6,7,8,9,10,18:23,24,25)
psych::pairs.panels(ctry_1week_data[,name_list], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

tf_ctry_1week_data <- ctry_1week_data[,name_list]
for (i in 1:dim(tf_ctry_1week_data)[2]){
  tf_ctry_1week_data[,i] <- (tf_ctry_1week_data[,i])^1
}
# tf_ctry_1week_data$total_cases_per_million <- log(tf_ctry_1week_data$total_cases_per_million)
psych::pairs.panels(tf_ctry_1week_data[,-c(1,2,4,9,13,14)], 
                    method = "spearman", # correlation method
                    hist.col = "#00AFBB",
                    density = TRUE,  # show density plots
                    ellipses = TRUE # show correlation ellipses
)

low_in <- which(tf_ctry_1week_data$gdp_per_capita<=4000)
mid_in <- which((tf_ctry_1week_data$gdp_per_capita <= 13000)&(tf_ctry_1week_data$gdp_per_capita>4000))
high_in <- which(tf_ctry_1week_data$gdp_per_capita > 13000)

psych::pairs.panels(tf_ctry_1week_data[mid_in,-c(1,2,4,9,13,14)], 
                    method = "pearson", # correlation method
                    hist.col = "#00AFBB",
                    density = TRUE,  # show density plots
                    ellipses = TRUE # show correlation ellipses
)


# Preliminary LM fit-------------------------------------------------------------------
fit_data <- tf_ctry_1week_data[high_in,]
test1 <- lm(total_cases_per_million ~ population_density + gdp_per_capita +
     Wind + dew_T + Skin_T + radia + precip, data = fit_data)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(test1);par(mfrow=c(1,1))
summary(test1)

# library(MASS)
# library(faraway)
a <- boxcox(test1, plotit = TRUE)
a <- data.frame(a)
lambda <- a[a$y==max(a$y),1]
test2 <- lm((total_cases_per_million^lambda-1)/lambda ~ population_density + gdp_per_capita +
              Wind + dew_T + Skin_T + radia + precip, data = fit_data)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(test2);par(mfrow=c(1,1))
summary(test2)
str(summary(test2))
anova(test2)
names(fit_data)
# Box-Tidwell trans----------------------------------------------------------------------------------------------------------
transData <- read.csv('./Data/NEW_COVID_RAW_DATA.csv')
car::boxTidwell(y ~ population_density, ~ gdp_per_capita+Wind+dew_T+Skin_T+radia + precip, data=transData, verbose=TRUE)
names(transData)




# i <- 'CHN'
# ctry <- ctry_bdy[ctry_bdy$ISO_A3 == i,]
# temp_ctry <- extract(raw_data, ctry)
# plot(ctry)
# plot(temp_ctry)
# plot(raw_data)
# a <- vector()
# for (i in temp_ctry){
#   a <- append(a, i)
# }
# a <- na.omit(a)
# mean(a)
# 
# View(ctry_bdy)
# mean_temp <- matrix(c('china',10), 1,2)
# mean_temp <- append(mean_temp, mean_temp)

# nan_list <- meantemp_data[is.nan(meantemp_data[,2]),1]
# normal_list <- meantemp_data[!is.nan(meantemp_data[,2]),1]
# 
# plot(ctry_bdy[ctry_bdy$ISO_A3 == nan_list[1],], xlim = c(-180,180), ylim = c(-90,90), col = 'red')
# for (i in nan_list[-1]){
#   lines(ctry_bdy[ctry_bdy$ISO_A3 == i,], col='red')
# }
# for (i in normal_list){
#   # print(class(i))
#   lines(ctry_bdy[ctry_bdy$ISO_A3 == i,], col='blue')
# }

# bdy <- as.vector(ctry_bdy$ISO_A3)
# bdy_name <- as.vector(ctry_bdy$SOVEREIGNT)
# ctry <- as.vector(ctry_data$iso_code)
# ctry_name <- as.vector(ctry_data$location)
# write.csv(cbind(bdy,bdy_name), 'ctry_bdy.csv')
# write.csv(cbind(ctry,ctry_name), 'ctry_data.csv')

# View(namelist)
# e <- extent(125,130,-25,-20)
# a <- extract(data1)
# extract(data1, c(125,-5))
# 
# sp.pts1 <- SpatialPoints(cbind(128,-23))
# v.hadcm <- extract(data1, sp.pts1)
# plot(v.hadcm, type='b')