

# =========================================================================================================================================
# CEGE0042: STDM Coursework ----- Part 1: Data Import and Pre-processing
# =========================================================================================================================================

# This is the code for the UCL MSc CEGE0042 coursework
# The aim is to conduct an exploratory spatio-temporal analysis of Vancouver crime data and make predictions.

# Most of the code is derived from Dr James' book 'Spatio-temporal Analytics in R'

# Data set name: Crime in Vancouver
# Data source: https://www.kaggle.com/datasets/wosaku/crime-in-vancouver
# District data of Vancouver could be download here: https://opendata.vancouver.ca/explore/dataset/local-area-boundary/export/?disjunctive.name

# Directories in the coursework will point to a folder called 'Coursework'.
# Please create this folder somewhere in your computer and use 'setwd' to set the working directory to point to it.
# For example, if 'Coursework' is placed in '/Users' then run setwd('/Users/Coursework').






# check and change working directory using getwd() & setwd()
getwd()
setwd("/Users/zmxu/Desktop/UCL MSc Geospatial Sciences/term2-周2-CEGE0042-Spatial-Temporal Data Analysis and Data Mining (STDM) /Coursework")

# import Vancouver Crime data
data <- read.csv("archive/crime.csv")
head(data,10)

# select interested attributes
crime <- data.frame(data[,c("TYPE","YEAR","MONTH","NEIGHBOURHOOD","Latitude","Longitude")])
print(nrow(crime))

# clean the data set, delete NA & 0
crime[crime==0] <- NA
crime <- na.omit(crime)
print(nrow(crime))
head(crime,10)

# check different TYEP & NEIGHBOURHOOD
unique(crime$TYPE)
unique(crime$NEIGHBOURHOOD)

# create year_month matrix (might introduce 0)
# all crimes
year_month <- matrix(data=0,nrow=15,ncol=12)
# group by crime TYPE
year_month_M     <- matrix(data=0,nrow=15,ncol=12)
year_month_OT    <- matrix(data=0,nrow=15,ncol=12)
year_month_ToV   <- matrix(data=0,nrow=15,ncol=12)
year_month_TfV   <- matrix(data=0,nrow=15,ncol=12)
year_month_ToB   <- matrix(data=0,nrow=15,ncol=12)
year_month_BEC   <- matrix(data=0,nrow=15,ncol=12)
year_month_BERO  <- matrix(data=0,nrow=15,ncol=12)
year_month_VCPSI <- matrix(data=0,nrow=15,ncol=12)
year_month_VCPSF <- matrix(data=0,nrow=15,ncol=12)
# group by NEIGHBOURHOOD
year_month_Strathcona              <- matrix(data=0,nrow=15,ncol=12)
year_month_Kerrisdale              <- matrix(data=0,nrow=15,ncol=12)
year_month_DunbarSouthlands        <- matrix(data=0,nrow=15,ncol=12)
year_month_GrandviewWoodland       <- matrix(data=0,nrow=15,ncol=12)
year_month_Sunset                  <- matrix(data=0,nrow=15,ncol=12)
year_month_WestEnd                 <- matrix(data=0,nrow=15,ncol=12)
year_month_CentralBusinessDistrict <- matrix(data=0,nrow=15,ncol=12)
year_month_HastingsSunrise         <- matrix(data=0,nrow=15,ncol=12)
year_month_VictoriaFraserview      <- matrix(data=0,nrow=15,ncol=12)
year_month_Fairview                <- matrix(data=0,nrow=15,ncol=12)
year_month_KensingtonCedarCottage  <- matrix(data=0,nrow=15,ncol=12)
year_month_WestPointGrey           <- matrix(data=0,nrow=15,ncol=12)
year_month_Shaughnessy             <- matrix(data=0,nrow=15,ncol=12)
year_month_RenfrewCollingwood      <- matrix(data=0,nrow=15,ncol=12)
year_month_Killarney               <- matrix(data=0,nrow=15,ncol=12)
year_month_RileyPark               <- matrix(data=0,nrow=15,ncol=12)
year_month_ArbutusRidge            <- matrix(data=0,nrow=15,ncol=12)
year_month_Musqueam                <- matrix(data=0,nrow=15,ncol=12)
year_month_MountPleasant           <- matrix(data=0,nrow=15,ncol=12)
year_month_Kitsilano               <- matrix(data=0,nrow=15,ncol=12)
year_month_StanleyPark             <- matrix(data=0,nrow=15,ncol=12)
year_month_SouthCambie             <- matrix(data=0,nrow=15,ncol=12)
year_month_Marpole                 <- matrix(data=0,nrow=15,ncol=12)
year_month_Oakridge                <- matrix(data=0,nrow=15,ncol=12)

year  <- c(2003:2017)
month <- c(1:12)

for (i in year){
  for (j in month){
    # all crimes
    year_month[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j))
    # group by TYPE
    year_month_M[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & TYPE=="Mischief"))
    year_month_OT[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & TYPE=="Other Theft"))
    year_month_ToV[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & TYPE=="Theft of Vehicle"))
    year_month_TfV[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & TYPE=="Theft from Vehicle"))
    year_month_ToB[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & TYPE=="Theft of Bicycle"))
    year_month_BEC[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & TYPE=="Break and Enter Commercial"))
    year_month_BERO[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & TYPE=="Break and Enter Residential/Other"))
    year_month_VCPSI[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & TYPE=="Vehicle Collision or Pedestrian Struck (with Injury)"))
    year_month_VCPSF[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & TYPE=="Vehicle Collision or Pedestrian Struck (with Fatality)"))
    # group by NEIGHBOURHOOD
    year_month_Strathcona[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Strathcona"))
    year_month_Kerrisdale[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Kerrisdale"))
    year_month_DunbarSouthlands[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Dunbar-Southlands"))
    year_month_GrandviewWoodland[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Grandview-Woodland"))
    year_month_Sunset[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Sunset"))
    year_month_WestEnd[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="West End"))
    year_month_CentralBusinessDistrict[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Central Business District"))
    year_month_HastingsSunrise[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Hastings-Sunrise"))
    year_month_VictoriaFraserview[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Victoria-Fraserview"))
    year_month_Fairview[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Fairview"))
    year_month_KensingtonCedarCottage[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Kensington-Cedar Cottage"))
    year_month_WestPointGrey[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="West Point Grey"))
    year_month_Shaughnessy[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Shaughnessy"))
    year_month_RenfrewCollingwood[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Renfrew-Collingwood"))
    year_month_Killarney[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Killarney"))
    year_month_RileyPark[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Riley Park"))
    year_month_ArbutusRidge[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Arbutus Ridge"))
    year_month_Musqueam[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Musqueam"))
    year_month_MountPleasant[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Mount Pleasant"))
    year_month_Kitsilano[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Kitsilano"))
    year_month_StanleyPark[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Stanley Park"))
    year_month_SouthCambie[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="South Cambie"))
    year_month_Marpole[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Marpole"))
    year_month_Oakridge[i-2002,j] <- nrow(subset(crime, YEAR==i & MONTH==j & NEIGHBOURHOOD=="Oakridge"))
  }
}

# all Crime - change row&col name
colnames(year_month)[1:ncol(year_month)] <- as.character(c(1:12))
rownames(year_month)[1:nrow(year_month)] <- as.character(c(2003:2017))

# covert year_month matrix to month series (delete 0)
# all crime
time_series_long <- as.vector(t(year_month))
time_series      <- time_series_long[1:175]
# group by TYPE
time_series_long  <- as.vector(t(year_month_M)) # TYPE=="Mischief"
time_series_M     <- time_series_long[1:175]
time_series_long  <- as.vector(t(year_month_OT)) # TYPE=="Other Theft"
time_series_OT    <- time_series_long[1:175]
time_series_long  <- as.vector(t(year_month_ToV)) # TYPE=="Theft of Vehicle"
time_series_ToV   <- time_series_long[1:175]
time_series_long  <- as.vector(t(year_month_TfV)) # TYPE=="Theft from Vehicle"
time_series_TfV   <- time_series_long[1:175]
time_series_long  <- as.vector(t(year_month_ToB)) # TYPE=="Theft of Bicycle"
time_series_ToB   <- time_series_long[1:175]
time_series_long  <- as.vector(t(year_month_BEC)) # TYPE=="Break and Enter Commercial"
time_series_BEC   <- time_series_long[1:175]
time_series_long  <- as.vector(t(year_month_BERO)) # TYPE=="Break and Enter Residential/Other"
time_series_BERO  <- time_series_long[1:175]
time_series_long  <- as.vector(t(year_month_VCPSI)) # TYPE=="Vehicle Collision or Pedestrian Struck (with Injury)"
time_series_VCPSI <- time_series_long[1:175]
time_series_long  <- as.vector(t(year_month_VCPSF)) # TYPE=="Vehicle Collision or Pedestrian Struck (with Fatality)"
time_series_VCPSF <- time_series_long[1:175]
# group by NEIGHBOURHOOD
time_series_long <- as.vector(t(year_month_Strathcona))
time_series_Strathcona <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_Kerrisdale))
time_series_Kerrisdale <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_DunbarSouthlands))
time_series_DunbarSouthlands <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_GrandviewWoodland))
time_series_GrandviewWoodland <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_Sunset))
time_series_Sunset <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_WestEnd))
time_series_WestEnd <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_CentralBusinessDistrict))
time_series_CentralBusinessDistrict <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_HastingsSunrise))
time_series_HastingsSunrise <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_VictoriaFraserview))
time_series_VictoriaFraserview <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_Fairview))
time_series_Fairview <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_KensingtonCedarCottage))
time_series_KensingtonCedarCottage <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_WestPointGrey))
time_series_WestPointGrey <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_Shaughnessy))
time_series_Shaughnessy <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_RenfrewCollingwood))
time_series_RenfrewCollingwood <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_Killarney))
time_series_Killarney <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_RileyPark))
time_series_RileyPark <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_ArbutusRidge))
time_series_ArbutusRidge <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_Musqueam))
time_series_Musqueam <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_MountPleasant))
time_series_MountPleasant <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_Kitsilano))
time_series_Kitsilano <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_StanleyPark))
time_series_StanleyPark <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_SouthCambie))
time_series_SouthCambie <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_Marpole))
time_series_Marpole <- time_series_long[1:175]
time_series_long <- as.vector(t(year_month_Oakridge))
time_series_Oakridge <- time_series_long[1:175]

# combine all district data to one matrix
time_series_by_neighbourhood <- rbind(time_series_Strathcona,time_series_Kerrisdale,time_series_DunbarSouthlands,
                                      time_series_GrandviewWoodland,time_series_Sunset,time_series_WestEnd,
                                      time_series_CentralBusinessDistrict,time_series_HastingsSunrise,time_series_VictoriaFraserview,
                                      time_series_Fairview,time_series_KensingtonCedarCottage,time_series_WestPointGrey,
                                      time_series_Shaughnessy,time_series_RenfrewCollingwood,time_series_Killarney,
                                      time_series_RileyPark,time_series_ArbutusRidge,time_series_Musqueam,
                                      time_series_MountPleasant,time_series_Kitsilano,time_series_StanleyPark,
                                      time_series_SouthCambie,time_series_Marpole,time_series_Oakridge)
# rename district name according to .shp file
rownames(time_series_by_neighbourhood)[1:nrow(time_series_by_neighbourhood)] <- as.character(c("Strathcona","Kerrisdale","Dunbar-Southlands",
                                                                                               "Grandview-Woodland","Sunset","West End",
                                                                                               "Downtown","Hastings-Sunrise","Victoria-Fraserview",
                                                                                               "Fairview","Kensington-Cedar Cottage","West Point Grey",
                                                                                               "Shaughnessy","Renfrew-Collingwood","Killarney",
                                                                                               "Riley Park","Arbutus-Ridge",  "Musqueam",
                                                                                               "Mount Pleasant","Kitsilano",  "StanleyPark",
                                                                                               "South Cambie","Marpole","Oakridge"))
# set month number
Month.number <- paste("Month ",1:175,sep="")
colnames(time_series_by_neighbourhood) <- as.character(Month.number)

# output to .csv, in order to link in ArcGIS
write.csv(x = time_series_by_neighbourhood,file = "time_series_by_neighbourhood.csv")
# 2 step before continue the codes:
# Step1: Needs to be done in ArcGIS software to link crime data to neighborhood data (.shp).
# Step2: Use the output to replace the "local_area_boundary.dbf" file in the "Vancouver-local-area-boundary" folder.
# After that, continue the following codes.

# Actually,
# "local_area_boundary.dbf" file in the "Vancouver-local-area-boundary" folder has already linked by the author in advance,
# so you can continue the work now.












# =========================================================================================================================================
# CEGE0042: STDM Coursework ----- Part 2: Data Visualization & Autocorrelation
# =========================================================================================================================================



# Data Visualization - Non Spatio-temporal data characteristics ==================================================

# ***Figure 2.1 Vancouver Crime Time Series and Statistical Information*********
par(mfrow=c(1,3))
# Time series plot
plot(time_series, xlab = "Year", ylab = "Crime Number", type="l", xaxt="n", main="Time series of Crime by Month")
axis(1, at=seq(1,180,12), labels=seq(2003,2017,1))
# Histogram
mu = mean(time_series)
hist(time_series, main="Histogram of Crime (Monthly)")
abline(v=mu, col="red")
# Q-Q plot
qqnorm(time_series, main="Normal Q-Q Plot of Crime (Monthly)")
qqline(time_series, col="red")
# ***Figure 2.1 end ************************************************************


# Figure: multi-plot: different Type Crime time series
# **Didn't shown in report. No extra space for it......pity**
par(mfrow=c(3,3))
plot(time_series_M, xlab = "Year", ylab = "Crime Number", type="l", xaxt="n", main="Mischief")
axis(1, at=seq(1,180,12), labels=seq(2003,2017,1))
plot(time_series_OT, xlab = "Year", ylab = "Crime Number", type="l", xaxt="n", main="Other Theft")
axis(1, at=seq(1,180,12), labels=seq(2003,2017,1))
plot(time_series_ToV, xlab = "Year", ylab = "Crime Number", type="l", xaxt="n", main="Theft of Vehicle")
axis(1, at=seq(1,180,12), labels=seq(2003,2017,1))
plot(time_series_TfV, xlab = "Year", ylab = "Crime Number", type="l", xaxt="n", main="Theft from Vehicle")
axis(1, at=seq(1,180,12), labels=seq(2003,2017,1))
plot(time_series_ToB, xlab = "Year", ylab = "Crime Number", type="l", xaxt="n", main="Theft of Bicycle")
axis(1, at=seq(1,180,12), labels=seq(2003,2017,1))
plot(time_series_BEC, xlab = "Year", ylab = "Crime Number", type="l", xaxt="n", main="Break and Enter Commercial")
axis(1, at=seq(1,180,12), labels=seq(2003,2017,1))
plot(time_series_BERO, xlab = "Year", ylab = "Crime Number", type="l", xaxt="n", main="Break and Enter Residential/Other")
axis(1, at=seq(1,180,12), labels=seq(2003,2017,1))
plot(time_series_VCPSI, xlab = "Year", ylab = "Crime Number", type="l", xaxt="n", main="Vehicle Collision or Pedestrian Struck (with Injury)")
axis(1, at=seq(1,180,12), labels=seq(2003,2017,1))
plot(time_series_VCPSF, xlab = "Year", ylab = "Crime Number", type="l", xaxt="n", main="Vehicle Collision or Pedestrian Struck (with Fatality)")
axis(1, at=seq(1,180,12), labels=seq(2003,2017,1))

# Data Visualization - Spatial characteristics ===================================================================
library(ggplot2)
library(OpenStreetMap)
library(raster)
library(ggmap)
library(osmdata)

# ***Figure 1.1 Spatial Distribution of Crime in Vancouver (2003.10)************
par(mfrow=c(1,1))
# Figure: Break and Enter Commercial in Vancouver, 2003
# add crime data: eg.2016 data
crime_data <- data.frame(subset(crime, YEAR==2003 & MONTH==10))
# plot Vancouver map
mad_map <- get_map(getbb("Vancouver"), maptype = "toner-background")
ggmap(mad_map)+
  geom_point (data=crime_data, aes(x=Longitude,y=Latitude), color = "#BA4A00",size=0.3)+
  ggtitle("Crime in Vancouver, 2003.10")+
  labs(x = "LON", y = "LAT")

# Figure: Crime distribution in 2003.10
library(rgdal)
library(tmap)
vancouver_districts <- readOGR(dsn="Vancouver-local-area-boundary/local_area_boundary.shp") # local_area_boundary.shp has already linked crime data
#tmap_style("cobalt") # set style
#tmap_style("white") # back to original
tm_shape(vancouver_districts)+ 
  tm_fill("Month_10", style="jenks", palette="Blues")+
  tm_borders("white")+
  tm_compass(position=c("left","top"))+
  tm_scale_bar(position=c(0.6,0.005))

#tm_shape(vancouver_districts)+ 
#  tm_polygons(col="Month_10",
#              style="jenks",
#              palette="PuBu",
#              legend.hist = TRUE)+
#  tm_layout(legend.outside = TRUE)+
#  tm_compass(position=c(0.085,0.795))+
#  tm_scale_bar(position=c(0.015,0.005))

#tm_shape(vancouver_districts)+ 
#  tm_polygons(col="Month_10",
#              style = "cont",
#              palette="Purples")+
#  tm_layout(legend.outside = TRUE)+
#  tm_compass(position=c(0.085,0.795))+
#  tm_scale_bar(position=c(0.015,0.005))
# ***Figure 1.1 end*************************************************************




# Temporal autocorrelation =======================================================================================

# Figure 2.2 Temporal autocorrelation: ACF&PACF of different time and space scales***
Downtown <- time_series_by_neighbourhood[7,1:175]
Marpole <- time_series_by_neighbourhood[23,1:175]
par(mfrow=c(2,4))
acf(colMeans(matrix(time_series,12)), main="ACF, Annual")
acf(time_series,lag.max=35, main="ACF, Monthly")
acf(Downtown,lag.max=35, main="ACF, Monthly, Downtown")
acf(Marpole,lag.max=35, main="ACF, Monthly, Marpole")
pacf(colMeans(matrix(time_series,12)), main="PACF, Annual")
pacf(time_series,lag.max=35, main="PACF, Monthly")
pacf(Downtown,lag.max=35, main="PACF, Monthly, Downtown")
pacf(Marpole,lag.max=35, main="PACF, Monthly, Marpole")
# Figure 2.2 end*********************************************************************



# Spatial autocorrelation ========================================================================================
library(spdep)
W <- nb2listw(poly2nb(vancouver_districts))
W
library(knitr)
kable(listw2mat(W))

# Global Spatial Autocorrelation Measures
# Moran Index
crime_matrix<-data.matrix(vancouver_districts@data[,-c(1:3)])
rownames(crime_matrix) <- vancouver_districts@data[,"name"]
crime_avg <- rowMeans(crime_matrix)
moran.test(x=crime_avg, listw=W) # Moran I 0.25908 low
moran.mc(x=crime_avg, listw=W, nsim=9999) # another way to calculate Moran I

# Figure 2.3 Spatial autocorrelation: Moran scatter plot & LISA plot ***********
# Figure: Moran scatter plot
par(mfrow=c(1,1))
moran.plot(crime_avg,W,main="Morans'I = 0.25908")

# local Moran's I
lm <- localmoran(x=rowMeans(crime_matrix), listw=W)
lm
# Figure: LISA （this part is done by GeoDa）
# Figure 2.3 end ***************************************************************


# Spatio-temporal autocorrelation ================================================================================
# Figure 2.4 Spatio-temporal autocorrelation************************************
# Figure: Heatmap
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(crime_matrix,Colv = NA, Rowv = NA, scale="column", col = coul, xlab="", ylab="", main="Heatmap")

source("starima_package.R")
Wmat <- listw2mat(W)
# Figure: STACF & STPACF
par(mfrow=c(1,2))
stacf(t(crime_matrix), Wmat, 48)
stpacf(t(crime_matrix), Wmat, 4)
# Figure 2.4 end ***************************************************************












# =========================================================================================================================================
# CEGE0042: STDM Coursework ----- Part 3: ARIMA & ANN
# =========================================================================================================================================


# ARIMA ==========================================================================================================
library(TSA)
library(forecast)

# train set & test set
time_series_train <- time_series[1:140]
time_series_test <- time_series[141:175]

# Figure 3.1 Train set temporal autocorrelation(before differencing)************
par(mfrow=c(1,1))
# Figure: time_series & ACF & PACF
tsdisplay(time_series_train,xlab="Time (month)",ylab="Crime Number")
# Figure: lag 1 to 12
lag.plot(time_series_train, lags=12, do.lines=FALSE)
# Figure 3.1 end****************************************************************

# Figure 3.2 Train set temporal autocorrelation(after differencing)*************
# Figure: diff=1, time_series & ACF & PACF
time_series_diff1 <- diff(time_series_train, lag=1, differences=1)
tsdisplay(time_series_diff1,xlab="Time (month)",ylab="Crime Number")
# Figure 3.2 *******************************************************************

# estimate by figure 12 & 14: ARIMA(1,1,1)
fit.arima111 <- Arima(time_series_train,order=c(1,1,1))
summary(fit.arima111)

# determine parameters automatically
fit.arima <- auto.arima(time_series_train,trace=T)
fit.arima # ARIMA(1,1,1)

summary(fit.arima) # model summary

# Figure 3.4 Vancouver Crime Prediction Results by ARIMA************************
# Figure: forcast & contrast
fit.Ar <- Arima(time_series_train, order=c(1,1,1))
pre.Ar <- Arima(time_series_test, model=fit.Ar)
matplot(cbind(pre.Ar$x, pre.Ar$fitted),ylab="Crime Number", xlab="Month", main="Vancouver Crime, ARIMA",type="l")
legend("bottomleft", title="Data", c("Real","Predict"), lty=c(1, 2), col=c("black", "red"))
# Figure 3.4 end****************************************************************

# Figure 3.2 Residual distribution**********************************************
# Figure: Model Residuals
checkresiduals(fit.arima) # residuals 1
tsdiag(fit.arima) # residuals 2
# Figure 3.2 end****************************************************************

# NRMSE value
source("starima_package.R")
NRMSE_ARIMA <- NRMSE(res=fit.Ar$residuals, obs=time_series_train)
NRMSE_ARIMA

NRMSE_ANN <- NRMSE(obs=time_series_test, pred=pre.Ar$fitted)
NRMSE_ARIMA


# ANN ============================================================================================================
library(nnet)
library(rgdal)
vancouver_districts <- readOGR(dsn="Vancouver-local-area-boundary/local_area_boundary.shp")
crime_matrix<-data.matrix(vancouver_districts@data[,-c(1:3)])
rownames(crime_matrix) <- vancouver_districts@data[,"name"]
X <- t(as.matrix(crime_matrix))
y <- as.matrix(X[-1,])

# Figure 4.1 Prediction results of different regions and models*****************
par(mfrow=c(2,2))
# Figure: ANN plot, Downtown
set.seed(319)
crime.nnet <- nnet(X[1:140, 1:22], y[1:140, 1:22], decay=0.01, linout = TRUE, size=6, maxit = 1000)
crime.pred <- predict(crime.nnet, y[141:174, 1:22])
crime.pred[,8]
matplot(cbind(y[141:174,8], crime.pred[,8]),ylab="Crime Number", xlab="Month", main="Downtown Crime, ANN", type="l")

NRMSE_ANN <- NRMSE(obs=X[141:174,8],pred=crime.pred[,8])
NRMSE_ANN

# Figure: ANN plot, Marpole
set.seed(319)
crime.nnet <- nnet(X[1:140, 1:22], y[1:140, 1:22], decay=0.01, linout = TRUE, size=6, maxit = 1000)
crime.pred <- predict(crime.nnet, y[141:174, 1:22])
crime.pred[,12]
matplot(cbind(y[141:174,12], crime.pred[,12]),ylab="Crime Number", xlab="Month", main="Marpole Crime, ANN", type="l")
NRMSE_ANN <- NRMSE(obs=X[141:174,12],pred=crime.pred[,12])
NRMSE_ANN

# create Vancouver Total Crime numbers of each month
X.sum <- as.matrix(rowSums(X))
X.sum <- cbind(X, X.sum)
colnames(X.sum)[ncol(X.sum)] <- as.character(c("Vancouver"))
y.sum <- as.matrix(X.sum[-1,])

# Figure: ANN plot, Vancouver
set.seed(319)
crime.nnet <- nnet(X.sum[1:140, 1:23], y.sum[1:140, 1:23], decay=10, linout = TRUE, size=6, maxit = 100)
crime.pred <- predict(crime.nnet, y.sum[141:174, 1:23])
crime.pred[,23]
matplot(cbind(y.sum[141:174,23], crime.pred[,23]),ylab="Crime Number", xlab="Month", main="Vancouver Crime, ANN", type="l")
# legend("bottomleft", title="Data", c("Real","Predict"), lty=c(1, 2), col=c("black", "red"))

NRMSE_ANN <- NRMSE(res=crime.nnet$residuals, obs=X.sum[1:140, 1:23])
NRMSE_ANN

NRMSE_ANN <- NRMSE(obs=X.sum[141:174,23],pred=crime.pred[,23])
NRMSE_ANN

matplot(cbind(pre.Ar$x, pre.Ar$fitted),ylab="Crime Number", xlab="Month", main="Vancouver Crime, ARIMA",type="l")
# Figure 4.1 end ***************************************************************

# Figure 3.5 Internal structure of ANN******************************************
par(mfrow=c(1,1))
# Figure: Visualisation of internal structure of ANN
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(crime.nnet)
summary(crime.nnet) # check the weights
# Figure 3.5 end****************************************************************



# Figure 3.6 Predictions vs Observed********************************************
# Figure: Scatterplot of observed vs predicted values
plot(crime.pred,y.sum[141:174, 1:23], main="ANN predictions vs observed, Vancouver", xlab="Observed", ylab="Predicted")
# Figure 3.6 end****************************************************************

# Figure: ARIMA & ANN Vancouver
#par(mfrow=c(2,1))
#matplot(cbind(pre.Ar$x, pre.Ar$fitted),ylab="Crime Number", xlab="Month", main="Vancouver Crime, ARIMA",type="l")
#legend("bottomleft", title="Data", c("Real","Predict"), lty=c(1, 2), col=c("black", "red"))
#matplot(cbind(y.sum[141:174,23], crime.pred[,23]),ylab="Crime Number", xlab="Month", main="Vancouver Crime, ANN", type="l")
#legend("bottomleft", title="Data", c("Real","Predict"), lty=c(1, 2), col=c("black", "red"))



