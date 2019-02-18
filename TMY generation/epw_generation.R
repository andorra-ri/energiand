library(eplusr)
library(ggplot2)
library(grid)
library(gridExtra)
library(plotly)
library(anytime)
library(readxl)
library(xlsx)
library(leaflet)

setwd("D:/GitHub/energiand/TMY generation")

epw_escaldes <- read_epw("data/tmy_era_42.509_1.535_2006_2015.epw") 
#epw file imported from PVGIS5. I get the TMY 2006-2015 as there are some errors in 2007-2016 Relative humidity
#I did some manual modifications in the original file: add location and add S after SAVING (line 5)
epw_escaldes$start_day_of_week <- "Sunday" #change 1st Weekday

#show some epw file information
epw_escaldes$city
epw_escaldes$country
print(epw_escaldes)
  #show location in Leaflet
label <- paste(epw_escaldes$longitude,';', epw_escaldes$latitude, ";", epw_escaldes$elevation,'m')
leaflet() %>% addTiles() %>%
  addMarkers(lng=epw_escaldes$longitude, lat=epw_escaldes$latitude, label=label)

#get epw variables in a dataframe
df_escaldes <- epw_escaldes$get_data()

#some descriptive statistics
summary(df_escaldes$dry_bulb_temperature)
df_temperature <- data.frame("month"=1:12, "mean_temperature"=NA, "year"=NA) 
  #calculation of mean monthly temperature
for (i in 1:12){ 
df <- df_escaldes[month==i]
monthly_temp <- mean(df$dry_bulb_temperature)
df_temperature[i, 2] <- monthly_temp
df_temperature[i, 3] <- df$year[i]
}
filename <- sprintf("output/mean_temp_%s_%s.xlsx", epw_escaldes$latitude, epw_escaldes$longitude)
write.xlsx(df_temperature, filename)

#create function to plot all the variables in a grid
plot_epw_variables <- function(df)
{
  datetime_2017 <- read_xlsx("data/datetime_2017.xlsx") #define x-axis (year 2017)
  x_axis <- datetime_2017$DateTime
  p1= qplot(x_axis, dry_bulb_temperature, data=df, geom="line", xlab= "", ylab= "ºC", main= "Dry bulb temperature")
  p2= qplot(x_axis, dew_point_temperature, data=df, geom="line", xlab= "", ylab= "ºC", main= "Dew point temperature")
  p3= qplot(x_axis, relative_humidity, data=df, geom="line", xlab= "", ylab= "%", main= "Relative humidity" )
  p4= qplot(x_axis, atmospheric_pressure, data=df, geom="line", xlab= "", ylab= "Pa", main= "Atmospheric pressure")
  p5= qplot(x_axis, global_horizontal_radiation, data=df, geom="line", xlab= "", ylab= expression("W/m"^2), main= "Global horizontal radiation")
  p6= qplot(x_axis, direct_normal_radiation, data=df, geom="line", xlab= "", ylab= expression("W/m"^2), main= "Direct normal radiation")
  p7= qplot(x_axis, diffuse_horizontal_radiation, data=df, geom="line", xlab= "", ylab= expression("W/m"^2), main= "Diffuse horizontal radiation")
  p8= qplot(x_axis, horizontal_infrared_radiation_intensity_from_sky, data=df, geom="line", xlab="", ylab=expression("W/m"^2), main="Infrared radiation downwards")
  p9= qplot(x_axis, wind_direction, data=df, geom="line", xlab= "", ylab= "º", main= "Wind direction")
  p10=qplot(x_axis, wind_speed, data=df, geom="line", xlab= "", ylab= "m/s", main= "Wind speed")
  grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol=2, nrow=5)
}
  #plot all variables in a grid
plot_epw_variables(df_escaldes)

#monthly correction to the final epw file
df_escaldes_corrected <- df_escaldes
  #monthly constants based on mean temperature comparison (PVGIS/ACDA)
acda_correction <- read_xlsx("output/mean_temp_comparison.xlsx")
for (i in 1:12){
df <- df_escaldes[month==i]  
df_escaldes_corrected[month==i]$dry_bulb_temperature <- df_escaldes_corrected[month==i]$dry_bulb_temperature - as.numeric(acda_correction[i, "correction"]) 
}
#check df corrected
summary(df_escaldes_corrected$dry_bulb_temperature)
plot_epw_variables(df_escaldes_corrected)

#corrected epw file 
epw_escaldes$set_data(df_escaldes_corrected)
epw_filename <- sprintf("output/%s_%s.epw", epw_escaldes$latitude, epw_escaldes$longitude) 
epw_escaldes$save(epw_filename)