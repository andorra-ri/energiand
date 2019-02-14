library(eplusr)
library(ggplot2)
library(grid)
library(gridExtra)
library(plotly)
library(anytime)
library(readxl)
library(xlsx)

setwd("D:/GitHub/energiand/TMY generation")

epw_escaldes <- read_epw("data/tmy_era_42.509_1.535_2007_2016.epw") 
#epw file imported from PVGIS5. 
#I did some manual modifications in the original file: add location and add S after SAVING (line 5)

#show some epw file information
epw_escaldes$city
epw_escaldes$country
print(epw_escaldes)

#get epw variables in a dataframe
df_escaldes <- epw_escaldes$get_data()

#some descriptive statistics
summary(df_escaldes$dry_bulb_temperature)
df_temperature <- data.frame("month"=1:12, "mean_temperature"=NA) 
  #calculation of mean monthly temperature
for (i in 1:12){ 
df <- df_escaldes[month==i]
monthly_temp <- mean(df$dry_bulb_temperature)
df_temperature[i, 2] <- monthly_temp
}
filename <- sprintf("output/mean_temp_%s_%s.xlsx", epw_escaldes$latitude, epw_escaldes$longitude)
write.xlsx(df_temperature, filename)

#plot all the variables in a grid
df <- df_escaldes
datetime_2017 <- read_xlsx("data/datetime_2017.xlsx") #define x-axis (year 2017)
x_axis <- datetime_2017$DateTime
p1= qplot(x_axis, dry_bulb_temperature, data=df, geom="line", xlab= "", ylab= "ºC", main= "dry_bulb_temperature")
p2= qplot(x_axis, relative_humidity, data=df, geom="line", xlab= "", ylab= "%", main= "relative_humidity" )
p3= qplot(x_axis, atmospheric_pressure, data=df, geom="line", xlab= "", ylab= "Pa", main= "atmospheric_pressure")
p4= qplot(x_axis, global_horizontal_radiation, data=df, geom="line", xlab= "", ylab= "W/m2", main= "global_horizontal_radiation")
p5= qplot(x_axis, direct_normal_radiation, data=df, geom="line", xlab= "", ylab= "W/m2", main= "direct_normal_radiation")
p6= qplot(x_axis, diffuse_horizontal_radiation, data=df, geom="line", xlab= "", ylab= "W/m2", main= "diffuse_horizontal_radiation")
p7= qplot(x_axis, wind_direction, data=df, geom="point", xlab= "", ylab= "º", main= "wind_direction")
p8= qplot(x_axis, wind_speed, data=df, geom="line", xlab= "", ylab= "m/s", main= "wind_speed")
p9= qplot(x_axis, total_sky_cover, data=df, geom="line", xlab= "", ylab= "Octes", main= "total_sky_cover")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=3, nrow=3)

#plot individual variables in plotly
ggplotly(p1)