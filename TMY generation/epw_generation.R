library(eplusr) # https://rdrr.io/cran/eplusr/man/Epw.html
library(ggplot2)
library(grid)
library(gridExtra)
library(plotly)
library(readxl)
library(leaflet)

# epw file imported from PVGIS5 (https://re.jrc.ec.europa.eu/pvg_tools/en/#TMY)
# I did some manual modifications in the original file: add location and add S after SAVING (line 5)
epw_escaldes <- read_epw("../data/tmy_42.549_1.698_2007_2016.epw")
#epw_escaldes$period(1, start_day_of_week = "Thursday") # Change 1st weekday

# Show some epw file information ---------------------------
epw_escaldes$location()$city
epw_escaldes$location()$country
print(epw_escaldes)
# show location in Leaflet
label <- paste(epw_escaldes$location()$longitude, ";", epw_escaldes$location()$latitude, ";", epw_escaldes$location()$elevation, "m")
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = epw_escaldes$location()$longitude, lat = epw_escaldes$location()$latitude, label = label)

# Get epw variables in a dataframe
df_escaldes <- epw_escaldes$data()

# Some descriptive statistics
summary(df_escaldes$dry_bulb_temperature)

# Create function to plot all the variables in a grid ---------------------------
plot_epw_variables <- function(df) {
  x_axis <- df$datetime
  p1 <- qplot(x_axis, dry_bulb_temperature, data = df, geom = "line", xlab = "", ylab = "ºC", main = "Dry bulb temperature")
  p2 <- qplot(x_axis, dew_point_temperature, data = df, geom = "line", xlab = "", ylab = "ºC", main = "Dew point temperature")
  p3 <- qplot(x_axis, relative_humidity, data = df, geom = "line", xlab = "", ylab = "%", main = "Relative humidity")
  p4 <- qplot(x_axis, atmospheric_pressure, data = df, geom = "line", xlab = "", ylab = "Pa", main = "Atmospheric pressure")
  p5 <- qplot(x_axis, global_horizontal_radiation, data = df, geom = "line", xlab = "", ylab = expression("W/m"^2), main = "Global horizontal radiation")
  p6 <- qplot(x_axis, direct_normal_radiation, data = df, geom = "line", xlab = "", ylab = expression("W/m"^2), main = "Direct normal radiation")
  p7 <- qplot(x_axis, diffuse_horizontal_radiation, data = df, geom = "line", xlab = "", ylab = expression("W/m"^2), main = "Diffuse horizontal radiation")
  p8 <- qplot(x_axis, horizontal_infrared_radiation_intensity_from_sky, data = df, geom = "line", xlab = "", ylab = expression("W/m"^2), main = "Infrared radiation downwards")
  p9 <- qplot(x_axis, wind_direction, data = df, geom = "line", xlab = "", ylab = "º", main = "Wind direction")
  p10 <- qplot(x_axis, wind_speed, data = df, geom = "line", xlab = "", ylab = "m/s", main = "Wind speed")
  grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2, nrow = 5)
}
# Plot all variables in a grid
plot_epw_variables(df_escaldes)

# Calculation of mean monthly temperature to compare with ACDA ---------------------------
df_temperature <- data.frame("month" = 1:12, "mean_temperature" = NA, "year" = NA)
for (i in 1:12) {
  df <- df_escaldes[month == i]
  monthly_temp <- mean(df$dry_bulb_temperature)
  df_temperature[i, 2] <- monthly_temp
  df_temperature[i, 3] <- df$year[i]
}
write.csv(x = df_temperature, file = paste0("../output/mean_temp_", epw_escaldes$location()$latitude, "_", epw_escaldes$location()$longitude, ".csv"), row.names = FALSE)

# Monthly correction to the final epw file using ACDA (http://www.acda.ad/) data ---------------------------
df_escaldes_corrected <- df_escaldes

# Monthly constants based on mean temperature comparison (PVGIS-ACDA)
acda_correction <- read_xlsx("../output/mean_temp_comparison.xlsx") # This excel needs to be constructed manually using ACDA values for each month and the previous generated .csv
for (i in 1:12) {
  df <- df_escaldes[month == i]
  df_escaldes_corrected[month == i]$dry_bulb_temperature <- df_escaldes_corrected[month == i]$dry_bulb_temperature - as.numeric(acda_correction[i, "correction"])
}

# Check the corrected df
summary(df_escaldes_corrected$dry_bulb_temperature)
plot_epw_variables(df_escaldes_corrected)

# Save the corrected epw file ---------------------------
epw_escaldes$set(df_escaldes_corrected)
epw_escaldes$save(paste0("../output/", epw_escaldes$location()$latitude, "_", epw_escaldes$location()$longitude, ".epw"))
