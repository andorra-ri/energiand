library(eplusr) # https://rdrr.io/cran/eplusr/man/Epw.html
library(ggplot2)
library(grid)
library(gridExtra)
library(readxl)
library(leaflet)

# epw file imported from PVGIS5 (https://re.jrc.ec.europa.eu/pvg_tools/en/#TMY)
# I did a manual modifications in the original file (add S after SAVING (line 5))
epw <- read_epw("../data/tmy_42.549_1.698_2007_2016.epw")

# Modify location data ---------------------------
epw$location(city = "ETR Grau Roig", state_province = "Encamp", country = "AND")

# epw$period(1, start_day_of_week = "Thursday") # Change 1st weekday

# Show some epw file information ---------------------------
epw$location()$city
epw$location()$country
print(epw)
# show location in Leaflet
label <- paste(epw$location()$longitude, ";", epw$location()$latitude, ";", epw$location()$elevation, "m")
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = epw$location()$longitude, lat = epw$location()$latitude, label = label)

# Load epw variables in a dataframe
df_data <- epw$data()

# Some descriptive statistics
summary(df_data$dry_bulb_temperature)
summary(df_data$global_horizontal_radiation)

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
plot_epw_variables(df_data)

# Calculation of mean monthly temperature to compare with ACDA ---------------------------
df_temperature <- data.frame("month" = 1:12, "mean_temperature" = NA, "year" = NA)
for (i in 1:12) {
  df <- df_data[month == i]
  monthly_temp <- mean(df$dry_bulb_temperature)
  df_temperature[i, 2] <- monthly_temp
  df_temperature[i, 3] <- df$year[i]
}
View(df_temperature)

# Monthly correction to the final epw file using ACDA (http://www.acda.ad/) data ---------------------------
df_data_corrected <- df_data

# Monthly constants based on mean temperature comparison (PVGIS-ACDA)
acda_correction <- data.frame(
  "mean_temperature_acda" = c(-0.9, -0.7, 1.5, 2.7, 6.6, 10.6, 13.7, 13.5, 10.3, 6.7, 2.3, 0.1), # This column needs to be constructed manually using ACDA values for each month
  "correction" = NA)
acda_correction$correction <- df_temperature$mean_temperature - acda_correction$mean_temperature_acda

for (i in 1:12) {
  df <- df_data[month == i]
  df_data_corrected[month == i]$dry_bulb_temperature <- df_data_corrected[month == i]$dry_bulb_temperature - acda_correction[i, "correction"]
}

# Check the corrected df
summary(df_data_corrected$dry_bulb_temperature)
plot_epw_variables(df_data_corrected)

# Save the corrected epw file ---------------------------
epw$set(df_data_corrected)
epw$save(paste0("../output/", "tmy_", epw$location()$latitude, "_", epw$location()$longitude, ".epw"))
