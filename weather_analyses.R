# Clean up and merge weather data
weather_data = function(df, tmax1, tmax2, prcp1, prcp2) {
  tmax_df = merge(tmax1, tmax2, by=c("FIPS.formula", "year", "Geographic.Name", "system.index"))
  tmax_df$t_max = (tmax_df$mean.x + tmax_df$mean.y)/2.0
  # Remove outliers? What counts as an outlier?
  prcp_df = merge(prcp1, prcp2, by=c("FIPS.formula", "year", "Geographic.Name", "system.index"))
  prcp_df$prcp = (prcp_df$mean.x + prcp_df$mean.y)/2.0
  weather = merge(tmax_df, prcp_df, by = c("FIPS.formula", "year", "Geographic.Name", "system.index"))
  weather = data.frame(weather$year, weather$FIPS.formula, weather$t_max, weather$prcp)
  names(weather) = c("year", "FIPS.formula", "tmax", "prcp")
  names(df) = c("year", "FIPS.formula", "count", "mean", "CRD")
  weather = full_join(weather, df, by = c("FIPS.formula", "year"))
  na.omit(weather, cols = c("mean"))
}

# ^^ Seems to make 2015 disappear? Didn't have this problem earlier. When I look at 
# na.omit(weather, cols=c("mean")), it's all there, so I'm a bit puzzled.

# Plot prcp and tmax
scatter_prcp = function(df, crop_name) {
  for (year in unique(df$year)) {
    plot(df$prcp[df$year == year], df$mean[df$year == year], xlab="Precipitation in July", ylab=paste0(crop_name, year))
    abline(lm(df$mean~df$prcp), col="green")
  }
}
scatter_tmax = function(df, crop_name) {
  for (year in unique(df$year)) {
    plot(df$tmax[df$year == year], df$mean[df$year == year], ylab=paste0(crop_name, year), xlab="T_Max in July")
    abline(lm(df$mean~df$tmax), col="green")
  }
}

overall_scatter = function(df) {
  cp <- ggplot(c_weather, aes(prcp, mean)) + geom_point(aes(colour=factor(year))) +
    labs(x = "Average July Precipitation", y = "Mean Rotation Difference", color = "Year", title = "Corn")
  
  ct <- ggplot(c_weather, aes(tmax, mean)) + geom_point(aes(colour=factor(year))) +
    labs(x = "Average July TMax", y = "Mean Rotation Difference", color = "Year", title = "Corn")
  
  sp <- ggplot(s_weather, aes(prcp, mean)) + geom_point(aes(colour=factor(year))) +
    labs(x = "Average July Precipitation", y = "Mean Rotation Difference", color = "Year", title = "Soy")
  
  st <- ggplot(s_weather, aes(tmax, mean)) + geom_point(aes(colour=factor(year))) +
    labs(x = "Average July TMax", y = "Mean Rotation Difference", color = "Year", title = "Soy")
  
  multiplot(cp, ct, sp, st)
}

#tmax_overall = function(df, crop_name) {
  #overall = ddply(c_weather, "FIPS.formula", summarise, mean = mean(mean), prcp = mean(prcp), tmax = mean(tmax))
#}