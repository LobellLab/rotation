# Clean up and merge weather data
weather_data = function(wnccpi) {
  df_weather = data.frame(FIPS = wnccpi$FIPS.formula, CRD = wnccpi$CRD, year = nccpi$year,
                          corn_diff = wnccpi$corn_diff, soy_diff = wnccpi$soy_diff,
                          corn_pbenefit = wnccpi$corn_pbenefit, soy_pbenefit = wnccpi$soy_pbenefit,
                          corn_prcp = (wnccpi$prcp_cc_mean + wnccpi$prcp_sc_mean)/2.0, soy_prcp = (wnccpi$prcp_ss_mean + wnccpi$prcp_cs_mean)/2.0,
                          corn_tmax = (wnccpi$tmax_cc_mean + wnccpi$tmax_sc_mean)/2.0, soy_tmax = (wnccpi$tmax_ss_mean + wnccpi$tmax_cs_mean)/2.0,
                          corn_tmin = (wnccpi$tmin_cc_mean + wnccpi$tmin_sc_mean)/2.0, soy_tmin = (wnccpi$tmin_ss_mean + wnccpi$tmin_cs_mean)/2.0,
                          corn_vpd = (wnccpi$vp_cc_mean + wnccpi$vp_sc_mean)/2.0, soy_vpd = (wnccpi$vp_ss_mean + wnccpi$vp_cs_mean)/2.0
  )
  return(df_weather)
}



# Scatterplot for each year
weather_scatter_per_year = function(df, crop_name, weather_name, val_name) {
  factor_name = paste0(crop_name, "_", weather_name)
  value_name = paste0(crop_name, "_", val_name)
  for (year in unique(df$year)) {
    plot(df[,factor_name][df$year == year], df[,value_name][df$year == year], xlab=paste(weather_name,"in July"), ylab=paste(crop_name, year))
    abline(lm(df[,value_name]~df[,factor_name]), col="green")
  }
}


# Does all the years at once, also does all the weather factors in one figure
weather_scatter = function(df, cropname) {
  factor_name = paste0(cropname, "_diff")
  v <- ggplot(df, aes_string(paste0(cropname,"_vpd"), factor_name)) + geom_point(aes(colour=factor(year), alpha = 0.75)) +
    labs(x = "Average July VPD", y = "Mean Rotation Difference", color = "Year", title = cropname) +
    geom_smooth()
  
  p <- ggplot(df, aes_string(paste0(cropname,"_prcp"), factor_name)) + geom_point(aes(colour=factor(year), alpha = 0.75)) +
    labs(x = "Average July Precipitation", y = "Mean Rotation Difference", color = "Year", title = cropname) +
    geom_smooth()
  
  t <- ggplot(df, aes_string(paste0(cropname,"_tmax"), factor_name)) + geom_point(aes(colour=factor(year), alpha = 0.75)) +
    labs(x = "Average July TMax", y = "Mean Rotation Difference", color = "Year", title = cropname) +
    geom_smooth()
  
  tm <- ggplot(df, aes_string(paste0(cropname,"_tmin"), factor_name)) + geom_point(aes(colour=factor(year), alpha = 0.75)) +
    labs(x = "Average July TMin", y = "Mean Rotation Difference", color = "Year", title = cropname) +
    geom_smooth()
  
  multiplot(v, p, t, tm, cols = 2)
}


# Hexplot for all years at once, all factors in one figure
weather_density = function(df, cropname) {
  factor_name = paste0(cropname, "_diff")
  v <- ggplot(df, aes_string(paste0(cropname,"_vpd"), factor_name)) +
    stat_binhex() + 
    labs(x = "Average July VPD", y = "Mean Rotation Difference", title = cropname)
  p <- ggplot(df, aes_string(paste0(cropname,"_prcp"), factor_name)) +
    stat_binhex() + 
    labs(x = "Average July Precipitation", y = "Mean Rotation Difference", title = cropname)
  t <- ggplot(df, aes_string(paste0(cropname,"_tmin"), factor_name)) + 
    stat_binhex() +
    labs(x = "Average July TMax", y = "Mean Rotation Difference", title = cropname)
  tm <- ggplot(df, aes_string(paste0(cropname,"_tmin"), factor_name)) + 
    stat_binhex() +
    labs(x = "Average July TMin", y = "Mean Rotation Difference", title = cropname)
  
  multiplot(v, p, t, tm, cols=2)
}