library('animation')

write_outliers_csv = function(corn_before_corn, corn_before_soy, soy_before_corn, soy_before_soy) {
  
  # Reload stuff without outliers
  corn <- cbind(corn = corn_before_corn, soy = soy_before_corn)
  corn_with_diff <- mutate(corn, diff = corn$soy.mean - corn$corn.mean)
  corn_means_eachyear = ddply(corn_with_diff, c("corn.year", "corn.FIPS.formula", "corn.count"), summarise, mean = mean(diff))
  names(corn_means_eachyear) = c("year", "FIPS", "count", "mean")
  soy <- cbind(corn = corn_before_soy, soy = soy_before_soy)
  soy_with_diff <- mutate(soy, diff = soy$corn.mean - soy$soy.mean)
  soy_means_eachyear = ddply(soy_with_diff, c("soy.year", "soy.FIPS.formula", "soy.count"), summarise, mean = mean(diff))
  names(soy_means_eachyear) = c("year", "FIPS", "count", "mean")
  
  # Find outliers in yields for corn and soy
  corn_outliers = data.frame(na.omit(corn_means_eachyear[corn_means_eachyear$mean > 300,])) 
  soy_outliers = data.frame(na.omit(soy_means_eachyear[soy_means_eachyear$mean > 100,])) 
  write.csv(corn_outliers, "corn_outliers.csv")
  write.csv(soy_outliers, "soy_outliers.csv")
  
  list(corn_outliers, soy_outliers)
}


# This seems not to work cos there isn't actually enough data among the outliers to use the FIPS map--
# Chris can you confirm? Thx! If this could work it'd be a nice way to see sample size per county.
pixel_outliers = function(corn_before_corn, corn_before_soy, soy_before_corn, soy_before_soy){
  outliers = write_outliers_csv(corn_before_corn, corn_before_soy, soy_before_corn, soy_before_soy)
  corn = outliers[[1]]
  soy = outliers[[2]]
  make_pixel_gif(corn, "Corn ", "outlier_pixels_corn.gif")
  make_pixel_gif(soy, "Soy ", "outlier_pixels_soy.gif")
}

