all_data_histogram = function(corn_before_corn, corn_before_soy, soy_before_corn, soy_before_soy) {
  # Histograms of all years and counties (aka not supes informative)
  par(mfrow=c(2,2))
  hist(corn_before_corn$mean, col = "cadetblue1", xlab="Mean Yield", main = "Corn Before Corn")
  abline(v = mean(na.omit(corn_before_corn$mean)), lwd = 3, col = "red")
  hist(corn_before_soy$mean, col="springgreen1",  xlab="Mean Yield", main = "Corn before Soy")
  abline(v = mean(na.omit(corn_before_soy$mean)), lwd = 3, col = "red")
  hist(soy_before_corn$mean, col="gold1",  xlab="Mean Yield", main = "Soy Before Corn")
  abline(v = mean(na.omit(soy_before_corn$mean)), lwd = 3, col = "red")
  hist(soy_before_soy$mean, col="plum1",  xlab="Mean Yield", main = "Soy Before Corn")
  abline(v = mean(na.omit(soy_before_soy$mean)), lwd = 3, col = "red")
}

all_data_sig = function(corn_before_corn, corn_before_soy, soy_before_corn, soy_before_soy) {
  p_corn <- t.test(na.omit(corn_before_corn$mean), na.omit(soy_before_corn$mean))[["p.value"]]
  p_soy <- t.test(na.omit(corn_before_soy$mean), na.omit(soy_before_soy$mean))[["p.value"]]
}

all_data_line_graph = function(corn_before_corn, corn_before_soy, soy_before_corn, soy_before_soy) {
  all_means <- data.frame(cbind(corn_before_corn$mean, corn_before_corn$year, corn_before_soy$mean, corn_before_soy$year, soy_before_corn$mean, soy_before_corn$year, soy_before_soy$mean, soy_before_soy$year))
  all_means <- na.omit(all_means)
  all_means <- setNames(all_means, c('cc', 'cc_year', 'cs', 'cs_year', 'sc', 'sc_year', 'ss', 'ss_year'))
  year_means_cc <- tapply(all_means$cc, all_means$cc_year, mean)
  year_means_cs <- tapply(all_means$cs, all_means$cs_year, mean)
  year_means_sc <- tapply(all_means$sc, all_means$sc_year, mean)
  year_means_ss <- tapply(all_means$ss, all_means$ss_year, mean)
  year_means <- data.frame(cbind(year_means_cc, year_means_cs, year_means_sc, year_means_ss))
  year_means <- setNames(cbind(rownames(year_means), year_means, row.names=NULL), c("year", "year_means_cc", "year_means_cs", "year_means_sc", "year_means_ss"))
  
  ggplot(data = year_means, aes(x = year)) +
    geom_line(aes(y = year_means_cc, color = "Corn before Corn"), group=1) +
    geom_line(aes(y = year_means_sc, color = "Soy before Corn"),group=1) +
    geom_line(aes(y = year_means_cs, color = "Corn before Soy"),group=1) +
    geom_line(aes(y = year_means_ss, color = "Soy before Soy"),group=1) +
    labs(
      x = "Year",
      y = "Mean Yield",
      color = "Rotation"
    )
}