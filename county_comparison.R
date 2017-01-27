# Divides 15 years into 3 figures
#      Pass in "diff" for rotation - nonrotation yields, or "diff_count" for corresponding pixel count
stationary_comparison = function(df, crop_name, val_name, title) {
  factor_name = paste0(crop_name, "_", val_name)
  par(mfrow=c(2,3))
  for (year in unique(df$year)[1:6]) {
    map.var(data.frame(fips = df$FIPS.formula[df$year==year], my = df[,factor_name][df$year==year]), titl = paste0(title, year), xleg =-97, yleg = 40, legend=TRUE, leg_size = 0.85)
  }
  for (year in unique(corn_means_eachyear$year)[7:12]) {
    map.var(data.frame(fips = df$FIPS.formula[df$year==year], my = df[,factor_name][df$year==year]), titl = paste0(title, year), xleg =-97, yleg = 40, legend=TRUE, leg_size = 0.85)
  }
  for (year in unique(corn_means_eachyear$year)[13:15]) {
    map.var(data.frame(fips = df$FIPS.formula[df$year==year], my = df[,factor_name][df$year==year]), titl = paste0(title, year), xleg =-97, yleg = 40, legend=TRUE, leg_size = 0.85)
  }
}



# GIF of yields per year
#    Pass in val_names: "diff" for rotation diff yield; "diff_count" for pixel count; 
#    or "pbenefit" for percent rotation benefit
make_gif = function(df, crop_name, val_name, title, filename) {
  factor_name = paste0(crop_name, "_", val_name)
  scale = 1.0
  par(mfrow=c(1,1))
  saveGIF({
    for (year in unique(na.omit(df$year))) {
      if(val_name == "diff_count"){
        scale = 1000.0
      }
      map.var(data.frame(fips = df$FIPS.formula[df$year==year], my = df[,factor_name][df$year==year]/scale), titl = paste0(crop_name, title, year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
    }
  }, interval = 2, ani.width=600, ani.height=800, loop=TRUE, movie.name = filename, outdir = getwd())
}


# Percent rotation benefit
pbenefit_hist = function(df, chris_corn, chris_soy, overlay) {
  par(mfrow=c(2,1))
  hist(df$corn_pbenefit, breaks = 30, xlab = "Percent Rotation Benefit", main = "Corn", ylab = "Num County-Year Instances",col = rgb(1,0,0,0.5), freq = FALSE)
  if(overlay){
    hist(chris_corn$percent.effect*100, col = rgb(0,0,1,0.5), breaks = 30, add = T, freq = FALSE)
  }
  hist(df$soy_pbenefit, xlab = "Percent Rotation Benefit", breaks = 30, main = "Soy", ylab = "Num County-Year Instances", col = rgb(1,0,0,0.5),freq = FALSE)
  if(overlay){
    hist(chris_soy$percent.effect*100, col = rgb(0,0,1,0.5), add = T, breaks=30, freq = FALSE)
  }
}



# Plots CRD-year pbenefits from my and Chris's data against one another
data_comparison_scatterplot = function(cnccpi, chris_corn, chris_soy){
  merge_me_corn = data.frame(cnccpi$CRD, cnccpi$year, cnccpi$corn_pbenefit)
  merge_me_soy = data.frame(cnccpi$CRD, cnccpi$year, cnccpi$soy_pbenefit)
  names(merge_me_corn) = c("CRD", "year", "pbenefit")
  names(merge_me_soy) = c("CRD", "year", "pbenefit")
  merged_corn = merge(merge_me_corn, chris_corn, by = c("year", "CRD"))
  merged_soy = merge(merge_me_soy, chris_soy, by = c("year", "CRD"))
  
  par(mfrow=c(2,1))
  plot(merged_corn$pbenefit, merged_corn$percent.effect, xlab = "EE Percent Benefit", ylab = "Chris Percent Effect", main = "Corn All County-Years")
  abline(lm(merged_corn$percent.effect~merged_corn$pbenefit), col = "plum1", lwd = 3)
  plot(merged_soy$pbenefit, merged_soy$percent.effect, xlab = "EE Percent Benefit", ylab = "Chris Percent Effect", main = "Soy All County-Years")
  abline(lm(merged_soy$percent.effect~merged_soy$pbenefit), col = "cadetblue1", lwd = 3)
}


# Percent rotation benefit versus rotated - nonrotated yields
pbenefit_vs_yield = function(df) {
  p1 <- ggplot(df, aes(x = corn_diff, y = corn_pbenefit)) + geom_point(aes(colour=factor(year), alpha = 0.75)) +
    labs(x = "Yields of Rotated Fields", y = "Percent Benefit of Rotation", color = "Year", title = "Corn") +
    geom_smooth()
  
  p2 <- ggplot(df, aes(x = soy_diff, y = soy_pbenefit)) + geom_point(aes(colour=factor(year), alpha = 0.75)) +
    labs(x = "Yields of Rotated Fields", y = "Percent Benefit of Rotation", color = "Year", title = "Soy") +
    geom_smooth()
  
  multiplot(p1,p2)
}  

# Plots county averages for all 15 years, pbenefit versus rotated yields (not rotation diff, actual rotation yields)
avg_pbenefit_by_county = function(wnccpi, FIPS) {
  if(FIPS){
    df = ddply(wnccpi, ~FIPS.formula, summarise, crot_mean = mean(wyield_sc_mean, na.rm=T), srot_mean = mean(wyield_cs_mean, na.rm=T), 
             cpb_mean = mean(corn_pbenefit, na.rm=T), spb_mean = mean(soy_pbenefit, na.rm=T))
  } else {
    df = ddply(wnccpi, ~CRD, summarise, crot_mean = mean(wyield_sc_mean, na.rm=T), srot_mean = mean(wyield_cs_mean, na.rm=T), 
               cpb_mean = mean(corn_pbenefit, na.rm=T), spb_mean = mean(soy_pbenefit, na.rm=T))
  }
  par(mfrow=c(1,2))
  plot(df$crot_mean, df$cpb_mean, ylab = "Percent Benefit Mean", xlab = "Yields of Rotated Fields Mean", main = "Mean % Rotation Benefit vs\n Rotation Yields\n for Corn")
  abline(lm(df$cpb_mean~df$crot_mean), col="green")
  
  plot(df$srot_mean, df$spb_mean, ylab = "Percent Benefit Mean", xlab = "Yields of Rotated Fields Mean", main = "Mean % Rotation Benefit vs\n Rotation Yields\n for Soy")
  abline(lm(df$spb_mean~df$srot_mean), col="blue")
  
  return(df)
}


# =========== BROKEN ============= BROKEN ============= BROKEN ==================
yield_anomaly = function(df_by_county, df, FIPS) {
  if(FIPS) {
    anomaly = join(df_by_county, df, by = "FIPS.formula")
    anomaly = ddply(anomaly, ~FIPS.formula, summarise, anomaly_crot = crot_mean - wyield_sc_mean, anomaly_srot = srot_mean - wyield_cs_mean,
          anomaly_cpb = cpb_mean - corn_pbenefit, anomaly_spb = spb_mean - soy_pbenefit)
  } else {
    anomaly = join(df_by_county, df, by = "CRD")
    anomaly = ddply(anomaly, ~CRD, summarise, anomaly_crot = crot_mean - wyield_sc_mean, anomaly_srot = srot_mean - wyield_cs_mean,
                    anomaly_cpb = cpb_mean - corn_pbenefit, anomaly_spb = spb_mean - soy_pbenefit)
  }
  par(mfrow=c(1,2))
  plot(anomaly$anomaly_crot, anomaly$anomaly_cpb, ylab = "Percent Benefit Mean", xlab = "Yields of Rotated Fields\n Difference From Average", main = "Mean % Rotation Benefit vs\n Yield Diff from Avg for \nCorn")
  abline(lm(anomaly$anomaly_cpb~anomaly$anomaly_crot), col="green")
  plot(anomaly$anomaly_srot, anomaly$anomaly_spb, ylab = "Percent Benefit Mean", xlab = "Yields of Rotated Fields\n Difference From Average", main = "Mean % Rotation Benefit vs\n Yield Diff from Avg for \nSoy")
  abline(lm(anomaly$anomaly_spb~anomaly$anomaly_srot), col="green")
}
# =========== BROKEN ============= BROKEN ============= BROKEN ==================

# Could probably put this in the main dataframe
get_variability_data = function(df, FIPS){
  if(FIPS){
    df_pdiff <- df %>%
      group_by(FIPS.formula) %>%
      summarise(pdiff_crot = quantile(wyield_sc_mean, c(0.1, 0.9), na.rm=T)[[2]] - quantile(wyield_sc_mean, c(0.1, 0.9), na.rm=T)[[1]], 
                pdiff_cnotrot = quantile(wyield_cc_mean, c(0.1, 0.9), na.rm=T)[[2]] - quantile(wyield_sc_mean, c(0.1, 0.9), na.rm=T)[[1]],
                pdiff_srot = quantile(wyield_cs_mean, c(0.1, 0.9), na.rm=T)[[2]] - quantile(wyield_cs_mean, c(0.1, 0.9), na.rm=T)[[1]], 
                pdiff_snotrot = quantile(wyield_ss_mean, c(0.1, 0.9), na.rm=T)[[2]] - quantile(wyield_ss_mean, c(0.1, 0.9), na.rm=T)[[1]])
  } else {
    df_pdiff <- df %>%
      group_by(CRD) %>%
      summarise(pdiff_crot = quantile(wyield_sc_mean, c(0.1, 0.9), na.rm=T)[[2]] - quantile(wyield_sc_mean, c(0.1, 0.9), na.rm=T)[[1]], 
                pdiff_cnotrot = quantile(wyield_cc_mean, c(0.1, 0.9), na.rm=T)[[2]] - quantile(wyield_sc_mean, c(0.1, 0.9), na.rm=T)[[1]],
                pdiff_srot = quantile(wyield_cs_mean, c(0.1, 0.9), na.rm=T)[[2]] - quantile(wyield_cs_mean, c(0.1, 0.9), na.rm=T)[[1]], 
                pdiff_snotrot = quantile(wyield_ss_mean, c(0.1, 0.9), na.rm=T)[[2]] - quantile(wyield_ss_mean, c(0.1, 0.9), na.rm=T)[[1]])
  }
  
  return(df_pdiff)
}


variability_scatter = function(df_pdiff) {
  par(mfrow=c(1,2))
  plot(df_pdiff$pdiff_crot, df_pdiff$pdiff_cnotrot, col = "green", xlab = "Corn Rotated Variability", ylab = "Not Rotated Variability")
  abline(0,1)
  plot(df_pdiff$pdiff_srot, df_pdiff$pdiff_snotrot, col = "blue", xlab = "Soy Rotated Variability", ylab = "Not Rotated Variability")
  abline(0,1)
}


# Variability difference between rotated and non-rotated fields
percentile_rotation_difference = function(df_pdiff) {
  par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
  hist(df_pdiff$pdiff_crot, col = rgb(1,0,0,0.5), xlab = "Difference in 90th and 10th percentiles", ylab = "Number of Fields (all years)", main = "Corn")
  hist(df_pdiff$pdiff_cnotrot, col = rgb(0,0,1,0.5), add = T)
  legend("topright", c("Rotated", "Not Rotated"), fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), cex = .85)
  
  hist(df_pdiff$pdiff_srot, col = rgb(1,0,0,0.5), xlab = "Difference in 90th and 10th percentiles", ylab = "Number of Fields (all years)", main = "Soy")
  hist(df_pdiff$pdiff_snotrot, col = rgb(0,0,1,0.5), add = T)
  legend("topright", c("Rotated", "Not Rotated"), fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), cex = .85)
  
  mtext("Variability of Rotated and Non-Rotated Fields", outer=TRUE)
}  



  

