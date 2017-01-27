set_up = function(percentile, outlier_val){
  nccpi = as.data.frame(fread("nccpi_collection.csv", check.names = TRUE))
  CRD = as.data.frame(fread("FIPS_CRD.csv", check.names = TRUE))
  names(CRD) = c("FIPS.formula", "CRD")
  nccpi = join(nccpi, CRD, by = "FIPS.formula")
  
  # Remove outliers.
  # Do for all nccpis because some counties might be really good and have almost no pixels 
  # in low nccpi buckets
  nccpi = remove_outliers(nccpi, percentile, outlier_val)
  
  wnccpi = ddply(nccpi, .(FIPS.formula, CRD, year, prcp_cc_mean, prcp_cs_mean, prcp_sc_mean, prcp_ss_mean, # If you want weather nccpis, add 'em here
                          tmax_cc_mean, tmax_cs_mean, tmax_sc_mean, tmax_ss_mean,
                          tmin_cc_mean, tmin_cs_mean, tmin_sc_mean, tmin_ss_mean,
                          vp_cc_mean, vp_cs_mean, vp_sc_mean, vp_ss_mean),
                 function(x) data.frame(wyield_cc_mean = weighted.mean(x$yield_cc_mean, x$yield_cc_count), 
                                        wyield_sc_mean = weighted.mean(x$yield_sc_mean, x$yield_sc_count), 
                                        wyield_cs_mean = weighted.mean(x$yield_cs_mean, x$yield_cs_count), 
                                        wyield_ss_mean = weighted.mean(x$yield_ss_mean, x$yield_ss_count),
                                        corn_diff_count = x$yield_sc_count - x$yield_cc_count,
                                        soy_diff_count = x$yield_cs_count - x$yield_ss_count,
                                        wyield_cc_nccpi1_mean = weighted.mean(x$yield_cc_nccpi1_mean, x$yield_cc_nccpi1_count), 
                                        wyield_cc_nccpi2_mean = weighted.mean(x$yield_cc_nccpi2_mean, x$yield_cc_nccpi2_count),
                                        wyield_cc_nccpi3_mean = weighted.mean(x$yield_cc_nccpi3_mean, x$yield_cc_nccpi3_count),
                                        wyield_cc_nccpi4_mean = weighted.mean(x$yield_cc_nccpi4_mean, x$yield_cc_nccpi4_count), 
                                        wyield_cc_nccpi5_mean = weighted.mean(x$yield_cc_nccpi5_mean, x$yield_cc_nccpi5_count),
                                        wyield_sc_nccpi1_mean = weighted.mean(x$yield_sc_nccpi1_mean, x$yield_sc_nccpi1_count), 
                                        wyield_sc_nccpi2_mean = weighted.mean(x$yield_sc_nccpi2_mean, x$yield_sc_nccpi2_count),
                                        wyield_sc_nccpi3_mean = weighted.mean(x$yield_sc_nccpi3_mean, x$yield_sc_nccpi3_count),
                                        wyield_sc_nccpi4_mean = weighted.mean(x$yield_sc_nccpi4_mean, x$yield_sc_nccpi4_count), 
                                        wyield_sc_nccpi5_mean = weighted.mean(x$yield_sc_nccpi5_mean, x$yield_sc_nccpi5_count),
                                        wyield_cs_nccpi1_mean = weighted.mean(x$yield_cs_nccpi1_mean, x$yield_cs_nccpi1_count), 
                                        wyield_cs_nccpi2_mean = weighted.mean(x$yield_cs_nccpi2_mean, x$yield_cs_nccpi2_count),
                                        wyield_cs_nccpi3_mean = weighted.mean(x$yield_cs_nccpi3_mean, x$yield_cs_nccpi3_count),
                                        wyield_cs_nccpi4_mean = weighted.mean(x$yield_cs_nccpi4_mean, x$yield_cs_nccpi4_count), 
                                        wyield_cs_nccpi5_mean = weighted.mean(x$yield_cs_nccpi5_mean, x$yield_cs_nccpi5_count),
                                        wyield_ss_nccpi1_mean = weighted.mean(x$yield_ss_nccpi1_mean, x$yield_ss_nccpi1_count), 
                                        wyield_ss_nccpi2_mean = weighted.mean(x$yield_ss_nccpi2_mean, x$yield_ss_nccpi2_count),
                                        wyield_ss_nccpi3_mean = weighted.mean(x$yield_ss_nccpi3_mean, x$yield_ss_nccpi3_count),
                                        wyield_ss_nccpi4_mean = weighted.mean(x$yield_ss_nccpi4_mean, x$yield_ss_nccpi4_count), 
                                        wyield_ss_nccpi5_mean = weighted.mean(x$yield_ss_nccpi5_mean, x$yield_ss_nccpi5_count)
                 ))
  
  wnccpi$corn_diff = wnccpi$wyield_sc_mean - wnccpi$wyield_cc_mean
  wnccpi$soy_diff = wnccpi$wyield_sc_mean - wnccpi$wyield_cc_mean
  
  wnccpi$corn_diff_nccpi1 = wnccpi$wyield_sc_nccpi1_mean - wnccpi$wyield_cc_nccpi1_mean
  wnccpi$soy_diff_nccpi1 = wnccpi$wyield_cs_nccpi1_mean - wnccpi$wyield_ss_nccpi1_mean
  wnccpi$corn_diff_nccpi2 = wnccpi$wyield_sc_nccpi2_mean - wnccpi$wyield_cc_nccpi2_mean
  wnccpi$soy_diff_nccpi2 = wnccpi$wyield_cs_nccpi2_mean - wnccpi$wyield_ss_nccpi2_mean
  wnccpi$corn_diff_nccpi3 = wnccpi$wyield_sc_nccpi3_mean - wnccpi$wyield_cc_nccpi3_mean
  wnccpi$soy_diff_nccpi3 = wnccpi$wyield_cs_nccpi3_mean - wnccpi$wyield_ss_nccpi3_mean
  wnccpi$corn_diff_nccpi4 = wnccpi$wyield_sc_nccpi4_mean - wnccpi$wyield_cc_nccpi4_mean
  wnccpi$soy_diff_nccpi4 = wnccpi$wyield_cs_nccpi4_mean - wnccpi$wyield_ss_nccpi4_mean
  wnccpi$corn_diff_nccpi5 = wnccpi$wyield_sc_nccpi5_mean - wnccpi$wyield_cc_nccpi5_mean
  wnccpi$soy_diff_nccpi5 = wnccpi$wyield_cs_nccpi5_mean - wnccpi$wyield_ss_nccpi5_mean
  
  wnccpi$corn_pbenefit = ((wnccpi$wyield_sc_mean/wnccpi$wyield_cc_mean) - 1)*100
  wnccpi$soy_pbenefit = ((wnccpi$wyield_cs_mean/wnccpi$wyield_ss_mean) - 1)*100
  
  wnccpi$corn_pbenefit_nccpi1 = ((wnccpi$wyield_sc_nccpi1_mean/wnccpi$wyield_cc_nccpi1_mean) - 1)*100
  wnccpi$soy_pbenefit_nccpi1 = ((wnccpi$wyield_cs_nccpi1_mean/wnccpi$wyield_ss_nccpi1_mean) - 1)*100
  wnccpi$corn_pbenefit_nccpi2 = ((wnccpi$wyield_sc_nccpi2_mean/wnccpi$wyield_cc_nccpi2_mean) - 1)*100
  wnccpi$soy_pbenefit_nccpi2 = ((wnccpi$wyield_cs_nccpi2_mean/wnccpi$wyield_ss_nccpi2_mean) - 1)*100
  wnccpi$corn_pbenefit_nccpi3 = ((wnccpi$wyield_sc_nccpi3_mean/wnccpi$wyield_cc_nccpi3_mean) - 1)*100
  wnccpi$soy_pbenefit_nccpi3 = ((wnccpi$wyield_cs_nccpi3_mean/wnccpi$wyield_ss_nccpi3_mean) - 1)*100
  wnccpi$corn_pbenefit_nccpi4 = ((wnccpi$wyield_sc_nccpi4_mean/wnccpi$wyield_cc_nccpi4_mean) - 1)*100
  wnccpi$soy_pbenefit_nccpi4 = ((wnccpi$wyield_cs_nccpi4_mean/wnccpi$wyield_ss_nccpi4_mean) - 1)*100
  wnccpi$corn_pbenefit_nccpi5 = ((wnccpi$wyield_sc_nccpi5_mean/wnccpi$wyield_cc_nccpi5_mean) - 1)*100
  wnccpi$soy_pbenefit_nccpi5 = ((wnccpi$wyield_cs_nccpi5_mean/wnccpi$wyield_ss_nccpi5_mean) - 1)*100

  return(wnccpi)
}

# If percentile is TRUE, then go by smallest percentile given by value. If FALSE, go by
# number of acres given by value (0.2 acres = 1 pixel).
remove_outliers = function(nccpi, percentile, value) {
  if(percentile) {
    pixels = quantile(nccpi$yield_cc_count, c(value), na.rm=T)
  } else {
    pixels = value/0.2
  }
    nccpi[nccpi$yield_cc_count > pixels &
            nccpi$yield_sc_count > pixels &
            nccpi$yield_cs_count > pixels &
            nccpi$yield_ss_count > pixels &
            nccpi$yield_cc_nccpi1_count > pixels &
            nccpi$yield_cc_nccpi2_count > pixels &
            nccpi$yield_cc_nccpi3_count > pixels &
            nccpi$yield_cc_nccpi4_count > pixels &
            nccpi$yield_cc_nccpi5_count > pixels &
            nccpi$yield_cs_nccpi1_count > pixels &
            nccpi$yield_cs_nccpi2_count > pixels &
            nccpi$yield_cs_nccpi3_count > pixels &
            nccpi$yield_cs_nccpi4_count > pixels &
            nccpi$yield_cs_nccpi5_count > pixels &
            nccpi$yield_sc_nccpi1_count > pixels &
            nccpi$yield_sc_nccpi2_count > pixels &
            nccpi$yield_sc_nccpi3_count > pixels &
            nccpi$yield_sc_nccpi4_count > pixels &
            nccpi$yield_sc_nccpi5_count > pixels &
            nccpi$yield_ss_nccpi1_count > pixels &
            nccpi$yield_ss_nccpi2_count > pixels &
            nccpi$yield_ss_nccpi3_count > pixels &
            nccpi$yield_ss_nccpi4_count > pixels &
            nccpi$yield_ss_nccpi5_count > pixels,]
}