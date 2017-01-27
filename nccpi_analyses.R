
nccpi_boxplot = function(nccpi, pbenefit, area_name) {
  corn_nccpi = rbind(data.frame(FIPS = nccpi[,area_name], year = nccpi$year, rot = nccpi$wyield_sc_nccpi1_mean, pbenefit = nccpi$corn_pbenefit_nccpi1, nccpi = "40-50"),
                 data.frame(FIPS = nccpi[,area_name], year = nccpi$year, rot = nccpi$wyield_sc_nccpi2_mean, pbenefit = nccpi$corn_pbenefit_nccpi2, nccpi = "50-60"), 
                 data.frame(FIPS = nccpi[,area_name], year = nccpi$year, rot = nccpi$wyield_sc_nccpi3_mean, pbenefit = nccpi$corn_pbenefit_nccpi3, nccpi = "60-70"), 
                 data.frame(FIPS = nccpi[,area_name], year = nccpi$year, rot = nccpi$wyield_sc_nccpi4_mean, pbenefit = nccpi$corn_pbenefit_nccpi4, nccpi = "70-80"), 
                 data.frame(FIPS = nccpi[,area_name], year = nccpi$year, rot = nccpi$wyield_sc_nccpi5_mean, pbenefit = nccpi$corn_pbenefit_nccpi5, nccpi = "80-90"))
  soy_nccpi = rbind(data.frame(FIPS = nccpi[,area_name], year = nccpi$year, rot = nccpi$wyield_cs_nccpi1_mean, pbenefit = nccpi$soy_pbenefit_nccpi1, nccpi = "40-50"),
                    data.frame(FIPS = nccpi[,area_name], year = nccpi$year, rot = nccpi$wyield_cs_nccpi2_mean, pbenefit = nccpi$soy_pbenefit_nccpi2, nccpi = "50-60"), 
                    data.frame(FIPS = nccpi[,area_name], year = nccpi$year, rot = nccpi$wyield_cs_nccpi3_mean, pbenefit = nccpi$soy_pbenefit_nccpi3, nccpi = "60-70"), 
                    data.frame(FIPS = nccpi[,area_name], year = nccpi$year, rot = nccpi$wyield_cs_nccpi4_mean, pbenefit = nccpi$soy_pbenefit_nccpi4, nccpi = "70-80"), 
                    data.frame(FIPS = nccpi[,area_name], year = nccpi$year, rot = nccpi$wyield_cs_nccpi5_mean, pbenefit = nccpi$soy_pbenefit_nccpi5, nccpi = "80-90"))
  
  if(pbenefit) {
    par(mfrow=c(2,1))
    boxplot(pbenefit~nccpi, data = corn_nccpi, ylab = "Weighted Percent Rotation Benefit", main = "Corn", xlab = "Soil NCCPI", col=rainbow(10))
    boxplot(pbenefit~nccpi, data = soy_nccpi, ylab = "Weighted Percent Rotation Benefit", main = "Soy", xlab = "Soil NCCPI", col = rainbow(10))
  } else {
    par(mfrow=c(2,1))
    boxplot(rot~nccpi, data = corn_nccpi, ylab = "Weighted Rotation Yield", main = "Corn", xlab = "Soil NCCPI", col=rainbow(10))
    boxplot(rot~nccpi, data = soy_nccpi, ylab = "Weighted Rotation Yield", main = "Soy", xlab = "Soil NCCPI", col = rainbow(10))
  }
}


