
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
    corn_nccpi$Crop = "Corn"
    soy_nccpi$Crop = "Soybeans"
    both = rbind(corn_nccpi, soy_nccpi)
    
    ggplot(both, aes(x = nccpi, y = pbenefit, fill = Crop)) +
       geom_boxplot(outlier.shape=NA, position=position_dodge(1)) + stat_boxplot(geom ='errorbar',position=position_dodge(1)) + 
      scale_fill_manual(values = c(brewer.pal(3, "Greens"))) +
      scale_y_continuous(limits = c(-10, 20)) + xlab("NCCPI Quintile") + ggtitle("Rotation Benefit by NCCPI") + 
      ylab("Weighted % \n Rotation Benefit") #+ theme(axis.title.y = element_text(
                                            #                                     margin = margin(0,20,0,0)))
    
    #par(mar=c(5,8.5,4,2))
    #par(mfrow=c(1,1))
    #boxplot(pbenefit~nccpi, outline=F,pmax = 0.95, pmin = 0.05, data = corn_nccpi, main = "Corn", xlab = "Soil NCCPI", col=rainbow(10))
    #mtext("Weighted % \n Rotation Benefit",side=2,las=1,line=2.2)
    #boxplot(pbenefit~nccpi,outline=F,data = soy_nccpi, main = "Soy", add = T, xlab = "Soil NCCPI", col = rainbow(10))
    #mtext("Weighted % \n Rotation Benefit",side=2,las=1,line=2.2)
    #par(mar=c(0,0,0,0))
    
  } else {
    par(mfrow=c(2,1))
    boxplot(rot~nccpi, data = corn_nccpi, ylab = "Weighted Rotation Yield", main = "Corn", xlab = "Soil NCCPI", col=rainbow(10))
    boxplot(rot~nccpi, data = soy_nccpi, ylab = "Weighted Rotation Yield", main = "Soy", xlab = "Soil NCCPI", col = rainbow(10))
  }
}


