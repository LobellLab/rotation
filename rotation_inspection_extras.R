stationary_auxiliaries(aux, "count", "alfalfa", "Alfalfa")

# Figure out which counties to use in analysis
alf_counts = ddply(aux, .(FIPS), summarise, mean = mean(alfalfa_count, na.rm = T))
dbl_counts = ddply(aux, .(FIPS), summarise, mean = mean(dbl_count, na.rm = T))
winwheat_counts = ddply(aux, .(FIPS), summarise, mean = mean(winwheat_count, na.rm = T))
oats_counts = ddply(aux, .(FIPS), summarise, mean = mean(oats_count, na.rm = T))
smlgr_counts = ddply(aux, .(FIPS), summarise, mean = mean(othsmlgrns_count, na.rm = T))
othcrops_counts = ddply(aux, .(FIPS), summarise, mean = mean(othcrops_count, na.rm = T))

cutoff = 200#10000*0.2
interFIPS = Reduce(intersect, list(alf_counts$FIPS[alf_counts$mean > cutoff], dbl_counts$FIPS[dbl_counts$mean > cutoff], winwheat_counts$FIPS[winwheat_counts$mean > cutoff], oats_counts$FIPS[oats_counts$mean > cutoff], othcrops_counts$FIPS[othcrops_counts$mean > cutoff], smlgr_counts$FIPS[smlgr_counts$mean > cutoff]))
length(interFIPS)

par(mfrow=c(2,3))
map.var(data.frame(fips = alf_counts$FIPS, my = alf_counts$mean/0.2), titl = paste0("Avg Area of Alfalfa \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = dbl_counts$FIPS, my = dbl_counts$mean/0.2), titl = paste0("Avg Area of Double Win.Wheat/Soy \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = winwheat_counts$FIPS, my = winwheat_counts$mean/0.2), titl = paste0("Avg Area of Win.Wheat \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = oats_counts$FIPS, my = oats_counts$mean/0.2), titl = paste0("Avg Area of Oats \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 39)
map.var(data.frame(fips = smlgr_counts$FIPS, my = smlgr_counts$mean/0.2), titl = paste0("Avg Area of Other Small Grains \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = othcrops_counts$FIPS, my = othcrops_counts$mean/0.2), titl = paste0("Avg Area of Other Crops \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)


boxplot_yields = function(year){
  boxplot(aux$yield_after_alfalfa_mean[aux$year==year & aux$yield_after_alfalfa_count > cutoff], 
          aux$yield_after_dbl_mean[aux$year==year & aux$yield_after_dbl_count > cutoff],
          aux$yield_after_winwheat_mean[aux$year==year & aux$yield_after_winwheat_count > cutoff],
          aux$yield_after_oats_mean[aux$year==year & aux$yield_after_oats_count > cutoff],
          aux$yield_after_othrsmlgrns_mean[aux$year==year & aux$yield_after_othrsmlgrns_count > cutoff],
          aux$yield_after_othcrops_mean[aux$year==year & aux$yield_after_othcrops_count > cutoff],
          names = c("Alfalfa", "Double Crop\n Win.Wheat/Soy", "Win.Wheat", 
                    "Oats", "Other Small \nGrains", "Other Crops"), 
          xlab = "Previous Crop", ylab = "Corn Yield", main = paste0("Corn Yield After Rotation\n With Previous Crop, ", year))
}

# Also do scatter plot for years and line graph across years

par(mfrow=c(2,3))
for(year in seq(2002:2008)){
  boxplot_yields(year)
}



# factor_name is either count or yield
get_main_crop_gif = function(aux, factor_name, short_name, crop_name, filename){
  aux = aux[aux$count > 0,]
  fn = paste0("yield_after_", short_name, "_", factor_name)
  par(mfrow=c(1,1))
  saveGIF({
    for (year in unique(na.omit(aux$year))) {
      map.var(data.frame(fips = aux$FIPS.formula[aux$year==year], my = aux[,fn][aux$year==year]/0.2), titl = paste0("Corn Area after ", crop_name,"\nin Acres ", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
    }
  }, interval = 2, ani.width=600, ani.height=800, loop=TRUE, movie.name = filename, outdir = getwd())
}

stationary_auxiliaries = function(aux, factor_name, short_name, crop_name) {
  fn = paste0(short_name, "_", factor_name)
  par(mfrow=c(2,3))
  for (year in unique(na.omit(aux$year))) {
    map.var(data.frame(fips = aux$FIPS[aux$year==year], my = aux[,fn][aux$year==year]/0.2), titl = paste0("Corn Area after ", crop_name,"\nin Acres ", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
  }
}



cluster_bar = function(df, name, graph_name){
  df = df[df$crop==name | df$crop==paste0("corn_vs_",name) | df$crop==paste0("soy_vs_",name),]
  
  # se_crop = c(round((sqrt(var(df$yield[df$crop==name])))/sqrt(length(df$yield[df$crop==name]))*1.96,3), 
  #                    round((sqrt(var(df$yield[df$crop==paste0("corn_vs_",name)])))/sqrt(df$yield[df$crop==paste0("corn_vs_",name)])*1.96,3),
  #                    round((sqrt(var(df$yield[df$crop==paste0("soy_vs_",name)])))/sqrt(df$yield[df$crop==paste0("soy_vs_",name)])*1.96,3))
  # 
  # limits = aes(ymax = mean(df$yield, na.rm=T) + se_crop, )
  # geom_errorbar(aes(ymin=MNT-se, ymax=MNT+se), size=.3, width=.2, position=position_dodge(.9))
  
  ggplot(df, aes(x = as.factor(year), y = yield, fill = as.factor(crop))) +
    geom_bar(position=position_dodge(), stat="identity") +
    xlab("Year") + ylab("Yield of Corn after Rotation")  +
    scale_fill_brewer(palette="Spectral", name = "Previously Rotated Crop", labels = c(graph_name, "Corn", "Soy"))
  
}

cluster_bar(corn_after_aux, "alfalfa", "Alfalfa")
cluster_bar(corn_after_aux, "dbl", "Dbl Soy/Winwheat")
cluster_bar(corn_after_aux, "winwheat", "WinWheat")
cluster_bar(corn_after_aux, "oats", "Oats")
cluster_bar(corn_after_aux, "othcrops", "Other Crops")
cluster_bar(corn_after_aux, "othsmlgrns", "Other Small Grains")