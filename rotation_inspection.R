library('ggplot2')
library('plyr')
library('dplyr')
library('tidyjson')
library('data.table')
library('animation') # You also need to brew install ImageMagick if you don't have it
library('Rmisc')
library('reshape')
library('jsonlite')
source('county_comparison.R')
source('ygap.trendmaps.R')

# Put this stuff here til you consolidate
source("set_up_data.R")
wnccpi = set_up(percentile = FALSE, outlier_val = cutoff)

# Warning: some of the code below is excessive and bad (sorry chris :( )
# I'll consolidate all of this eventually
weight_me = function(aux){
  a = ddply(aux, .(FIPS, year),
            function(x) data.frame(alfalfa_mean = weighted.mean(x$alfalfa_mean, x$alfalfa_count, na.rm=T),
                                   dbl_mean = weighted.mean(x$dbl_mean, x$dbl_count,na.rm=T),
                                   winwheat_mean = weighted.mean(x$winwheat_mean, x$winwheat_count,na.rm=T),
                                   oats_mean = weighted.mean(x$oats_mean, x$oats_count,na.rm=T),
                                   othcrops_mean = weighted.mean(x$othcrops_mean, x$othcrops_count,na.rm=T),
                                   othsmlgrns_mean = weighted.mean(x$othsmlgrns_mean, x$othsmlgrns_count,na.rm=T),
                                   alfalfa_count = x$alfalfa_count, dbl_count = x$dbl_count, winwheat_count = x$winwheat_count,
                                   oats_count = x$oats_count, othcrops_count = x$othcrops_count, othsmlgrns_count = x$othsmlgrns_count
            )
  )
  return(a)
}

alfalfa = as.data.frame(fread('corn_after_alfalfa.csv', check.names=TRUE))
alfalfa_df = data.frame(alfalfa$FIPS.formula, alfalfa$year, alfalfa$mean, alfalfa$count)
names(alfalfa_df) = c("FIPS", "year", "alfalfa_mean", "alfalfa_count")
dbl = as.data.frame(fread('corn_after_dbl.csv', check.names=TRUE))
dbl_df = data.frame(dbl$FIPS.formula, dbl$year, dbl$mean, dbl$count)
names(dbl_df) = c("FIPS", "year", "dbl_mean", "dbl_count")
winwheat = as.data.frame(fread('corn_after_winwheat.csv', check.names=TRUE))
winwheat_df = data.frame(winwheat$FIPS.formula, winwheat$year, winwheat$mean, winwheat$count)
names(winwheat_df) = c("FIPS", "year", "winwheat_mean", "winwheat_count")
oats = as.data.frame(fread('corn_after_oats.csv', check.names=TRUE))
oats_df = data.frame(oats$FIPS.formula, oats$year, oats$mean, oats$count)
names(oats_df) = c("FIPS", "year", "oats_mean", "oats_count")
othcrops = as.data.frame(fread('corn_after_othcrops.csv', check.names=TRUE))
othcrops_df = data.frame(othcrops$FIPS.formula, othcrops$year, othcrops$mean, othcrops$count)
names(othcrops_df) = c("FIPS", "year", "othcrops_mean", "othcrops_count")
othsmlgrns = as.data.frame(fread('corn_after_othrsmlgrns.csv', check.names=TRUE))
othsmlgrns_df = data.frame(othsmlgrns$FIPS.formula, othsmlgrns$year, othsmlgrns$mean, othsmlgrns$count)
names(othsmlgrns_df) = c("FIPS", "year", "othsmlgrns_mean", "othsmlgrns_count")
aux = Reduce(function(x, y) merge(x, y, all=TRUE), list(alfalfa_df, dbl_df, winwheat_df, oats_df, othcrops_df, othsmlgrns_df))
aux = weight_me(aux)










# Ok it's safe to look now

# Make sure you're using the same subset of counties to compare the auxiliary crop with the rotated corn
# Aux is a data frame of weighted mean yields of corn after rotation with one of six auxiliary crops
# NOTE: While in the other files I used a cutoff of 15000 acres for the wnccpi, here I used the cutoff
# for the auxiliary crops.
find_significant_FIPS = function(aux, short_name_list, cutoff){
  all_crops = data.frame()
  for (name in short_name_list) {
    
    count = paste0(name, "_count")
    mean_name = paste0(name, "_mean")
    FIPS_counts = ddply(aux, .(FIPS, year), summarise, mean = mean(aux[,count], na.rm = T))
    all_years = data.frame()
    
    # For each year, use only matching FIPS
    for (y in 2001:2014) {
      aux_to_use = aux[aux[,count] > cutoff & aux$year == y,] # Enough pixels
      aux_df = data.frame(crop = name, yield = aux_to_use[,mean_name], count = aux_to_use[,count], year = y, FIPS = aux_to_use$FIPS)
      FIPS_to_use = aux_to_use$FIPS # Use those same FIPS to look at corn-corn and soy-corn
      wnccpi_to_use = wnccpi[wnccpi$year == y & wnccpi$FIPS.formula %in% FIPS_to_use,]
      if(nrow(wnccpi_to_use) > 0){
        c2u = data.frame(FIPS = wnccpi_to_use$FIPS.formula, year = wnccpi_to_use$year, yield = wnccpi_to_use$wyield_cc_mean, count = NA, crop = paste0("corn_vs_", name))
        s2u = data.frame(FIPS = wnccpi_to_use$FIPS.formula, year = wnccpi_to_use$year, yield = wnccpi_to_use$wyield_sc_mean, count = NA, crop = paste0("soy_vs_", name))
        all_years = rbind(all_years, aux_df, c2u, s2u)
      }
    }
    all_crops = rbind(all_crops, all_years)
  }
  return(all_crops)
}


cutoff = 200 # @ Chris, for wnccpi this is 3000; I think I'm confused on what the 0.2 conversion does, maybe this is too smol?

corn_after_aux = find_significant_FIPS(aux, c("alfalfa", "dbl", "winwheat", "oats", "othsmlgrns", "othcrops"), cutoff)
average_corn = ddply(corn_after_aux, .(FIPS, crop), summarise, meanyield = mean(yield, na.rm=T), meancount = mean(count, na.rm=T))

# @ Chris, I'd also like to be able to efficiently get the differences between for example alfalfa and corn_vs_alfalfa (aka corn yield after alfalfa
# and corn yield after corn). ddply and aggregate don't seem to do what I want? Do I actually need these or do you think the existing graphs are fine?

boxplots = function(corn_after_aux, crop_list){
  par(mfrow=c(2,3))
  for(name in crop_list){
    boxplot(corn_after_aux$yield[corn_after_aux$crop == name],
            corn_after_aux$yield[corn_after_aux$crop == paste0("corn_vs_", name)],
            corn_after_aux$yield[corn_after_aux$crop == paste0("soy_vs_", name)],
            names = c(name, "Corn", "Soy"), ylab = "Corn Yield After Rotated Crop", main = name,
            xlab = "Rotated Crop",  col = terrain.colors(3))
  }
}
boxplots(corn_after_aux, c("alfalfa", "dbl", "winwheat", "oats", "othsmlgrns", "othcrops"))

par(mfrow=c(1,1))
boxplot(corn_after_aux$yield[corn_after_aux$crop == "alfalfa"], corn_after_aux$yield[corn_after_aux$crop == "dbl"],
        corn_after_aux$yield[corn_after_aux$crop == "winwheat"], corn_after_aux$yield[corn_after_aux$crop == "oats"],
        corn_after_aux$yield[corn_after_aux$crop == "othcrops"], corn_after_aux$yield[corn_after_aux$crop == "othsmlgrns"],
        names = c("Alfalfa", "Dbl Soy/Winwheat", "WinWheat", "Oats", "Other \nCrops", "Other \nSmall Grains"),
        xlab = "Rotated Crop", ylab = "Corn Yield After Rotated Crop", col = terrain.colors(8))

# Problem: there should be equal numbers of FIPS between the first and second two graphs
par(mfrow=c(1,3))
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="alfalfa"], my = average_corn$meanyield[average_corn$crop=="alfalfa"]), titl = paste0("Avg Yield of Corn Rotated after Alfalfa \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="corn_vs_alfalfa"], my = average_corn$meanyield[average_corn$crop=="corn_vs_alfalfa"]), titl = paste0("Avg Yield of Corn Rotated after Corn \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="soy_vs_alfalfa"], my = average_corn$meanyield[average_corn$crop=="soy_vs_alfalfa"]), titl = paste0("Avg Yield of Corn Rotated after Soy \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)

par(mfrow=c(1,3))
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="dbl"], my = average_corn$meanyield[average_corn$crop=="dbl"]), titl = paste0("Avg Yield of Corn Rotated after Dbl \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="corn_vs_dbl"], my = average_corn$meanyield[average_corn$crop=="corn_vs_dbl"]), titl = paste0("Avg Yield of Corn Rotated after Corn \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="soy_vs_dbl"], my = average_corn$meanyield[average_corn$crop=="soy_vs_dbl"]), titl = paste0("Avg Yield of Corn Rotated after Soy \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)


