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

source("minor_crops_setup.R")
source("minor_crops_soy..R")

# Put this stuff here til you consolidate
source("set_up_data.R")
wnccpi = set_up(percentile = FALSE, outlier_val = 1000)
aux = minor_setup()
soy_aux = minor_setup_soy()

# Make sure you're using the same subset of counties to compare the auxiliary crop with the rotated corn
# Aux is a data frame of weighted mean yields of corn after rotation with one of six auxiliary crops

find_significant_FIPS = function(aux, short_name_list, cutoff){
  all_crops = data.frame()
  for (name in short_name_list) {
    
    count = paste0(name, "_count")
    mean_name = paste0(name, "_mean")
    FIPS_counts = ddply(aux, .(FIPS, year), summarise, mean = mean(aux[,count], na.rm = T))
    all_years = data.frame()
    
    # For each year, use only matching FIPS

    for (y in c(unique(aux$year))) {
      aux_to_use = aux[aux[,count] > cutoff & aux$year == y,] # Enough pixels
      aux_df = data.frame(crop = name, yield = aux_to_use[,mean_name], count = aux_to_use[,count], year = y, FIPS = aux_to_use$FIPS)
      FIPS_to_use = aux_to_use$FIPS # Use those same FIPS to look at corn-corn and soy-corn
      wnccpi_to_use = wnccpi[wnccpi$year == y & wnccpi$FIPS.formula %in% FIPS_to_use,]
      if(nrow(wnccpi_to_use) > 0){

        if (grepl("nccpi", mean_name)) {
          m = substr(toString(name), nchar(name) - 5, nchar(name))
          cname = paste0("wyield_cc_", m,"_mean")
          sname = paste0("wyield_sc_", m,"_mean")
        } else {
          cname = "wyield_cc_mean"
          sname = "wyield_sc_mean"
        }
        c2u = data.frame(FIPS = wnccpi_to_use$FIPS.formula, year = wnccpi_to_use$year, yield = wnccpi_to_use[,cname], count = NA, crop = paste0("corn_vs_", name))
        s2u = data.frame(FIPS = wnccpi_to_use$FIPS.formula, year = wnccpi_to_use$year, yield = wnccpi_to_use[,sname], count = NA, crop = paste0("soy_vs_", name))

        all_years = rbind(all_years, aux_df, c2u, s2u)
      }
    }
    all_crops = rbind(all_crops, all_years)
  }
  return(all_crops)
}


# alfalfa - corn after alfalfa
# corn_vs_alfalfa - corn after corn (same fips as alfalfa)
# soy_vs_alfalfa - corn after soy (same fips as alfalfa)
cutoff = 1000/0.2 

corn_after_aux = find_significant_FIPS(aux, c("alfalfa", "dbl", "winwheat", "oats", "othsmlgrns", "othcrops",
                                              "alfalfa_nccpi1", "alfalfa_nccpi2", "alfalfa_nccpi3", "alfalfa_nccpi4", "alfalfa_nccpi5",
                                              "dbl_nccpi1", "dbl_nccpi2", "dbl_nccpi3", "dbl_nccpi4", "dbl_nccpi5",
                                              "winwheat_nccpi1", "winwheat_nccpi2", "winwheat_nccpi3", "winwheat_nccpi4", "winwheat_nccpi5",
                                              "oats_nccpi1", "oats_nccpi2", "oats_nccpi3", "oats_nccpi4", "oats_nccpi5"),cutoff)
                                              #"othcrops_nccpi1", "othcrops_nccpi2", "othcrops_nccpi3", "othcrops_nccpi4", "othcrops_nccpi5",
                                              #"othsmlgrns_nccpi1", "othsmlgrns_nccpi2", "othsmlgrns_nccpi3", "othsmlgrns_nccpi4", "othsmlgrns_nccpi5"), cutoff)
average_corn = ddply(corn_after_aux, .(FIPS, crop), summarise, meanyield = mean(yield, na.rm=T), meancount = mean(count, na.rm=T))

soy_after_aux = find_significant_FIPS(soy_aux, c("alfalfa", "dbl", "winwheat", "oats",
                                                 "alfalfa_nccpi1", "alfalfa_nccpi2", "alfalfa_nccpi3", "alfalfa_nccpi4", "alfalfa_nccpi5",
                                                 "dbl_nccpi1", "dbl_nccpi2", "dbl_nccpi3", "dbl_nccpi4", "dbl_nccpi5",
                                                 "winwheat_nccpi1", "winwheat_nccpi3", "winwheat_nccpi4", "winwheat_nccpi5",
                                                 "oats_nccpi1", "oats_nccpi2", "oats_nccpi3", "oats_nccpi4", "oats_nccpi5"),500/0.2)


# @ Chris, I'd also like to be able to efficiently get the differences between for example alfalfa and corn_vs_alfalfa (aka corn yield after alfalfa
# and corn yield after corn). ddply and aggregate don't seem to do what I want? Do I actually need these or do you think the existing graphs are fine?

boxplots = function(corn_after_aux, crop_list, graph_list){
  par(mfrow=c(2,2))
  i = 1
  for(name in crop_list){
    gn = graph_list[i]
    boxplot(corn_after_aux$yield[corn_after_aux$crop == name],
            corn_after_aux$yield[corn_after_aux$crop == paste0("corn_vs_", name)],
            corn_after_aux$yield[corn_after_aux$crop == paste0("soy_vs_", name)],
            names = c(gn, "Corn", "Soy"), ylab = "Corn Yield After Rotated Crop", main = gn,
            xlab = "Rotated Crop",  col = terrain.colors(3))
    i = i + 1
  }
}
boxplots(corn_after_aux, c("alfalfa", "dbl", "winwheat", "oats"), c("Alfalfa", "Double Cropped Winter \nWheat & Soy",
                                                                    "Winter Wheat", "Oats"))


par(mfrow=c(1,1))
boxplot(corn_after_aux$yield[corn_after_aux$crop == "alfalfa"], corn_after_aux$yield[corn_after_aux$crop == "dbl"],
        corn_after_aux$yield[corn_after_aux$crop == "winwheat"], corn_after_aux$yield[corn_after_aux$crop == "oats"],
        corn_after_aux$yield[corn_after_aux$crop == "othcrops"], corn_after_aux$yield[corn_after_aux$crop == "othsmlgrns"],
        names = c("Alfalfa", "Dbl Soy/Winwheat", "WinWheat", "Oats", "Other \nCrops", "Other \nSmall Grains"),
        xlab = "Rotated Crop", ylab = "Corn Yield After Rotated Crop", col = terrain.colors(8))


par(mfrow=c(1,3))
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="alfalfa"], my = average_corn$meanyield[average_corn$crop=="alfalfa"]), titl = paste0("Avg Yield of Corn Rotated after Alfalfa \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="corn_vs_alfalfa"], my = average_corn$meanyield[average_corn$crop=="corn_vs_alfalfa"]), titl = paste0("Avg Yield of Corn Rotated after Corn \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="soy_vs_alfalfa"], my = average_corn$meanyield[average_corn$crop=="soy_vs_alfalfa"]), titl = paste0("Avg Yield of Corn Rotated after Soy \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)

par(mfrow=c(1,3))
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="dbl"], my = average_corn$meanyield[average_corn$crop=="dbl"]), titl = paste0("Avg Yield of Corn Rotated after Dbl \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="corn_vs_dbl"], my = average_corn$meanyield[average_corn$crop=="corn_vs_dbl"]), titl = paste0("Avg Yield of Corn Rotated after Corn \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)
map.var(data.frame(fips = average_corn$FIPS[average_corn$crop=="soy_vs_dbl"], my = average_corn$meanyield[average_corn$crop=="soy_vs_dbl"]), titl = paste0("Avg Yield of Corn Rotated after Soy \nin Acres Across Years"), legend=TRUE, leg_size = 0.7, xleg =-97, yleg = 40)


# For each nccpi and each crop
# Alfalfa: nccpi1 doesnt exist
minor_boxplot = function(corn_after_aux, crop_name, graph_name) {
  par(mfrow=c(1,1))
  boxplot(corn_after_aux$yield[corn_after_aux$crop == paste0(crop_name, "_nccpi1")],
          corn_after_aux$yield[corn_after_aux$crop == paste0("corn_vs_", crop_name,"_nccpi1")],
          corn_after_aux$yield[corn_after_aux$crop == paste0("soy_vs_", crop_name, "_nccpi1")],
          corn_after_aux$yield[corn_after_aux$crop == paste0(crop_name, "_nccpi2")],
          corn_after_aux$yield[corn_after_aux$crop == paste0("corn_vs_", crop_name,"_nccpi2")],
          corn_after_aux$yield[corn_after_aux$crop == paste0("soy_vs_", crop_name, "_nccpi2")],
          corn_after_aux$yield[corn_after_aux$crop == paste0(crop_name,"_nccpi3")],
          corn_after_aux$yield[corn_after_aux$crop == paste0("corn_vs_", crop_name, "_nccpi3")],
          corn_after_aux$yield[corn_after_aux$crop == paste0("soy_vs_", crop_name, "_nccpi3")],
          corn_after_aux$yield[corn_after_aux$crop == paste0(crop_name, "_nccpi4")],
          corn_after_aux$yield[corn_after_aux$crop == paste0("corn_vs_", crop_name, "_nccpi4")],
          corn_after_aux$yield[corn_after_aux$crop == paste0("soy_vs_", crop_name, "_nccpi4")],
          corn_after_aux$yield[corn_after_aux$crop == paste0(crop_name, "_nccpi5")],
          corn_after_aux$yield[corn_after_aux$crop == paste0("corn_vs_", crop_name, "_nccpi5")],
          corn_after_aux$yield[corn_after_aux$crop == paste0("soy_vs_", crop_name, "_nccpi5")],
          names = c(paste0(graph_name, "\n NCCPI 1"), "Corn\n NCCPI 1", "Soy\n NCCPI 1",
                    paste0(graph_name, "\n NCCPI 2"), "Corn\n NCCPI 2", "Soy\n NCCPI 2",
                    paste0(graph_name, "\n NCCPI 3"), "Corn\n NCCPI 3", "Soy\n NCCPI 3",
                    paste0(graph_name, "\n NCCPI 4"), "Corn\n NCCPI 4", "Soy\n NCCPI 4", 
                    paste0(graph_name, "\n NCCPI 5"), "Corn\n NCCPI 5", "Soy\n NCCPI 5"),
          xlab = "Rotated Crop", ylab = "Corn Yield After Rotated Crop", col = terrain.colors(3))
}

minor_boxplot(corn_after_aux, "alfalfa", "Alfalfa")
#   with corn for comparison? (should probz get differences too)

nccpi_hist = function(corn_after_aux, crop_name, graph_name, nccpi_num, breaknum, xlim0, xlim1, ylim0, ylim1, legx, legy) {
  hist(corn_after_aux$yield[corn_after_aux$crop == paste0(crop_name, "_nccpi", nccpi_num)], xlim = c(xlim0, xlim1), ylim = c(ylim0, ylim1), xlab = "Yield of Corn", main = paste0("Yields of Corn After ", graph_name, ", Corn or Soy \n for NCCPI ", nccpi_num), col=rgb(1,1,0,0.7), breaks = breaknum)
  hist(corn_after_aux$yield[corn_after_aux$crop == paste0("corn_vs_", crop_name, "_nccpi", nccpi_num)], col=rgb(0,1,1,0.4), add = T, breaks = breaknum)
  hist(corn_after_aux$yield[corn_after_aux$crop == paste0("soy_vs_", crop_name, "_nccpi", nccpi_num)], col=rgb(1,0.5,0,0.4), add = T, breaks = breaknum)
  legend(c(graph_name, "Corn", "Soy"), y.intersp = 1.15, x = legx, y = legy, bty = "n", fill = c(rgb(1,1,0,0.7), rgb(0,1,1,0.4), rgb(1,0.5,0,0.4)))
}

par(mfrow=c(2,2))
nccpi_hist(corn_after_aux, 'alfalfa', 'Alfalfa', 2, 10, 900, 2000, 0, 3, 1600, 2.5)
nccpi_hist(corn_after_aux, 'alfalfa', 'Alfalfa', 3, 10, 900, 2000, 0, 9, 1000, 6)
nccpi_hist(corn_after_aux, 'alfalfa', 'Alfalfa', 4, 10, 900, 2000, 0, 20, 1000, 15)
nccpi_hist(corn_after_aux, 'alfalfa', 'Alfalfa', 5, 10, 900, 2000, 0, 17, 1000, 13)

par(mfrow=c(2,3))
nccpi_hist(corn_after_aux, 'dbl', 'Double Winter Wheat-Soy', 1, 15, 0, 2100, 0, 20, 200, 15)
nccpi_hist(corn_after_aux, 'dbl', 'Double Winter Wheat-Soy', 2, 15, 0, 2100, 0, 60, 200, 50)
nccpi_hist(corn_after_aux, 'dbl', 'Double Winter Wheat-Soy', 3, 15, 0, 2100, 0, 60, 200, 50)
nccpi_hist(corn_after_aux, 'dbl', 'Double Winter Wheat-Soy', 4, 15, 0, 2100, 0, 60, 200, 50)
nccpi_hist(corn_after_aux, 'dbl', 'Double Winter Wheat-Soy', 5, 15, 0, 2100, 0, 25, 200, 20)

nccpi_hist(corn_after_aux, 'winwheat', 'Winter Wheat', 1, 15, 0, 2100, 0, 5, 200, 3)
nccpi_hist(corn_after_aux, 'winwheat', 'Winter Wheat', 2, 15, 0, 2100, 0, 30, 200, 20)
nccpi_hist(corn_after_aux, 'winwheat', 'Winter Wheat', 3, 15, 0, 2100, 0, 20, 200, 15)
nccpi_hist(corn_after_aux, 'winwheat', 'Winter Wheat', 4, 15, 0, 2100, 0, 10, 200, 5)
nccpi_hist(corn_after_aux, 'winwheat', 'Winter Wheat', 5, 15, 0, 2500, 0, 15, 200, 10)

par(mfrow=c(1,3))
nccpi_hist(corn_after_aux, 'oats', 'Oats', 3, 5, 1000, 2100, 0, 5, 1200, 4)
nccpi_hist(corn_after_aux, 'oats', 'Oats', 4, 5, 1000, 2100, 0, 5, 1000, 4)
nccpi_hist(corn_after_aux, 'oats', 'Oats', 5, 5, 1000, 2100, 0, 5, 1200, 4)



# SIG: Alf 2 corn & soy, alf 5 corn & soy (p < 0.05)
t.test(corn_after_aux$yield[corn_after_aux$crop=='alfalfa_nccpi5'],corn_after_aux$yield[corn_after_aux$crop=='corn_vs_alfalfa_nccpi5'])
t.test(corn_after_aux$yield[corn_after_aux$crop=='alfalfa_nccpi5'],corn_after_aux$yield[corn_after_aux$crop=='soy_vs_alfalfa_nccpi5'])

# SIG: none
t.test(corn_after_aux$yield[corn_after_aux$crop=='dbl_nccpi5'],corn_after_aux$yield[corn_after_aux$crop=='corn_vs_dbl_nccpi5'])
t.test(corn_after_aux$yield[corn_after_aux$crop=='dbl_nccpi5'],corn_after_aux$yield[corn_after_aux$crop=='soy_vs_dbl_nccpi5'])

# SIG: none
t.test(corn_after_aux$yield[corn_after_aux$crop=='winwheat_nccpi5'],corn_after_aux$yield[corn_after_aux$crop=='corn_vs_winwheat_nccpi5'])
t.test(corn_after_aux$yield[corn_after_aux$crop=='winwheat_nccpi5'],corn_after_aux$yield[corn_after_aux$crop=='soy_vs_winwheat_nccpi5'])

# SIG: both 4, both 5 (p<0.05)
t.test(corn_after_aux$yield[corn_after_aux$crop=='oats_nccpi5'],corn_after_aux$yield[corn_after_aux$crop=='corn_vs_oats_nccpi5'])
t.test(corn_after_aux$yield[corn_after_aux$crop=='oats_nccpi5'],corn_after_aux$yield[corn_after_aux$crop=='soy_vs_oats_nccpi5'])

alf_line <- c(NA, mean(corn_after_aux$yield[corn_after_aux$crop=='alfalfa_nccpi2'],na.rm=T), 
              mean(corn_after_aux$yield[corn_after_aux$crop=='alfalfa_nccpi3'],na.rm=T),
              mean(corn_after_aux$yield[corn_after_aux$crop=='alfalfa_nccpi4'],na.rm=T),
              mean(corn_after_aux$yield[corn_after_aux$crop=='alfalfa_nccpi5'],na.rm=T))
dbl_line <- c(mean(corn_after_aux$yield[corn_after_aux$crop=='dbl_nccpi1'],na.rm=T),
              mean(corn_after_aux$yield[corn_after_aux$crop=='dbl_nccpi2'],na.rm=T), 
              mean(corn_after_aux$yield[corn_after_aux$crop=='dbl_nccpi3'],na.rm=T),
              mean(corn_after_aux$yield[corn_after_aux$crop=='dbl_nccpi4'],na.rm=T),
              mean(corn_after_aux$yield[corn_after_aux$crop=='dbl_nccpi5'],na.rm=T))
winwheat_line <- c(mean(corn_after_aux$yield[corn_after_aux$crop=='winwheat_nccpi1'],na.rm=T),
              mean(corn_after_aux$yield[corn_after_aux$crop=='winwheat_nccpi2'],na.rm=T), 
              mean(corn_after_aux$yield[corn_after_aux$crop=='winwheat_nccpi3'],na.rm=T),
              mean(corn_after_aux$yield[corn_after_aux$crop=='winwheat_nccpi4'],na.rm=T),
              mean(corn_after_aux$yield[corn_after_aux$crop=='winwheat_nccpi5'],na.rm=T))
oats_line <- c(NA,
              NA, 
              mean(corn_after_aux$yield[corn_after_aux$crop=='oats_nccpi3'],na.rm=T),
              mean(corn_after_aux$yield[corn_after_aux$crop=='oats_nccpi4'],na.rm=T),
              mean(corn_after_aux$yield[corn_after_aux$crop=='oats_nccpi5'],na.rm=T))
line_db = data.frame(mean = alf_line, id = "Alfalfa", sig = c(NA, T, F, F, T), nccpi = c(1,2,3,4,5))
line_db = rbind(line_db, data.frame(mean = dbl_line, id = "Double WinWheat - Soy", sig = c(F, F, F, F, F), nccpi = c(1,2,3,4,5)),
                data.frame(mean = winwheat_line, id = "Winter Wheat", sig = c(F, F, F, F, F), nccpi = c(1,2,3,4,5)),
                data.frame(mean = oats_line, id = "Oats", sig = c(F, F, F, T, T), nccpi = c(1,2,3,4,5)))

ggplot(data = line_db, aes(x =nccpi, y = mean, group = factor(id), color = factor(id))) +
  geom_line() + 
  scale_color_discrete(name="Crop",
                       labels=c("Alfalfa", "Double WinWheat - Soy", "Winter Wheat", "Oats")) +
  labs(x = "NCCPI Quintile", y = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nAverage Yield", title = "Average Yield by NCCPI for Corn\n Rotated after Minor Crops") +
  geom_point(data = line_db, mapping = aes(shape=factor(sig)),size = 3) +
  theme(axis.title.y = element_text(angle=0)) +
  scale_shape_manual(values=c(1, 19), labels=c("Not Significant", "Significant", "No Data"), name = "Significantly different\n from non-rotated \n and rotated corn yields")


# ===============
# % KS test for difference between CC, SC and oats-corn, alfalfa-corn, was all under D = 0.5
nccpi_hist = function(corn_after_aux, crop_name, graph_name, nccpi_num, breaknum, xlim0, xlim1, ylim0, ylim1, legx, legy) {
  hist(corn_after_aux$yield[corn_after_aux$crop == paste0(crop_name, "_nccpi", nccpi_num)], xlim = c(xlim0, xlim1), ylim = c(ylim0, ylim1), xlab = "Yield of Corn", main = paste0("Yields of Corn After ", graph_name, ", Corn or Soy \n for NCCPI ", nccpi_num), col=rgb(1,1,0,0.7), breaks = breaknum)
  hist(corn_after_aux$yield[corn_after_aux$crop == paste0("corn_vs_", crop_name, "_nccpi", nccpi_num)], col=rgb(0,1,1,0.4), add = T, breaks = breaknum)
  hist(corn_after_aux$yield[corn_after_aux$crop == paste0("soy_vs_", crop_name, "_nccpi", nccpi_num)], col=rgb(1,0.5,0,0.4), add = T, breaks = breaknum)
  legend(c(graph_name, "Corn", "Soy"), y.intersp = 1.15, x = legx, y = legy, bty = "n", fill = c(rgb(1,1,0,0.7), rgb(0,1,1,0.4), rgb(1,0.5,0,0.4)))
}

par(mfrow=c(2,2))
nccpi_hist(corn_after_aux, 'alfalfa', 'Alfalfa', 2, 10, 900, 2000, 0, 3, 1600, 2.5)
nccpi_hist(corn_after_aux, 'alfalfa', 'Alfalfa', 3, 10, 900, 2000, 0, 9, 1000, 6)
nccpi_hist(corn_after_aux, 'alfalfa', 'Alfalfa', 4, 10, 900, 2000, 0, 20, 1000, 15)
nccpi_hist(corn_after_aux, 'alfalfa', 'Alfalfa', 5, 10, 900, 2000, 0, 17, 1000, 13)

par(mfrow=c(2,3))
nccpi_hist(corn_after_aux, 'dbl', 'Double Winter Wheat-Soy', 1, 15, 0, 2100, 0, 20, 200, 15)
nccpi_hist(corn_after_aux, 'dbl', 'Double Winter Wheat-Soy', 2, 15, 0, 2100, 0, 60, 200, 50)
nccpi_hist(corn_after_aux, 'dbl', 'Double Winter Wheat-Soy', 3, 15, 0, 2100, 0, 60, 200, 50)
nccpi_hist(corn_after_aux, 'dbl', 'Double Winter Wheat-Soy', 4, 15, 0, 2100, 0, 60, 200, 50)
nccpi_hist(corn_after_aux, 'dbl', 'Double Winter Wheat-Soy', 5, 15, 0, 2100, 0, 25, 200, 20)

nccpi_hist(corn_after_aux, 'winwheat', 'Winter Wheat', 1, 15, 0, 2100, 0, 5, 200, 3)
nccpi_hist(corn_after_aux, 'winwheat', 'Winter Wheat', 2, 15, 0, 2100, 0, 30, 200, 20)
nccpi_hist(corn_after_aux, 'winwheat', 'Winter Wheat', 3, 15, 0, 2100, 0, 20, 200, 15)
nccpi_hist(corn_after_aux, 'winwheat', 'Winter Wheat', 4, 15, 0, 2100, 0, 10, 200, 5)
nccpi_hist(corn_after_aux, 'winwheat', 'Winter Wheat', 5, 15, 0, 2500, 0, 15, 200, 10)

par(mfrow=c(1,3))
nccpi_hist(corn_after_aux, 'oats', 'Oats', 3, 5, 1000, 2100, 0, 5, 1200, 4)
nccpi_hist(corn_after_aux, 'oats', 'Oats', 4, 5, 1000, 2100, 0, 5, 1000, 4)
nccpi_hist(corn_after_aux, 'oats', 'Oats', 5, 5, 1000, 2100, 0, 5, 1200, 4)

