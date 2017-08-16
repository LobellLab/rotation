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

vals = as.data.frame(fread('vals_list.csv.txt', check.names = TRUE))
names = names(as.data.frame(fread('names_list', check.names = TRUE)))
all_corn = as.data.frame(fread('all_corn.csv', check.names = TRUE))
all_corn = data.frame(all_corn$FIPS.formula, all_corn$year, all_corn$yield_mean, all_corn$yield_count)
names(all_corn) = c("FIPS", "year", "yield", "count")

load("soy_hist.Rda")
load("corn_hist.Rda")
names(soy_df) = c("index", "count", "year", "FIPS")
names(corn_df) = c("index", "count", "year", "FIPS")


# Functions =========================================================================================================

convert_to_names = function(l) {
  namelist = c()
  for(i in l){
    namelist = c(namelist, names[[which(vals == i)[[1]]]])
  }
  return(namelist)
}


top_three_line_graph = function(top_soy_per_year, top_corn_per_year) {
  soy_indices = c(26, 36, 24) # The actual crops: Double crop winter-wheat/soy; alfalfa; winter-wheat
  corn_indices = c(26, 36, 24) # For corn rotation too
  
  top_soy_per_year = top_soy_per_year[top_soy_per_year$index %in% soy_indices,]
  top_corn_per_year = top_corn_per_year[top_corn_per_year$index %in% corn_indices,]
  
  ggplot(data = top_soy_per_year, aes(x = year, y = count/0.2, group = factor(index), color = factor(index))) +
    geom_line() + 
    scale_color_discrete(name="Crop", 
                         breaks=c(26, 36, 24),
                         labels=c("DoubleCrop \nWinWheat/Soy", "Alfalfa", "Winter Wheat")) +
    labs(x = "Year", y = "Acres", title = "Top 3 Crops Rotated\n With Soy (after Soy and Corn)")
  
  ggplot(data = top_corn_per_year, aes(x = year, y = count/0.2, group = factor(index), color = factor(index))) +
    geom_line() + 
    scale_color_discrete(name="Crop", 
                         breaks=c(26, 36, 24),
                         labels=c("DoubleCrop \nWinWheat/Soy", "Alfalfa", "Winter Wheat")) +
    labs(x = "Year", y = "Acres", title = "Top 3 Crops Rotated\n With Corn (after Soy and Corn)")
}


# top_three_barplot = function(top_soy_overall, top_corn_overall) {
#   barplot_indices_soy = c(26, 36, 24, 25, 37, 44)
#   barplot_soy = top_soy_overall[top_soy_overall$index %in% barplot_indices_soy,]
#   barplot_names_soy = c("DblCrop Win\nWheat/Soy", "Alfalfa", "WinWheat", "Other Small\nGrains", "Other Hay\nNon Alfalfa", "Other Crops")
#   barplot_indices_corn = c(26, 36, 24, 25, 44, 28)
#   barplot_corn = top_corn_overall[top_corn_overall$index %in% barplot_indices_corn,]
#   barplot_names_corn = c("DblCrop Win\nWheat/Soy", "Alfalfa", "WinWheat", "Other Small\nGrains", "Other Crops", "Oats")
#   par(mfrow=c(2,1))
#   barplot(barplot_soy$count/0.2, las = 2, names.arg = barplot_names_soy, ylab = "Avg. Acreage", main = "Crops Rotated with Soy", col = terrain.colors(6))
#   barplot(barplot_corn$count/0.2, las = 2, names.arg = barplot_names_corn, ylab = "Avg. Acreage", main = "Crops Rotated with Corn", col = terrain.colors(6))
# }

top_three_barplot = function(top_soy_overall, top_corn_overall) {
  library(plotrix)
  par(bty="n")
  #barplot_indices_soy = c(26, 36, 24, 25, 37, 44)
  #barplot_soy = top_soy_overall[top_soy_overall$index %in% barplot_indices_soy,]
  #barplot_names_soy = c("DblCrop Win\nWheat/Soy", "Alfalfa", "WinWheat", "Other Small\nGrains", "Other Hay\nNon Alfalfa", "Other Crops")
  barplot_indices_corn = c(5, 1, 26, 36, 24, 28)
  barplot_corn = top_corn_overall[top_corn_overall$index %in% barplot_indices_corn,]
  barplot_names_corn = c("Soybeans", "Corn", "Double Crop \nWinter Wheat & Soy", "Alfalfa", "Winter Wheat", "Oats")
  #par(mfrow=c(2,1))
  #barplot(barplot_soy$count/0.2, las = 2, names.arg = barplot_names_soy, ylab = "Avg. Acreage", main = "Crops Rotated with Soy", col = terrain.colors(6))
  
  gap.barplot(barplot_corn$count/0.2,xlim = c(0,7), xaxt='n',xlab="", ytics=c(0, 5000, 10000, 15000, 20000, 795000, 800000, 910000), gap = c(21000, 787000, 800000, 900000), las = 1, ylab = "", main = "Crops Rotated with Corn", col = terrain.colors(8))
  mtext("Average \nAcreage",side=2,las=1,line=0.5)
  axis.break(2,22800,style="slash") 
  staxlab(1, 1:6, barplot_names_corn, top.line= 2)
}

top_three_barplot(top_soy_overall, top_corn_overall)

# ===================================================================================================================

# Check out the biggest contributors of pixel count on average
top_soy_per_year <- soy_df %>%
  group_by(year, index) %>%
  summarise(count = mean(count))

top_corn_per_year <- corn_df %>%
  group_by(year, index) %>%
  summarise(count = mean(count))

top_soy_overall = head(arrange(ddply(soy_df, ~index, summarise, count = mean(count)), desc(count)), 15)
top_corn_overall = head(arrange(ddply(corn_df, ~index, summarise, count = mean(count)), desc(count)), 15)
#top_soy_overall = top_soy_overall[top_soy_overall$index != 5 & top_soy_overall$index != 1,] # Don't need corn or soybeans
#top_corn_overall = top_corn_overall[top_corn_overall$index != 5 & top_corn_overall$index != 1,]

top_soy_indices = top_soy_overall$index
top_corn_indices = top_corn_overall$index
soy_names = convert_to_names(top_soy_indices)
corn_names = convert_to_names(top_corn_indices)

# The following functions pick out indices manually
top_three_line_graph(top_soy_per_year, top_corn_per_year)
top_three_barplot(top_soy_overall, top_corn_overall)


# ========================================================================================================================================
# The following code I ran once, then saved the data.frames in "soy_hist.Rda" and "corn_hist.Rda"
# cos it took so so so long to run
#
# soy <- fromJSON('hist_collection_soy.geojson', simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
# soy_df = data.frame(soy$features$properties)
# corn <- fromJSON('hist_collection_corn.geojson', simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
# corn_df = data.frame(corn$features$properties)
# 
# get_total_means = function(df) {
#   for(i in seq(1:length(df$histogram))) {
#     df$histogram[[i]] = data.frame(index = df$histogram[[i]][,1], count = df$histogram[[i]][,2])
#   }
#   return(df)
# }
# 
# # Takes sooooooooo long....
# consolidate = function(df) {
#   ndf = data.frame()
#   for(i in seq(1:length(df$histogram))){
#     ndf = rbind(ndf, cbind(df$histogram[[i]], df$year[i], df$FIPS.formula[i]))
#   }
#   return(ndf)
# }
# 
# soy_df = get_total_means(soy_df)
# corn_df = get_total_means(corn_df)
# soy_df = consolidate(soy_df)
# names(soy_df) = c("index", "count", "year", "FIPS")
# corn_df = consolidate(corn_df)
# names(corn_df) = c("index", "count", "year", "FIPS")
# 
# save(soy_df, file = "soy_hist.Rda")
# save(corn_df, file = "corn_hist.Rda")
