library('ggplot2')
library('plyr')
library('dplyr')
library('data.table')
library('animation') # You also need to brew install ImageMagick if you don't have it
library('Rmisc')

source("preliminary_rotation_analyses.R") # Does all the preliminary stuff I did super early in the quarter
source("county_comparison.R")             # Maps yields and pixels for counties' rotation differences each year. Comes with gifs!
source("ygap.trendmaps.R")                # Mappy stuff
cnames=paste(ctab$NAME,ctab$STATE_NAME,sep=', ')
source("yield_outliers.R")                # Use this to write csvs of outliers. Not used in this script yet but relevant function is "write_outliers_csv."
source("weather_analyses.R")

# Load data
corn_before_corn = as.data.frame(fread("corn_before_corn.csv", check.names = TRUE))
corn_before_soy = as.data.frame(fread("corn_before_soy.csv", check.names = TRUE))
soy_before_corn = as.data.frame(fread("soy_before_corn.csv", check.names = TRUE))
soy_before_soy = as.data.frame(fread("soy_before_soy.csv", check.names = TRUE))
cc_prcp = as.data.frame(fread("cc_prcp.csv", check.names = TRUE))
cs_prcp = as.data.frame(fread("cs_prcp.csv", check.names = TRUE))
sc_prcp = as.data.frame(fread("sc_prcp.csv", check.names = TRUE))
ss_prcp = as.data.frame(fread("ss_prcp.csv", check.names = TRUE))
cc_tmax = as.data.frame(fread("cc_tmax.csv", check.names = TRUE))
cs_tmax = as.data.frame(fread("cs_tmax.csv", check.names = TRUE))
sc_tmax = as.data.frame(fread("sc_tmax.csv", check.names = TRUE))
ss_tmax = as.data.frame(fread("ss_tmax.csv", check.names = TRUE))
CRD = as.data.frame(fread("FIPS_CRD.csv", check.names = TRUE))

# Cool line graph wow
all_data_line_graph(corn_before_corn, corn_before_soy, soy_before_corn, soy_before_soy)

#==================================================================================================================
# COUNTY COMPARISONS

# Do some stuff to get the rotation difference
corn <- cbind(corn = corn_before_corn, soy = soy_before_corn)                            # All the corn
corn_with_diff <- mutate(corn, diff = corn$soy.mean - corn$corn.mean)                    # Rotation - not rotation
corn_with_diff = corn_with_diff[corn_with_diff$diff < 300 & corn_with_diff$diff > -300,] # Remove outliers
soy <- cbind(corn = corn_before_soy, soy = soy_before_soy)                               # All the soy
soy_with_diff <- mutate(soy, diff = soy$corn.mean - soy$soy.mean)                        # Rotation - not rotation
soy_with_diff = soy_with_diff[soy_with_diff$diff < 100 & soy_with_diff$diff > -100,]     # Remove outliers

# Next, clean up the dataframes so I can actually use them with nice names n stuff:
# Just FIPS and means
corn_means_allyears = ddply(corn_with_diff, c("corn.FIPS.formula"), summarise, mean = mean(diff))
names(corn_means_allyears) = c("FIPS", "mean")
soy_means_allyears = ddply(soy_with_diff, c("soy.FIPS.formula"), summarise, mean = mean(diff))
names(soy_means_allyears) = c("FIPS", "mean")

# Year, FIPS, pixel count, rotation difference mean, CRD
corn_means_eachyear = ddply(corn_with_diff, c("corn.year", "corn.FIPS.formula", "corn.count"), summarise, mean = mean(diff))
names(corn_means_eachyear) = c("year", "FIPS", "count", "mean")
corn_means_eachyear = full_join(corn_means_eachyear, CRD, by = "FIPS")
soy_means_eachyear = ddply(soy_with_diff, c("soy.year", "soy.FIPS.formula", "soy.count"), summarise, mean = mean(diff))
names(soy_means_eachyear) = c("year", "FIPS", "count", "mean")
soy_means_eachyear = full_join(soy_means_eachyear, CRD, by = "FIPS")


# All the years together
all_years_map(corn_means_allyears, soy_means_allyears)

# Stationary maps (comes in 3 figures)
stationary_yield_comparison(corn_means_eachyear, "Corn ") # Yield
stationary_yield_comparison(soy_means_eachyear, "Soy ")
stationary_pixel_comparison(corn_means_eachyear, "Corn ") # Pixel count
stationary_pixel_comparison(soy_means_eachyear, "Soy ")

# GIFs! This makes a gif and saves it to your working directory. Comes in two flavors: yield and pixel
make_yield_gif(corn_means_eachyear, "Corn ", "corn_yield.gif")
make_yield_gif(soy_means_eachyear, "Soy ", "soy_yield.gif")
make_pixel_gif(corn_means_eachyear, "Corn ", "corn_pixel.gif")
make_pixel_gif(soy_means_eachyear, "Soy ", "soy_pixel.gif")


#==================================================================================================================
# WEATHER DATA

# Get the means of the weather data for corn and soy, and put tmax and prcp in the same dataframe
c_weather = weather_data(corn_means_eachyear, cc_tmax, sc_tmax, cc_prcp, sc_prcp)
s_weather = weather_data(soy_means_eachyear, ss_tmax, cs_tmax, ss_prcp, cs_prcp)

# Weather scatter plots with best fit lines.
par(mfrow=c(3,5))
scatter_prcp(c_weather, "Corn ") # Skips 2015! Help?
scatter_tmax(s_weather, "Soy ")


# All of the years together
overall_scatter(c_weather, s_weather)

#====================================================================================================================================================================================================================================================
# OUTLIERS

# Make a gif of the pixel count of outlier counties -- does not work, see yield_outliers.R
# pixel_outliers(corn_before_corn, corn_before_soy, soy_before_corn, soy_before_soy)

# =============================================================
# TODO 
# Outliers (aka huge rotation diffs) cause
# CRD gifs --> CRD doesn't seem to work with map like FIPS does
# Run full regression on all variables (tmax, prcp) and plot partials
