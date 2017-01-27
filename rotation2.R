library('ggplot2')
library('plyr')
library('dplyr')
library('data.table')
library('animation') # You also need to brew install ImageMagick if you don't have it
library('Rmisc')
library('reshape')

source("preliminary_rotation_analyses.R") # Does all the preliminary stuff I did super early in the quarter
source("county_comparison.R")             # Maps yields and pixels for counties' rotation differences each year. Comes with gifs!
source("ygap.trendmaps.R")                # Mappy stuff
cnames=paste(ctab$NAME,ctab$STATE_NAME,sep=', ')
source("yield_outliers.R")                # Use this to write csvs of outliers. Not used in this script yet but relevant function is "write_outliers_csv."
source("weather_analyses.R")
source("nccpi_analyses.R")
source("set_up_data.R")

# SET UP ==========***********===========**********====================***********===========**********====================***********===========**********====================***********===========**********==========

wnccpi = set_up(percentile = FALSE, outlier_val = 3000) # Take out counties with fewer than 3000 acres
# wnccpi = set_up(percentile = TRUE, outlier_val = 0.15) # Take out 15% smallest counties
cnccpi = aggregate(.~CRD+year, data = wnccpi, mean)   # Organized by CRD; this is the trick where you
                                                      # just average the FIPS values in each CRD (thx Chris!)

chris_corn = as.data.frame(fread("maizeCRDyear.csv", check.names=TRUE))
chris_corn = chris_corn[chris_corn$CRD < 2000,]
chris_soy = as.data.frame(fread("soyCRDyear.csv", check.names=TRUE))
chris_soy = chris_soy[chris_soy$CRD < 2000,]

# ==========***********===========**********====================***********===========**********====================***********===========**********====================***********===========**********==========
# NCCPI 

nccpi_boxplot(wnccpi, FALSE, "CRD") # Rotation yield
nccpi_boxplot(wnccpi, TRUE, "CRD")  # Percent benefit
nccpi_boxplot(wnccpi, FALSE, "FIPS.formula") # Rotation yield
nccpi_boxplot(wnccpi, TRUE, "FIPS.formula")  # Percent benefit

# ==========***********===========**********====================***********===========**********====================***********===========**********====================***********===========**********==========
# COUNTY COMPARISON (all functions in this section are from county_comparison.R)

# Is there a good way to make the legends consistent with one another?
stationary_comparison(wnccpi, "corn", "", "Corn Rotation \nEffect Yield")
stationary_comparison(wnccpi, "soy", "", "Soy Rotation \nEffect Yield")
stationary_comparison(wnccpi, "corn", "_count", "Corn Rotation \nEffect Pixels")
stationary_comparison(wnccpi, "soy", "_count", "Soy Rotation \nEffect Pixels")

make_gif(wnccpi, "corn", "diff", "Rotation \nEffect Yield", "corn_yield.gif")
make_gif(wnccpi, "corn", "diff_count", "Rotation \nEffect Pixels", "corn_pixels.gif")
make_gif(wnccpi, "corn", "pbenefit", "Rotation \nPercent Benefit", "corn_pbenefit.gif")
make_gif(wnccpi, "soy", "diff", "Rotation \nEffect Yield", "soy_yield.gif")
make_gif(wnccpi, "soy", "diff_count", "Rotation \nEffect Pixels", "soy_pixels.gif")
make_gif(wnccpi, "soy", "pbenefit", "Rotation \nPercent Benefit", "soy_pbenefit.gif")

pbenefit_hist(cnccpi, chris_corn, chris_soy, TRUE) # Percent benefit overlay with Chris's CRD data (FALSE for w/o overlay)

data_comparison_scatterplot(cnccpi, chris_corn, chris_soy)
pbenefit_vs_yield(wnccpi)
nccpi_by_county = avg_pbenefit_by_county(wnccpi, TRUE) # True for by FIPS, false for by CRD
yield_anomaly(df = wnccpi, df_by_county = nccpi_by_county, TRUE) # BROKEN
df_pdiff = get_variability_data(wnccpi, TRUE)
variability_scatter(df_pdiff)
percentile_rotation_difference(df_pdiff)

# ==========***********===========**********====================***********===========**********====================***********===========**********====================***********===========**********==========
# WEATHER 

weather_FIPS = weather_data(wnccpi)

weather_scatter_per_year(weather_FIPS, crop_name = "corn", weather_name = "prcp", val_name = "diff") # This will make 15 scatterplots

weather_scatter(weather_FIPS, "corn")
weather_scatter(weather_FIPS, "soy")

weather_density(weather_FIPS, "corn")
weather_density(weather_FIPS, "soy")

# ==========***********===========**********====================***********===========**********====================***********===========**********====================***********===========**********==========
# TODO: 
#   Fix yield_anomaly
#   Histogram of most common crops 
#         --> Look at CDLs of year before corn years/soy years for each year
#   Line graph of total number of fields from each crop per year

