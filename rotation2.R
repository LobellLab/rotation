library('ggplot2')
library('plyr')
library('dplyr')
library('data.table')
library('animation') # You also need to brew install ImageMagick if you don't have it
library('Rmisc')
library('reshape')
require(rms)

source("preliminary_rotation_analyses.R") # Does all the preliminary stuff I did super early in the quarter
source("county_comparison.R")             # Maps yields and pixels for counties' rotation differences each year. Comes with gifs!
source("ygap.trendmaps.R")                # Mappy stuff
cnames=paste(ctab$NAME,ctab$STATE_NAME,sep=', ')
source("yield_outliers.R")                # Use this to write csvs of outliers. Not used in this script yet but relevant function is "write_outliers_csv."
source("weather_analyses.R")
source("nccpi_analyses.R")
source("set_up_data.R")
source("regression.R")

# TODO
# Add regressions
# Do difference graphs for minor crops
# Regressions for minor crops

# SET UP ==========***********===========**********====================***********===========**********====================***********===========**********====================***********===========**********==========

wnccpi = set_up(percentile = FALSE, outlier_val = 1000) # Take out counties with fewer than 3000 acres
# wnccpi = set_up(percentile = TRUE, outlier_val = 0.15) # Take out 15% smallest counties
cnccpi = aggregate(.~CRD+year, data = wnccpi, mean, na.action = na.pass)   # Organized by CRD; this is the trick where you
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
yield_anomaly(df = wnccpi, df_by_county = nccpi_by_county, TRUE)
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
# Regressions

regression_barplot = function(cnccpi) {

  regr = regression_df(cnccpi)
  cregr = regr[substr(regr$name, 1, 15) == "soy_before_corn" | substr(regr$name, 1, 16) == "corn_before_corn",]
  sregr = regr[substr(regr$name, 1, 14) == "soy_before_soy" | substr(regr$name, 1, 15) == "corn_before_soy",]
  
  # Left panel
  cp <- cregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(outcome_yield))
  raw_cp = ((as.numeric(cp[1,2]/cp[2,2]))-1)*100
  
  sp <- sregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(outcome_yield))
  raw_sp = ((as.numeric(sp[2,2]/sp[1,2]))-1)*100
  
  # Middle panel - environmental (ignore rotation)
  clm = lm(formula = outcome_yield ~ vpd + prcp + tmax + tmin + nccpi, data = cregr)
  cregr$predictions = predict.lm(clm, cregr)
  
  slm = lm(formula = outcome_yield ~ vpd + prcp + tmax + tmin + nccpi, data = sregr)
  sregr$predictions = predict.lm(slm, sregr)
  
  cpbenefit <- cregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(predictions))
  
  spbenefit <- sregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(predictions))
  
  predicted_cp = ((as.numeric(cpbenefit[1,2]/cpbenefit[2,2]))-1)*100
  predicted_sp = ((as.numeric(spbenefit[2,2]/spbenefit[1,2]))-1)*100
  
  # Right panel is summary(clm) estimate for yearagocrop/ predicted mean C (using yearagocrop in fmla)
  # ^ except should be rotated, not CC
  cregr$stateyear = paste0(substr(cregr$CRD, 1, 2), cregr$year)
  clm = lm(formula = outcome_yield ~ yearagocrop + vpd + prcp + tmax + tmin + nccpi, data = cregr, x=T, y=T)
  slm = lm(formula = outcome_yield ~ yearagocrop + vpd + prcp + tmax + tmin + nccpi, data = sregr, x=T, y=T)
  # 
  # ctest <- bootcov(clm, cregr$stateyear, B=1000)
  # stest <- bootcov(slm, sregr$stateyear, B=1000)
  # ll <- capture.output(print(test))  
  # print(david.lm)
  # effect <- unlist(strsplit(ll, split=" +"))[min(which(unlist(strsplit(ll, split=" +"))=="yearagocrop=S"))+1]
  # sterror<- unlist(strsplit(ll, split=" +"))[min(which(unlist(strsplit(ll, split=" +"))=="yearagocrop=S"))+2]
  # trdf$diff[8+i] <- ((as.numeric(effect)*multiplier)/mean(yieldsub$OUTCOME_YLD))
  # trdf$stdev[8+i] <- (as.numeric(sterror)/mean(yieldsub$OUTCOME_YLD))
  
  cregr$predictions = predict.lm(clm, cregr)
  sregr$predictions = predict.lm(slm, sregr)
  cpbenefit2 <- cregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(predictions))
  spbenefit2 <- sregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(predictions))

  effect_cp = (as.numeric(78.566335/cpbenefit2[1,2]))*100
  effect_sp = (as.numeric(29.32575/spbenefit2[2,2]))*100

  # CLM yearagocropS = 78.566335
  # SLM yearagocropC = 29.32575
  
  # Do error bars
  
  par(mfrow=c(1,2))
  barplot(c(raw_sp, predicted_sp, effect_sp), ylim = c(0,10), ylab = "Percent Rotation Benefit", names=c("Raw", "Environmental\nPrediction", "Effect Size"), col = terrain.colors(3), main = "Soy")
  barplot(c(raw_cp, predicted_cp, effect_cp), ylim = c(0,10), ylab = "Percent Rotation Benefit", names=c("Raw", "Environmental\nPrediction", "Effect Size"), col = terrain.colors(3), main = "Corn")

}

#======
map.var(data.frame(fips = cnccpi$CRD, my = cnccpi$corn_pbenefit), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
