library('ggplot2')
library('plyr')
<<<<<<< HEAD
library('dplyr')
=======
>>>>>>> 6c5b412fa2bbae955cfe2b796ddb93a3271ddc03

# Set up data
corn_before_corn = read.csv("corn_before_corn.csv")
corn_before_soy = read.csv("corn_before_soy.csv")
soy_before_corn = read.csv("soy_before_corn.csv")
soy_before_soy = read.csv("soy_before_soy.csv")

all_means <- data.frame(cbind(corn_before_corn$mean, corn_before_corn$year, corn_before_soy$mean, corn_before_soy$year, soy_before_corn$mean, soy_before_corn$year, soy_before_soy$mean, soy_before_soy$year))
all_means <- na.omit(all_means)
all_means <- setNames(all_means, c('cc', 'cc_year', 'cs', 'cs_year', 'sc', 'sc_year', 'ss', 'ss_year'))

# This is used for the line graphs below
year_means_cc <- tapply(all_means$cc, all_means$cc_year, mean)
year_means_cs <- tapply(all_means$cs, all_means$cs_year, mean)
year_means_sc <- tapply(all_means$sc, all_means$sc_year, mean)
year_means_ss <- tapply(all_means$ss, all_means$ss_year, mean)
year_means <- data.frame(cbind(year_means_cc, year_means_cs, year_means_sc, year_means_ss))
year_means <- setNames(cbind(rownames(year_means), year_means, row.names=NULL), c("year", "year_means_cc", "year_means_cs", "year_means_sc", "year_means_ss"))

# Significant, cool
p_corn <- t.test(na.omit(corn_before_corn$mean), na.omit(soy_before_corn$mean))[["p.value"]]
p_soy <- t.test(na.omit(corn_before_soy$mean), na.omit(soy_before_soy$mean))[["p.value"]]

# Histograms of all years and counties (aka not supes informative)
par(mfrow=c(2,2))
hist(corn_before_corn$mean, col = "cadetblue1", xlab="Mean Yield", main = "Corn Before Corn")
abline(v = mean(na.omit(corn_before_corn$mean)), lwd = 3, col = "red")
hist(corn_before_soy$mean, col="springgreen1",  xlab="Mean Yield", main = "Corn before Soy")
abline(v = mean(na.omit(corn_before_soy$mean)), lwd = 3, col = "red")
hist(soy_before_corn$mean, col="gold1",  xlab="Mean Yield", main = "Soy Before Corn")
abline(v = mean(na.omit(soy_before_corn$mean)), lwd = 3, col = "red")
hist(soy_before_soy$mean, col="plum1",  xlab="Mean Yield", main = "Soy Before Corn")
abline(v = mean(na.omit(soy_before_soy$mean)), lwd = 3, col = "red")


# Example year line graph (do this for all years below)
means_2005 <- year_means[year_means$year == 2005,]
means_2005 <- na.omit(means_2005)
se <- c(round((sqrt(var(year_means$year_means_cc)))/sqrt(length(year_means$year_means_cc))*1.96,3), round((sqrt(var(year_means$year_means_sc)))/sqrt(length(year_means$year_means_sc))*1.96,3), round((sqrt(var(year_means$year_means_cs)))/sqrt(length(year_means$year_means_cs))*1.96,3), round((sqrt(var(year_means$year_means_ss)))/sqrt(length(year_means$year_means_ss))*1.96,3))
R <- cbind(data.frame(c(means_2005$year_means_cc, means_2005$year_means_sc, means_2005$year_means_cs, means_2005$year_means_ss)),c("Corn before Corn", "Soy before Corn", "Corn before Soy", "Soy before Soy"))
R <- setNames(R, c("mean", "rotation"))
ggplot(R, aes(y=mean, x=as.factor(rotation))) + geom_bar(fill = "cadetblue3", color="plum2", position=position_dodge(), stat="identity", colour='black') + xlab("Rotations for 2005") + ylab("Mean Yield") + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=.3, width=.2, position=position_dodge(.9))

# Rotation means/year
ggplot(data = year_means, aes(x = year)) +
geom_line(aes(y = year_means_cc, color = "Corn before Corn"), group=1) +
geom_line(aes(y = year_means_sc, color = "Soy before Corn"),group=1) +
geom_line(aes(y = year_means_cs, color = "Corn before Soy"),group=1) +
geom_line(aes(y = year_means_ss, color = "Soy before Soy"),group=1) +
labs(
x = "Year",
y = "Mean Yield",
color = "Rotation"
)

#=============================================================
# County comparison
par(mfrow=c(2,2))
source("ygap.trendmaps.R")
cnames=paste(ctab$NAME,ctab$STATE_NAME,sep=', ')

CRD = read.csv("FIPS_CRD.csv")
# Difference between rotations/not rotations
corn <- cbind(corn = corn_before_corn, soy = soy_before_corn)
corn_with_diff <- mutate(corn, diff = corn$soy.mean - corn$corn.mean)
corn_with_diff = corn_with_diff[corn_with_diff$diff < 300,] # REMOVE OUTLIERS
soy <- cbind(corn = corn_before_soy, soy = soy_before_soy)
soy_with_diff <- mutate(soy, diff = soy$corn.mean - soy$soy.mean)
soy_with_diff = soy_with_diff[soy_with_diff$diff < 100,] # REMOVE OUTLIERS

# Mean across all the years
corn_means_allyears = ddply(corn_with_diff, c("corn.FIPS.formula"), summarise, mean = mean(diff))
names(corn_means_allyears) = c("FIPS", "mean")
soy_means_allyears = ddply(soy_with_diff, c("soy.FIPS.formula"), summarise, mean = mean(diff))
names(soy_means_allyears) = c("FIPS", "mean")

# Means for each instance of each year/same info as corn_with_diff, just better organized
corn_means_eachyear = ddply(corn_with_diff, c("corn.year", "corn.FIPS.formula", "corn.count"), summarise, mean = mean(diff))
names(corn_means_eachyear) = c("year", "FIPS", "count", "mean")

corn_means_eachyear = full_join(corn_means_eachyear, CRD, by = "FIPS")
soy_means_eachyear = ddply(soy_with_diff, c("soy.year", "soy.FIPS.formula", "soy.count"), summarise, mean = mean(diff))
names(soy_means_eachyear) = c("year", "FIPS", "count", "mean")
soy_means_eachyear = full_join(soy_means_eachyear, CRD, by = "FIPS")

soy_means_eachyear = ddply(soy_with_diff, c("soy.year", "soy.FIPS.formula", "soy.count"), summarise, mean = mean(diff))
names(soy_means_eachyear) = c("year", "FIPS", "count", "mean")


# All the years together
par(mfrow=c(1,2))
map.var(data.frame(fips = corn_means_allyears$FIPS, my = corn_means_allyears$mean), titl = "Rotation - Not Rotation\n Corn for all years", legend=TRUE, xleg = -97, yleg = 40, leg_size = 0.85)
map.var(data.frame(fips = soy_means_allyears$FIPS, my = soy_means_allyears$mean), titl = "Rotation - Not Rotation\n Soy Yields for all years", legend=TRUE,  xleg = -97, yleg = 40, leg_size = 0.85)

# Separate years for corn, split into three figures
# NOTE: The next few figures are all split into 3 pages with 6 figures able to fit on each page,
# then there's code to turn all of them into a gif at the bottom.
par(mfrow=c(2,3))
for (year in unique(corn_means_eachyear$year)[1:6]) {
  map.var(data.frame(fips = corn_means_eachyear$FIPS[corn_means_eachyear$year==year], my = corn_means_eachyear$mean[corn_means_eachyear$year==year]), titl = paste0("Corn ", year), xleg =-97, yleg = 40, legend=TRUE, leg_size = 0.85)
}
for (year in unique(corn_means_eachyear$year)[7:12]) {
  map.var(data.frame(fips = corn_means_eachyear$FIPS[corn_means_eachyear$year==year], my = corn_means_eachyear$mean[corn_means_eachyear$year==year]), titl = paste0("Corn ", year), xleg =-97, yleg = 40, legend=TRUE, leg_size = 0.85)
}
for (year in unique(corn_means_eachyear$year)[13:15]) {
  map.var(data.frame(fips = corn_means_eachyear$FIPS[corn_means_eachyear$year==year], my = corn_means_eachyear$mean[corn_means_eachyear$year==year]), titl = paste0("Corn ", year), xleg =-97, yleg = 40, legend=TRUE, leg_size = 0.85)
}

# "" for soy 
par(mfrow=c(2,3))
for (year in unique(soy_means_eachyear$year)[1:6]) {
map.var(data.frame(fips = soy_means_eachyear$FIPS[soy_means_eachyear$year==year], my = soy_means_eachyear$mean[soy_means_eachyear$year==year]), titl = paste0("Soy ", year), xleg =-97, yleg = 40, legend=TRUE, leg_size = 0.85)
}
for (year in unique(soy_means_eachyear$year)[7:12]) {
map.var(data.frame(fips = soy_means_eachyear$FIPS[soy_means_eachyear$year==year], my = soy_means_eachyear$mean[soy_means_eachyear$year==year]), titl = paste0("Soy ", year), xleg =-97, yleg = 40, legend=TRUE, leg_size = 0.85)
}
for (year in unique(soy_means_eachyear$year)[13:15]) {
map.var(data.frame(fips = soy_means_eachyear$FIPS[soy_means_eachyear$year==year], my = soy_means_eachyear$mean[soy_means_eachyear$year==year]), titl = paste0("Soy ", year), xleg =-97, yleg = 40, legend=TRUE, leg_size = 0.85)
}


# Pixels for corn
par(mfrow=c(2,3))
names(corn_means_eachyear) = c("year", "FIPS", "count", "mean") # woops I'll fix this so everything's named consistently my b
for (year in unique(corn_means_eachyear$year)[1:6]) {
map.var(data.frame(fips = corn_means_eachyear$FIPS[corn_means_eachyear$year==year], my = corn_means_eachyear$count[corn_means_eachyear$year==year]/1000), titl = paste0("Corn\n Pixels/1000 \n", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
}

for (year in unique(corn_means_eachyear$year)[7:12]) {
map.var(data.frame(fips = corn_means_eachyear$FIPS[corn_means_eachyear$year==year], my = corn_means_eachyear$count[corn_means_eachyear$year==year]/1000), titl = paste0("Corn\n Pixels/1000 \n", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
}

for (year in unique(corn_means_eachyear$year)[13:15]) {
map.var(data.frame(fips = corn_means_eachyear$FIPS[corn_means_eachyear$year==year], my = corn_means_eachyear$count[corn_means_eachyear$year==year]/1000), titl = paste0("Corn\n Pixels/1000 \n", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
}

# Pixels for soy
names(soy_means_eachyear) = c("year", "FIPS", "count", "mean") # woops
for (year in unique(soy_means_eachyear$year)[1:6]) {
map.var(data.frame(fips = soy_means_eachyear$FIPS[soy_means_eachyear$year==year], my = soy_means_eachyear$count[soy_means_eachyear$year==year]/1000), titl = paste0("Soy\n Pixels/1000 \n", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
}
for (year in unique(soy_means_eachyear$year)[7:12]) {
map.var(data.frame(fips = soy_means_eachyear$FIPS[soy_means_eachyear$year==year], my = soy_means_eachyear$count[soy_means_eachyear$year==year]/1000), titl = paste0("Soy\n Pixels/1000 \n", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
}
for (year in unique(soy_means_eachyear$year)[13:15]) {
map.var(data.frame(fips = soy_means_eachyear$FIPS[soy_means_eachyear$year==year], my = soy_means_eachyear$count[soy_means_eachyear$year==year]/1000), titl = paste0("Soy\n Pixels/1000 \n", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
}

par(mfrow=c(1,1))
library('animation')
# You also need to brew install ImageMagick

# Animate! 
# Corn yields
saveGIF({
  for (year in unique(na.omit(corn_means_eachyear$year))) {
    map.var(data.frame(fips = corn_means_eachyear$FIPS[corn_means_eachyear$year==year], my = corn_means_eachyear$mean[corn_means_eachyear$year==year]), titl = paste0("Corn Rotation\n Diff", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
  }

}, interval = 2, ani.width=600, ani.height=800, loop=TRUE, movie.name = "corn_yields.gif", outdir = getwd())

# Corn yields CRD
saveGIF({
  for (year in unique(na.omit(corn_means_eachyear$year))) {
    CRD_corn = aggregate(corn_means_eachyear$mean, by=list(CRD=corn_means_eachyear$CRD, year=corn_means_eachyear$year), data = corn_means_eachyear, FUN = mean)
    # Seems like there are fewer CRDs than FIPS so I take the mean, but that ends up looking v weird...
    map.var(data.frame(CRD = CRD_corn$CRD[CRD_corn$year==year], my = CRD_corn$mean[CRD_corn$year==year]), titl = paste0("Corn Rotation\n Diff for CRD", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40, district = "CRD")
  }
}, interval = 2, ani.width=600, ani.height=800, loop=TRUE, movie.name = "corn_yields.gif", outdir = getwd())


}, interval = 2, ani.width=600, ani.height=800, loop=TRUE, outdir = getwd())

# Corn pixels
saveGIF({
  for (year in unique(na.omit(corn_means_eachyear$year))) {
    map.var(data.frame(fips = corn_means_eachyear$FIPS[corn_means_eachyear$year==year], my = corn_means_eachyear$count[corn_means_eachyear$year==year]/1000), titl = paste0("Corn\n Pixels/1000 \n", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
  }
}, interval = 2, ani.width=600, ani.height=800, loop=TRUE,movie.name = "corn_pixels.gif", outdir = getwd())



# Soy yields
saveGIF({
  for (year in unique(na.omit(soy_means_eachyear$year))) {
    map.var(data.frame(fips = soy_means_eachyear$FIPS[soy_means_eachyear$year==year], my = soy_means_eachyear$mean[soy_means_eachyear$year==year]), titl = paste0("Soy Rotation\n Diff", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
  }
}, interval = 2, ani.width=600, ani.height=800, loop=TRUE, movie.name = "soy_yields.gif", outdir = getwd())

# Soy pixels
saveGIF({
  for (year in unique(na.omit(soy_means_eachyear$year))) {
    map.var(data.frame(fips = soy_means_eachyear$FIPS[soy_means_eachyear$year==year], my = soy_means_eachyear$count[soy_means_eachyear$year==year]/1000), titl = paste0("Soy\n Pixels/1000 \n", year), legend=TRUE, leg_size = 0.85, xleg =-97, yleg = 40)
  }

}, interval = 2, ani.width=600, ani.height=800, loop=TRUE, movie.name = "soy_pixels.gif", outdir = getwd())


#=============================================================

# Scatter plots for weather data

# CORN:
library("dplyr")
sc_tmax = read.csv("sc_tmax.csv")
cc_tmax = read.csv("cc_tmax.csv")
c_tmax = merge(sc_tmax, cc_tmax, by=c("FIPS.formula", "year", "Geographic.Name", "system.index"))
c_tmax$t_max = (c_tmax$mean.x + c_tmax$mean.y)/2.0
sc_prcp = read.csv("sc_prcp.csv")
cc_prcp = read.csv("cc_prcp.csv")
c_prcp = merge(sc_prcp, cc_prcp, by=c("FIPS.formula", "year", "Geographic.Name", "system.index"))
c_prcp$prcp = (c_prcp$mean.x + c_prcp$mean.y)/2.0
c_weather = merge(c_tmax, c_prcp, by = c("FIPS.formula", "year", "Geographic.Name", "system.index"))
names(corn_means_eachyear) = c("year", "FIPS.formula", "count", "mean") # woops
c_weather = full_join(c_weather, corn_means_eachyear, by = c("FIPS.formula", "year"))

par(mfrow=c(3,5))
# Corn weather
for (year in unique(c_weather$year)) {
plot(c_weather$prcp[c_weather$year == year], c_weather$mean[c_weather$year == year], xlab="Precipitation in July", ylab=paste0("Corn", year))
abline(lm(c_weather$mean~c_weather$prcp), col="green")
}
for (year in unique(c_weather$year)) {
plot(c_weather$t_max[c_weather$year == year], c_weather$mean[c_weather$year == year], ylab=paste0("Corn", year), xlab="T_Max in July")
abline(lm(c_weather$mean~c_weather$t_max), col="green")
}

# Soy weather 
ss_tmax = read.csv("ss_tmax.csv")
cs_tmax = read.csv("cs_tmax.csv")
s_tmax = merge(ss_tmax, cs_tmax, by=c("FIPS.formula", "year", "Geographic.Name", "system.index"))
s_tmax$t_max = (s_tmax$mean.x + s_tmax$mean.y)/2.0
ss_prcp = read.csv("ss_prcp.csv")
cs_prcp = read.csv("cs_prcp.csv")
s_prcp = merge(ss_prcp, cs_prcp, by=c("FIPS.formula", "year", "Geographic.Name", "system.index"))
s_prcp$prcp = (s_prcp$mean.x + s_prcp$mean.y)/2.0
s_weather = merge(s_tmax, s_prcp, by = c("FIPS.formula", "year", "Geographic.Name", "system.index"))
names(soy_means_eachyear) = c("year", "FIPS.formula", "count", "mean") # woops
s_weather = full_join(s_weather, soy_means_eachyear, by = c("FIPS.formula", "year"))

par(mfrow=c(3,5))
# Soy weather
for (year in unique(s_weather$year)) {
plot(s_weather$prcp[s_weather$year == year], s_weather$mean[s_weather$year == year], xlab="Precipitation in July", ylab=paste0("Soy", year))
abline(lm(s_weather$mean~s_weather$prcp), col="green")
}
for (year in unique(s_weather$year)) {
plot(s_weather$t_max[s_weather$year == year], s_weather$mean[s_weather$year == year], ylab=paste0("Soy", year), xlab="T_Max in July")
abline(lm(s_weather$mean~s_weather$t_max), col="green")
}

#=============================================================

corn_before_corn_meters = read.csv("corn_before_corn_meters.csv")
soy_before_corn_meters = read.csv("soy_before_corn_meters.csv")
corn_before_soy_meters = read.csv("corn_before_soy_meters.csv")
soy_before_soy_meters = read.csv("soy_before_soy_meters")


corn <- cbind(corn = corn_before_corn, soy = soy_before_corn)
corn_with_diff <- mutate(corn, diff = corn$soy.mean - corn$corn.mean)
corn_means_eachyear = ddply(corn_with_diff, c("corn.year", "corn.FIPS.formula", "corn.count"), summarise, mean = mean(diff))
names(corn_means_eachyear) = c("year", "FIPS", "count", "mean")
soy <- cbind(corn = corn_before_soy, soy = soy_before_soy)
soy_with_diff <- mutate(soy, diff = soy$corn.mean - soy$soy.mean)
soy_means_eachyear = ddply(soy_with_diff, c("soy.year", "soy.FIPS.formula", "soy.count"), summarise, mean = mean(diff))
names(soy_means_eachyear) = c("year", "FIPS", "count", "mean")

# Find outliers in yields for corn and soy
corn_outliers = data.frame(na.omit(corn_means_eachyear[corn_means_eachyear$mean > 300,])) 
# @Chris, there's a bunch of NAs here and idk really where they come from; 
# I think they're in the original CSV as in there just weren't data for that county that year. 
# I've just been removing them but I wonder if I should do that first when I load the csvs in.
soy_outliers = data.frame(na.omit(soy_means_eachyear[soy_means_eachyear$mean > 100,])) 
# write.csv(corn_outliers, "corn_outliers.csv") # Commented out cos I don't wanna do this every time
# write.csv(soy_outliers, "soy_outliers.csv")


#=============================================================
# CRD-FIPS stuff


# =============================================================
#TODO 
# Find out why there are outliers
# CRD maps/gifs (4) and compare
# Run full regression on all variables (tmax, prcp) and plot partials
# Check yield samples against sample sizes + see if there's not enough data (?) # small samples for counties
# do weather analyses overall (waiting on george)
# panel.smooth --> one plot for all the years, color points by year

#TODO 
# Fix the animations not looping more than once
# Find out why there are 0s for precipitation --> R and EE debugging
# CRD maps/gifs (4)
# Run full regression on all variables (tmax, prcp) and plot partials (5)
# Regress on one variable and plot residuals against other variable
# Check yield samples against sample sizes + see if there's not enough data

