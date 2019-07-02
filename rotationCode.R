library('ggplot2')
library('grid')
library('gridExtra')
library('plyr')
library('dplyr')
library('data.table')
library('animation') # You also need to brew install ImageMagick if you don't have it
library('Rmisc')
library('reshape')
library("choroplethr")
library('lmtest')
source("set_up_data.R")
require(rms)

set.seed(1)



# Satellite data
wnccpi = set_up(percentile = FALSE, outlier_val = 1000) # Take out counties with fewer than 1000 acres
wnccpi = wnccpi[ , !(names(wnccpi) %in% c("County.Name"))]
cnccpi = aggregate(.~CRD+year, data = wnccpi, mean, na.rm=T, na.action = na.pass)   # Organized by CRD
cnccpi = cnccpi[cnccpi$year > 2006 & cnccpi$year < 2013,] # Use this to compare to field-level data, otherwise don't run

# Field-level data
field_corn = as.data.frame(fread("maizeCRDyear.csv", check.names=TRUE))
field_corn = field_corn[field_corn$CRD < 2000,]
field_soy = as.data.frame(fread("soyCRDyear.csv", check.names=TRUE))
field_soy = field_soy[field_soy$CRD < 2000,]





# ==========================================================================================================================
# Figures 1 and 2 ===============================================================================================================
# Corn choropleth (Fig. 1):
call = ddply(cnccpi, c("CRD"), summarise, mean = mean(corn_pbenefit, na.rm=TRUE))
CRD = as.data.frame(fread("FIPS_CRD.csv", check.names = TRUE))
names(CRD) = c("FIPS.formula", "CRD")
c = join(call, CRD, by = "CRD")
call2 = data.frame(c$FIPS.formula, c$mean)
names(call2) = c("region", "value")
field_call = ddply(field_corn, c("CRD"), summarise, mean = mean(percent.effect, na.rm=TRUE))
field_c = join(field_call, CRD, by = "CRD")
field_call2 = data.frame(field_c$FIPS.formula, field_c$mean*100)
names(field_call2) = c("region", "value")

call_diff = within(merge(field_call2,call2,by="region"), {
  value <- value.x - value.y
})[,c("region","value")]

p1 <- county_choropleth(call2, state_zoom=c("iowa", "illinois", "indiana"), title = "Satellite Data for Corn",legend = "Average Percent\n Raw Difference", num_colors = 1) 
p1 <- p1 + scale_fill_gradient(limits = range(c(call2$value, field_call2$value)), low = "white", high = "#2b279a",  breaks = c(-3, 0, 3, 6, 9, 12)) + labs(fill='Average Percent\n Raw Difference')
p2 <- county_choropleth(field_call2, state_zoom=c("iowa", "illinois", "indiana"), title = "Field-level Data for Corn", legend = "Average Percent\n Raw Difference", num_colors=1)
p2 <- p2 + scale_fill_gradient(limits = range(c(call2$value, field_call2$value)), low = "white", high = "#2b279a",breaks =c(-3, 0, 3, 6, 9, 12)) + labs(fill='Average Percent\n Raw Difference')
multiplot(p1,p2)

# Soy choropleth (Fig. 2):
sall = ddply(cnccpi, c("CRD"), summarise, mean = mean(soy_pbenefit, na.rm=TRUE))
s = join(sall, CRD, by = "CRD")
sall2 = data.frame(s$FIPS.formula, s$mean)
names(sall2) = c("region", "value")
field_sall = ddply(field_soy, c("CRD"), summarise, mean = mean(percent.effect, na.rm=TRUE))
field_s = join(field_sall, CRD, by = "CRD")
field_sall2 = data.frame(field_s$FIPS.formula, field_s$mean*100)
names(field_sall2) = c("region", "value")

sall_diff = within(merge(field_sall2,sall2,by="region"), {
  value <- value.x - value.y
})[,c("region","value")]

p1 <- county_choropleth(sall2, state_zoom=c("iowa", "illinois", "indiana"), title = "Satellite Data for Soybean", legend = "Average Percent\n Raw Difference", num_colors=1)
p1 <- p1 + scale_fill_gradient(limits = range(c(field_sall2$value, sall2$value)), low = "white", high = "#2b279a", breaks = c(4, 6, 8, 10, 12, 14, 16))+ labs(fill='Average Percent\n Raw Difference')
p2 <- county_choropleth(field_sall2, state_zoom=c("iowa", "illinois", "indiana"), title = "Field-Level Farmer Data for Soybean", legend = "Average Percent\n Raw Difference", num_colors=1)
p2 <- p2 + scale_fill_gradient(limits = range(c(field_sall2$value, sall2$value)), low = "white", high = "#2b279a", breaks = c(4, 6, 8, 10, 12, 14, 16))+ labs(fill='Average Percent\n Raw Difference')
multiplot(p1,p2)
# ==========================================================================================================================
  



# ==========================================================================================================================
# Figure 3 ===============================================================================================================

# Need disaggregated data for regression
disagg =  as.data.frame(fread("disaggregated.csv", check.names=TRUE))
CRD = as.data.frame(fread("FIPS_CRD.csv", check.names = TRUE))
names(CRD) = c("FIPS.formula", "CRD")
disagg = join(disagg, CRD, by = "FIPS.formula")
disagg = disagg[disagg$year > 2006 & disagg$year < 2013,] # Use this to compare to field data results --> Make sure it matches up with cnccpi!
disagg = data.frame(FIPS = disagg$FIPS.formula, CRD = disagg$CRD, year = disagg$year, rotation = disagg$rotation, nccpi = disagg$nccpi_bucket, 
                    outcome_yield = disagg$yield, prcp = disagg$prcp, tmax = disagg$tmax, tmin = disagg$tmin, vpd = disagg$vp)


cc = disagg[disagg$rotation == "corn-corn",] 
sc = disagg[disagg$rotation == "soy-corn",]
ss = disagg[disagg$rotation == "soy-soy",]
cs = disagg[disagg$rotation == "corn-soy",]
cc$yearagocrop = "C"
cs$yearagocrop = "C"
sc$yearagocrop = "S"
ss$yearagocrop = "S"

cregr = rbind(cc, sc)
sregr  = rbind(ss, cs)


# Left panel ===============================================================================================================================
raw_cp = mean(cnccpi$corn_pbenefit,na.rm=T)
raw_sp = mean(cnccpi$soy_pbenefit,na.rm=T)
round(raw_cp,1)
round(raw_sp,1)

# Middle panel - environmental (ignore rotation) ===========================================================================================
# For full dataset, rerun above code without line 87, and make sure year is in the formulas. For 2007-2012, take out year.
clm = lm(formula = outcome_yield ~ vpd + prcp + tmax+ year+tmin + nccpi, data = cregr[complete.cases(cregr),])
cregr$predictions = predict.lm(clm, cregr)
slm = lm(formula = outcome_yield ~ vpd + prcp + tmax +year+ tmin + nccpi, data = sregr[complete.cases(sregr),])
sregr$predictions = predict.lm(slm, sregr)

cpbenefit <- cregr %>%
  group_by(yearagocrop) %>%
  summarise(ave=mean(predictions,na.rm=T))

spbenefit <- sregr %>%
  group_by(yearagocrop) %>%
  summarise(ave=mean(predictions,na.rm=T))

predicted_cp = as.numeric(cpbenefit[2,2] - cpbenefit[1,2])/sd(cregr$predictions,na.rm=T)*100 # Definition of effect size: mean of experimental - mean of control / standard deviation of both groups
predicted_sp = as.numeric(spbenefit[1,2] - spbenefit[2,2])/sd(sregr$predictions)*100
round(predicted_cp,1)
round(predicted_sp,1)

# Right panel ==============================================================================================================================
cregr$stateyear = paste0(substr(cregr$CRD, 1, 2), cregr$year)
sregr$stateyear = paste0(substr(sregr$CRD, 1, 2), sregr$year)

# For full dataset, rerun above code without line 87, and make sure year is in the formulas. For 2007-2012, take out year.
ctest <- bootcov(ols(formula = outcome_yield ~ yearagocrop + vpd + year + prcp + tmax + tmin + nccpi, data = cregr[complete.cases(cregr),], x=TRUE, y=TRUE), cluster = cregr$stateyear, group = cregr$yearagocrop, B=1000)
stest <- bootcov(ols(formula = outcome_yield ~ yearagocrop + vpd + year + prcp + tmax + tmin + nccpi, data = sregr[complete.cases(sregr),], x=TRUE, y=TRUE), group = sregr$yearagocrop, cluster = sregr$stateyear, B=1000)

cll <- capture.output(print(ctest))  
ceffect <- unlist(strsplit(cll, split=" +"))[min(which(unlist(strsplit(cll, split=" +"))=="yearagocrop=S"))+1]
c_sterror<- unlist(strsplit(cll, split=" +"))[min(which(unlist(strsplit(cll, split=" +"))=="yearagocrop=S"))+2]

sll <- capture.output(print(stest))  
seffect <- unlist(strsplit(sll, split=" +"))[min(which(unlist(strsplit(sll, split=" +"))=="yearagocrop=S"))+1]
s_sterror<- unlist(strsplit(sll, split=" +"))[min(which(unlist(strsplit(sll, split=" +"))=="yearagocrop=S"))+2]

cr = as.numeric(ceffect)/mean(cregr$outcome_yield)*100   # This should translate to the benefit/penalty for a change in rotation per unit of yield, 
sr = as.numeric(seffect)/mean(sregr$outcome_yield)*-100   # with all else (stateyear + environmental factors) held constant
round(cr,1)
round(sr,1)

# Range of values
ccoef = data.frame(ctest$boot.Coef)
crange = range(ccoef$yearagocrop.S)
crange[1]/mean(cregr$outcome_yield)*100
crange[2]/mean(cregr$outcome_yield)*100

scoef = data.frame(stest$boot.Coef)
srange = range(scoef$yearagocrop.S)
srange[1]/mean(sregr$outcome_yield)*-100
srange[2]/mean(sregr$outcome_yield)*-100


# T test
mu_satC = cr
mu_satS = sr
sd_satC = as.numeric(c_sterror)*sqrt(nrow(cregr))
sd_satS = as.numeric(s_sterror)*sqrt(nrow(sregr))
mu_comCR = 4.2599022 # Hardcoded from Seifert data
mu_comSR = 10.3374688
sd_comCR = 0.8712389
sd_comSR = 1.2033644
NC = nrow(cregr)
NS = nrow(sregr)
allfieldcorn = as.data.frame(fread("nsamp_CRD_maize.csv", check.names=TRUE))
allfieldsoy = as.data.frame(fread("nsamp_CRD_soy.csv", check.names=TRUE))
NCR = sum(allfieldcorn$n, na.rm=T)
NSR = sum(allfieldsoy$n, na.rm=T)
tcorn = abs( (mu_satC - mu_comCR)/(sqrt( (((sd_satC)^2)/NC) + (((sd_comCR)^2)/NCR) )) )
tsoy = abs( (mu_satS - mu_comSR)/(sqrt( (((sd_satS)^2)/NS) + (((sd_comSR)^2)/NSR) )) )
pt(tcorn, NS + NSR - 2)
pt(tsoy, NS + NSR - 2)


# Per CRD
# Assign regr to be cregr or sregr
totalSig = 0
for (ith_crd in unique(regr$CRD)) {
  crd_regr = regr[regr$CRD==ith_crd,]
  # Put in/take out year depending on which part of the paper. This should be the full dataset.
  btest = bootcov(ols(formula = outcome_yield ~ yearagocrop + vpd + year + prcp + tmax + tmin + nccpi, data = crd_regr[complete.cases(crd_regr),], x=TRUE, y=TRUE), group = crd_regr$yearagocrop, B=1000)
  crd_effect <- unlist(strsplit(capture.output(print(btest)), split=" +"))[min(which(unlist(strsplit(capture.output(print(btest)), split=" +"))=="yearagocrop=S"))+1]
  p <- unlist(strsplit(capture.output(print(btest)), split=" +"))[min(which(unlist(strsplit(capture.output(print(btest)), split=" +"))=="yearagocrop=S"))+4]
  if (p <= 0.05){
    totalSig = totalSig + 1
  }
}



# Putting together the figure ===================================================================================================================
# Note: Figure 3 uses the FULL dataset
c1 = data.frame(effect = c(raw_cp, predicted_cp, cr), Source = "Satellite", condition = c("a. Raw Difference",
                                                                                "b. Environmental Difference",
                                                                                "c. Effect Size"))

# These hard-coded numbers are from Seifert dataset
c2 = rbind(c1, data.frame(effect = c(3.4851746, -0.7240256, 4.2599022), Source = "Field-Level Rainfed", condition = c("a. Raw Difference",
                                                                                                                      "b. Environmental Difference",
                                                                                                                      "c. Effect Size")))
# These hard-coded numbers are from Seifert dataset
corndf = rbind(c2, data.frame(effect = c(4.7161200, 3.1098195, 1.7055143), Source = "Field-Level Irrigated", condition = c("a. Raw Difference",
                                                                                                                           "b. Environmental Difference",
                                                                                                                           "c. Effect Size")))
s1 = data.frame(effect = c(raw_sp, predicted_sp, sr), Source = "Satellite", condition = c("a. Raw Difference",
                                                                               "b. Environmental Difference",
                                                                               "c. Effect Size"))

# These hard-coded numbers are from Seifert dataset
s2 = rbind(s1, data.frame(effect = c(19.5298348, 9.7071527,  10.3374688), Source = "Field-Level Rainfed", condition = c("a. Raw Difference",
                                                                                                                        "b. Environmental Difference",
                                                                                                                        "c. Effect Size")))
# These hard-coded numbers are from Seifert dataset
soydf = rbind(s2, data.frame(effect = c(11.803456, 2.2422741, 9.5897829), Source = "Field-Level Irrigated", condition = c("a. Raw Difference",
                                                                                                                          "b. Environmental Difference",
                                                                                                                          "c. Effect Size")))
cols=c("Field-Level Irrigated" = "cadetblue2", "Field-Level Rainfed" = "deepskyblue1", "Satellite" = "lightgoldenrod1")
p1 <- ggplot(corndf, aes(fill=Source, y=effect, x=condition))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title="Corn") + geom_bar(position="dodge", stat="identity") + xlab("") + ylab("Yield Difference (%)") + 
  scale_fill_manual(values=cols)+
  scale_colour_manual(values=cols)

p2 <- ggplot(soydf, aes(fill=Source, y=effect, x=condition))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title="Soybean") + geom_bar(position="dodge", stat="identity") + xlab("") + ylab("Yield Difference (%)") + 
  scale_fill_manual(values=cols)+
  scale_colour_manual(values=cols)

multiplot(p1, p2)                                                                                                                         



# ==========================================================================================================================
# Figure 4 ===============================================================================================================
data_comparison_scatterplot = function(cnccpi, field_corn, field_soy){
  merge_me_corn = data.frame(cnccpi$CRD, cnccpi$year, cnccpi$corn_pbenefit)
  merge_me_soy = data.frame(cnccpi$CRD, cnccpi$year, cnccpi$soy_pbenefit)
  names(merge_me_corn) = c("CRD", "year", "pbenefit")
  names(merge_me_soy) = c("CRD", "year", "pbenefit")
  merged_corn = merge(merge_me_corn, field_corn, by = c("year", "CRD"))
  merged_soy = merge(merge_me_soy, field_soy, by = c("year", "CRD"))
  
  par(mfrow=c(1,2), mar = c(6, 8, 4.1, 1))
  plot(merged_corn$pbenefit,pch=20,ylab="",xlab="", merged_corn$percent.effect*100, main = "Corn")
  mtext(("Field-Level\n Data Raw\n Difference (%)"),side=2,las=1,line=2)
  mtext(("Satellite Data Raw Difference (%)"),side=1,las=1,line=2.5)
  abline(lm(merged_corn$percent.effect*100~merged_corn$pbenefit), col = "red", lwd = 3)
  plot(merged_soy$pbenefit,pch=20,ylab="",xlab="", merged_soy$percent.effect*100, main = "Soybean")
  mtext(("Field-Level \nData Raw\n Difference (%)"),side=2,las=1,line=2)
  mtext(("Satellite Data Raw Difference (%)"),side=1,las=1,line=2.5)
  abline(lm(merged_soy$percent.effect*100~merged_soy$pbenefit), col = "blue", lwd = 3)
}

data_comparison_scatterplot(cnccpi, field_corn, field_soy)

# ==========================================================================================================================







# ==========================================================================================================================
# Figure 5 ===============================================================================================================
pbenefit_hist = function(df, field_corn, field_soy, overlay) {
  par(mfrow=c(2,1))
  hist(df$corn_pbenefit, breaks = 30, xlim=c(-13,30), xlab = "Raw Difference (%)", main = "Corn", ylab = "",col = rgb(1,0,0,0.5), freq = FALSE)
  mtext("CRD-year\n instances",side=2,las=1,line=2.2)
  if(overlay){
    hist(field_corn$percent.effect*100, col = rgb(0,0,1,0.5), breaks = 30, ylim = 0.15,add = T, freq = FALSE)
    legend(y.intersp = 1.15, x = 10, y = 0.15, bty = "n", c("Satellite data", "Field-level data"), fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
  }
  hist(df$soy_pbenefit,xlim=c(-13,30), ylim = c(0, 0.15), xlab = "Raw Difference (%)", breaks = 30, main = "Soybean", ylab = "", col = rgb(1,0,0,0.5),freq = FALSE)
  mtext("CRD-year\n instances",side=2,las=1,line=2.2)
  if(overlay){
    hist(field_soy$percent.effect*100, col = rgb(0,0,1,0.5), add = T, breaks=30, freq = FALSE)
    legend(c("Satellite data", "Field-Level data"), y.intersp = 1.15, x = 15, y = 0.11, bty = "n", fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
  }
}

pbenefit_hist(cnccpi, field_corn, field_soy, TRUE)
# ==========================================================================================================================









# ==========================================================================================================================
# Figure 6 ===============================================================================================================
# Remember to use the full dataset
soy_nccpi = rbind(data.frame(pbenefit = mean(cnccpi$soy_pbenefit_nccpi1, na.rm=T), nccpi = "40-50"),
                  data.frame(pbenefit = mean(cnccpi$soy_pbenefit_nccpi2, na.rm=T), nccpi = "50-60"), 
                  data.frame(pbenefit = mean(cnccpi$soy_pbenefit_nccpi3, na.rm=T), nccpi = "60-70"), 
                  data.frame(pbenefit = mean(cnccpi$soy_pbenefit_nccpi4, na.rm=T), nccpi = "70-80"), 
                  data.frame(pbenefit = mean(cnccpi$soy_pbenefit_nccpi5, na.rm=T), nccpi = "80-90"))

corn_nccpi = rbind(data.frame(pbenefit = mean(cnccpi$corn_pbenefit_nccpi1, na.rm=T), nccpi = "40-50"),
                   data.frame(pbenefit = mean(cnccpi$corn_pbenefit_nccpi2, na.rm=T), nccpi = "50-60"), 
                   data.frame(pbenefit = mean(cnccpi$corn_pbenefit_nccpi3, na.rm=T), nccpi = "60-70"), 
                   data.frame(pbenefit = mean(cnccpi$corn_pbenefit_nccpi4, na.rm=T), nccpi = "70-80"), 
                   data.frame(pbenefit = mean(cnccpi$corn_pbenefit_nccpi5, na.rm=T), nccpi = "80-90"))


p1 <-ggplot(corn_nccpi, aes(x = nccpi, y = pbenefit, group=1)) + geom_line(size=1)+geom_point(aes(color=nccpi, size = 2)) +
  scale_color_manual(values = c("#ff7961", "#ffaa54", "#dbff54", "#7ad623", "#65f99c")) +
  scale_y_continuous(limits = c(0, 5)) + xlab("") +
  ylab("")+ theme(legend.position="none")+    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"))

p2 <- ggplot(soy_nccpi, aes(x = nccpi, y = pbenefit, group=1)) + geom_line(size=1)+geom_point(aes(color=nccpi, size = 2)) +
  scale_color_manual(values = c("#ff7961", "#ffaa54", "#dbff54", "#7ad623", "#65f99c")) +
  scale_y_continuous(limits = c(5, 10)) + xlab("") +
  ylab("")+ theme(legend.position="none")+    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(p1, p2, nrow = 2,
             bottom = textGrob(
               "NCCPI Index",
               gp = gpar(fontsize = 13)
             ), left = textGrob(
               "Effect (%)\n\n\n",
               gp = gpar(fontsize = 13)
             ), right = textGrob(
               "Corn \n \n\n\n\n \n \n \n \n \n  \n \n \n Soybean\n \n \n\n",
               gp = gpar(fontsize = 13)
             )
)
# ==========================================================================================================================










# ==========================================================================================================================
# Figure 7 ===============================================================================================================
wald = function(data, vars) {
  waldResults = data.frame()
  for (v in vars) {
    data[which(data[,v] < quantile(data[,v],probs=c(.25), na.rm=T)), paste0(v, 'qu')] <-1
    data[which(data[,v]  >= quantile(data[,v] ,probs=c(.25), na.rm=T)
               & data[,v]   < quantile(data[,v] ,probs=c(.50), na.rm=T)), paste0(v, 'qu')] <- 2
    data[which(data[,v]  >= quantile(data[,v] ,probs=c(.50), na.rm=T)
               & data[,v]   < quantile(data[,v] ,probs=c(.75), na.rm=T) ), paste0(v, 'qu')] <- 3
    data[which(data[,v]  >= quantile(data[,v] ,probs=c(.75), na.rm=T)), paste0(v, 'qu')] <- 4
    data[,paste0(v, 'qu')] <- as.factor(data[,paste0(v, 'qu')])
    
    f0 = formula(paste0("outcome_yield ~ yearagocrop + prcp + tmax + year + tmin + vp + ", v, "qu"))
    f1 = formula(paste0("outcome_yield ~ yearagocrop + prcp + tmax + year + tmin + vp + ", v, "qu", "*yearagocrop"))
    test0 = ols(formula = f0, data = data, x=TRUE, y=TRUE)
    test1 = ols(formula = f1, data = data, x=TRUE, y=TRUE)
    w = waldtest(test0, test1)
    pval = p.adjust(w[2,4], method="bonferroni")
    waldResults = rbind(waldResults, data.frame("var" = v, "pval" = pval))
  }
  return(waldResults)
}


cdata = cregr[complete.cases(cregr),] # cregr comes from Figure 3. To match the paper you want to use the full satellite dataset.
names(cdata)[10] = c("vp")
sdata = sregr[complete.cases(sregr),]
names(sdata)[10] = c("vp")
vars = c("prcp", "tmax", "tmin", "vp")
wRC = wald(cdata, vars)
wRS = wald(sdata, vars)


for  (i in seq(length(vars))) {
  v = vars[i]
  print(v)
  
  sc = paste0(v, "_sc_mean")
  cc = paste0(v, "_cc_mean")
  cs = paste0(v, "_cs_mean")
  ss = paste0(v, "_ss_mean")
  cQ = quantile(c(cnccpi[,sc], cnccpi[,cc]), na.rm=T)
  sQ = quantile(c(cnccpi[,cs], cnccpi[,ss]), na.rm=T)
  
  corn = data.frame(col = "cadetblue4", pbenefit = c(mean(cnccpi$corn_pbenefit[cnccpi[,sc] <= cQ[[2]] & cnccpi[,cc] <= cQ[[2]]], na.rm=T), 
                                                     mean(cnccpi$corn_pbenefit[cnccpi[,cc]  > cQ[[2]] & cnccpi[,sc] > cQ[[2]] & cnccpi[,cc]  <= cQ[[3]] & cnccpi[,sc] <= cQ[[3]]], na.rm=T), 
                                                     mean(cnccpi$corn_pbenefit[cnccpi[,cc] > cQ[[3]] & cnccpi[,sc] > cQ[[3]] & cnccpi[,cc] <= cQ[[4]] & cnccpi[,sc] <= cQ[[4]]], na.rm=T), 
                                                     mean(cnccpi$corn_pbenefit[cnccpi[,cc]  > cQ[[4]] & cnccpi[,sc] > cQ[[4]]], na.rm=T)),
                    quartiles = c(1,2,3,4), sigs = (wRC$pval[wRC$var == v] <= 0.05))
  print(corn$sigs)
  corn$sigs = as.integer(as.logical(corn$sigs))
  
  
  soy = data.frame(col = "seagreen4", pbenefit = c(mean(cnccpi$soy_pbenefit[cnccpi[,cs] <= sQ[[2]] & cnccpi[,ss] <= sQ[[2]]], na.rm=T), 
                                                   mean(cnccpi$soy_pbenefit[cnccpi[,cs]  > sQ[[2]] & cnccpi[,ss] > sQ[[2]] & cnccpi[,ss]  <= sQ[[3]] & cnccpi[,cs] <= sQ[[3]]], na.rm=T), 
                                                   mean(cnccpi$soy_pbenefit[cnccpi[,ss] > sQ[[3]] & cnccpi[,cs] > sQ[[3]] & cnccpi[,ss] <= sQ[[4]] & cnccpi[,cs] <= sQ[[4]]], na.rm=T), 
                                                   mean(cnccpi$soy_pbenefit[cnccpi[,ss]  > sQ[[4]] & cnccpi[,cs] > sQ[[4]]], na.rm=T)),
                   quartiles = c(1,2,3,4), sigs = (wRS$pval[wRS$var == v] <= 0.05))
  soy$sigs = as.integer(as.logical(soy$sigs))
  
  label1 = paste0(v, "_corn")
  label2 = paste0(v, "_soy")
  
  print(corn$sigs)
  assign(label1, ggplot(corn, aes(quartiles, pbenefit, color="cadetblue4")) + geom_path(size=corn$sigs + 1) +
           theme(legend.position="none") +
           ylab("") + xlab("") + ylim(c(0, 4.0)) + 
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black")))
  
  assign(label2, ggplot(soy, aes(quartiles, pbenefit, color="seagreen4")) + geom_path(size=soy$sigs + 1) + 
           theme(legend.position="none") +
           ylab("") + xlab("")+ ylim(c(6, 13.0)) + 
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black")))
  
}

grid.arrange(prcp_corn, tmax_corn, tmin_corn, vp_corn, prcp_soy, tmax_soy,
             tmin_soy, vp_soy, nrow = 2,
             bottom = textGrob(
               "Quartile",
               gp = gpar(fontsize = 16)
             ),
             left = textGrob(
               "Effect (%)",
               gp = gpar(fontsize = 18)
             ),
             right = textGrob(
               "Corn \n \n \n \n \n \n \n \n \n \n \n Soybean",
               gp = gpar(fontsize = 16)
             ), 
             top = textGrob(
               "Precipitation                               Max. Temp                               Min. Temp                               VPD",
               gp = gpar(fontsize = 14)
             )
)
# ==========================================================================================================================
