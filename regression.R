regression_barplot = function(cnccpi) {
  
  regr = regression_df(cnccpi)
  cregr = regr[substr(regr$name, 1, 15) == "soy_before_corn" | substr(regr$name, 1, 16) == "corn_before_corn",]
  sregr = regr[substr(regr$name, 1, 14) == "soy_before_soy" | substr(regr$name, 1, 15) == "corn_before_soy",]
  
  # Left panel
  cp <- cregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(outcome_yield,na.rm=T))
  raw_cp = ((as.numeric(cp[1,2]/cp[2,2]))-1)*100
  
  sp <- sregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(outcome_yield,na.rm=T))
  raw_sp = ((as.numeric(sp[2,2]/sp[1,2]))-1)*100
  
  # Middle panel - environmental (ignore rotation)
  clm = lm(formula = outcome_yield ~ vpd + prcp + tmax + tmin + nccpi, data = cregr[complete.cases(cregr),])
  cregr$predictions = predict.lm(clm, cregr)
  
  slm = lm(formula = outcome_yield ~ vpd + prcp + tmax + tmin + nccpi, data = sregr[complete.cases(sregr),])
  sregr$predictions = predict.lm(slm, sregr)
  
  cpbenefit <- cregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(predictions,na.rm=T))
  
  spbenefit <- sregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(predictions,na.rm=T))
  
  predicted_cp = ((as.numeric(cpbenefit[1,2]/cpbenefit[2,2]))-1)*100
  predicted_sp = ((as.numeric(spbenefit[2,2]/spbenefit[1,2]))-1)*100
  
  # Right panel is summary(clm) estimate for yearagocrop/ predicted mean C (using yearagocrop in fmla)
  # ^ except should be rotated, not CC
  
  cregr$stateyear = paste0(substr(cregr$CRD, 1, 2), cregr$year)
  sregr$stateyear = paste0(substr(sregr$CRD, 1, 2), sregr$year)
  cregr = cregr[complete.cases(cregr),] %>% group_by(stateyear) %>% filter(var(outcome_yield) <= 50000)
  cregr = cregr[complete.cases(cregr),] %>% group_by(stateyear) %>% filter(n() >= 85)
  sregr = sregr[complete.cases(sregr),] %>% group_by(stateyear) %>% filter(n() >= 80)
  clm = lm(formula = outcome_yield ~ yearagocrop + vpd + prcp + tmax + tmin + nccpi, data = cregr[complete.cases(cregr),], x=T, y=T)
  slm = lm(formula = outcome_yield ~ yearagocrop + vpd + prcp + tmax + tmin + nccpi, data = sregr[complete.cases(sregr),], x=T, y=T)
  ctest <- bootcov(clm, cregr$stateyear[complete.cases(cregr)], B=10000,fitter = ols)
  stest <- bootcov(slm, sregr$stateyear[complete.cases(sregr)], B=1000, fitter = ols)
  
  
  
  # group by stateyear + eliminate small samples
  
  #coef(summary(ctest))
  #ll <- capture.output(print(ctest))
  #print(david.lm)
  #effect <- unlist(strsplit(ll, split=" +"))[min(which(unlist(strsplit(ll, split=" +"))=="yearagocrop=S"))+1]
  #sterror<- unlist(strsplit(ll, split=" +"))[min(which(unlist(strsplit(ll, split=" +"))=="yearagocrop=S"))+2]
  
  #cregr$predictions = predict.lm(clm, cregr)
  # sregr$predictions = predict.lm(slm, sregr)
  cpbenefit2 <- cregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(predictions))
  spbenefit2 <- sregr %>%
    group_by(yearagocrop) %>%
    summarise(ave=mean(predictions))
  # 
  effect_cp = (as.numeric(66.37/cpbenefit2[1,2]))*100
  effect_sp = (as.numeric(32.7/spbenefit2[2,2]))*100
  
  # CLM yearagocropS = 78.566335
  # SLM yearagocropC = 29.32575
  
  # Do error bars
  
  par(mfrow=c(1,2))
  barplot(c(raw_sp, predicted_sp, effect_sp), ylim = c(0,10), ylab = "Percent Rotation Benefit", names=c("Raw", "Environmental\nPrediction", "Effect Size"), col = terrain.colors(3), main = "Soy")
  barplot(c(raw_cp, predicted_cp, effect_cp), ylim = c(0,10), ylab = "Percent Rotation Benefit", names=c("Raw", "Environmental\nPrediction", "Effect Size"), col = terrain.colors(3), main = "Corn")
  
}


regression_df = function(cnccpi) {
  sc1 <- data.frame(name = "soy_before_corn_nccpi1", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_sc_nccpi1_mean, yearagocrop = "S", nccpi = 1,
                   tmax = cnccpi$tmax_sc_mean, prcp = cnccpi$prcp_sc_mean, vpd = cnccpi$vp_sc_mean, tmin = cnccpi$tmin_sc_mean)
  sc2 <- data.frame(name = "soy_before_corn_nccpi2", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_sc_nccpi2_mean, yearagocrop = "S", nccpi = 2,
                   tmax = cnccpi$tmax_sc_mean, prcp = cnccpi$prcp_sc_mean, vpd = cnccpi$vp_sc_mean, tmin = cnccpi$tmin_sc_mean)
  sc3 <- data.frame(name = "soy_before_corn_nccpi3", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_sc_nccpi3_mean, yearagocrop = "S", nccpi = 3,
                   tmax = cnccpi$tmax_sc_mean, prcp = cnccpi$prcp_sc_mean, vpd = cnccpi$vp_sc_mean, tmin = cnccpi$tmin_sc_mean)
  sc4 <- data.frame(name = "soy_before_corn_nccpi4", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_sc_nccpi4_mean, yearagocrop = "S", nccpi = 4,
                   tmax = cnccpi$tmax_sc_mean, prcp = cnccpi$prcp_sc_mean, vpd = cnccpi$vp_sc_mean, tmin = cnccpi$tmin_sc_mean)
  sc5 <- data.frame(name = "soy_before_corn_nccpi5", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_sc_nccpi5_mean, yearagocrop = "S", nccpi = 5,
                   tmax = cnccpi$tmax_sc_mean, prcp = cnccpi$prcp_sc_mean, vpd = cnccpi$vp_sc_mean, tmin = cnccpi$tmin_sc_mean)
  
  cc1 <- data.frame(name = "corn_before_corn_nccpi1", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_cc_nccpi1_mean, yearagocrop = "C", nccpi = 1,
                    tmax = cnccpi$tmax_cc_mean, prcp = cnccpi$prcp_cc_mean, vpd = cnccpi$vp_cc_mean, tmin = cnccpi$tmin_cc_mean)
  cc2 <- data.frame(name = "corn_before_corn_nccpi2", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_cc_nccpi1_mean, yearagocrop = "C", nccpi = 2,
                    tmax = cnccpi$tmax_cc_mean, prcp = cnccpi$prcp_cc_mean, vpd = cnccpi$vp_cc_mean, tmin = cnccpi$tmin_cc_mean)
  cc3 <- data.frame(name = "corn_before_corn_nccpi3", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_cc_nccpi1_mean, yearagocrop = "C", nccpi = 3,
                    tmax = cnccpi$tmax_cc_mean, prcp = cnccpi$prcp_cc_mean, vpd = cnccpi$vp_cc_mean, tmin = cnccpi$tmin_cc_mean)
  cc4 <- data.frame(name = "corn_before_corn_nccpi4", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_cc_nccpi1_mean, yearagocrop = "C", nccpi = 4,
                    tmax = cnccpi$tmax_cc_mean, prcp = cnccpi$prcp_cc_mean, vpd = cnccpi$vp_cc_mean, tmin = cnccpi$tmin_cc_mean)
  cc5 <- data.frame(name = "corn_before_corn_nccpi5", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_cc_nccpi1_mean, yearagocrop = "C", nccpi = 5,
                    tmax = cnccpi$tmax_cc_mean, prcp = cnccpi$prcp_cc_mean, vpd = cnccpi$vp_cc_mean, tmin = cnccpi$tmin_cc_mean)
  
  cs1 <- data.frame(name = "corn_before_soy_nccpi1", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_cs_nccpi1_mean, yearagocrop = "C", nccpi = 1,
                    tmax = cnccpi$tmax_cs_mean, prcp = cnccpi$prcp_cs_mean, vpd = cnccpi$vp_cs_mean, tmin = cnccpi$tmin_cs_mean)
  cs2 <- data.frame(name = "corn_before_soy_nccpi2", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_cs_nccpi1_mean, yearagocrop = "C", nccpi = 2,
                    tmax = cnccpi$tmax_cs_mean, prcp = cnccpi$prcp_cs_mean, vpd = cnccpi$vp_cs_mean, tmin = cnccpi$tmin_cs_mean)
  cs3 <- data.frame(name = "corn_before_soy_nccpi3", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_cs_nccpi1_mean, yearagocrop = "C", nccpi = 3,
                    tmax = cnccpi$tmax_cs_mean, prcp = cnccpi$prcp_cs_mean, vpd = cnccpi$vp_cs_mean, tmin = cnccpi$tmin_cs_mean)
  cs4 <- data.frame(name = "corn_before_soy_nccpi4", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_cs_nccpi1_mean, yearagocrop = "C", nccpi = 4,
                    tmax = cnccpi$tmax_cs_mean, prcp = cnccpi$prcp_cs_mean, vpd = cnccpi$vp_cs_mean, tmin = cnccpi$tmin_cs_mean)
  cs5 <- data.frame(name = "corn_before_soy_nccpi5", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_cs_nccpi1_mean, yearagocrop = "C", nccpi = 5,
                    tmax = cnccpi$tmax_cs_mean, prcp = cnccpi$prcp_cs_mean, vpd = cnccpi$vp_cs_mean, tmin = cnccpi$tmin_cs_mean)
  
  ss1 <- data.frame(name = "soy_before_soy_nccpi1",year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_ss_nccpi1_mean, yearagocrop = "S", nccpi = 1,
                    tmax = cnccpi$tmax_ss_mean, prcp = cnccpi$prcp_ss_mean, vpd = cnccpi$vp_ss_mean, tmin = cnccpi$tmin_ss_mean)
  ss2 <- data.frame(name = "soy_before_soy_nccpi2", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_ss_nccpi1_mean, yearagocrop = "S", nccpi = 2,
                    tmax = cnccpi$tmax_ss_mean, prcp = cnccpi$prcp_ss_mean, vpd = cnccpi$vp_ss_mean, tmin = cnccpi$tmin_ss_mean)
  ss3 <- data.frame(name = "soy_before_soy_nccpi3", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_ss_nccpi1_mean, yearagocrop = "S", nccpi = 3,
                    tmax = cnccpi$tmax_ss_mean, prcp = cnccpi$prcp_ss_mean, vpd = cnccpi$vp_ss_mean, tmin = cnccpi$tmin_ss_mean)
  ss4 <- data.frame(name = "soy_before_soy_nccpi4", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_ss_nccpi1_mean, yearagocrop = "S", nccpi = 4,
                    tmax = cnccpi$tmax_ss_mean, prcp = cnccpi$prcp_ss_mean, vpd = cnccpi$vp_ss_mean, tmin = cnccpi$tmin_ss_mean)
  ss5 <- data.frame(name = "soy_before_soy_nccpi5", year = cnccpi$year, CRD = cnccpi$CRD, outcome_yield = cnccpi$wyield_ss_nccpi1_mean, yearagocrop = "S", nccpi = 5,
                    tmax = cnccpi$tmax_ss_mean, prcp = cnccpi$prcp_ss_mean, vpd = cnccpi$vp_ss_mean, tmin = cnccpi$tmin_ss_mean)
  
  regr = rbind(sc1, sc2, sc3, sc4, sc5, cc1, cc2, cc3, cc4, cc5, 
               cs1, cs2, cs3, cs4, cs5, ss1, ss2, ss3, ss4, ss5)
  return(regr)
}