weight_me = function(aux){
  a = ddply(aux, .(FIPS, year),
            function(x) data.frame(alfalfa_mean = weighted.mean(x$alfalfa_mean, x$alfalfa_count, na.rm=T),
                                   alfalfa_nccpi1_mean = weighted.mean(x$alfalfa_nccpi1_mean, x$alfalfa_nccpi1_count, na.rm=T),
                                   alfalfa_nccpi2_mean = weighted.mean(x$alfalfa_nccpi2_mean, x$alfalfa_nccpi2_count, na.rm=T),
                                   alfalfa_nccpi3_mean = weighted.mean(x$alfalfa_nccpi3_mean, x$alfalfa_nccpi3_count, na.rm=T),
                                   alfalfa_nccpi4_mean = weighted.mean(x$alfalfa_nccpi4_mean, x$alfalfa_nccpi4_count, na.rm=T),
                                   alfalfa_nccpi5_mean = weighted.mean(x$alfalfa_nccpi5_mean, x$alfalfa_nccpi5_count, na.rm=T),
                                   dbl_mean = weighted.mean(x$dbl_mean, x$dbl_count,na.rm=T),
                                   dbl_nccpi1_mean = weighted.mean(x$dbl_nccpi1_mean, x$dbl_nccpi1_count, na.rm=T),
                                   dbl_nccpi2_mean = weighted.mean(x$dbl_nccpi2_mean, x$dbl_nccpi2_count, na.rm=T),
                                   dbl_nccpi3_mean = weighted.mean(x$dbl_nccpi3_mean, x$dbl_nccpi3_count, na.rm=T),
                                   dbl_nccpi4_mean = weighted.mean(x$dbl_nccpi4_mean, x$dbl_nccpi4_count, na.rm=T),
                                   dbl_nccpi5_mean = weighted.mean(x$dbl_nccpi5_mean, x$dbl_nccpi5_count, na.rm=T),
                                   winwheat_mean = weighted.mean(x$winwheat_mean, x$winwheat_count,na.rm=T),
                                   winwheat_nccpi1_mean = weighted.mean(x$winwheat_nccpi1_mean, x$winwheat_nccpi1_count, na.rm=T),
                                   winwheat_nccpi2_mean = weighted.mean(x$winwheat_nccpi2_mean, x$winwheat_nccpi2_count, na.rm=T),
                                   winwheat_nccpi3_mean = weighted.mean(x$winwheat_nccpi3_mean, x$winwheat_nccpi3_count, na.rm=T),
                                   winwheat_nccpi4_mean = weighted.mean(x$winwheat_nccpi4_mean, x$winwheat_nccpi4_count, na.rm=T),
                                   winwheat_nccpi5_mean = weighted.mean(x$winwheat_nccpi5_mean, x$winwheat_nccpi5_count, na.rm=T),
                                   oats_mean = weighted.mean(x$oats_mean, x$oats_count,na.rm=T),
                                   oats_nccpi1_mean = weighted.mean(x$oats_nccpi1_mean, x$oats_nccpi1_count, na.rm=T),
                                   oats_nccpi2_mean = weighted.mean(x$oats_nccpi2_mean, x$oats_nccpi2_count, na.rm=T),
                                   oats_nccpi3_mean = weighted.mean(x$oats_nccpi3_mean, x$oats_nccpi3_count, na.rm=T),
                                   oats_nccpi4_mean = weighted.mean(x$oats_nccpi4_mean, x$oats_nccpi4_count, na.rm=T),
                                   oats_nccpi5_mean = weighted.mean(x$oats_nccpi5_mean, x$oats_nccpi5_count, na.rm=T),
                                   othcrops_mean = weighted.mean(x$othcrops_mean, x$othcrops_count,na.rm=T),
                                   othcrops_nccpi1_mean = weighted.mean(x$othcrops_nccpi1_mean, x$othcrops_nccpi1_count, na.rm=T),
                                   othcrops_nccpi2_mean = weighted.mean(x$othcrops_nccpi2_mean, x$othcrops_nccpi2_count, na.rm=T),
                                   othcrops_nccpi3_mean = weighted.mean(x$othcrops_nccpi3_mean, x$othcrops_nccpi3_count, na.rm=T),
                                   othcrops_nccpi4_mean = weighted.mean(x$othcrops_nccpi4_mean, x$othcrops_nccpi4_count, na.rm=T),
                                   othcrops_nccpi5_mean = weighted.mean(x$othcrops_nccpi5_mean, x$othcrops_nccpi5_count, na.rm=T),
                                   othsmlgrns_mean = weighted.mean(x$othsmlgrns_mean, x$othsmlgrns_count,na.rm=T),
                                   othsmlgrns_nccpi1_mean = weighted.mean(x$othsmlgrns_nccpi1_mean, x$othsmlgrns_nccpi1_count, na.rm=T),
                                   othsmlgrns_nccpi2_mean = weighted.mean(x$othsmlgrns_nccpi2_mean, x$othsmlgrns_nccpi2_count, na.rm=T),
                                   othsmlgrns_nccpi3_mean = weighted.mean(x$othsmlgrns_nccpi3_mean, x$othsmlgrns_nccpi3_count, na.rm=T),
                                   othsmlgrns_nccpi4_mean = weighted.mean(x$othsmlgrns_nccpi4_mean, x$othsmlgrns_nccpi4_count, na.rm=T),
                                   othsmlgrns_nccpi5_mean = weighted.mean(x$othsmlgrns_nccpi5_mean, x$othsmlgrns_nccpi5_count, na.rm=T),
                                   alfalfa_count = x$alfalfa_count, alfalfa_nccpi1_count = x$alfalfa_nccpi1_count, alfalfa_nccpi2_count = x$alfalfa_nccpi2_count, alfalfa_nccpi3_count = x$alfalfa_nccpi3_count, alfalfa_nccpi4_count = x$alfalfa_nccpi4_count, alfalfa_nccpi5_count = x$alfalfa_nccpi5_count, 
                                   dbl_count = x$dbl_count, dbl_nccpi1_count = x$dbl_nccpi1_count, dbl_nccpi2_count = x$dbl_nccpi2_count, dbl_nccpi3_count = x$dbl_nccpi3_count, dbl_nccpi4_count = x$dbl_nccpi4_count, dbl_nccpi5_count = x$dbl_nccpi5_count, 
                                   winwheat_count = x$winwheat_count, winwheat_nccpi1_count = x$winwheat_nccpi1_count, winwheat_nccpi2_count = x$winwheat_nccpi2_count, winwheat_nccpi3_count = x$winwheat_nccpi3_count, winwheat_nccpi4_count = x$winwheat_nccpi4_count, winwheat_nccpi5_count = x$winwheat_nccpi5_count, 
                                   oats_count = x$oats_count, oats_nccpi1_count = x$oats_nccpi1_count, oats_nccpi2_count = x$oats_nccpi2_count, oats_nccpi3_count = x$oats_nccpi3_count, oats_nccpi4_count = x$oats_nccpi4_count, oats_nccpi5_count = x$oats_nccpi5_count, 
                                   othcrops_count = x$othcrops_count, othcrops_nccpi1_count = x$othcrops_nccpi1_count, othcrops_nccpi2_count = x$othcrops_nccpi2_count, othcrops_nccpi3_count = x$othcrops_nccpi3_count, othcrops_nccpi4_count = x$othcrops_nccpi4_count, othcrops_nccpi5_count = x$othcrops_nccpi5_count, 
                                   othsmlgrns_count = x$othsmlgrns_count, othsmlgrns_nccpi1_count = x$othsmlgrns_nccpi1_count, othsmlgrns_nccpi2_count = x$othsmlgrns_nccpi2_count, othsmlgrns_nccpi3_count = x$othsmlgrns_nccpi3_count, othsmlgrns_nccpi4_count = x$othsmlgrns_nccpi4_count, othsmlgrns_nccpi5_count = x$othsmlgrns_nccpi5_count
            )
  )
  return(a)
}

minor_setup = function() {
  alfalfa = as.data.frame(fread('corn_after_alfalfa.csv', check.names=TRUE))
  alfalfa_df = data.frame(alfalfa$FIPS.formula, alfalfa$year, alfalfa$mean, alfalfa$count)
  names(alfalfa_df) = c("FIPS", "year", "alfalfa_mean", "alfalfa_count")
  
  alfalfa_n = as.data.frame(fread('corn_after_alfalfa_nccpi.csv', check.names=TRUE))
  alfalfa_n_df = data.frame(alfalfa_n$FIPS.formula, alfalfa_n$year,
                            alfalfa_n$yield_after_alfalfa_nccpi1_mean, alfalfa_n$yield_after_alfalfa_nccpi1_count,
                            alfalfa_n$yield_after_alfalfa_nccpi2_mean, alfalfa_n$yield_after_alfalfa_nccpi2_count,
                            alfalfa_n$yield_after_alfalfa_nccpi3_mean, alfalfa_n$yield_after_alfalfa_nccpi3_count,
                            alfalfa_n$yield_after_alfalfa_nccpi4_mean, alfalfa_n$yield_after_alfalfa_nccpi4_count,
                            alfalfa_n$yield_after_alfalfa_nccpi5_mean, alfalfa_n$yield_after_alfalfa_nccpi5_count)
  names(alfalfa_n_df) = c("FIPS", "year", "alfalfa_nccpi1_mean", "alfalfa_nccpi1_count",
                          "alfalfa_nccpi2_mean", "alfalfa_nccpi2_count", "alfalfa_nccpi3_mean", "alfalfa_nccpi3_count",
                          "alfalfa_nccpi4_mean", "alfalfa_nccpi4_count", "alfalfa_nccpi5_mean", "alfalfa_nccpi5_count")
  
  dbl = as.data.frame(fread('corn_after_dbl.csv', check.names=TRUE))
  dbl_df = data.frame(dbl$FIPS.formula, dbl$year, dbl$mean, dbl$count)
  names(dbl_df) = c("FIPS", "year", "dbl_mean", "dbl_count")
  
  dbl_n = as.data.frame(fread('corn_after_dbl_nccpi.csv', check.names=TRUE))
  dbl_n_df = data.frame(dbl_n$FIPS.formula, dbl_n$year,
                        dbl_n$yield_after_dbl_nccpi1_mean, dbl_n$yield_after_dbl_nccpi1_count,
                        dbl_n$yield_after_dbl_nccpi2_mean, dbl_n$yield_after_dbl_nccpi2_count,
                        dbl_n$yield_after_dbl_nccpi3_mean, dbl_n$yield_after_dbl_nccpi3_count,
                        dbl_n$yield_after_dbl_nccpi4_mean, dbl_n$yield_after_dbl_nccpi4_count,
                        dbl_n$yield_after_dbl_nccpi5_mean, dbl_n$yield_after_dbl_nccpi5_count)
  names(dbl_n_df) = c("FIPS", "year", "dbl_nccpi1_mean", "dbl_nccpi1_count",
                      "dbl_nccpi2_mean", "dbl_nccpi2_count", "dbl_nccpi3_mean", "dbl_nccpi3_count",
                      "dbl_nccpi4_mean", "dbl_nccpi4_count", "dbl_nccpi5_mean", "dbl_nccpi5_count")
  
  
  winwheat = as.data.frame(fread('corn_after_winwheat.csv', check.names=TRUE))
  winwheat_df = data.frame(winwheat$FIPS.formula, winwheat$year, winwheat$mean, winwheat$count)
  names(winwheat_df) = c("FIPS", "year", "winwheat_mean", "winwheat_count")
  
  winwheat_n = as.data.frame(fread('corn_after_winwheat_nccpi.csv', check.names=TRUE))
  winwheat_n_df = data.frame(winwheat_n$FIPS.formula, winwheat_n$year,
                             winwheat_n$yield_after_winwheat_nccpi1_mean, winwheat_n$yield_after_winwheat_nccpi1_count,
                             winwheat_n$yield_after_winwheat_nccpi2_mean, winwheat_n$yield_after_winwheat_nccpi2_count,
                             winwheat_n$yield_after_winwheat_nccpi3_mean, winwheat_n$yield_after_winwheat_nccpi3_count,
                             winwheat_n$yield_after_winwheat_nccpi4_mean, winwheat_n$yield_after_winwheat_nccpi4_count,
                             winwheat_n$yield_after_winwheat_nccpi5_mean, winwheat_n$yield_after_winwheat_nccpi5_count)
  names(winwheat_n_df) = c("FIPS", "year", "winwheat_nccpi1_mean", "winwheat_nccpi1_count",
                           "winwheat_nccpi2_mean", "winwheat_nccpi2_count", "winwheat_nccpi3_mean", "winwheat_nccpi3_count",
                           "winwheat_nccpi4_mean", "winwheat_nccpi4_count", "winwheat_nccpi5_mean", "winwheat_nccpi5_count")
  
  oats = as.data.frame(fread('corn_after_oats.csv', check.names=TRUE))
  oats_df = data.frame(oats$FIPS.formula, oats$year, oats$mean, oats$count)
  names(oats_df) = c("FIPS", "year", "oats_mean", "oats_count")
  
  oats_n = as.data.frame(fread('corn_after_oats_nccpi.csv', check.names=TRUE))
  oats_n_df = data.frame(oats_n$FIPS.formula, oats_n$year,
                         oats_n$yield_after_oats_nccpi1_mean, oats_n$yield_after_oats_nccpi1_count,
                         oats_n$yield_after_oats_nccpi2_mean, oats_n$yield_after_oats_nccpi2_count,
                         oats_n$yield_after_oats_nccpi3_mean, oats_n$yield_after_oats_nccpi3_count,
                         oats_n$yield_after_oats_nccpi4_mean, oats_n$yield_after_oats_nccpi4_count,
                         oats_n$yield_after_oats_nccpi5_mean, oats_n$yield_after_oats_nccpi5_count)
  names(oats_n_df) = c("FIPS", "year", "oats_nccpi1_mean", "oats_nccpi1_count",
                       "oats_nccpi2_mean", "oats_nccpi2_count", "oats_nccpi3_mean", "oats_nccpi3_count",
                       "oats_nccpi4_mean", "oats_nccpi4_count", "oats_nccpi5_mean", "oats_nccpi5_count")
  
  othcrops = as.data.frame(fread('corn_after_othcrops.csv', check.names=TRUE))
  othcrops_df = data.frame(othcrops$FIPS.formula, othcrops$year, othcrops$mean, othcrops$count)
  names(othcrops_df) = c("FIPS", "year", "othcrops_mean", "othcrops_count")
  
  othcrops_n = as.data.frame(fread('corn_after_othcrops_nccpi.csv', check.names=TRUE))
  othcrops_n_df = data.frame(othcrops_n$FIPS.formula, othcrops_n$year,
                             othcrops_n$yield_after_othcrops_nccpi1_mean, othcrops_n$yield_after_othcrops_nccpi1_count,
                             othcrops_n$yield_after_othcrops_nccpi2_mean, othcrops_n$yield_after_othcrops_nccpi2_count,
                             othcrops_n$yield_after_othcrops_nccpi3_mean, othcrops_n$yield_after_othcrops_nccpi3_count,
                             othcrops_n$yield_after_othcrops_nccpi4_mean, othcrops_n$yield_after_othcrops_nccpi4_count,
                             othcrops_n$yield_after_othcrops_nccpi5_mean, othcrops_n$yield_after_othcrops_nccpi5_count)
  names(othcrops_n_df) = c("FIPS", "year", "othcrops_nccpi1_mean", "othcrops_nccpi1_count",
                           "othcrops_nccpi2_mean", "othcrops_nccpi2_count", "othcrops_nccpi3_mean", "othcrops_nccpi3_count",
                           "othcrops_nccpi4_mean", "othcrops_nccpi4_count", "othcrops_nccpi5_mean", "othcrops_nccpi5_count")
  
  othsmlgrns = as.data.frame(fread('corn_after_othrsmlgrns.csv', check.names=TRUE))
  othsmlgrns_df = data.frame(othsmlgrns$FIPS.formula, othsmlgrns$year, othsmlgrns$mean, othsmlgrns$count)
  names(othsmlgrns_df) = c("FIPS", "year", "othsmlgrns_mean", "othsmlgrns_count")
  
  othsmlgrns_n = as.data.frame(fread('corn_after_othrsmlgrns_nccpi.csv', check.names=TRUE))
  othsmlgrns_n_df = data.frame(othsmlgrns_n$FIPS.formula, othsmlgrns_n$year,
                               othsmlgrns_n$yield_after_othrsmlgrns_nccpi1_mean, othsmlgrns_n$yield_after_othrsmlgrns_nccpi1_count,
                               othsmlgrns_n$yield_after_othrsmlgrns_nccpi2_mean, othsmlgrns_n$yield_after_othrsmlgrns_nccpi2_count,
                               othsmlgrns_n$yield_after_othrsmlgrns_nccpi3_mean, othsmlgrns_n$yield_after_othrsmlgrns_nccpi3_count,
                               othsmlgrns_n$yield_after_othrsmlgrns_nccpi4_mean, othsmlgrns_n$yield_after_othrsmlgrns_nccpi4_count,
                               othsmlgrns_n$yield_after_othrsmlgrns_nccpi5_mean, othsmlgrns_n$yield_after_othrsmlgrns_nccpi5_count)
  names(othsmlgrns_n_df) = c("FIPS", "year", "othsmlgrns_nccpi1_mean", "othsmlgrns_nccpi1_count",
                             "othsmlgrns_nccpi2_mean", "othsmlgrns_nccpi2_count", "othsmlgrns_nccpi3_mean", "othsmlgrns_nccpi3_count",
                             "othsmlgrns_nccpi4_mean", "othsmlgrns_nccpi4_count", "othsmlgrns_nccpi5_mean", "othsmlgrns_nccpi5_count")
  
  aux = Reduce(function(x, y) merge(x, y, all=TRUE), list(alfalfa_df, alfalfa_n_df, dbl_df, dbl_n_df, winwheat_df, winwheat_n_df, oats_df, oats_n_df, othcrops_df, othcrops_n_df, othsmlgrns_df, othsmlgrns_n_df))
  aux = weight_me(aux)
  return(aux)
}