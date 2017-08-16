weight_me_soy = function(aux){
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
                                   
                                   winwheat_nccpi3_mean = weighted.mean(x$winwheat_nccpi3_mean, x$winwheat_nccpi3_count, na.omit(x)),#na.rm=T),
                                   winwheat_nccpi4_mean = weighted.mean(x$winwheat_nccpi4_mean, x$winwheat_nccpi4_count, na.rm=T),
                                   winwheat_nccpi5_mean = weighted.mean(x$winwheat_nccpi5_mean, x$winwheat_nccpi5_count, na.rm=T),
                                   oats_mean = weighted.mean(x$oats_mean, x$oats_count,na.rm=T),
                                   oats_nccpi1_mean = weighted.mean(x$oats_nccpi1_mean, x$oats_nccpi1_count, na.rm=T),
                                   oats_nccpi2_mean = weighted.mean(x$oats_nccpi2_mean, x$oats_nccpi2_count, na.rm=T),
                                   oats_nccpi3_mean = weighted.mean(x$oats_nccpi3_mean, x$oats_nccpi3_count, na.rm=T),
                                   oats_nccpi4_mean = weighted.mean(x$oats_nccpi4_mean, x$oats_nccpi4_count, na.rm=T),
                                   oats_nccpi5_mean = weighted.mean(x$oats_nccpi5_mean, x$oats_nccpi5_count, na.rm=T),
                            
                                   alfalfa_count = x$alfalfa_count, alfalfa_nccpi1_count = x$alfalfa_nccpi1_count, alfalfa_nccpi2_count = x$alfalfa_nccpi2_count, alfalfa_nccpi3_count = x$alfalfa_nccpi3_count, alfalfa_nccpi4_count = x$alfalfa_nccpi4_count, alfalfa_nccpi5_count = x$alfalfa_nccpi5_count, 
                                   dbl_count = x$dbl_count, dbl_nccpi1_count = x$dbl_nccpi1_count, dbl_nccpi2_count = x$dbl_nccpi2_count, dbl_nccpi3_count = x$dbl_nccpi3_count, dbl_nccpi4_count = x$dbl_nccpi4_count, dbl_nccpi5_count = x$dbl_nccpi5_count, 
                                   winwheat_count = x$winwheat_count, winwheat_nccpi1_count = x$winwheat_nccpi1_count, winwheat_nccpi3_count = x$winwheat_nccpi3_count, winwheat_nccpi4_count = x$winwheat_nccpi4_count, winwheat_nccpi5_count = x$winwheat_nccpi5_count, 
                                   oats_count = x$oats_count, oats_nccpi1_count = x$oats_nccpi1_count, oats_nccpi2_count = x$oats_nccpi2_count, oats_nccpi3_count = x$oats_nccpi3_count, oats_nccpi4_count = x$oats_nccpi4_count, oats_nccpi5_count = x$oats_nccpi5_count
                                   
            )
  )
  return(a)
}

minor_setup_soy = function() {

  alfalfa_n = as.data.frame(fread('soy_after_alfalfa_nccpi.csv', check.names=TRUE))
  alfalfa_n_df = data.frame(alfalfa_n$FIPS.formula, alfalfa_n$year, alfalfa_n$soy_after_alfalfa_mean, alfalfa_n$soy_after_alfalfa_count, 
                            alfalfa_n$soy_after_alfalfa_nccpi1_mean, alfalfa_n$soy_after_alfalfa_nccpi1_count,
                            alfalfa_n$soy_after_alfalfa_nccpi2_mean, alfalfa_n$soy_after_alfalfa_nccpi2_count,
                            alfalfa_n$soy_after_alfalfa_nccpi3_mean, alfalfa_n$soy_after_alfalfa_nccpi3_count,
                            alfalfa_n$soy_after_alfalfa_nccpi4_mean, alfalfa_n$soy_after_alfalfa_nccpi4_count,
                            alfalfa_n$soy_after_alfalfa_nccpi5_mean, alfalfa_n$soy_after_alfalfa_nccpi5_count)
  names(alfalfa_n_df) = c("FIPS", "year", "alfalfa_mean", "alfalfa_count", "alfalfa_nccpi1_mean", "alfalfa_nccpi1_count",
                          "alfalfa_nccpi2_mean", "alfalfa_nccpi2_count", "alfalfa_nccpi3_mean", "alfalfa_nccpi3_count",
                          "alfalfa_nccpi4_mean", "alfalfa_nccpi4_count", "alfalfa_nccpi5_mean", "alfalfa_nccpi5_count")

  dbl_n = as.data.frame(fread('soy_after_dbl_nccpi.csv', check.names=TRUE))
  dbl_n_df = data.frame(dbl_n$FIPS.formula, dbl_n$year, dbl_n$soy_after_dbl_mean, dbl_n$soy_after_dbl_count,
                        dbl_n$soy_after_dbl_nccpi1_mean, dbl_n$soy_after_dbl_nccpi1_count,
                        dbl_n$soy_after_dbl_nccpi2_mean, dbl_n$soy_after_dbl_nccpi2_count,
                        dbl_n$soy_after_dbl_nccpi3_mean, dbl_n$soy_after_dbl_nccpi3_count,
                        dbl_n$soy_after_dbl_nccpi4_mean, dbl_n$soy_after_dbl_nccpi4_count,
                        dbl_n$soy_after_dbl_nccpi5_mean, dbl_n$soy_after_dbl_nccpi5_count)
  names(dbl_n_df) = c("FIPS", "year", "dbl_mean", "dbl_count", "dbl_nccpi1_mean", "dbl_nccpi1_count",
                      "dbl_nccpi2_mean", "dbl_nccpi2_count", "dbl_nccpi3_mean", "dbl_nccpi3_count",
                      "dbl_nccpi4_mean", "dbl_nccpi4_count", "dbl_nccpi5_mean", "dbl_nccpi5_count")
  
  winwheat_n = as.data.frame(fread('soy_after_winwheat_nccpi.csv', check.names=TRUE))
  winwheat_n_df = data.frame(winwheat_n$FIPS.formula, winwheat_n$year, winwheat_n$soy_after_winwheat_mean, winwheat_n$soy_after_winwheat_count,
                             winwheat_n$soy_after_winwheat_nccpi1_mean, winwheat_n$soy_after_winwheat_nccpi1_count,
                             winwheat_n$soy_after_winwheat_nccpi2_count,
                             winwheat_n$soy_after_winwheat_nccpi3_mean, winwheat_n$soy_after_winwheat_nccpi3_count,
                             winwheat_n$soy_after_winwheat_nccpi4_mean, winwheat_n$soy_after_winwheat_nccpi4_count,
                             winwheat_n$soy_after_winwheat_nccpi5_mean, winwheat_n$soy_after_winwheat_nccpi5_count)
  names(winwheat_n_df) = c("FIPS", "year", "winwheat_mean", "winwheat_count", "winwheat_nccpi1_mean", "winwheat_nccpi1_count",
                           "winwheat_nccpi2_count", "winwheat_nccpi3_mean", "winwheat_nccpi3_count",
                           "winwheat_nccpi4_mean", "winwheat_nccpi4_count", "winwheat_nccpi5_mean", "winwheat_nccpi5_count")

  oats_n = as.data.frame(fread('soy_after_oats_nccpi.csv', check.names=TRUE))
  oats_n_df = data.frame(oats_n$FIPS.formula, oats_n$year, oats_n$soy_after_oats_mean, oats_n$soy_after_oats_count,
                         oats_n$soy_after_oats_nccpi1_mean, oats_n$soy_after_oats_nccpi1_count,
                         oats_n$soy_after_oats_nccpi2_mean, oats_n$soy_after_oats_nccpi2_count,
                         oats_n$soy_after_oats_nccpi3_mean, oats_n$soy_after_oats_nccpi3_count,
                         oats_n$soy_after_oats_nccpi4_mean, oats_n$soy_after_oats_nccpi4_count,
                         oats_n$soy_after_oats_nccpi5_mean, oats_n$soy_after_oats_nccpi5_count)
  names(oats_n_df) = c("FIPS", "year", "oats_mean", "oats_count", "oats_nccpi1_mean", "oats_nccpi1_count",
                       "oats_nccpi2_mean", "oats_nccpi2_count", "oats_nccpi3_mean", "oats_nccpi3_count",
                       "oats_nccpi4_mean", "oats_nccpi4_count", "oats_nccpi5_mean", "oats_nccpi5_count")

  soy_aux = Reduce(function(x, y) merge(x, y, all=TRUE), list(alfalfa_n_df, dbl_n_df, winwheat_n_df, oats_n_df))
  soy_aux = weight_me_soy(soy_aux)
  return(aux)
}