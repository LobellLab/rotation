
require(dplyr)
require(classInt)
require(fields)
#crop=''#change this to run for maize ('') or soy ('soy')

#yielddir='S:/Lobell_Lab/satellite/output/by_state'
#setwd(yielddir)
#outdir='c:/Users/dlobell/Documents/papers/scym_longhist/'
#for making maps and labeling counties in plots
#load('c:/climate_fse/USshapefiles.Rdata') #loads cnty and states sp objects
load("USshapefiles.Rdata")
#load(file=paste0(outdir,'caleq.Rdata'))

cnames=paste(ctab$NAME,ctab$STATE_NAME,sep=', ')


map.var=function(x, xleg, yleg, legend=TRUE,leg_size = 0.6, mask=NULL,titl,nclass,pal=rainbow(10,end=0.8),xlim=c(-97,-85),ylim=c(37,44),sym=F,breaks=NULL,ncol=2, district = "FIPS") {
  # make vector with same order as cnty object
  #   if (is.null(mask)) mask = seq(1:length(x))
  #   x2=data.frame(counties,x)[mask,] # 'counties' comes from gcmyields.Rdata, different than fips.found in weather data
  if(district=="FIPS") {
    newx=merge(data.frame(fips=ctab$FIPS),x,by=1,all.x=T,sort=F)
    newx=merge(data.frame(fips=ctab$FIPS),newx,all=T,sort=F)[,2]  #for some reason this seems needed to sort in right order, first doesn't do it
  } else { 
    CRDd = read.csv("FIPS_CRD.csv") # I can't find anything in ctab that's used other than FIPS so it seems like I should be able to switch this table in
    newx=merge(data.frame(fips=CRDd$CRD),x,by=1,all.x=T,sort=F)
    newx=merge(data.frame(fips=CRDd$CRD),newx,all=T,sort=F)[,2]  #for some reason this seems needed to sort in right order, first doesn't do it
  }
  # print(head(newx))
  symra=round(range(1.1*c(newx,-1*newx),na.rm=T))
  if (sym==T) {
    q10=classIntervals(newx,n=nclass,style='fixed',fixedBreaks=seq(symra[1],symra[2],length.out=nclass+1))
  } else if (!is.null(breaks)) {
    nFixedClasses = length(breaks)-1
    q10=classIntervals(newx,n=nFixedClasses,style='fixed',fixedBreaks=breaks)
  } else {
    q10=classIntervals(newx,n=nclass,style='pretty')
  }
  pcolors=findColours(q10,pal,under='<',over='>',cut=F)
  plot(cnty,border=gray(.5),col=pcolors,xlim=xlim,ylim=ylim,asp=1,lwd=1+3*is.finite(newx))
  plot(states,border=gray(.1),lwd=2,add=T)
#  legend('topright',ncol=ncol,fill=attr(pcolors,"palette"),legend=names(attr(pcolors,"table")),bg="white")
  if(legend){
    legend(x=xleg, y=yleg, ncol=1,fill=attr(pcolors,"palette"),legend=names(attr(pcolors,"table")),bg="white", cex = leg_size)
  }
  degAxis(1)
  degAxis(2,las=1)
  box()
  title(titl,cex=0.75, adj=0)
}

plotgap.bars=function(fips,use.cal=F,ylim=c(8,16),tlabs=''){
  temp=alldat[alldat$fips==fips & alldat$yr>0,]
  #first do the bias correction ?
  if (use.cal) temp[,c(3:9)] = temp[,3:9] * allcal$coef[2] + allcal$coef[1]
  
  tempav = apply(temp,2,function(x) mean(as.numeric(x),na.rm=T))
  pd = tempav[c('field_yavg','field_y95','uy')]
  pd2=as.numeric(avgap2[avgap2$fips==fips,c('infieldgap','fullgap')])
  pd3=c(pd,pd[1]*c(1,1+pd2/100))
  
  labs=c('Overall \n average \n yield',"Average of \n field's 95th \n percentile",
         'Overall 95th \n percentile \n of yield')
  a=barplot(pd,ylim=ylim,space=.2,xpd=F,names=labs,ylab='SCYM Yield (ton/ha)',cex.names=1,padj=.4)
  percup = 100*(pd[-1]/pd[1]-1)
#  tlabs=c(substitute(paste('YH'[IF],' = ',pup,'%'),list(pup=round(percup[1],0))),
#          substitute(paste('YH'[O],' = ',pup,'%'),list(pup=round(percup[2],0))))
#just hard coding the labels, since hard to make subscripts   
 #    leg=substitute(paste(lab,', ',R^2,' = ',cr),list(lab=labs[crop],cr=round(cor1^2,2)))
  print(percup)
    text(a[2:3],pd[2:3],tlabs,pos=3)
#  title(cnames[ctab$FIPS==fips])
}


plotgap.ts=function(fips,use.cal=F,ylim=c(0,60)){
  temp=alldat[alldat$fips==fips & alldat$yr>0,]
  #first do the bias correction ?
  
  ord=order(temp$yr)
  temp=temp[ord,]
  plot(temp$yr,temp$fullgap,type='o',xlab='Year',ylab=expression(paste('YH'[O],' or YH'[IF],' (% of Y'[avg],')')),
       ylim=ylim, main='',pch=19)
  lines(temp$yr,temp$fieldgap,col=2,type='o')
  fit1=lm(temp$fullgap~temp$yr)
  fit2=lm(temp$fieldgap~temp$yr)
  abline(fit1$coef,lty=2,col=1)
  abline(fit2$coef,lty=2,col=2)
  legend('topleft',pch=19,col=1:2,leg=c(expression('YH'[O]),expression('YH'[IF])),bty='n',cex=1.4)
}

plotperc.ts=function(fips,perc=maizeperc,use.cal=F,ylim=c(0,60)){
  temp=perc[perc$FIPS.formula ==fips,]
  #first do the bias correction ?
  pd=temp[,c('year','p5','p10','p50','p90','p95')]
  ylim=range(pd[,-1])
  plot(pd$year,pd$p10,xlab='Year',ylab='Yield',ylim=ylim,type='l')
  lines(pd$year,pd$p90)
  plot(pd$year,pd$p90-pd$p10,xlab='Year',ylab='Yield',type='l')

  #  fit1=lm(temp$fullgap~temp$yr)
  # fit2=lm(temp$fieldgap~temp$yr)
  # abline(fit1$coef,lty=2,col=1)
  # abline(fit2$coef,lty=2,col=2)
  # legend('topleft',pch=19,col=1:2,leg=c(expression('YH'[O]),expression('YH'[IF])),bty='n',cex=1.4)
}

plotfrac.ts = function(fips,ylim=c(10,35),crop='maize'){
  if (crop == 'maize') means=maizemeans else means = soymeans
    temp=means[means$FIPS.formula ==fips,]
    plot(temp$year,temp$frac80,type='l',xlab='Year',ylab='% of Production',
         ylim=ylim, main='',pch=19,lwd=2)
    lines(temp$year,temp$frac90,col=2,type='l',lwd=2)
    fit1=lm(temp$frac80~temp$year)
    fit2=lm(temp$frac90~temp$year)
    abline(fit1$coef,lty=2,col=1)
    abline(fit2$coef,lty=2,col=2)
    
}

#PLOTS OF NASS MEANS AND TRENDS TO COMPARE WITH SCYM MAPS
# f=read.csv('../../nass/nass_cornsoy_sep16.csv')
# f$cntyfips=f$County.ANSI
# f$cntyfips=paste0(substr(rep('00',nrow(f)),rep(1,nrow(f)),3-nchar(f$County.ANSI)),f$cntyfips)
# f$fips=paste0(f$State.ANSI,f$cntyfips)
# f$Value=as.numeric(gsub(',','',f$Value))
# ufips=unique(f$fips)
# setwd(outdir)
# vars=c('Year','Value')
# nassval = lapply(ufips,function(x) {
#   temp=list()
#   temp$corn=f[f$fips == x & f$Data.Item == "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE",vars]
#   temp$cornarea=f[f$fips == x & f$Data.Item == "CORN, GRAIN - ACRES HARVESTED",vars]
#   temp$soy=f[f$fips == x & f$Data.Item == "SOYBEANS - YIELD, MEASURED IN BU / ACRE",vars]
#   temp$soyarea=f[f$fips == x & f$Data.Item == "SOYBEANS - ACRES HARVESTED",vars]
#   temp
# })
# 
# cornvals = t(sapply(nassval,function(x) {
#   yr = x$corn$Year
#   yld = x$corn$Value
#   if (length(yld) > 10){
#   yavg = mean(yld,na.rm=T)
#   ytrend = lm(yld~yr)$coef[2]
#   c(yavg,ytrend)} else c(NA,NA)
# }))
# cornvals=cornvals/15.9
# 
# soyvals = t(sapply(nassval,function(x) {
#   yr = x$soy$Year
#   yld = x$soy$Value
#   if (length(yld) > 10){
#     yavg = mean(yld,na.rm=T)
#     ytrend = lm(yld~yr)$coef[2]
#     c(yavg,ytrend)} else c(NA,NA)
# }))
# soyvals=soyvals/14.9
# 
# #calc avg production for later
# cornprod = sapply(nassval,function(x) {
#   yr = x$corn$Year
#   yld = x$corn$Value
#   area = x$cornarea$Value
#   avprod = mean(yld*area)
# })
# soyprod = sapply(nassval,function(x) {
#   yr = x$soy$Year
#   yld = x$soy$Value
#   area = x$soyarea$Value
#   avprod = mean(yld*area)
# })
# 
# 
# x11(w=15,h=10)
# par(mfrow=c(2,2))
# x=data.frame(fips=ufips,my = cornvals[,1])
# breaks=seq(8,12,1)
# mpal=tim.colors(100)[1:90]
# map.var(x,titl='nass mean maize yield',pal=mpal,
#         breaks=breaks)
# x=data.frame(fips=ufips,my = cornvals[,2])
# breaks=seq(-.4,.4,.1)
# tpal=rev(brewer.pal(8,'RdBu'))
# map.var(x,titl='nass trend maize yield',pal=tpal,
#         breaks=breaks)
# x=data.frame(fips=ufips,my = soyvals[,1])
# breaks=seq(2,3.4,.2)
# mpal=tim.colors(100)[1:90]
# map.var(x,titl='nass mean soy yield',pal=mpal,
#         breaks=breaks)
# x=data.frame(fips=ufips,my = soyvals[,2])
# breaks=seq(-.1,.1,.02)
# tpal=rev(brewer.pal(8,'RdBu'))
# map.var(x,titl='nass trend soy yield',pal=tpal,
#         breaks=breaks)
# 
# #PLOTS OF PERCENTILES BY COUNTY AND TRENDS FOR DIFFERENT PERCENTILES
# maizeperc=read.csv('c:/climate_fse/satellite/maize_perc_byyr.csv')
# soyperc=read.csv('c:/climate_fse/satellite/soy_perc_byyr.csv')
# maizemean=read.csv('c:/climate_fse/satellite/maize_mean_byyr.csv')
# soymean=read.csv('c:/climate_fse/satellite/soy_mean_byyr.csv')
# maizemean80=read.csv('c:/climate_fse/satellite/maize_mean80_byyr.csv')
# soymean80=read.csv('c:/climate_fse/satellite/soy_mean80_byyr.csv')
# maizemean90=read.csv('c:/climate_fse/satellite/maize_mean90_byyr.csv')
# soymean90=read.csv('c:/climate_fse/satellite/soy_mean90_byyr.csv')
# 
# mcal=function(x) holdcal$maize$coef[1] + holdcal$maize$coef[2]*x
# scal=function(x) holdcal$soy$coef[1] + holdcal$soy$coef[2]*x
# 
# maizemeans=data.frame(maizemean,mean80 = maizemean80$mean,mean90 = maizemean90$mean)
# maizemeans$frac80 = mcal(maizemeans$mean80)/mcal(maizemeans$mean) * 20
# maizemeans$frac90 = mcal(maizemeans$mean90)/mcal(maizemeans$mean) * 10
# 
# soymeans=data.frame(soymean,mean80 = soymean80$mean,mean90 = soymean90$mean)
# soymeans$frac80 = scal(soymeans$mean80)/scal(soymeans$mean) * 20
# soymeans$frac90 = scal(soymeans$mean90)/scal(soymeans$mean) * 10
# 
# years=unique(maizemeans$year)
# #use avg production to weight counties. shows avg fraction of counties prod
# #from top x% of fields
# mfbyyr=t(sapply(years, function(x){
#   temp=maizemeans[which(maizemeans$year == x),]
#   temp2 = merge(temp,data.frame(fips=ufips,prod=cornprod),by.x='FIPS.formula',by.y='fips',all.x=T)
#   out=c(sum(temp2$frac80 * temp2$prod,na.rm=T)/sum(temp2$prod,na.rm=T),
#   sum(temp2$frac90 * temp2$prod,na.rm=T)/sum(temp2$prod,na.rm=T))
# }))
# 
# sfbyyr=t(sapply(years, function(x){
#   temp=soymeans[which(soymeans$year == x),]
#   temp2 = merge(temp,data.frame(fips=ufips,prod=soyprod),by.x='FIPS.formula',by.y='fips',all.x=T)
#   out=c(sum(temp2$frac80 * temp2$prod,na.rm=T)/sum(temp2$prod,na.rm=T),
#         sum(temp2$frac90 * temp2$prod,na.rm=T)/sum(temp2$prod,na.rm=T))
# }))
# 
# ind=1#20% highest fields
# cols=c(3,4)
# ts1 = function(x,y,col=1){
#   fit=lm(y~x)
#   lines(x,y,col=col,lwd=2,type='o')
#   abline(fit$coef,lty=2,lwd=2,col=col)
#   print(summary(fit)$coef)
#   ind=which(x==2012)
#   fit2=lm(y[-ind]~x[-ind])
#   print(summary(fit2)$coef)
#   #abline(fit2$coef,lty=3,lwd=2,col=col)
# }
# 
# x11(w=12,h=5)
# par(mfrow=c(1,2))
# 
# for (ind in 1:2) {
#   ylim=range(c(mfbyyr[,ind],sfbyyr[,ind]))*c(.9,1.1)
#   plot(years,mfbyyr[,ind],col=cols[1],xlab='Year',ylab='Percent of Total',ylim=ylim,type='o',lwd=2)
#   ts1(years,mfbyyr[,ind],col=cols[1])
#   ts1(years,sfbyyr[,ind],col=cols[2])
# }
# 
# #for each county estimate trends and fraction at 2000 and 2015
# #VERY BIZARRE BUT THIS WORKS FOR MAIZE AND NOT SOY
# # pred.yr=c(2000,2015)
# # mfrac= data.frame(maizemeans %>% filter(FIPS.formula != 17139 ) %>%
# #   group_by(FIPS.formula) %>% do(
# #     cf1 = head(lm(frac80 ~ year,data=.)$fitted,1),
# #     cf2 = tail(lm(frac80 ~ year,data=.)$fitted,1)))
# # 
# # sfrac= data.frame(soymeans %>% filter(FIPS.formula != 17139 ) %>%
# #   group_by(FIPS.formula) %>% do(
# #     cf1 = head(lm(frac80 ~ year,data=.)$fitted,1),
# #     cf2 = tail(lm(frac80 ~ year,data=.)$fitted,1)))
# 
# #THIS WORKS FOR BOTH MAIZE AND SOY
# strend= data.frame(soymeans %>% filter(FIPS.formula != 17139 ) %>%
#                     group_by(FIPS.formula) %>% do(
#                       cf1 = lm(frac80 ~ year,data=.)$coef[2],
#                    startval = lm(frac80 ~ year,data=.)$fitted[1]))
# mtrend= data.frame(maizemeans %>% filter(FIPS.formula != 17139 ) %>%
#                      group_by(FIPS.formula) %>% do(
#                        cf1 = lm(frac80 ~ year,data=.)$coef[2],
#                    startval = lm(frac80 ~ year,data=.)$fitted[1]))
# 
# # plot(as.numeric(mtrend[,2])*15,as.numeric(mfrac[,3])-as.numeric(mfrac[,2]))
# # plot(as.numeric(strend[,2])*15,as.numeric(sfrac[,3])-as.numeric(sfrac[,2]))
# 
# # sfips=unique(soymeans$FIPS.formula)
# # sfips=sfips[sfips!=17139]
# # sfrac2 = t(sapply(sfips,function(x){
# #   temp=soymeans[which(soymeans$FIPS.formula == x),]
# #   fit=lm(temp$frac80~temp$year)
# #   c(head(fit$fitted,1),tail(fit$fitted,1))
# # }))
# # par(mfrow=c(2,2)) 
# # for (i in 1:4) plot(soymeans[soymeans$FIPS.formula==sfips[i],c('year','frac80')])
# 
# 
# x11()
# ylim=c(20,30)
# nyr=length(years)
# plot(mtrend$startval,as.numeric(mtrend$startval)+(nyr-1)*as.numeric(mtrend$cf1),xlim=ylim,ylim=ylim,col=cols[1],xlab='Fraction, 2000',ylab='Fraction, 2015')
# abline(c(0,1),lwd=3)
# points(strend$startval,as.numeric(strend$startval)+(nyr-1)*as.numeric(strend$cf1),col=cols[2],pch=17)
# # plot(mfrac[,2],mfrac[,3],xlim=ylim,ylim=ylim,col=cols[1],xlab='Fraction, 2000',ylab='Fraction, 2015')
# # abline(c(0,1),lwd=3)
# # points(sfrac[,2],sfrac[,3],col=cols[2],pch=17)
# for (i in 2:3) {class(mtrend[,i])='numeric';class(strend[,i])='numeric'}
# grbl=paste0(c(brewer.pal(3,'Greens')[2:3],brewer.pal(3,'Blues')[2:3]),'')
# dens=list(density(mtrend$startval),density(mtrend$startval+(nyr-1)*mtrend$cf1),
#           density(strend$startval),density(strend$startval+(nyr-1)*strend$cf1))
# plot(dens[[1]],col=grbl[1],xlim=c(20,30),lwd=3)
# for (i in 2:4) lines(dens[[i]],col=grbl[i],lwd=3)
# 
# vars=c('p5','p10','p50','p90','p95')
# maizeptrends = t(sapply(ufips,function(x) {
#   temp=maizeperc[maizeperc$FIPS.formula == x,]
#   yr = temp$year
#   if (sum(is.finite(temp$p50)) > 10){
#   trends = apply(temp[,vars],2,function(k){
#     lm(k~yr)$coef[2]
#   })} else trends=rep(NA,length(vars))
#   trends
# }))
# soyptrends = t(sapply(ufips,function(x) {
#   temp=soyperc[soyperc$FIPS.formula == x,]
#   yr = temp$year
#   if (sum(is.finite(temp$p50)) > 10){
#     trends = apply(temp[,vars],2,function(k){
#       lm(k~yr)$coef[2]
#     })} else trends=rep(NA,length(vars))
#   trends
# }))
# maize.avstats = t(sapply(ufips,function(x) {
#   temp=maizemean[maizemean$FIPS.formula == x,]
#   if (sum(is.finite(temp$mean)) > 10){
#     trend = lm(temp$mean~temp$year)$coef[2]
#     avg = mean(temp$mean,na.rm=T)
#     out=c(avg,trend)
#     } else out=rep(NA,2)
#   out
# }))
# soy.avstats = t(sapply(ufips,function(x) {
#   temp=soymean[soymean$FIPS.formula == x,]
#   if (sum(is.finite(temp$mean)) > 10){
#     trend = lm(temp$mean~temp$year)$coef[2]
#     avg = mean(temp$mean,na.rm=T)
#     out=c(avg,trend)
#   } else out=rep(NA,2)
#   out
# }))
# 
# 
# x11(w=15,h=10)
# par(mfrow=c(2,2))
# x=data.frame(fips=ufips,y = ptrends[,'p50'])
# breaks=seq(-40,40,10)
# tpal=rev(brewer.pal(8,'RdBu'))
# map.var(x,titl='scym trend maize yield,median',pal=tpal,
#         breaks=breaks)
# x=data.frame(fips=ufips,y = maizeptrends[,'p5'])
# map.var(x,titl='scym trend maize yield, 5th',pal=tpal,
#         breaks=breaks)
# x=data.frame(fips=ufips,y = maizeptrends[,'p95'])
# map.var(x,titl='scym trend maize yield, 95th',pal=tpal,
#         breaks=breaks)
# 
# #compare scym and nass mean and trends 2000-15
# scatxy=function(x,y,col=1,xlab='',ylab='',xlim=range(x),ylim=range(y)){
#   fit=lm(y~x)
#   plot(x,y,col=col,
#        xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim)
#   abline(c(0,1),lwd=2)
#   abline(fit$coef,lty=2,lwd=2)
#   #return correlation
#   cr=round(cor(x,y,use='p'),2)
#   legend('bottomright',leg=paste0('r = ',cr),bty='n')
# }
# 
# hold=list()
# hold[[1]]=list(cornvals,maize.avstats,maizeptrends)
# hold[[2]]=list(soyvals,soy.avstats,soyptrends)
# 
# for (crop.i in 1:2){
#   x11(w=10,h=10)
#   par(mfrow=c(2,2))
#   if (crop.i==1) ylims=list(list(c(5,20),c(-50,50)),list(c(1,6),c(-10,10)))
#   ylim=unlist(ylims[[crop.i]][1])
#   scatxy(hold[[crop.i]][[2]][,1]/100,hold[[crop.i]][[1]][,1],xlim=ylim,ylim=ylim,xlab='SCYM mean yield',
#          ylab='NASS mean yield')
#   ylim=unlist(ylims[[crop.i]][2])
#   scatxy(hold[[crop.i]][[2]][,2],hold[[crop.i]][[1]][,2]*100,xlim=ylim,ylim=ylim,xlab='SCYM mean yield trend',
#          ylab='NASS mean yield trend')
#   scatxy(hold[[crop.i]][[3]][,'p50'],hold[[crop.i]][[2]][,2],ylim=ylim,xlim=ylim,xlab='SCYM mean yield trend',
#        ylab='SCYM median yield trend')
#   scatxy(hold[[crop.i]][[3]][,'p10'],hold[[crop.i]][[3]][,'p90'],ylim=ylim,xlim=ylim,xlab='SCYM 10thperc yield trend',
#          ylab='SCYM 90thperc yield trend')
# }
# 
# #maps of both
# breaklist=list(list(seq(8,12,1),seq(-40,40,10)),list(seq(2,3.8,.2),seq(-10,10,2)))
# scymbreaklist=list(list(seq(12,18,1),seq(-40,40,10)),list(seq(3,4.8,.2),seq(-10,10,2)))
# for (crop.i in 1:2){
#     
#   x11(w=15,h=10)
#   par(mfrow=c(2,2))
#   x=data.frame(fips=ufips,my = hold[[crop.i]][[1]][,1])
#   breaks=breaklist[[crop.i]][[1]]
#   mpal=tim.colors(100)[1:90]
#   map.var(x,titl='nass mean yield',pal=mpal,
#           breaks=breaks)
#   x=data.frame(fips=ufips,my = hold[[crop.i]][[2]][,1]/100)
#   breaks=scymbreaklist[[crop.i]][[1]]
#   map.var(x,titl='scym mean yield',pal=mpal,
#           breaks=breaks)
#   
#   x=data.frame(fips=ufips,my = hold[[crop.i]][[1]][,2]*100)
#   breaks=breaklist[[crop.i]][[2]]
#   tpal=rev(brewer.pal(8,'RdBu'))
#   map.var(x,titl='nass trend yield',pal=tpal,
#           breaks=breaks)
#   x=data.frame(fips=ufips,my = hold[[crop.i]][[2]][,2])
#   breaks=scymbreaklist[[crop.i]][[2]]
#   tpal=rev(brewer.pal(8,'RdBu'))
#   map.var(x,titl='scym trend yield',pal=tpal,
#           breaks=breaks)
# }

# #below is old
# 
# #first make a big data frame
# state.names=c('iowa','illinois','indiana')
# clist=c('maize','soy')
# use.cal=T
# holdcal=list()
# holdalld=list()
# for (crop in 1:2){
#   load(paste0(clist[crop],'.allStateYieldSummary.Rdat')) 
#   
#    #remove counties with less than 1% of pixels in corn - without removing avg year
#   alldat=alldat[-which(alldat$fracpix < 0.01),]
#   alldat$stcode=substr(alldat$fips,1,2)
#   st.use=c(17,18,19)#define which states should be included
#   allav = alldat %>% filter(stcode %in% st.use & yr>=2000) %>% group_by(fips) %>% summarize(my=mean(my,na.rm=T),nass=mean(nass.yld,na.rm=T))
#   allcal = lm(allav$nass ~ allav$my)
#   cal = function(x) allcal$coef[1]+allcal$coef[2]*x
#   if (use.cal==F) cal = function(x) x
#   alldat$fullgap = 100*(cal(alldat$uy)/cal(alldat$my)-1)
#   alldat$fieldgap = 100*(cal(alldat$field_y95)/cal(alldat$my)-1)
#   alldat$percgap.infield = 100*(alldat$fieldgap/alldat$fullgap)
#   nyr=length(unique(alldat$yr))-1
#   avgap = alldat %>% group_by(fips) %>% filter(yr>0) %>% summarize(
#     avfullgap = mean(fullgap,na.rm=T),avfieldgap=mean(fieldgap,na.rm=T),nyr=sum(is.finite(fullgap)))
#   gapav=alldat %>% group_by(fips)%>% filter(yr == 0) 
#   #%>% select(field_yavg,field_y95,field_95th_ofavgs,uy)
#   gapav$fullgap = 100*(cal(gapav$uy)/cal(gapav$field_yavg)-1)
#   gapav$infieldgap = 100*(cal(gapav$field_y95)/cal(gapav$field_yavg)-1)
#   gapav$bwfieldgap = 100*(cal(gapav$field_95th_ofavgs)/cal(gapav$field_yavg)-1)
#   avgap2=merge(avgap,gapav,by='fips')
#   #compute trends, but only for counties with enough good years
#   alldat=merge(alldat,avgap[,c('fips','nyr')],by='fips',all.x=T)
#   trendgap = alldat %>% group_by(fips) %>% filter(yr>0, nyr>5) %>% do(
#     trendfull = lm(fullgap~yr,data=.)$coef[2]*(nyr-1),trendfield=lm(fieldgap~yr,data=.)$coef[2]*(nyr-1),
#     sigfull = summary(lm(fullgap~yr,data=.))$coef[2,4],sigfield=summary(lm(fieldgap~yr,data=.))$coef[2,4])
#   trendgap=data.frame(trendgap)
#   holdcal[[crop]]=allcal
#   holdalld[[crop]]=list(alldat,avgap2,trendgap)
# }
# 
# #figure 3 (showing average values of YH0 and YHIF)
# x11(w=20,h=10)
# par(mfrow=c(2,4))
# 
# labs=list(list('(a) Story, IA Maize',expression(paste('(b) Average Maize YH'[O],'(%)')),
#             expression(paste('(c) Average Maize YH'[IF],'(%)')),expression(paste(
#               '(d) Maize YH vs. Y'[avg]))),
#           list('(e) Story, IA Soybean',expression(paste('(f) Average Soy YH'[O],'(%)')),
#                expression(paste('(g) Average Soy YH'[IF],'(%)')),expression(paste(
#                  '(h) Soy YH vs. Y'[avg]))))
# for (crop in 1:2){
#   allcal=holdcal[[crop]]
#   alldat=holdalld[[crop]][[1]]
#   avgap2=holdalld[[crop]][[2]]
#   trendgap=holdalld[[crop]][[3]]
# 
#   #first an example of average yield and yield of 95th percentiles
#   if (crop == 1) ylim=c(8,16) else ylim=c(2,6)
# #   fips=19103#Johnston IA
# #   fips=19015#Boone IA
#   fips=19169#Story IA
# if (crop == 1) tlabs=c(expression(paste('YH'[IF],'= 20%')),expression(paste('YH'[O],'= 32%')))
# if (crop == 2) tlabs=c(expression(paste('YH'[IF],'= 16%')),expression(paste('YH'[O],'= 23%')))
# 
#   plotgap.bars(fips,use.cal=T,ylim=ylim,tlabs=tlabs)
#   title(labs[[crop]][[1]],adj=0)
#   box()
# 
#   #now a map of average overall gap
# x=avgap2[,c('fips','avfullgap')]
# breaks=seq(20,55,5)
# if (crop==2) breaks=seq(10,45,5)
# map.var(x,titl=labs[[crop]][[2]],pal=terrain.colors(10),
#         breaks=breaks)
# 
#   #now a map of average infield gap
# x=avgap2[,c('fips','avfieldgap')]
# map.var(x,titl=labs[[crop]][[3]],pal=terrain.colors(10),
#         breaks=breaks/2)
# 
#   #now a plot of YH vs. Yavg
# 
# xlab='County Average Maize SCYM Yield (t/ha)'; ylim=c(0,55)
# if (crop==2) {xlab='County Average Soy SCYM Yield (t/ha)';ylim=c(0,55)}
# plot(avgap2$field_yavg,avgap2$avfullgap,xlab=xlab,ylab='Average YH (%)',
#      ylim=ylim)
# panel.smooth(avgap2$field_yavg,avgap2$avfullgap,lwd=2,col.smooth=1)
# panel.smooth(avgap2$field_yavg,avgap2$avfieldgap,lwd=2,col.smooth=2,col=2)
# legend('bottomleft',pch=19,col=1:2,leg=c(expression('YH'[O]),expression('YH'[IF])),bty='n',cex=1.4)
# title(labs[[crop]][[4]],adj=0)
# }
# outdir='c:/Users/dlobell/Documents/papers/scym_longhist/'
# savePlot(paste0(outdir,'fig3.png'),ty='png')
# savePlot(paste0(outdir,'fig3.pdf'),ty='pdf')
# 
# #now for trends
# #figure 4
# x11(w=15,h=10)
# par(mfrow=c(2,3))
# 
# labs=list(list('(a) Story, IA Maize',expression(paste('(b) Change in Maize YH'[O],' (%)')),
#                expression(paste('(c) Change in Maize YH'[IF],' (%)'))),
#           list('(d) Story, IA Soybean',expression(paste('(e) Change in Soy YH'[O],' (%)')),
#                expression(paste('(f) Change in Soy YH'[IF],' (%)'))))
# for (crop in 1:2){
#   allcal=holdcal[[crop]]
#   alldat=holdalld[[crop]][[1]]
#   avgap2=holdalld[[crop]][[2]]
#   trendgap=holdalld[[crop]][[3]]
#   
#   #first an example of average yield and yield of 95th percentiles
#   if (crop == 1) ylim=c(8,16) else ylim=c(2,6)
# #  if (crop == 1) ylim=c(12,23) else ylim=c(2,7)
#   #   fips=19103#Johnston IA
#   #   fips=19015#Boone IA
#   fips=19169#Story IA
#   plotgap.ts(fips)
#   title(labs[[crop]][[1]],adj=0)
#   box()
#   
#   #now a map of trend in average overall gap
#   pal=two.colors(20,start='darkblue',end='darkred',middle=gray(.9))
#   x=trendgap[,c('fips','trendfull')]; class(x[,2])='numeric'
#   map.var(x,titl=labs[[crop]][[2]],
#           pal=pal,breaks=seq(-20,20,5)*ifelse(crop==2,.5,1))
#   x=trendgap[,c('fips','trendfield')]; class(x[,2])='numeric'
#   #version w/ insigificant trends masked out
#   #x[,2] = x[,2] * (as.numeric(trendgap[,'sigfull'])<.1)
#   map.var(x,titl=labs[[crop]][[3]],
#           pal=pal,breaks=seq(-10,10,2.5)*ifelse(crop==2,.5,1))
#   
# }
# savePlot(paste0(outdir,'fig4.png'),ty='png')
# savePlot(paste0(outdir,'fig4.pdf'),ty='pdf')
# 
# #save the calibration equations for other figures
# names(holdcal) = c('maize','soy')
# save(holdcal,file=paste0(outdir,'caleq.Rdata'))
# 
# #calculate the average YH values for each crop
#   #calculate total production in each county
#   f=read.csv('../../nass/nass_cornsoy_sep15.csv')
#   f$cntyfips=f$County.ANSI
#   f$cntyfips=paste0(substr(rep('00',nrow(f)),rep(1,nrow(f)),3-nchar(f$County.ANSI)),f$cntyfips)
#   f$fips=paste0(f$State.ANSI,f$cntyfips)
#   f$Value=as.numeric(gsub(',','',f$Value))
#   ufips=unique(f$fips)
#   
#   vars=c('Year','Value')
#   nassval = lapply(ufips,function(x) {
#     temp=list()
#     temp$corn=f[f$fips == x & f$Data.Item == "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE",vars]
#     temp$cornarea=f[f$fips == x & f$Data.Item == "CORN, GRAIN - ACRES HARVESTED",vars]
#     temp$soy=f[f$fips == x & f$Data.Item == "SOYBEANS - YIELD, MEASURED IN BU / ACRE",vars]
#     temp$soyarea=f[f$fips == x & f$Data.Item == "SOYBEANS - ACRES HARVESTED",vars]
#     temp
#   })
# 
#   cornprodav = sapply(nassval,function(x) mean(x$corn$Value*x$cornarea$Value))
#   soyprodav = sapply(nassval,function(x) mean(x$soy$Value*x$soyarea$Value))
#   
#   nassprod=data.frame(fips=ufips,cprod=cornprodav,sprod=soyprodav)
# for (crop in 1:2){
#   avgap2=holdalld[[crop]][[2]]
#   temp=merge(avgap2,nassprod,by='fips')
#   if (crop==1) {
#     avfull = sum(temp$cprod * temp$avfullgap,na.rm=T)/sum(temp$cprod,na.rm=T)
#     avfield = sum(temp$cprod * temp$avfieldgap,na.rm=T)/sum(temp$cprod,na.rm=T)
#   } 
#   if (crop==2) {
#     avfull = sum(temp$sprod * temp$avfullgap,na.rm=T)/sum(temp$sprod,na.rm=T)
#     avfield = sum(temp$sprod * temp$avfieldgap,na.rm=T)/sum(temp$sprod,na.rm=T)
#   }
#   print(paste(crop,'avfull',avfull,'avfield',avfield))
# }
# 
# 
#   
