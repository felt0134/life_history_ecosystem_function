

# ESS plot ---------------------------------------------------------------------

# Make plot to compare Sonoran ESS among
# bet hedger, predictive germinator, and adaptive tracker under median levels
# of density dependence


# Run through this part manually....

# Variable and unpredictable
myFec=30
myFecSigma = 0.5*myFec   # this makes it variable
myRho = 0          # 0 =  unpredictable, 1 = perfectly predictable
myAlpha = 0.015
mySeedSurv = 0.9
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1

# Find ESS g fraction given a series of g variances
gVars = c(0,0.1,0.5,1,2,4)
ESS_gFrac_given_gVar = rep(NA,length(gVars))
for(iGvar in 1:length(gVars)){
  mySigmaG = gVars[iGvar]
  print(paste0("Doing ",iGvar," in ",length(gVars), " g vars"))
  source("find_gFrac.R")
  print(paste0("ESS - ESS2 = ", ESS-ESS2)) # compare two methods for determining ESS
  ESS_gFrac_given_gVar[iGvar] = ESS
}

# Find ESS g var given a series of g fractions
gFracs = logit(seq(0.30,0.90,0.1))
ESS_gVar_given_gFrac = rep(NA,length(gFracs))
for(iGfrac in 1:length(gFracs)){
  myGfrac = gFracs[iGfrac]
  print(paste0("Doing ",iGfrac," in ",length(gFracs), " g vars"))
  source("find_gVar.R")
  ESS_gVar_given_gFrac[iGfrac] = ESS
}

# CUSTOMIZE
gVars_BH=gVars
gFracs_BH<-gFracs
ESS_gFrac_given_gVar_BH<-ESS_gFrac_given_gVar
ESS_gVar_given_gFrac_BH<-ESS_gVar_given_gFrac

# Now do variable and predictable
myFec=30
myFecSigma = 0.5*myFec   # this makes it variable
myRho = 0.8          # 0 =  unpredictable, 1 = perfectly predictable
myAlpha = 0.015
mySeedSurv = 0.9
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1

# Find ESS g fraction given a series of g variances
gVars = c(0,0.1,0.5,1,2,4)
ESS_gFrac_given_gVar = rep(NA,length(gVars))
for(iGvar in 1:length(gVars)){
  mySigmaG = gVars[iGvar]
  print(paste0("Doing ",iGvar," in ",length(gVars), " g vars"))
  source("find_gFrac.R")
  print(paste0("ESS - ESS2 = ", ESS-ESS2)) # compare two methods for determining ESS
  ESS_gFrac_given_gVar[iGvar] = ESS
}

# Find ESS g var given a series of g fractions
gFracs = logit(seq(0.3,0.9,0.1))
#gFracs = logit(seq(0.39,0.99,0.1))
ESS_gVar_given_gFrac = rep(NA,length(gFracs))
for(iGfrac in 1:length(gFracs)){
  myGfrac = gFracs[iGfrac]
  print(paste0("Doing ",iGfrac," in ",length(gFracs), " g vars"))
  source("find_gVar.R")
  ESS_gVar_given_gFrac[iGfrac] = ESS
}

# CUSTOMIZE
gVars_PG=gVars
gFracs_PG<-gFracs
ESS_gFrac_given_gVar_PG<-ESS_gFrac_given_gVar
ESS_gVar_given_gFrac_PG<-ESS_gVar_given_gFrac

# Now do low variability and moderate predictability
myFec=30
myFecSigma = 0.05*myFec   # this makes it variable
myRho = 0.4          # 0 =  unpredictable, 1 = perfectly predictable
myAlpha = 0.015
mySeedSurv = 0.9
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1

# Find ESS g fraction given a series of g variances
gVars = c(0,0.1,0.5,1,2,4)
ESS_gFrac_given_gVar = rep(NA,length(gVars))
for(iGvar in 1:length(gVars)){
  mySigmaG = gVars[iGvar]
  print(paste0("Doing ",iGvar," in ",length(gVars), " g vars"))
  source("find_gFrac.R")
  print(paste0("ESS - ESS2 = ", ESS-ESS2)) # compare two methods for determining ESS
  ESS_gFrac_given_gVar[iGvar] = ESS
}

# Find ESS g var given a series of g fractions

#gFracs = logit(seq(0.30,0.90,0.1))
gFracs = logit(seq(0.39,0.99,0.1))
ESS_gVar_given_gFrac = rep(NA,length(gFracs))
for(iGfrac in 1:length(gFracs)){
  myGfrac = gFracs[iGfrac]
  print(paste0("Doing ",iGfrac," in ",length(gFracs), " g vars"))
  source("find_gVar.R")
  ESS_gVar_given_gFrac[iGfrac] = ESS
}

# CUSTOMIZE
gVars_AT=gVars
gFracs_AT<-gFracs
ESS_gFrac_given_gVar_AT<-ESS_gFrac_given_gVar
ESS_gVar_given_gFrac_AT<-ESS_gVar_given_gFrac

# Plot the main two-panel panel sensitivity figure

pdf('figures/sonoran_ESS.pdf',
    width=7,height=5)

layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# Panel label setup
line = 0.75
cex = 1.0
side = 3
adj=-0.025

# Predictive germinator
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
abline(h=0.68,col='grey',lwd=2,lty=1)
abline(v=0.99,col='grey',lwd=2,lty=1)
points(gVars_PG,ESS_gFrac_given_gVar_PG,type="o",pch=16,cex=1.5,col="blue")
lines(gVars_PG,ESS_gFrac_given_gVar_PG,type="o",col="blue")
points(ESS_gVar_given_gFrac_PG,inv.logit(gFracs_PG),pch=16,cex=1.5,col="red")
lines(ESS_gVar_given_gFrac_PG,inv.logit(gFracs_PG),col="red")
axis(1)
axis(2)
box()
mtext('Variable and Predictable',side=3,line=0.50,cex=0.75)
mtext("A", side=side, line=line, cex=cex, adj=adj)
legend(1.2, 0.35, legend=c("Optimal gerimation fraction", "Optimal germination variance"),         #alpha legend: 0.015, 150
       col=c("red", "blue"), lty=1.1,lwd=4,cex=0.6,box.lty=0)

# Bet Hedger
# layout(matrix(1:1, ncol=1))
# par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
abline(h=0.61,col='grey',lwd=2,lty=1)
abline(v=0.0,col='grey',lwd=2,lty=1)
points(gVars_BH,ESS_gFrac_given_gVar_BH,pch=16,cex=1.5,col="blue")
lines(gVars_BH,ESS_gFrac_given_gVar_BH,col="blue")
points(ESS_gVar_given_gFrac_BH,inv.logit(gFracs_BH),pch=16,cex=1.5,col="red")
lines(ESS_gVar_given_gFrac_BH,inv.logit(gFracs_BH),col="red")
axis(1)
box()
mtext('Variable and Unpredictable',side=3,line=0.50,cex=0.75)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('Germination variance',side=1,line=3,cex=1.25,outer=T)
mtext('Germination fraction',side=2,line=3,cex=1.25,outer=TRUE)

dev.off()
#done

# Plot supporting figure of ESS for low variability and predictable environment
pdf('figures/ESS_median_alpha_AT_sonoran.pdf',
    width=7,height=5)
layout(matrix(1:1, ncol=1))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')
# adaptive tracking: CLEAN THIS UP
plot(gVars_AT,ESS_gFrac_given_gVar_AT,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),pch=16,cex=1.5,col="blue",cex.lab=1.5,
     xlab="",ylab="",main='',axes=T)
points(ESS_gVar_given_gFrac_AT,inv.logit(gFracs_AT),pch=16,cex=1.5,col="red")
lines(ESS_gVar_given_gFrac_AT,inv.logit(gFracs_AT),col="red")
mtext('Germination fraction',side=2,line=-3,cex=1.5,outer=TRUE)
mtext('Germination variance',side=1,line=3,cex=1.5,outer=TRUE)
mtext('Not Variable and Predictable',side=3,line=.5,cex=1.0)
dev.off()
#done

#-------------------------------------------------------------------------------
# Density dependence plot, supporting figure -----------------------------------

# Rate of self-thinning distribution and self-thinning curves
source('empirical_look.R')

pdf('figures/density_dependence_sonoran_multipanel.pdf',
    width=8.5,height=5)

par(oma=c(6, 5, 6, 5), mar=c(1, 3, 1, 1),pty='s',mfrow=c(1,2))

# Panel label setup
line = 0.75
cex = 1.25
side = 3
adj=-0.025

# Histogram
hist(sonoran_full$max.div.by.half.sat,breaks = 20,xlim=c(10,80),
     col="grey",main='',ylab='',xlab='')
abline(v=12.85,col='red',lwd=4,lty=1)
abline(v=30.72,col='black',lwd=4,lty=1)
abline(v=72.39,col='blue',lwd=4,lty=1)
mtext('Rate of self thinning',side=1,line=2.5,cex=1.5)
mtext("A", side=side, line=line, cex=cex, adj=adj)
mtext('Frequency',side=2,line=2.5,cex=1.5)

# density dependence curves
maxN=2500
out = grow_res(seeds_res=1:maxN,Fec=30,alpha=0.015,seedSurv=0.9,G_res=1,1)
plot(1:maxN,out$yield,type="l",ylim=c(0,2000),xlim=c(-1,2500),xlab="",ylab="",
     cex.lab=1.5,cex=1,lwd=3,col='black')
out = grow_res(seeds_res=1:maxN,Fec=10,alpha=0.005,seedSurv=0.9,G_res=1,1)
lines(1:maxN,out$yield,type="l",lwd=3,col='blue')
out = grow_res(seeds_res=1:maxN,Fec=80,alpha=0.040,seedSurv=0.9,G_res=1,1)
lines(1:maxN,out$yield,type="l",lwd=3,col='red')

# Legend of parameter values, will vary 
legend(800, 700, legend=c(expression(paste(alpha,'=0.015, ',lambda,'=30')),
                          expression(paste(alpha,'=0.005, ',lambda,'=10')),
                          expression(paste(alpha,'=0.040, ',lambda,'=80'))),
       col=c("black", "blue",'red'), lty=1,lwd=2,cex=0.75,box.lty=0)
#text(185, 1200, 'Typical Sonoran species',cex=0.5)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('Germinants',side=1,line=2.5,cex=1.5)
mtext('Total seed yield',side=2,line=2.5,cex=1.5)

dev.off()
#done

#-------------------------------------------------------------------------------
# Sensitivity plot -------------------------------------------------------------


# make plot to compare Sonoran sensitivity between
# bet hedger and predictive germinator under median levels
# of density dependence


pdf('figures/sonoran_bivariate.pdf',
    width=7,height=5)


layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj=-0.05

# Predictive germinate
gFrac = 0.69
gVar = 1.0
myFec = 30
myAlpha = 0.015
myFecSigma = 0.5*myFec
myRho=0.8
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1
mySeedSurv = 0.9

# Simulate rates
rates = get_F_G(tot_time, mu=c(myFec,gFrac,gFrac),sigma=c(myFecSigma,gVar,gVar), rho=myRho)
rates=data.frame(rates)

# Simulate resident population
production = seeds = rep(NA, tot_time)
seeds[1] = 2   # initial population
for(i in 2:tot_time){
  out = grow_res(seeds_res=seeds[i-1],Fec=rates$Fec[i],alpha=myAlpha,seedSurv=mySeedSurv,G_res=rates$G1[i],G_inv=rates$G2[i])
  seeds[i] = out$seeds
  production[i] = out$yield
}


# Plot it
sensitivity = lm(production[burn_in:tot_time]~rates$Fec[burn_in:tot_time])

plot(production[burn_in:tot_time]~rates$Fec[burn_in:tot_time],xlab="",ylab="",cex.lab=1.5,
     xlim=c(0,3*myFec),ylim=c(0,6000),
     main='',axes=F)
abline(sensitivity, col="red",lwd=2)
legend("topleft",paste("Slope =",round(coef(sensitivity)[2],2)),bty="n",cex = 1.00)
axis(2)
axis(1)
box()
mtext('Predictive Germinator',side=3,line=0.25,cex=1)
mtext("A", side=side, line=line, cex=cex, adj=adj)
mtext('Production',side=2,line=2.5,cex=1.5)

# Now do bet hedger
gFrac = 0.62
gVar = 0
myFec = 30
myAlpha = 0.015
myFecSigma = 0.5*myFec
myRho=0.0
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1
mySeedSurv = 0.9

# Simulate rates
rates = get_F_G(tot_time, mu=c(myFec,gFrac,gFrac),sigma=c(myFecSigma,gVar,gVar), rho=myRho)
rates=data.frame(rates)

# Simulate resident population
production = seeds = rep(NA, tot_time)
seeds[1] = 2   # initial population
for(i in 2:tot_time){
  out = grow_res(seeds_res=seeds[i-1],Fec=rates$Fec[i],alpha=myAlpha,seedSurv=mySeedSurv,G_res=rates$G1[i],G_inv=rates$G2[i])
  seeds[i] = out$seeds
  production[i] = out$yield
}

sensitivity = lm(production[burn_in:tot_time]~rates$Fec[burn_in:tot_time])

plot(production[burn_in:tot_time]~rates$Fec[burn_in:tot_time],xlab="",ylab="",cex.lab=1.5,
     xlim=c(0,3*myFec),ylim=c(0,6000),
     main='',axes=F)
abline(sensitivity, col="red",lwd=2)
legend("topleft",paste("Slope =",round(coef(sensitivity)[2],2)),bty="n",cex = 1.0)
axis(1)
box()
mtext('Bet-hedger',side=3,line=0.25,cex=1)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('Resources',side=1,line=2.5,cex=1.5,outer=TRUE)

dev.off()
#done

#-------------------------------------------------------------------------------
# Lag effects plot -------------------------------------------------------------

# Lag effect for median levels of density dependence

png('figures/sonoran_lag_effects.png',
    width=700,height=500)

layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj=-0.05

# Predictive germinator
gFrac = 0.69
gVar = 1.0
myFec = 30
myAlpha = 0.015
myFecSigma = 0.5*myFec
myRho=0.80
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1
mySeedSurv = 0.9

# Simulate rates
rates = get_F_G(tot_time, mu=c(myFec,gFrac,gFrac),sigma=c(myFecSigma,gVar,gVar), rho=myRho)
rates=data.frame(rates)

# Simulate resident population
production = seeds = rep(NA, tot_time)
seeds[1] = 2   # initial population
for(i in 2:tot_time){
  out = grow_res(seeds_res=seeds[i-1],Fec=rates$Fec[i],alpha=myAlpha,seedSurv=mySeedSurv,G_res=rates$G1[i],G_inv=rates$G2[i])
  seeds[i] = out$seeds
  production[i] = out$yield
}

#look at temporal sequence
#plot(production[(burn_in+1):tot_time],type = "l")

# Plot it
pg_lag<-lm(production[(burn_in+1):tot_time]~production[burn_in:(tot_time-1)])
#summary(pg_lag)
# Adjusted R-squared:  0.002968
# p-value: 0.0003322

plot(production[burn_in:(tot_time-1)],production[(burn_in+1):tot_time],xlab="",ylab="",cex.lab=1.5,
     xlim=c(0,6100),ylim=c(0,6100),
     main='',axes=F)
abline(pg_lag, col="red",lwd=2)
legend("topleft",paste("Slope =",round(coef(pg_lag)[2],2)),bty="n",cex = 0.75)
axis(2)
axis(1)
box()
mtext('Predictive Germinator',side=3,line=0.25,cex=1)
mtext("A", side=side, line=line, cex=cex, adj=adj)
mtext('Current-year production',side=2,line=2.5,cex=1.5)

# Now do bet hedger
gFrac = 0.62
gVar = 0
myFec = 30
myAlpha = 0.015
myFecSigma = 0.5*myFec
myRho=0.0
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1
mySeedSurv = 0.9

# Simulate rates
rates = get_F_G(tot_time, mu=c(myFec,gFrac,gFrac),sigma=c(myFecSigma,gVar,gVar), rho=myRho)
rates=data.frame(rates)

# Simulate resident population
production = seeds = rep(NA, tot_time)
seeds[1] = 2   # initial population
for(i in 2:tot_time){
  out = grow_res(seeds_res=seeds[i-1],Fec=rates$Fec[i],alpha=myAlpha,seedSurv=mySeedSurv,G_res=rates$G1[i],G_inv=rates$G2[i])
  seeds[i] = out$seeds
  production[i] = out$yield
}

#look at temporal sequence
#plot(production[(burn_in+1):tot_time],type = "l")

bh_lag<-lm(production[(burn_in+1):tot_time]~production[burn_in:(tot_time-1)])
#summary(bh_lag)
# Adjusted R-squared:  0.002365
# p-value: 0.001217

plot(production[burn_in:(tot_time-1)],production[(burn_in+1):tot_time],xlab="",ylab="",cex.lab=1.5,
     xlim=c(0,6100),ylim=c(0,6100),
     main='',axes=F)
abline(bh_lag, col="red",lwd=2)
legend("topleft",paste("Slope =",round(coef(bh_lag)[2],2)),bty="n",cex = 0.75)
axis(1)
box()
mtext('Bet-hedger',side=3,line=0.25,cex=1)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('Previous-year production',side=1,line=2.5,cex=1.5,outer=TRUE)

dev.off()

#done

#-------------------------------------------------------------------------------
# Sensitivity difference plots: density dependence -----------------------------

# Upload saved data frame and subset for plot
sensitivity_sonoran_alphas<-read.csv('derived_data/sonoran_sensitvity_alphas.csv')

#merge with 'max.div.by.half.sat.model.df' produced in 'empirical_look.R'
sensitivity_sonoran_alphas <- merge(sensitivity_sonoran_alphas, max.div.by.half.sat.model.df,by=c('ranges'))

bh_sens_alphas_2<-subset(sensitivity_sonoran_alphas,life_history=='bet_hedging')
bh_sens_alphas_2<-bh_sens_alphas_2[c("1","5","3"),] #re-order

pg_sens_alphas_2<-subset(sensitivity_sonoran_alphas,life_history=='predictive_germinator')
pg_sens_alphas_2<-pg_sens_alphas_2[c("2","6","4"),]

# Plot the change in sensitivity with difference fec and alpha for different life histories

pdf('figures/sens_diff_sonoran_half_sat.pdf',
    width=6,height=5)

mar.default <- c(5,4,2,2) + 0.1
par(mar = mar.default + c(1, 1, 0, 0),mgp=c(3,1,0))
plot(sensitivity.coefficients~max.div.by.half.sat,data=bh_sens_alphas_2,ylab='Sensitivity',cex=4,
     xlab='Rate at which yield saturates with density',col='deepskyblue',pch=19,ylim=c(0,195),
     xlim=c(8,75),cex.lab=1.5,main='')          #alpha xlim=c(0.00,0.045)
lines(sensitivity.coefficients~max.div.by.half.sat,data=bh_sens_alphas_2,col='deepskyblue')
points(sensitivity.coefficients~jitter(max.div.by.half.sat,0.5),data=pg_sens_alphas_2,ylab='Sensitivity',cex=4,
       xlab='Competition',col='goldenrod',pch=19,ylim=c(0,180),cex.lab=1.5)
lines(sensitivity.coefficients~max.div.by.half.sat,data=pg_sens_alphas_2,col='goldenrod')
legend(25, 150, legend=c("Predictive Germinator", "Bet-hedger"),         #alpha legend: 0.015, 150
       col=c("goldenrod", "deepskyblue"), lty=1,lwd=4,cex=1.3,box.lty=0)

dev.off()

#done

#-------------------------------------------------------------------------------
# Sensitivity difference plots: seed survival ----------------------------------

# This is sensitivity difference for median levels of
# density dependence. Note that the results for this analysis with
# low density dpendence produce qualitatively the same result

# Upload derived df and subset for plot
sensitivity_sonoran_ss<-read.csv('derived_data/sonoran_sensitvity_SS.csv')
bh_sens_ss_2<-subset(sensitivity_sonoran_ss,life_history=='bet_hedger')
pg_sens_ss_2<-subset(sensitivity_sonoran_ss,life_history=='predictive_germinator')

# Plot the change in sensitivity with difference fec and alpha for different life histories
pdf('figures/sens_diff_sonoran_seed_survival_medianDD.pdf',
    width=6,height=5)

mar.default <- c(5,4,2,2) + 0.1
par(mar = mar.default + c(1, 1, 0, 0),mgp=c(3,1,0))
plot(sensitivity.coefficients~ss,data=bh_sens_ss_2,ylab='Sensitivity',cex=4,
     xlab='Seed survival',col='deepskyblue',pch=19,ylim=c(61,66),xlim=c(0.45,0.95),
     cex.lab=1.5,main='')
lines(sensitivity.coefficients~ss,data=bh_sens_ss_2,col="deepskyblue",lwd=2)
points(sensitivity.coefficients~ss,data=pg_sens_ss_2,ylab='Sensitvity',cex=4,
       xlab='Seed survival',col='goldenrod',pch=19,ylim=c(61,67),cex.lab=1.5)
lines(sensitivity.coefficients~ss,data=pg_sens_ss_2,col="goldenrod",lwd=2)
legend(0.5, 62.5, legend=c("Predictive Germinator", "Bet-hedger"),
       col=c("goldenrod", "deepskyblue"), lty=1,lwd=4,cex=1.3,box.lty=0)

dev.off()
#done

#-------------------------------------------------------------------------------
# Density dependence combinations ESS supporting figure ------------------------

# to make this figure, I recommend opening this R script and running what you need:
# source('ESS_Loops.R')

pdf('figures/sonoran_ESS_DD.pdf',
    width=7,height=5)

layout(matrix(1:4, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# Panel label setup
line = 0.75
cex = 1.25
side = 3
adj=-0.3


# A predictive germinator: slow self thinning
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_plastic[1])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_plastic[1])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_plastic[1])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_plastic[1])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.0, 0.15, 'Slow saturation with density',cex=0.75)
axis(2) #Y axis
box()
mtext('Variable and Predictable',side=3,line=0.250,cex=0.75)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# B predictive germinator: rapid self thinning
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_plastic[3])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_plastic[3])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_plastic[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_plastic[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.0, 0.15, 'Rapid saturation with density',cex=0.75)
axis(1)
axis(2)
box()

# Bet Hedger

# C bet hedger: slow self thinning
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_bet_hedger[1])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_bet_hedger[1])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_bet_hedger[1])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_bet_hedger[1])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.0, 0.15, 'Slow saturation with density',cex=0.75)
axis(2) #Y axis
box()
mtext('Variable and Unpredictable',side=3,line=0.250,cex=0.75)
mtext("B", side=side, line=line, cex=cex, adj=adj)

# D predictive germinator: rapid self thinning
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_bet_hedger[3])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_bet_hedger[3])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_bet_hedger[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_bet_hedger[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.0, 0.15, 'Rapid saturation with density',cex=0.75)
axis(1)
axis(2)
box()
mtext('Germination variance',side=1,line=3,cex=1.5,outer=T)
mtext('Germination fraction',side=2,line=0,cex=1.5,outer=TRUE)

dev.off()

#-------------------------------------------------------------------------------
# Plot out ESS for different SS supporting figure ------------------------------

pdf('figures/sonoran_ESS_SS.pdf',
    width=7,height=5)

layout(matrix(1:4, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# Panel label setup
line = 0.75
cex = 1.25
side = 3
adj=-0.3

# Predictive germinator: SS = 0.7
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_ss_plastic[2])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_ss_plastic[2])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_ss_plastic[2])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_ss_plastic[2])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.5, 0.3, 'Seed survival = 0.7',cex=0.75)
axis(2) #Y axis
box()
mtext('Variable and Predictable',side=3,line=0.250,cex=0.75)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# Predictive germinate: SS = 0.5
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_ss_plastic[3])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_ss_plastic[3])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_ss_plastic[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_ss_plastic[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.5, 0.3, 'Seed survival = 0.5',cex=0.75)
axis(1)
axis(2)
box()

# Bet Hedger

#bet hedger: SS = 0.7
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_ss_bet_hedger[2])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_ss_bet_hedger[2])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_ss_bet_hedger[2])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_ss_bet_hedger[2])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.5, 0.3, 'Seed survival = 0.7',cex=0.75)
axis(2) #Y axis
box()
mtext('Variable and Unpredictable',side=3,line=0.250,cex=0.75)
mtext("B", side=side, line=line, cex=cex, adj=adj)
#text(0.05,0.99,'B',cex=1.25)

# D bet hedger SS = 0.5
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.2),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_ss_bet_hedger[3])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_ss_bet_hedger[3])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_ss_bet_hedger[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_ss_bet_hedger[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.5, 0.3, 'Seed survival = 0.5',cex=0.75)
axis(1)
axis(2)
box()
mtext('Germination variance',side=1,line=3,cex=1.5,outer=T)
mtext('Germination fraction',side=2,line=0,cex=1.5,outer=TRUE)

dev.off()

#done

# done--------------------------------------------------------------------------


