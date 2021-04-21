
# Modeled parameter set  plots

# Sensitivity ------------------------------------------------------------------

png('figures/modeled_sensitivity_bivariate_lowdd.png',
    width=700,height=500)

# Set up multi-panel
layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj=-0.05

# Predictive germinator: lowest density dependence

gFrac = 0.84
gVar = 4
myFec = 2
myAlpha = 0.001
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
#summary(sensitivity)
#slope = 679.22

plot(production[burn_in:tot_time]~rates$Fec[burn_in:tot_time],xlab="",ylab="",cex.lab=1.5,
     xlim=c(0,3*myFec),ylim=c(0,3550),
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
gFrac = 0.68
gVar = 0
myFec = 2
myAlpha = 0.001
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
#summary(sensitivity)
#slope = 401.86

plot(production[burn_in:tot_time]~rates$Fec[burn_in:tot_time],xlab="",ylab="",cex.lab=1.5,
     xlim=c(0,3*myFec),ylim=c(0,3550),
     main='',axes=F)
abline(sensitivity, col="red",lwd=2)
legend("topleft",paste("Slope =",round(coef(sensitivity)[2],2)),bty="n",cex = 1.0)
#axis(2)
axis(1)
box()
mtext('Bet-hedger',side=3,line=0.25,cex=1)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('Resources',side=1,line=2.5,cex=1.5,outer=TRUE)

dev.off()

#(679.22 - 401.86)/401.86

#####
# Lag effects plot -------------------------------------------------------------

# Lag effects, use lowest magnitude of density dependence/self-thinning

png('figures/modeled_lag_effects.png',
    width=700,height=500)


# Multi-panel setup
layout(matrix(1:2, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0, 1, 0, 0),pty='s')

# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj=-0.05

# Predictive germinator
gFrac = 0.84
gVar = 4
myFec = 2
myAlpha = 0.001
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

#look at temporal sequence
#plot(production[(burn_in+1):500],type = "l")

# Plot it
pg_lag<-lm(production[(burn_in+1):tot_time]~production[burn_in:(tot_time-1)])
#summary(pg_lag)
# Adjusted R-squared: 0.006
# p-value: 0.014

plot(production[burn_in:(tot_time-1)],production[(burn_in+1):tot_time],xlab="",ylab="",cex.lab=1.5,
     xlim=c(0,3700),ylim=c(0,3700),
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
gFrac = 0.68
gVar = 0
myFec = 2
myAlpha = 0.001
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
#plot(production[(burn_in+1):500],type = "l")

bh_lag<-lm(production[(burn_in+1):tot_time]~production[burn_in:(tot_time-1)])
summary(bh_lag)
# Adjusted R-squared: 0.25
# p-value: very low

plot(production[burn_in:(tot_time-1)],production[(burn_in+1):tot_time],xlab="",ylab="",cex.lab=1.5,
     xlim=c(0,3700),ylim=c(0,3700),
     main='',axes=F)
abline(bh_lag, col="red",lwd=2)
legend("topleft",paste("Slope =",round(coef(bh_lag)[2],2)),bty="n",cex = 0.75)
#axis(2)
axis(1)
box()
mtext('Bet-hedger',side=3,line=0.25,cex=1)
mtext("B", side=side, line=line, cex=cex, adj=adj)
mtext('Previous-year production',side=1,line=2.5,cex=1.5,outer=TRUE)

dev.off()

#done
#####
# Sensitivity difference -------------------------------------------------------

sensitivity_modeled_alphas<-read.csv('derived_data/modeled_sensitvity_alphas.csv')
sensitivity_modeled_alphas <- merge(sensitivity_modeled_alphas, max.div.by.half.sat.model.df.modeled_2,by=c('ranges'))

# Quick summary stats

bh_sens_alphas_modeled_2<-subset(sensitivity_modeled_alphas,life_history=='bet_hedging')
bh_sens_alphas_modeled_2<-bh_sens_alphas_modeled_2[c("1","5","3"),] #re-order

pg_sens_alphas_modeled_2<-subset(sensitivity_modeled_alphas,life_history=='predictive_germinator')
pg_sens_alphas_modeled_2<-pg_sens_alphas_modeled_2[c("2","6","4"),]

#plot the change in sensitivity with difference fec and alpha for different life histories
pdf('figures/sens_diff_modeled_half_sat.pdf',
    width=6,height=5)

mar.default <- c(5,4,2,2) + 0.1
par(mar = mar.default + c(1, 1, 0, 0),mgp=c(3,1,0))
plot(sensitivity.coefficients~max.div.by.half.sat,data=bh_sens_alphas_modeled_2,ylab='Sensitivity',cex=4,
     xlab='Rate at which yield saturates with density',col='deepskyblue',pch=19,ylim=c(100,700),   #alpha xlab: xlim=c(0.0005,0.0045)
     xlim=c(3,11),cex.lab=1.5,main='')
lines(sensitivity.coefficients~max.div.by.half.sat,data=bh_sens_alphas_modeled_2,col='deepskyblue')
points(sensitivity.coefficients~jitter(max.div.by.half.sat,0.2),data=pg_sens_alphas_modeled_2,ylab='Sensitivity',cex=4,
       xlab='Competition',col='goldenrod',pch=19,ylim=c(100,700),cex.lab=1.5)
lines(sensitivity.coefficients~max.div.by.half.sat,data=pg_sens_alphas_modeled_2,col='goldenrod')
legend(3, 250, legend=c("Predictive Germinator", "Bet-hedger"),        # alpha legend: 0.0005, 250
       col=c("goldenrod", "deepskyblue"), lty=1,lwd=4,cex=1.3,box.lty=0)

dev.off()

#done

######
# ESS for plasticity -----------------------------------------------------------

# Plot these DD ESS combinations out for plasticity for supporting figure

pdf('figures/modeled_ESS_DD_plastic.pdf',
    width=7,height=5)

# Multi-panel set up
layout(matrix(1:3, ncol=3))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# Panel label setup
line = 0.75
cex = 1.25
side = 3
adj=-.05


# A predictive germinator: slow self thinning
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.6),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_plastic[1])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_plastic[1])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_plastic[1])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_plastic[1])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.5, 0.2, 'Slow saturation with density',cex=1)
axis(1) # X axis
axis(2) #Y axis
box()
mtext('Variable and Predictable',side=3,line=-2.5,cex=1.5,outer=T)
mtext("A", side=side, line=line, cex=cex, adj=adj)
#text(0.05,0.99,'A',cex=1.25)

# B predictive germinator: moderate self thinning
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.6),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_plastic[2])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_plastic[2])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_plastic[2])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_plastic[2])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.5, 0.2, 'Moderate saturation with density',cex=1)
axis(1) # X axis
box()
mtext("B", side=side, line=line, cex=cex, adj=adj)

# C predictive germinator: moderate self thinning
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.6),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_plastic[3])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_plastic[3])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_plastic[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_plastic[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2.5, 0.2, 'Fast saturation with density',cex=1)
axis(1) # X axis
box()
mtext("C", side=side, line=line, cex=cex, adj=adj)
mtext('Germination variance',side=1,line=-2,cex=1.5,outer=T)
mtext('Germination fraction',side=2,line=3,cex=1.5,outer=TRUE)

dev.off()

#done
#####
# ESS for bet hedging ----------------------------------------------------------

# Plot these DD ESS combinations out for bet hedging for supporting figure 

pdf('figures/modeled_ESS_DD_bet_hedger.pdf',
    width=7,height=5)

# Multi-panel setup
layout(matrix(1:3, ncol=3))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# Panel label setup
line = 0.75
cex = 1.25
side = 3
adj=-.05


# A predictive germinator: slow self thinning
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.1),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_bet_hedger[1])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_bet_hedger[1])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_bet_hedger[1])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_bet_hedger[1])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2, 0.2, 'Slow saturation with density',cex=1)
axis(1) # X axis
axis(2) #Y axis
box()
mtext('Variable and Unpredictable',side=3,line=-2.5,cex=1.5,outer=T)
mtext("A", side=side, line=line, cex=cex, adj=adj)
#text(0.05,0.99,'A',cex=1.25)

# B predictive germinator: moderate self thinning
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.1),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_bet_hedger[2])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_bet_hedger[2])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_bet_hedger[2])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_bet_hedger[2])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2, 0.2, 'Moderate saturation with density',cex=1)
axis(1) # X axis
box()
mtext("B", side=side, line=line, cex=cex, adj=adj)

# C predictive germinator: moderate self thinning
plot(NULL,type="o",ylim=c(0,1.0),
     xlim=c(0,4.1),cex.lab=1.5,
     xlab="",ylab="",main='',axes=F)
points(gVars,data.frame(ESS_gFrac_given_gVar_list_bet_hedger[3])$ESS_gFrac_given_gVar,
       type="o",pch=16,cex=1.5,col="blue")
lines(gVars,data.frame(ESS_gFrac_given_gVar_list_bet_hedger[3])$ESS_gFrac_given_gVar,
      type="o",col="blue")
points(data.frame(ESS_gVar_given_gFrac_list_bet_hedger[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
lines(data.frame(ESS_gVar_given_gFrac_list_bet_hedger[3])$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
text(2, 0.2, 'Fast saturation with density',cex=1)
axis(1) # X axis
box()
mtext("C", side=side, line=line, cex=cex, adj=adj)
mtext('Germination variance',side=1,line=-2,cex=1.5,outer=T)
mtext('Germination fraction',side=2,line=3,cex=1.5,outer=TRUE)

dev.off()

#done
#done-----


