
library(mvtnorm)

# Simulate Fecundity and Germination rates for two populations;
# both have the same fecundity, but can have differences in germination
# mean and variance. The F-G covariance is fixed for both.

  get_F_G = function(Nyears, mu, sigma , rho){
    # mu is a vector of dim 3: mean rates for fecundity, germ pop 1, germ pop 2
    # sigma is a vector of dim 3: variance for fecundity, germ pop 1, germ pop 2
    # rho is the correlation of fecundity and germination
    
    # set up variance-covariance matrix
    vcov = matrix(0,3,3)
    diag(vcov) = sigma^2
    vcov[2,1] = vcov[1,2] = rho*sigma[1]*sigma[2]  # turn correlation into covariance
    vcov[3,1] = vcov[1,3] = rho*sigma[1]*sigma[3]
    vcov[2,3] = vcov[3,2] = sigma[2]*sigma[3]  # make G1 and G2 perfectly correlated
    
    # simulate values
    out = rmvnorm(Nyears, mu, vcov)
    #?rmvnorm
    # inverse logit transform on germination columns
    out[,2:3] = exp(out[,2:3])/(1+exp(out[,2:3])) 
    
    # prevent negative values in fecundity
    out[out<0] = 0
    
    colnames(out)=c("Fec","G1","G2")
    return(out)
}

# The population model. This tracks the population size
# of a resident, and outputs the new number of seeds,
# as well as the number of germinated plants.
# It also reports the population growth rate of the resident,
# and the population growth rate of an invader with a different germination rate.
grow_res = function(seeds_res,Fec,alpha,seedSurv,G_res,G_inv){
  	# This is an annual plant model, so it tracks populations as numbers of seeds

  	# N = population at time t
  	# Fec = fecundity
    # alpha and seedSurv are  scalars
  	# G_res = germination rates
  	# output =  list: seeds at time t+1, germinated plants at t+1
  
    #update resident
  	seedbank_carryover = seedSurv*(1-G_res)*seeds_res
  	seed_production = Fec*G_res*seeds_res/(1+alpha*G_res*seeds_res)
  	seeds_updated = seedbank_carryover + seed_production
  	# seeds_updated = rpois(1,seeds_updated)  # demographic stochasticity
  	r_res = log(seeds_updated/seeds_res)
  	
  	#update invader
  	seeds_init=1
  	seeds_inv = seedSurv*(1-G_inv)*seeds_init + Fec*G_inv*seeds_init/(1+alpha*G_res*seeds_res)
  	r_inv = log(seeds_inv/seeds_init)
  	
  	return(list("seeds" = seeds_updated, "yield" = seed_production, "r_res" = r_res, "r_inv" = r_inv))
  }
# 
# grow_inv = function(seeds_res,Fec,alpha,seedSurv,G_res,G_inv){
#   	seeds_init = 1
#   	seeds_new = seedSurv*(1-G_inv)*seeds_init+(Fec*seeds_init*G_inv)/(1+alpha*seeds_res*G_res)
#     r_inv = log(seeds_new/seeds_init)
# }


# Make a data frame from list object
  
list.to.df<-function(x,type){
  df.coefficients <- do.call("rbind", x)
  df.coefficients.2 <- data.frame(df.coefficients, row.names=NULL)
  colnames(df.coefficients.2)  <- c("slope")
  df.coefficients.2$model<-type
  df.coefficients.2$ID <- 1:nrow(df.coefficients.2)
  return(df.coefficients.2)
}


# Plot different ESS for different fecundity and alphas for Sonoran parameter set

# Take the different ESS values from germ fraction and variance (ESS loops script)
# for different different levels of alpha and fecundity and create the
# ESS plot to assess where the two traits overlap for a given environment
# x= the list of different germination fractions, y= the list for different germination variances
# visually assess this list to find the ESS for a given combination of fec and alpha in
# a given environment: seeing how sensitive ESS is to different levels of fec and alpha....

plot_ess_fec_alpha<-function(x,y,my_title){
  
  for(idata in 1:length(x)){
    
    #ensure these values are correct
    #gVars = c(0,0.5,1,2,4,6)
    gVars = c(0,0.1,0.5,1,2,4)
    gFracs = logit(seq(0.3,0.99,0.1))
    
    #make data frame for germination fractions
    data <-x[idata]
    data<-data.frame(data)
    
    # Sonoran parameter set
    if(mean(data$alpha.val)==0.005){ESS_gVar_given_gFrac=data.frame(y[1])}
    else if(mean(data$alpha.val)==0.015){ESS_gVar_given_gFrac=data.frame(y[2])}
    else if(mean(data$alpha.val)==0.04){ESS_gVar_given_gFrac=data.frame(y[3])}
    
    val.alpha = mean(data$alpha.val)
    val.fec = mean(data$fec.val)
    
    mar.default <- c(5,4,2,2) + 0.1
    par(mar = mar.default + c(1, 1, 0, 0))
    
    plot(gVars,data$ESS_gFrac_given_gVar,type="o",ylim=c(0,1),pch=16,cex=1.5,col="blue",cex.lab=1.5,
         xlab="Germination variance",ylab="Germination fraction",main=my_title)
    points(ESS_gVar_given_gFrac$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
    lines(ESS_gVar_given_gFrac$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
    text(0.5, 0.2, 'Alpha',cex=1.2)
    text(0.5, 0.15, 'Fecundity',cex=1.2)
    text(1.5, 0.2, val.alpha,cex=1)
    text(1.5, 0.15, val.fec,cex=1)
    
  }

}


# Plot different ESS for different fecundity and alphas for Sonoran parameter set
  
plot_ess_fec_alpha_modeled<-function(x,y,my_title){
  
  for(idata in 1:length(x)){
    
    #ensure these values are correct
    #gVars = c(0,0.5,1,2,4,6)
    gVars = c(0,0.1,0.5,1,2,4)
    gFracs = logit(seq(0.3,0.99,0.1))
    
    #make dataframe for germination fractions
    data <-x[idata]
    data<-data.frame(data)
    
    # Align the correct ESS g grac and g var with eachother: initial modeled parameter set
    if(mean(data$alpha.val)==0.001){ESS_gVar_given_gFrac=data.frame(y[1])}
    else if(mean(data$alpha.val)==0.002){ESS_gVar_given_gFrac=data.frame(y[2])}
    else if(mean(data$alpha.val)==0.004){ESS_gVar_given_gFrac=data.frame(y[3])}
    
    val.alpha = mean(data$alpha.val)
    val.fec = mean(data$fec.val)
    
    mar.default <- c(5,4,2,2) + 0.1
    par(mar = mar.default + c(1, 1, 0, 0))
    
    plot(gVars,data$ESS_gFrac_given_gVar,type="o",ylim=c(0,1),pch=16,cex=1.5,col="blue",cex.lab=1.5,
         xlab="Germination variance",ylab="Germination fraction",main=my_title)
    points(ESS_gVar_given_gFrac$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
    lines(ESS_gVar_given_gFrac$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
    text(0.5, 0.2, 'Alpha',cex=1.2)
    text(0.5, 0.15, 'Fecundity',cex=1.2)
    text(1.5, 0.2, val.alpha,cex=1)
    text(1.5, 0.15, val.fec,cex=1)
    
  }
  
}


# Plot ESS for different seed survivals 
  
plot_ess_seed_survivals<-function(x,y,my_title,highss=T){
  
  for(idata in 1:length(x)){
    
    
    # Make data frame for germination fractions
    data <-x[idata]
    data<-data.frame(data)
    
    val.ss = mean(data$survival.val)
    
    #align the correct ESS g grac and g var with eachother
    if(mean(data$survival.val)==0.9){ESS_gVar_given_gFrac=data.frame(y[1])}
    else if(mean(data$survival.val)==0.7){ESS_gVar_given_gFrac=data.frame(y[2])}
    else if(mean(data$survival.val)==0.5){ESS_gVar_given_gFrac=data.frame(y[3])}
    
    #ensure these values are correct
    if(highss==T){
      gVars = c(0,0.1,0.5,1,2,4)
      gFracs = logit(seq(0.35,0.95,0.1))}
    
    else{ 
      gVars = c(0,0.5,1,2,4,6)
      gFracs = logit(seq(0.39,0.99,0.1))}
    
    
    #gFracs = logit(seq(0.3,0.9,0.1))
    
    mar.default <- c(5,4,2,2) + 0.1
    par(mar = mar.default + c(1, 1, 0, 0))
    
    plot(gVars,data$ESS_gFrac_given_gVar,type="o",ylim=c(0,1),pch=16,cex=1.5,col="blue",cex.lab=1.5,
         xlab="Germination variance",ylab="Germination fraction",main=my_title)
    points(ESS_gVar_given_gFrac$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
    lines(ESS_gVar_given_gFrac$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
    text(1, 0.2, 'Seed Survival',cex=1.2)
    #text(0.5, 0.15, 'Fecundity',cex=1.2)
    text(1, 0.15, val.ss,cex=1)
    #text(1.5, 0.15, val.fec,cex=1)
    
  }
  }
 
 #ESS for different seed survivals for modeled parameter set. This was never used,
 # can probably delete
  
# plot_ess_seed_survivals_modeled<-function(x,y,my_title,highss=T){
#   
#   for(idata in 1:length(x)){
#     
#     
#     #make data frame for germination fractions
#     data <-x[idata]
#     data<-data.frame(data)
#     
#     val.ss = mean(data$survival.val)
#     
#     #align the correct ESS g grac and g var with eachother
#     if(mean(data$survival.val)==0.9){ESS_gVar_given_gFrac=data.frame(y[1])}
#     else if(mean(data$survival.val)==0.7){ESS_gVar_given_gFrac=data.frame(y[2])}
#     else if(mean(data$survival.val)==0.5){ESS_gVar_given_gFrac=data.frame(y[3])}
#     
#     #ensure these values are correct
#     if(highss==T){
#       gVars = c(0,0.5,1,2,4,6)
#       gFracs = logit(seq(0.30,0.90,0.1))}
#     
#     else{ 
#       gVars = c(0,0.5,1,2,4,6)
#       gFracs = logit(seq(0.39,0.99,0.1))}
#     
#     
#     #gFracs = logit(seq(0.3,0.9,0.1))
#     
#     mar.default <- c(5,4,2,2) + 0.1
#     par(mar = mar.default + c(1, 1, 0, 0))
#     
#     plot(gVars,data$ESS_gFrac_given_gVar,type="o",ylim=c(0,1),pch=16,cex=1.5,col="blue",cex.lab=1.5,
#          xlab="Germination variance",ylab="Germination fraction",main=my_title)
#     points(ESS_gVar_given_gFrac$ESS_gVar_given_gFrac,inv.logit(gFracs),pch=16,cex=1.5,col="red")
#     lines(ESS_gVar_given_gFrac$ESS_gVar_given_gFrac,inv.logit(gFracs),col="red")
#     text(1, 0.2, 'Seed Survival',cex=1.2)
#     #text(0.5, 0.15, 'Fecundity',cex=1.2)
#     text(1, 0.15, val.ss,cex=1)
#     #text(1.5, 0.15, val.fec,cex=1)
#     
#   }
# }


# Get sensitivity from ESS for different fecundity and alpha. This focuses on 
# getting sensitivity from the two environments in which there was environmental 
# variability

get_sensitivity_fec_alpha<-function(myFec,gFrac,gVar,myAlpha){

myFecSigma = 0.5*myFec
  
rates = get_F_G(tot_time, mu=c(myFec,gFrac,gFrac),sigma=c(myFecSigma,gVar,gVar), rho=myRho)
rates=data.frame(rates)

production = seeds = rep(NA, tot_time)
seeds[1] = 2   # initial population
for(i in 2:tot_time){
  out = grow_res(seeds_res=seeds[i-1],Fec=rates$Fec[i],alpha=myAlpha,seedSurv=mySeedSurv,G_res=rates$G1[i],G_inv=rates$G2[i])
  seeds[i] = out$seeds
  production[i] = out$yield
}

sensitivity = lm(production[burn_in:tot_time]~rates$Fec[burn_in:tot_time])
val<-data.frame(sensitivity$coefficients)
val$coef<-rownames(val)
val$alpha<-myAlpha

return(val)

}


# Get sensitivity from ESS for different seed survivals

get_sensitivity_ss<-function(gFrac,gVar,mySeedSurv){
  
  myFecSigma = 0.5*myFec
  
  rates = get_F_G(tot_time, mu=c(myFec,gFrac,gFrac),sigma=c(myFecSigma,gVar,gVar), rho=myRho)
  rates=data.frame(rates)
  
  production = seeds = rep(NA, tot_time)
  seeds[1] = 2   # initial population
  for(i in 2:tot_time){
    out = grow_res(seeds_res=seeds[i-1],Fec=rates$Fec[i],alpha=myAlpha,seedSurv=mySeedSurv,G_res=rates$G1[i],G_inv=rates$G2[i])
    seeds[i] = out$seeds
    production[i] = out$yield
  }
  
  sensitivity = lm(production[burn_in:tot_time]~rates$Fec[burn_in:tot_time])
  val<-data.frame(sensitivity$coefficients)
  val$coef<-rownames(val)
  val$ss<-mySeedSurv
  
  return(val)
  
}

###########

#cleaned 1/12/2021

