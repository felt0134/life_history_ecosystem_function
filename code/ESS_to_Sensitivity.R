# ESS to sensitivity for Sonoran Parameter set

# This script take the ESS estimates from ESS_Loops.R script and uses them
# to get estimates of sensitivity and lag effects in the different environments


# Run ESS_Loops.R first. This script finds evolutionary stable strategies for different combinations
# environmental variability and predictability under different Sonoran parameter values of the
# rate of yield saturation with density (alpha, fecundity combinations) and seed survival. These values
# have already been input into the ESS_Loops.R script and were identified using
# the empirical_look.R script. Parameter combinations are also available in Table S1

source("ESS_Loops.R")

# Varying Fecundity and Alpha --------------------------------------------------

# PREDICTIVE GERMINATOR #

# Hold the other parameters constant at these values
mySeedSurv = 0.9
myRho = 0.8 
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1

# Take a look at different ESS

# Weak density dependence 
plot_ess_fec_alpha(x=ESS_gFrac_given_gVar_list_plastic[1],y=ESS_gVar_given_gFrac_list_plastic,my_title = 'Predictive Germinator')

# Get intersection point/where ESS is for this environment. There may be subtle variation
# depending on the simulation. This doesn't not influence the underlying results.

# abline(v=1.3)
# abline(h=0.73)

# Get the model coefficients
pg_low_alpha<-get_sensitivity_fec_alpha(gVar=1.3,gFrac=0.73,myAlpha=0.005,myFec=10)
pg_low_alpha$life_history <-'predictive_germinator'
pg_low_alpha$ranges <- 'low'

##

# Median density dependence in Sonoran community #
plot_ess_fec_alpha(x=ESS_gFrac_given_gVar_list_plastic[2],y=ESS_gVar_given_gFrac_list_plastic,my_title = 'Predictive Germinator')

# abline(h=0.69)
# abline(v=1)

# Get the model coefficients
pg_middle_alpha<-get_sensitivity_fec_alpha(gVar=1,gFrac=0.69,myAlpha=0.015,myFec=30)
pg_middle_alpha$life_history <-'predictive_germinator'
pg_middle_alpha$ranges <- 'medium'

##

# High high density dependence  #
plot_ess_fec_alpha(x=ESS_gFrac_given_gVar_list_plastic[3],y=ESS_gVar_given_gFrac_list_plastic,my_title = 'Predictive Germinator')

# abline(h=0.7)
# abline(v=1)

# Get the model coefficients
pg_high_alpha<-get_sensitivity_fec_alpha(gVar=1.0,gFrac=0.7,myAlpha=0.040,myFec=80)
pg_high_alpha$life_history <-'predictive_germinator'
pg_high_alpha$ranges <- 'high'

# Bind the coefficient data sets
pg_alphas<-rbind(pg_high_alpha,pg_middle_alpha,pg_low_alpha)

# Isolate slopes
pg_sens_alphas<-subset(pg_alphas,coef==c('rates$Fec[burn_in:tot_time]'))
pg_sens_alphas$sensitivity.coefficients<-round(pg_sens_alphas$sensitivity.coefficients,2)

# look at variability of seed production under strong density dependence 

# gVar=1.0
# gFrac=0.7
# myAlpha=0.040
# myFec=80
# myFecSigma = 0.5*myFec   # this makes it variable        # 0 =  unpredictable, 1 = perfectly predictable
# tot_time = 5000 # length of each simulation
# burn_in = tot_time/5 + 1
# rates = get_F_G(tot_time, mu=c(myFec,gFrac,gFrac),sigma=c(myFecSigma,gVar,gVar), rho=myRho)
# rates=data.frame(rates)
# sd(rates$G1)
# summary(rates)
# 
# #simulate resident population
# production = seeds = rep(NA, tot_time)
# seeds[1] = 2   # initial population
# for(i in 2:tot_time){
#   out = grow_res(seeds_res=seeds[i-1],Fec=rates$Fec[i],alpha=myAlpha,seedSurv=mySeedSurv,G_res=rates$G1[i],G_inv=rates$G2[i])
#   seeds[i] = out$seeds
#   production[i] = out$yield
# }
# 
# sd(production[2:5000])


##


# BET HEDGER #

#plot_ess_fec_alpha(x=ESS_gFrac_given_gVar_list_bet_hedger,y=ESS_gVar_given_gFrac_list_bet_hedger,my_title = 'Bet Hedger')

# Hold other parameters constant
myRho = 0
mySeedSurv = 0.9
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1

# Low density dependence
plot_ess_fec_alpha(x=ESS_gFrac_given_gVar_list_bet_hedger[1],y=ESS_gVar_given_gFrac_list_bet_hedger,my_title = 'Bet Hedger')

# abline(h=0.63)
# abline(v=0)

# Get the model coefficients
bh_low_alpha<-get_sensitivity_fec_alpha(gVar=0,gFrac=0.63,myAlpha=0.005,myFec=10)
bh_low_alpha$life_history <-'bet_hedging'
bh_low_alpha$ranges <- 'low'
  
##

# Median density dependence
plot_ess_fec_alpha(x=ESS_gFrac_given_gVar_list_bet_hedger[2],y=ESS_gVar_given_gFrac_list_bet_hedger,my_title = 'Bet Hedger')

# abline(h=0.62)
# abline(v=0)

# Get the model coefficients
bh_middle_alpha<-get_sensitivity_fec_alpha(gVar=0,gFrac=0.62,myAlpha=0.015,myFec=30)
bh_middle_alpha$life_history <-'bet_hedging'
bh_middle_alpha$ranges <- 'medium'

##

# High density dependence
plot_ess_fec_alpha(x=ESS_gFrac_given_gVar_list_bet_hedger[3],y=ESS_gVar_given_gFrac_list_bet_hedger,my_title = 'Bet Hedger')

# abline(h=0.62)
# abline(v=0)

# Get the model coefficients
bh_high_alpha<-get_sensitivity_fec_alpha(gVar=0,gFrac=0.62,myAlpha=0.040,myFec=80)
bh_high_alpha$life_history <-'bet_hedging'
bh_high_alpha$ranges <- 'high'

# Bind them
bh_alphas<-rbind(bh_high_alpha,bh_middle_alpha,bh_low_alpha)

# Isolate slopes
bh_sens_alphas<-subset(bh_alphas,coef==c('rates$Fec[burn_in:tot_time]'))
bh_sens_alphas$sensitivity.coefficients<-round(bh_sens_alphas$sensitivity.coefficients,2)

# Merge the sensitivity datasets and save them for later use
sens_alphas<-rbind(bh_sens_alphas,pg_sens_alphas)
row.names(sens_alphas) <- NULL
sens_alphas<-sens_alphas[-c(2)]

# look at variability of seed production under strong density dependence 

# gVar=0
# gFrac=0.62
# myAlpha=0.040
# myFec=80
# myFecSigma = 0.5*myFec   # this makes it variable        # 0 =  unpredictable, 1 = perfectly predictable
# tot_time = 5000 # length of each simulation
# burn_in = tot_time/5 + 1
# rates = get_F_G(tot_time, mu=c(myFec,gFrac,gFrac),sigma=c(myFecSigma,gVar,gVar), rho=myRho)
# rates=data.frame(rates)
# sd(rates$G1)
# sd(rates$Fec)
# #39.78
# 
# #simulate resident population
# production = seeds = rep(NA, tot_time)
# seeds[1] = 2   # initial population
# for(i in 2:tot_time){
#   out = grow_res(seeds_res=seeds[i-1],Fec=rates$Fec[i],alpha=myAlpha,seedSurv=mySeedSurv,G_res=rates$G1[i],G_inv=rates$G2[i])
#   seeds[i] = out$seeds
#   production[i] = out$yield
# }
# 
# sd(production[2:5000])

# Save this dataset for later use
write.csv(sens_alphas,file="derived_data/sonoran_sensitvity_alphas.csv")

#######
# Varying Seed Survival --------------------------------------------------------

# PREDICTIVE GERMINATOR #

# Hold other parameters constant: assume 'median' value of density dependence 

myFec=30   
myRho = 0.8
myAlpha=0.015
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1

#take a look at different ESS

# High seed survival, mySeedSurv = 0.9. Have already looked at this
# since it is the original parameter set

plot_ess_seed_survivals(x=ESS_gFrac_given_gVar_list_ss_plastic[1],y=ESS_gVar_given_gFrac_list_ss_plastic,
                        my_title='Predictive Germinator',highss = T)

# abline(h=0.69)
# abline(v=1)

# Get the model coefficients: 

# SS = 0.9
pg_high_ss<-get_sensitivity_ss(gVar=1,gFrac=0.69,mySeedSurv = 0.9)
pg_high_ss$life_history <-'predictive_germinator'

##

# For seed survival of 0.7 #
plot_ess_seed_survivals(x=ESS_gFrac_given_gVar_list_ss_plastic[2],y=ESS_gVar_given_gFrac_list_ss_plastic,
                        my_title='Predictive Germinator',highss = F)

# Get the model coefficients: 

# abline(h=0.83)
# abline(v=0.9)

# Get the model coefficients
pg_medium_ss<-get_sensitivity_ss(gVar=0.9,gFrac=0.83,mySeedSurv = .7)
pg_medium_ss$life_history <-'predictive_germinator'

##

# For seed survival of 0.5 
plot_ess_seed_survivals(x=ESS_gFrac_given_gVar_list_ss_plastic[3],y=ESS_gVar_given_gFrac_list_ss_plastic,
                        my_title = 'Predictive Germinator',highss=T)

# abline(h=0.96)
# abline(v=1.4)

# Get the model coefficients
pg_low_ss<-get_sensitivity_ss(gVar=1.4,gFrac=0.96,mySeedSurv = 0.5)
pg_low_ss$life_history <-'predictive_germinator'

# Bind them
pg_ss<-rbind(pg_high_ss,pg_medium_ss,pg_low_ss)

# Isolate slopes
pg_sens_ss<-subset(pg_ss,coef==c('rates$Fec[burn_in:tot_time]'))


# BET HEDGER #

# Hold other parameters constant: assume 'median' value of density dependence
myFec=30 # median DD 
myRho = 0.0
myAlpha = 0.015 # median DD
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1

# Take a look at different ESS

# For seed survival = 0.9, already done this
plot_ess_seed_survivals(x=ESS_gFrac_given_gVar_list_ss_bet_hedger[1],y=ESS_gVar_given_gFrac_list_ss_bet_hedger,
                        highss=T,my_title='Bet Hedger')

# abline(h=0.62)
# abline(v=0)

# Get coefficients
bh_high_ss<-get_sensitivity_ss(gVar=0,gFrac=0.62,mySeedSurv = 0.9)
bh_high_ss$life_history <-'bet_hedger'

##

# For seed survival of 0.7 #

plot_ess_seed_survivals(x=ESS_gFrac_given_gVar_list_ss_bet_hedger[2],y=ESS_gVar_given_gFrac_list_ss_bet_hedger,
                        highss=T,my_title='Bet Hedger')

# abline(h=0.85)
# abline(v=0)

# Get coefficients
bh_medium_ss<-get_sensitivity_ss(gVar=0,gFrac=0.85,mySeedSurv = 0.7)
bh_medium_ss$life_history <-'bet_hedger'

##

# seed survival of 0.5 #

plot_ess_seed_survivals(x=ESS_gFrac_given_gVar_list_ss_bet_hedger[3],y=ESS_gVar_given_gFrac_list_ss_bet_hedger,
                        highss=T,my_title='Bet Hedger')

# abline(h=0.93)
# abline(v=0)

# Get the model coefficients

bh_low_ss<-get_sensitivity_ss(gVar=0,gFrac=0.93,mySeedSurv = 0.5)
bh_low_ss$life_history <-'bet_hedger'

# Bind them
bh_ss<-rbind(bh_high_ss,bh_medium_ss,bh_low_ss)

# Isolate slopes
bh_sens_ss<-subset(bh_ss,coef==c('rates$Fec[burn_in:tot_time]'))

# Merge the sensitivity data sets and save them for later use
sens_ss<-rbind(bh_sens_ss,pg_sens_ss)

#View(sens_alphas)
row.names(sens_ss) <- NULL
sens_sss<-sens_ss[-c(2)]
sens_ss$density_dependence<-'median'

# Save for later use
write.csv(sens_ss,file="derived_data/sonoran_sensitvity_SS.csv")

# done --------


