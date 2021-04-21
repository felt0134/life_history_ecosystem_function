
# ESS to sensitivity for modeled/hypothetical parameter set

# You need to run this first:
source("ESS_Loops_Modeled.R")

# Varying Fecundity and Alpha -----------------------------------------------

# PREDICTIVE GERMINATOR #

# Hold these parameters constant
mySeedSurv = 0.9
myRho = 0.8 
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1

# Take a look at different ESS

# Weak density dependence 
plot_ess_fec_alpha_modeled(x=ESS_gFrac_given_gVar_list_plastic[1],y=ESS_gVar_given_gFrac_list_plastic,my_title = 'Predictive Germinator')

# Get intersection point/where ESS is for this environment

# abline(v=4.0)
# abline(h=0.84)

#get the model coefficients
pg_low_alpha<-get_sensitivity_fec_alpha(gVar=4,gFrac=0.84,myAlpha=0.001,myFec=2)
pg_low_alpha$life_history <-'predictive_germinator'
pg_low_alpha$ranges <-'low'

##

# Medium density dependence
plot_ess_fec_alpha_modeled(x=ESS_gFrac_given_gVar_list_plastic[2],y=ESS_gVar_given_gFrac_list_plastic,my_title = 'Predictive Germinator')

# abline(h=0.8)
# abline(v=2.3)

pg_middle_alpha<-get_sensitivity_fec_alpha(gVar=2.3,gFrac=0.80,myAlpha=0.002,myFec=4)
pg_middle_alpha$life_history <-'predictive_germinator'
pg_middle_alpha$ranges <- 'medium'

##

# Highest density dependence  #
plot_ess_fec_alpha(x=ESS_gFrac_given_gVar_list_plastic[3],y=ESS_gVar_given_gFrac_list_plastic,my_title = 'Predictive Germinator')

# abline(h=0.63)
# abline(v=1.1)

# Get the model coefficients
pg_high_alpha<-get_sensitivity_fec_alpha(gVar=1.3,gFrac=0.63,myAlpha=0.004,myFec=8)
pg_high_alpha$life_history <-'predictive_germinator'
pg_high_alpha$ranges <- 'high'

# Bind them
pg_alphas<-rbind(pg_high_alpha,pg_middle_alpha,pg_low_alpha)

# Isolate slopes
pg_sens_alphas<-subset(pg_alphas,coef==c('rates$Fec[burn_in:tot_time]'))
pg_sens_alphas$sensitivity.coefficients<-round(pg_sens_alphas$sensitivity.coefficients,2)

# BET HEDGER #

#plot_ess_fec_alpha_modeled(x=ESS_gFrac_given_gVar_list_bet_hedger,y=ESS_gVar_given_gFrac_list_bet_hedger,my_title = 'Bet Hedger')

# Hold these parameters constant
mySeedSurv = 0.9
myRho = 0.0
tot_time = 5000 # length of each simulation
burn_in = tot_time/5 + 1

# Low density dependence
plot_ess_fec_alpha_modeled(x=ESS_gFrac_given_gVar_list_bet_hedger[1],y=ESS_gVar_given_gFrac_list_bet_hedger,my_title = 'Bet Hedger')

# abline(h=0.68)
# abline(v=0)

# Get the model coefficients
bh_low_alpha<-get_sensitivity_fec_alpha(gVar=0,gFrac=0.68,myAlpha=0.001,myFec=2)
bh_low_alpha$life_history <-'bet_hedging'
bh_low_alpha$ranges <- 'low'

##

# Medium density dependence
plot_ess_fec_alpha_modeled(x=ESS_gFrac_given_gVar_list_bet_hedger[2],y=ESS_gVar_given_gFrac_list_bet_hedger,my_title = 'Bet Hedger')

# abline(h=0.64)
# abline(v=0)

# Get coefficients
bh_middle_alpha<-get_sensitivity_fec_alpha(gVar=0,gFrac=0.64,myAlpha=0.002,myFec=4)
bh_middle_alpha$life_history <-'bet_hedging'
bh_middle_alpha$ranges <- 'medium'

##

# High density dependence

plot_ess_fec_alpha_modeled(x=ESS_gFrac_given_gVar_list_bet_hedger[3],y=ESS_gVar_given_gFrac_list_bet_hedger,my_title = 'Bet Hedger')

# abline(h=0.63)
# abline(v=0)

# Get the model coefficients
bh_high_alpha<-get_sensitivity_fec_alpha(gVar=0,gFrac=0.63,myAlpha=0.004,myFec=8)
bh_high_alpha$life_history <-'bet_hedging'
bh_high_alpha$ranges <- 'high'

# Bind them
bh_alphas<-rbind(bh_high_alpha,bh_middle_alpha,bh_low_alpha)

# Isolate slopes
bh_sens_alphas<-subset(bh_alphas,coef==c('rates$Fec[burn_in:tot_time]'))
bh_sens_alphas$sensitivity.coefficients<-round(bh_sens_alphas$sensitivity.coefficients,2)


# Merge and save data frame ----------------------------

#merge the sensitivity datasets and save them for later use...
sens_alphas<-rbind(bh_sens_alphas,pg_sens_alphas)
#View(sens_alphas)
row.names(sens_alphas) <- NULL
sens_alphas<-sens_alphas[-c(2)]
write.csv(sens_alphas,file="derived_data/modeled_sensitvity_alphas.csv")

# done ------
