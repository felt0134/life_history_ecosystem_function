#ESS Loop for Sonoran Desert parameters

# First clear work space, set directory, and load libraries/functions needed 

# End of prep

# Vary fecundity and alpha for predictable and variable environment ------------

# 10/0.005
# 30/0.015
# 80/0.04
# All equals 2000 per-capita population size

# Sonoran alpha list
alpha.list<-c(0.005,0.015,0.040) #levels reflect range of empirical values

ESS_gFrac_given_gVar_list_plastic<-list()
ESS_gVar_given_gFrac_list_plastic<-list()

for(ialpha in 1:length(alpha.list)){
  
competition <- alpha.list[ialpha]

if(competition==0.005){fecundity=10} 
else if(competition==0.015){fecundity=30} 
else if(competition==0.040){fecundity=80}

myFec=fecundity
myFecSigma = 0.5*myFec   # this makes it variable
myRho = 0.8      # 0 =  unpredictable, 1 = perfectly predictable
myAlpha = competition
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
  ESS_gFrac_given_gVar[iGvar] = ESS
}

ESS_gFrac_given_gVar<-data.frame(ESS_gFrac_given_gVar)
ESS_gFrac_given_gVar$id<-rownames(ESS_gFrac_given_gVar) #add a row ID for merging purposes
ESS_gFrac_given_gVar$alpha.val<-competition
ESS_gFrac_given_gVar$fec.val<-fecundity
ESS_gFrac_given_gVar_list_plastic[[ialpha]] <-ESS_gFrac_given_gVar

# Find ESS g var given a series of g fractions
tot_time = 10000 # length of each simulation
gFracs = logit(seq(0.3,0.9,0.1))
ESS_gVar_given_gFrac = rep(NA,length(gFracs))
for(iGfrac in 1:length(gFracs)){
  myGfrac = gFracs[iGfrac]
  print(paste0("Doing ",iGfrac," in ",length(gFracs), " g vars"))
  source("find_gVar.R")
  ESS_gVar_given_gFrac[iGfrac] = ESS
}

ESS_gVar_given_gFrac<-data.frame(ESS_gVar_given_gFrac)
ESS_gVar_given_gFrac$id<-rownames(ESS_gVar_given_gFrac)
ESS_gVar_given_gFrac$alpha.val<-competition
ESS_gVar_given_gFrac$fec.val<-fecundity
ESS_gVar_given_gFrac_list_plastic[[ialpha]] <-ESS_gVar_given_gFrac


}

# Take a peek
#plot_ess_fec_alpha(x=ESS_gFrac_given_gVar_list_plastic,y=ESS_gVar_given_gFrac_list_plastic,my_title = 'Predictive Germinator')

######

# Vary fecundity and alpha for variable and unpredictable environment ----------

# Updated list
alpha.list<-c(0.005,0.015,0.040)

ESS_gFrac_given_gVar_list_bet_hedger<-list()
ESS_gVar_given_gFrac_list_bet_hedger<-list()

for(ialpha in 1:length(alpha.list)){
  
  competition <- alpha.list[ialpha]
  
  if(competition==0.005){fecundity=10} 
  else if(competition==0.015){fecundity=30} 
  else if(competition==0.040){fecundity=80} 
  
  myFec=fecundity
  myFecSigma = 0.5*myFec   # this makes it variable
  myRho = 0      # 0 =  unpredictable, 1 = perfectly predictable. This is all that is changed...
  myAlpha = competition
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
    ESS_gFrac_given_gVar[iGvar] = ESS
  }

  ESS_gFrac_given_gVar<-data.frame(ESS_gFrac_given_gVar)
  ESS_gFrac_given_gVar$id<-rownames(ESS_gFrac_given_gVar) #add a row ID for merging purposes
  ESS_gFrac_given_gVar$alpha.val<-competition
  ESS_gFrac_given_gVar$fec.val<-fecundity
  ESS_gFrac_given_gVar_list_bet_hedger[[ialpha]] <-ESS_gFrac_given_gVar
  
  gFracs = logit(seq(0.3,0.9,0.1))
  ESS_gVar_given_gFrac = rep(NA,length(gFracs))
  for(iGfrac in 1:length(gFracs)){
    myGfrac = gFracs[iGfrac]
    print(paste0("Doing ",iGfrac," in ",length(gFracs), " g vars"))
    source("find_gVar.R")
    ESS_gVar_given_gFrac[iGfrac] = ESS
  }
  
  ESS_gVar_given_gFrac<-data.frame(ESS_gVar_given_gFrac)
  ESS_gVar_given_gFrac$id<-rownames(ESS_gVar_given_gFrac)
  ESS_gVar_given_gFrac$alpha.val<-competition
  ESS_gVar_given_gFrac$fec.val<-fecundity
  ESS_gVar_given_gFrac_list_bet_hedger[[ialpha]] <-ESS_gVar_given_gFrac
  
  
}

# Take a look
#plot_ess_fec_alpha(x=ESS_gFrac_given_gVar_list_bet_hedger,y=ESS_gVar_given_gFrac_list_bet_hedger,my_title = 'Bet Hedger')

########

# Vary seed survival for variable and predictable environment ------------------

# Seed survival list
ss.list<-c(0.9,0.7,0.5)

ESS_gFrac_given_gVar_list_ss_plastic<-list()
ESS_gVar_given_gFrac_list_ss_plastic<-list()

for(iseed in 1:length(ss.list)){
  
  seed.survival <- ss.list[iseed]
  
  myFec=30 # median level of DD
  myFecSigma = 0.5*myFec   # this makes it variable
  myRho = 0.8      # 0 =  unpredictable, 1 = perfectly predictable
   myAlpha = 0.015 # median level of DD
  #myAlpha = 0.005 #low DD
  mySeedSurv = seed.survival
  tot_time = 5000 # length of each simulation
  burn_in = tot_time/5 + 1
  
  # Find ESS g fraction given a series of g variances
  gVars = c(0,0.1,0.5,1,2,4)
  ESS_gFrac_given_gVar = rep(NA,length(gVars))
  for(iGvar in 1:length(gVars)){
    mySigmaG = gVars[iGvar]
    print(paste0("Doing ",iGvar," in ",length(gVars), " g vars"))
    source("find_gFrac.R")
    ESS_gFrac_given_gVar[iGvar] = ESS
  }
  
  tot_time = 10000 # length of each simulation
  gFracs = logit(seq(0.35,0.95,0.1))
  ESS_gVar_given_gFrac = rep(NA,length(gFracs))
  for(iGfrac in 1:length(gFracs)){
    myGfrac = gFracs[iGfrac]
    print(paste0("Doing ",iGfrac," in ",length(gFracs), " g vars"))
    source("find_gVar.R")
    ESS_gVar_given_gFrac[iGfrac] = ESS
  }
  
  
  ESS_gFrac_given_gVar<-data.frame(ESS_gFrac_given_gVar)
  ESS_gFrac_given_gVar$survival.val <-mySeedSurv
  ESS_gFrac_given_gVar_list_ss_plastic[[iseed]] <-ESS_gFrac_given_gVar
  
  ESS_gVar_given_gFrac<-data.frame(ESS_gVar_given_gFrac)
  ESS_gVar_given_gFrac$survival.val <-mySeedSurv
  ESS_gVar_given_gFrac_list_ss_plastic[[iseed]] <-ESS_gVar_given_gFrac
  
  
}


#Take a look
# plot_ess_seed_survivals(x=ESS_gFrac_given_gVar_list_ss_plastic,y=ESS_gVar_given_gFrac_list_ss_plastic,
#                         my_title='Predictive Germinator',highss = T)

######

# Vary seed survival for variable and unpredictable environment ----------------

# Seed survival list
ss.list<-c(0.9,0.7,0.5)

ESS_gFrac_given_gVar_list_ss_bet_hedger<-list()
ESS_gVar_given_gFrac_list_ss_bet_hedger<-list()

for(iseed in 1:length(ss.list)){
  
  seed.survival <- ss.list[iseed]
  
  myFec=30 # median level of DD
  myFecSigma = 0.5*myFec   # this makes it variable
  myRho = 0      # 0 =  unpredictable, 1 = perfectly predictable
  myAlpha = 0.015 # median level of DD
  #myAlpha = 0.005 #low DD
  mySeedSurv = seed.survival
  tot_time = 5000 # length of each simulation
  burn_in = tot_time/5 + 1

  gVars = c(0,0.1,0.5,1,2,4)
  ESS_gFrac_given_gVar = rep(NA,length(gVars))
  for(iGvar in 1:length(gVars)){
    mySigmaG = gVars[iGvar]
    print(paste0("Doing ",iGvar," in ",length(gVars), " g vars"))
    source("find_gFrac.R")
    print(paste0("ESS - ESS2 = ", ESS-ESS2)) # compare two methods for determining ESS
    ESS_gFrac_given_gVar[iGvar] = ESS
  }
  
  tot_time = 10000 # length of each simulation
  gFracs = logit(seq(0.35,0.95,0.1))
  ESS_gVar_given_gFrac = rep(NA,length(gFracs))
  for(iGfrac in 1:length(gFracs)){
    myGfrac = gFracs[iGfrac]
    print(paste0("Doing ",iGfrac," in ",length(gFracs), " g vars"))
    source("find_gVar.R")
    ESS_gVar_given_gFrac[iGfrac] = ESS
  }
  
  ESS_gFrac_given_gVar<-data.frame(ESS_gFrac_given_gVar)
  ESS_gFrac_given_gVar$survival.val <-mySeedSurv
  ESS_gFrac_given_gVar_list_ss_bet_hedger[[iseed]] <-ESS_gFrac_given_gVar
  
  ESS_gVar_given_gFrac<-data.frame(ESS_gVar_given_gFrac)
  ESS_gVar_given_gFrac$survival.val <-mySeedSurv
  ESS_gVar_given_gFrac_list_ss_bet_hedger[[iseed]] <-ESS_gVar_given_gFrac
  
  
  
}


# Take a look
# plot_ess_seed_survivals(x=ESS_gFrac_given_gVar_list_ss_bet_hedger,y=ESS_gVar_given_gFrac_list_ss_bet_hedger,
#                         highss=T,my_title='Bet Hedger')

# done -------------------------------------------------------------------------





