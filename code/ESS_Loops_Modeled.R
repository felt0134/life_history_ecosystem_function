
# ESS loops for toy model parameter set where the rate of yield saturation is
# far slower than that empirically observed in Sonoran populations

# Get rate of yield for hypothetical parameter set-----------------

# Hypothetical parameter set with low density dependence/slower rate of yield
# saturaion with density than observed in Sonoran populations

# Do 2000 per capita pop size, nice and clean

range<-c('low','medium','high')
Alpha<-c(0.001,0.002,0.004)
Fecundity<-c(2,4,8)

modeled.values<-data.frame(range,Alpha,Fecundity)

#list of species to get relatavized values...
list.range<-modeled.values$range

#list to store ratios
max.div.by.half.sat.list.model<-list()

# maximum
maxN = 300

# Loop through the sonoroan alpha-fec combo
for(irange in 1:length(list.range)){
  
  ranges<-list.range[irange]
  ranges.data<-subset(modeled.values,range==ranges)
  
  #get output
  out = grow_res(seeds_res=1:maxN,Fec=ranges.data$Fecundity,alpha=ranges.data$Alpha,seedSurv=0.9,G_res=1,1)
  
  #set and run the model
  individuals<-1:maxN
  seeds<-out$yield
  data.dd<-data.frame(cbind(individuals,seeds))
  
  #initial parameter estimate approach
  m1 <- stats::getInitial(seeds ~ SSlogis(individuals,Asym, xmid, scal), data=data.dd)
  max.div.by.half.sat<-m1[1]/m1[2] #asymptote divided by half saturation point
  max.div.by.half.sat<-data.frame(max.div.by.half.sat)
  max.div.by.half.sat$ranges<-ranges
  
  #store in list
  max.div.by.half.sat.list.model[[irange]] <- max.div.by.half.sat
  
  
}

# Make into data frame
max.div.by.half.sat.model.df.modeled <- do.call("rbind", max.div.by.half.sat.list.model)
max.div.by.half.sat.model.df.modeled_2<-max.div.by.half.sat.model.df.modeled #for later use in plots

# Vary fecundity and alpha for predictable and variable environment ------------


# Three hypothetical levels beyond the empirical range
alpha.list<-c(0.001,0.002,0.004)
# 2/0.001
# 4/0.002
# 8/0.004
# all have 2000 per-capita population size

ESS_gFrac_given_gVar_list_plastic<-list()
ESS_gVar_given_gFrac_list_plastic<-list()

for(ialpha in 1:length(alpha.list)){
  
  competition <- alpha.list[ialpha]
  
  # New parameter set
  if(competition==0.001){fecundity=2}
  else if(competition==0.002){fecundity=4}
  else if(competition==0.004){fecundity=8}
  
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

# Take a look
#plot_ess_fec_alpha_modeled(x=ESS_gFrac_given_gVar_list_plastic,y=ESS_gVar_given_gFrac_list_plastic,my_title = 'Predictive Germinator')

# Vary fecundity and alpha for variable and unpredictable environment ----------


# Three new levels to better reflect empirical values for per cap pop size
alpha.list<-c(0.001,0.002,0.004)

ESS_gFrac_given_gVar_list_bet_hedger<-list()
ESS_gVar_given_gFrac_list_bet_hedger<-list()

for(ialpha in 1:length(alpha.list)){
  
  competition <- alpha.list[ialpha]
  
  # New parameter set
  if(competition==0.001){fecundity=2}
  else if(competition==0.002){fecundity=4}
  else if(competition==0.004){fecundity=8} 
  
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
# plot_ess_fec_alpha_modeled(x=ESS_gFrac_given_gVar_list_bet_hedger,y=ESS_gVar_given_gFrac_list_bet_hedger,my_title = 'Bet Hedger')

# done--------------------------------------------------------------------------



