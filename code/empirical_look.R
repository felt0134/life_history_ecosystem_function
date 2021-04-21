
# This script allows you to Look at empirical parameter estimates of 
# fecundity and alpha from the Sonoran Desert winter annual plant community

# Look at empirical density dependence for Sonoran Desert ----------------------


#first load in the fecundity data sets

# Load  data
empirical_alphas<-read.csv('empirical_data/empirical_alphas.csv')
# head(empirical_alphas)
# hist(empirical_alphas$Alpha)
# summary(empirical_alphas)

# Sonoran Desert annuals
fec.sonoran <- read.csv('empirical_data/12sp_K_intrasp_onlyfits.csv')
fec.sonoran.av<-aggregate(Kt~species,mean,data=fec.sonoran)
fec.sonoran.av$site<-'Sonoran'
colnames(fec.sonoran.av) <- c('Species','Fecundity','Site')
#head(fec.sonoran.av)

# Merge with alphas data set
alpha_fec_sonoran<-merge(fec.sonoran.av,empirical_alphas,
                                   by=c('Species','Site'))

# head(alpha_fec_sonoran)
# View(alpha_fec_sedgewick_sonoran)

##

# Look at density dependence 

# Loop it

# List of species to get relative d values...
list.species<-alpha_fec_sonoran$Species

# List to store ratios
max.div.by.half.sat.list<-list()

# maximum number
maxN = 300

# Loop through each species

for(ispecies in 1:length(list.species)){
  
  species<-list.species[ispecies]
  species.data<-subset(alpha_fec_sonoran,Species==species)

#get output
out = grow_res(seeds_res=1:maxN,Fec=species.data$Fecundity,alpha=species.data$Alpha,seedSurv=0.9,G_res=1,1)

#set and run the model
individuals<-1:maxN
seeds<-out$yield
data.dd<-data.frame(cbind(individuals,seeds))

# To estimate the rate at which yield saturates with density: 
# Divide asymptote by the half saturation point, using nonlinear initial paramater function. 
# This approach was compared with other estimates from other nonlinear functions 
# and it gives the same output for this metric

#initial paramter estimate approach
m1 <- stats::getInitial(seeds ~ SSlogis(individuals,Asym, xmid, scal), data=data.dd)
max.div.by.half.sat<-(m1[1])/(m1[2]) #asymptote divided by half saturation point
max.div.by.half.sat<-data.frame(max.div.by.half.sat)
max.div.by.half.sat$Species<-species

#store in list
max.div.by.half.sat.list[[ispecies]] <- max.div.by.half.sat


}

# Make this list into a data frame
max.div.by.half.sat.df<- do.call("rbind", max.div.by.half.sat.list)
#hist(max.div.by.half.sat.df$max.div.by.half.sat)
#summary(max.div.by.half.sat.df)

#look at correlation between  and population size
alpha_fec_sonoran$eq <- alpha_fec_sonoran$Fecundity/alpha_fec_sonoran$Alpha

# Merge with original data frame
sonoran_full<-merge(alpha_fec_sonoran,max.div.by.half.sat.df,by='Species')

# look at correlations between these parameters in the Sonoran community

# cor(sonoran_full$Alpha,sonoran_full$Fecundity)
# #-0.32
# cor(sonoran_full$eq,sonoran_full$Fecundity)
# #0.80
# cor(sonoran_full$eq,sonoran_full$Alpha)
# #-0.68
# cor(sonoran_full$eq,sonoran_full$max.div.by.half.sat)
#0.88

# do same loop for the 'modeled' alphas used in the simulation study

#make data frame: 'hypothetical' parameter set 

# Parameter set constrained by the range of Sonoran estimates 

# Set per-capita population size to 2000

range<-c('low','medium','high')
Alpha<-c(0.005,0.015,0.040)
Fecundity<-c(10,30,80)
#10/0.005 = 2000

#create dataframe of these values
modeled.values<-data.frame(range,Alpha,Fecundity)

#list of species to get relativized values...
list.range<-modeled.values$range

#list to store ratios
max.div.by.half.sat.list.model<-list()
half.sat.list<-list()

# maximum
maxN = 300

# Loop through the sonoroan alpha-fecundity combinations

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
  
  #half saturation constant
  half.sat<-m1[2] #asymptote divided by half saturation point
  half.sat<-data.frame(half.sat)
  half.sat$ranges<-ranges
  
  #store in list
  max.div.by.half.sat.list.model[[irange]] <- max.div.by.half.sat
  half.sat.list[[irange]] <- half.sat
  
  
}

# Make into data frame
max.div.by.half.sat.model.df <- do.call("rbind", max.div.by.half.sat.list.model)
half.sat.model.df <- do.call("rbind", half.sat.list)
half.sat.model.df<-half.sat.model.df[c("xmid2","xmid1","xmid"),] #re-order


# done -------------------------------------------------------------------------
