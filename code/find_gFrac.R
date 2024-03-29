
# For one germination variance and given environment, find ESS germination fraction
# Call from master.R

# loop through germination fraction grid
gRes = logit(seq(0.05,0.95,0.05))
gInv_step = 0.01
res_grid = rbar_grid = matrix(NA,5,length(gRes))
counter=0
for(iR in 1:length(gRes)){
  gInv = logit(seq(inv.logit(gRes[iR]) - 2*gInv_step, inv.logit(gRes[iR]) + 2*gInv_step, gInv_step))
  for(iI in 1:length(gInv)){
    
   # simulate rates
   rates = get_F_G(tot_time, mu=c(myFec,gRes[iR],gInv[iI]),sigma=c(myFecSigma,mySigmaG,mySigmaG), rho=myRho)
   rates=data.frame(rates)
   
   #simulate resident population
   seeds_res = rep(NA, tot_time)
   r_inv = rep(NA, tot_time)
   seeds_res[1] = 2   # initial population
   for(i in 2:tot_time){
     out = grow_res(seeds_res=seeds_res[i-1],Fec=rates$Fec[i],alpha=myAlpha,seedSurv=mySeedSurv,G_res=rates$G1[i],G_inv=rates$G2[i])
     seeds_res[i] = out$seeds
     r_inv[i] = out$r_inv
   } 
   res_grid[iI,iR] = mean(seeds_res[burn_in:tot_time])
   r_bar = mean(r_inv[burn_in:(tot_time-1)])
   rbar_grid[iI,iR] = r_bar
   
   # report progress
   counter = counter + 1
   if(counter%%10==0) print(paste(counter,"out of",length(gRes)*length(gInv),"complete"))
   
  }
}

delta_g = seq(-2*gInv_step,2*gInv_step,gInv_step)

slopes = numeric(length(gRes))
for(i in 1:length(gRes)){
  slopes[i] = coef(lm(rbar_grid[,i]~delta_g))[2]
}

if(sum(slopes<0)==0){
  ESS = 1
}else if(sum(slopes>0)==0){
  ESS = 0
}else{
  # fit spline through slopes
  ss = smooth.spline(x=inv.logit(gRes),y=slopes,df=6)
  # plot(gRes,slopes)
  # lines(predict(ss))
  ss_fun = function(x) predict(ss, x)$y 
  # find where slope spline crosses zero
  tmp = uniroot(ss_fun,interval=c(min(inv.logit(gRes)),max(inv.logit(gRes))),extendInt = "yes")$root #added the 'extend', see if it works...
  ESS = tmp
  
  # alternative approach: find where max rbar is closest to zero
  rbar_max = apply(rbar_grid,2,FUN="max")
  ESS2 = inv.logit(gRes[which(abs(rbar_max)==min(abs(rbar_max)))])
  
  # make sure rbar_max closest to zero is also where g_inv = g_res (position = 3)
  rbar_max_position = numeric(length(rbar_max))
  for(i in 1:length(rbar_max)) rbar_max_position[i] = which(rbar_grid[,i]==rbar_max[i])
  ESS3 = 3 == rbar_max_position[ which(abs(rbar_max)==min(abs(rbar_max))) ]
  
  # plot all three together
  old_par = par(no.readonly=TRUE)
  par(mfrow=c(3,1),mar=c(2.5,4,0,1),oma=c(2,0,1,0))
  plot(inv.logit(gRes),slopes)
  lines(predict(ss))
  abline(v=ESS,col="red")
  plot(inv.logit(gRes),rbar_max)
  abline(h=0,lty="dotted")
  abline(v=ESS2,col="red")
  plot(inv.logit(gRes),rbar_max_position)
  abline(h=3,lty="dotted")
  mtext("inv.logit(g_res)",side=1,outer=T)
  par(old_par)
  
}

# make figure for supplement
# source("ESS_algorithm_fig.R")


