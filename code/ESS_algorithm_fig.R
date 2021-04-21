# Called from find_gFrac.R

# # visualize invasion growth rates
# contour(inv.logit(gRes),delta_g,t(rbar_grid),xlab="Resident g",ylab="delta g")
# image(inv.logit(gRes),delta_g,t(rbar_grid),xlab="Resident g",ylab="delta g")
# 
# plot(delta_g,rbar_grid[,3])
# matplot(delta_g,rbar_grid,type="l")
# abline(h=0)

# function to make colors transparent
## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

# set up data frame for plotting
D=expand.grid(gres=inv.logit(gRes),d=delta_g)
D=D[order(D$gres,D$d),]
D$ginv=D$gres+D$d*4
D$rbar=as.numeric(rbar_grid)
D$col=ifelse(D$rbar<=0,t_col("blue",percent = 70, name="myBlue"),t_col("red",percent = 70, name="myRed"))

png("ESS_supp_fig.png",height=6,width=6,res=400,units="in")

par(mfrow=c(2,1),tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,5,1,1))

symbols(D$gres,D$ginv,circles=sqrt(abs(D$rbar)),xlim=c(0,1),fg=D$col,bg=D$col,inches=0.25,
        xlab="Resident g",ylab="Mutant g")
abline(0,1)
mtext("A",side=3,line=0.1,adj=0)

plot(inv.logit(gRes),slopes,xlim=c(0,1),xlab="Resident g", ylab="Slope",pch=16)
abline(h=0,col="darkgray")
lines(predict(ss))
abline(v=ESS,col="red")
mtext("B",side=3,line=0.1,adj=0)

dev.off()
