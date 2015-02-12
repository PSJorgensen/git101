###############   THIS IS MY FIRST SCRIPT USING R STUDIO, GIT AND GITHUB

## first executable command - the random walk, which is what this currently feels like

#setting parameters
set.seed(233)
sim.number<-100
sim.length<-100
n.vec<-rep(NA,sim.length)
n0<-rnorm(1)  

#simulation loop
for(j in 1:sim.number){
#random walk loop
for(i in 1:sim.length){
if(i==1){n.old<-n0}
if(i>1){n.old<-n.vec[i-1]}
n.vec[i]<-n.old+rnorm(1)  
} # end random walk loop - now collect the data
if(j == 1){n.sims<-n.vec}
if(j > 1){n.sims<-cbind(n.sims,n.vec)}
}

#doing summary statistics of the simulations
summary.df<-as.data.frame(t(
  apply(
      as.data.frame(n.sims), 1, function(x) c(mean(x),sd(x))
    )
    ))

summary.df[,"n.plus.sd"]<-summary.df[,1]+summary.df[,2]
summary.df[,"n.minus.sd"]<-summary.df[,1]-summary.df[,2]

# plotting summary results
plot(summary.df[,1], type = "l", ylim = c(min(summary.df),max(summary.df)),
     xlab = "time", ylab = "n")

polygon(x = c(1:sim.number, sim.number:1),
        y = c(summary.df[,"n.minus.sd"], rev(summary.df[,"n.plus.sd"])), 
        col = "grey70")
        )

lines(summary.df[,1])