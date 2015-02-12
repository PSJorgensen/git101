###############   THIS IS MY FIRST SCRIPT USING R STUDIO, GIT AND GITHUB

## first executable command - the random walk, which is what this currently feels like

#compiling function
rand.walk<-function(sim.number,sim.length){

#creating initital objects
n0<-rnorm(1)  
n.vec<-rep(NA,sim.length)
  
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

n.sims.df<-as.data.frame(n.sims)
names(n.sims.df)<-seq(1:sim.number)



#doing summary statistics of the simulations
summary.df<-as.data.frame(t(
  apply(
      as.data.frame(n.sims), 1, function(x) c(mean(x),sd(x))
    )
    ))

names(summary.df)<-c("mean","sd")

summary.df[,"n.plus.sd"]<-summary.df[,1]+summary.df[,2]
summary.df[,"n.minus.sd"]<-summary.df[,1]-summary.df[,2]


# initiating printing of plot of summary results
plot(summary.df[,"mean"],
     type = "l",
     ylim = c(min(summary.df),max(summary.df)),
     xlab = "time",
     ylab = "n"
     )

#plotting standard deviation
polygon(x = c(1:sim.length, sim.length:1),
        y = c(summary.df[,"n.minus.sd"], rev(summary.df[,"n.plus.sd"])), 
        col = "grey70"
        )

#plotting mean
lines(summary.df[,1])


## creating a list to return objects
return.object<-list()
return.object[["summary"]]<-summary.df
return.object[["sims"]]<-n.sims.df

## returning objects
return(return.object)

}


#setting parameters
set.seed(234)
output<-rand.walk(sim.number=500,sim.length=200)
