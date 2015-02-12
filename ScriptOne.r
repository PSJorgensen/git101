###############   THIS IS MY FIRST SCRIPT USING R STUDIO, GIT AND GITHUB

## first executable command - the random walk, which is what this currently feels like

#setting parameters
set.seed(235)
sim.length<-100
n.vec<-rep(NA,sim.length)
n0<-rnorm(1)  

#random walk loop
for(i in 1:sim.length){
if(i==1){ni<-n0}
if(i>1){ni<-n.vec[i-1]}
n.vec[i]<-ni+rnorm(1)  
}

# plotting results
plot(n.vec,type="l")

