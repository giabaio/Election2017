# Load the relevant packages
library(sp)
library(R2jags)
library(maptools)
library(arm)
library(plyr)
library(xlsx)
library(lattice)

# Source utility functions
source("Utils.R")

# Load the data
load("data.Rdata")
# Adds the electorate total
data=merge(data,elec15[,c("ConstituencyName","Electorate15")],by="ConstituencyName")

# Defines model code & data
model.code = "model.txt"
working.dir <- paste(getwd(),"/",sep="")

# Defines the data list
# Data on observed proportion of vote for each party at the 2015 election
vote=data.frame(data$Con15,data$Lab15,data$UKIP15,data$LD15,data$SNP15,data$Green15,data$PC15,data$Other15)/100
vote[is.na(vote)]=0
# Number of parties
P=ncol(vote)
# Number of constituencies considered
C=nrow(vote)
# Proportion of Remain vote in each contituency
prem=data$pct_rem
# Loads data from the polls & fix (remove rows with missing data on the Greens, for simplicity)
polls=read.xlsx("polls.xls",sheetIndex=1)
polls=polls[!is.na(polls$Green),]
Npolls=nrow(polls)/2 
y=as.matrix(polls[1:(2*Npolls),c(2:9)])
ref=polls$EU_Ref    # 1=Remain, 2=Leave

# Defines prior mean (and sd, proportionally) for the alpha and beta effect (to induce prior on the vote share and the seats won)
ma=c(-.5,-3.5,-2.1,-2.2,-2.5,-4.5,-1.5)
mb=c(.4,-4,.7,.1,-.1,-1,-1.5)

# Runs the model
# Generates the initial values
inits <- function(){
    list()
}
# Defines the number of iteration, burn-in and thinning, and runs JAGS
n.iter <- 5000
n.burnin <- 2500
n.thin <- floor((n.iter-n.burnin)/500)
# Defines the data list
dataJags <- list(P=P,N=apply(y,1,sum,na.rm=T),C=C,vote=vote,prem=prem,pi15=apply(vote,2,mean),ref=ref,
                 tau.alpha=c(0,1/(.02*(ma^2))),tau.beta=c(0,1/(.02*(mb^2))),Npolls=Npolls,mu.alpha=c(0,ma),mu.beta=c(0,mb)) 
# Runs the model (forward sampling only --- no polls included). This is necessary only once
# Defines the parameters to be monitored
params <- c("alpha","beta","pi","pi17")
m0 <- jags(dataJags, inits=inits, params, model.file=model.code,n.chains=2, n.iter, n.burnin, n.thin,
    DIC=FALSE, working.directory=working.dir, progress.bar="text")

# Runs the model with the poll data
# Discount past polls (as a function of time passed from today)
disc=.1
# Dynamic analysis -- runs the model for consecutive days (need to fiddle with the starting point to have different values)
start=as.Date(paste0("2017-05-",(length(el)+1)))
today=seq.Date(from=start,to=Sys.Date(),by=1)
model=lapply(1:length(today),function(i) elec.today(today[i]))
el=lapply(1:length(model),function(i) make.election(model[[i]]$m))

# Plots the dynamic trend
range=c(as.Date("2017-05-01"),as.Date(paste0("2017-05-",length(el))))
all=matrix(unlist(lapply(1:length(el),function(i) el[[i]]$tab)),nrow=8)
colnames(all)=rep(colnames(el[[1]]$tab$tab),length(el))
rownames(all)=rownames(el[[1]]$tab$tab)
cols=c("blue","red","mediumpurple3","darkorange1","gold","green","olivedrab4","dark grey")
par(mar=c(2.1,4.1,4.1,2.1))
coefplot(all[,4],sds=rep(0,nrow(all)),lower.conf.bound=all[,3],upper.conf.bound=all[,5],vertical=F,ylim=c(0,400),
  main=paste0("Posterior number of seats: polls from\n",format(range[1],"%d %B")," to ",format(range[2],"%d %B")),
  col.pts=cols,varnames=rep("",8))
steps=seq(9,ncol(all),by=5); offset=seq(from=0,length.out=length(steps),by=.05)
for (i in 1:length(steps)) {
  coefplot(all[,steps[i]],sds=rep(0,nrow(all)),lower.conf.bound=all[,(steps[i]-1)],
     upper.conf.bound=all[,(steps[i]+1)],vertical=F,add=T,offset=offset[i],col.pts=cols)
}
legend("topright",bty="n",col=cols,legend=c("Con","Lab","UKIP","Lib Dem","SNP","Green","PCY","Other"),cex=1,lty=1,pch=20)

# Analysis of latest results
last=length(el)
post.el=el[[last]]
Winner17=post.el$Winner17
tabs=lapply(1:nrow(Winner17),function(i) table(Winner17[i,])/ncol(Winner17))
mat=ldply(tabs,rbind)
rownames(mat)=data$ConstituencyName
res=data.frame(Name=data$ConstituencyName,PCON15CD=data$PCON15CD,Country=data$Country,Region=data$Region,Winner15=data$Winner15)
for (c in 1:C) {
  res[c,"Winner17"]=paste0(colnames(mat)[which.max(mat[c,])]," ",
     cut(mat[,which.max(mat[c,])],breaks=c(.45,.5,.55,.6,.8,1),include.lowest=T)[c])
}
res$Winner17=factor(res$Winner17)
levels(res$Winner15)[4]="Lib Dem"
levels(res$Winner15)[6]="SNP"
for (i in 1:nrow(res)) {
   res$swing[i]=!grepl(res$Winner15[i],res$Winner17[i])
}

# Plot seats
plot.seats(post.el,model[[last]]$m)

# Swinging constituencies
mat[is.na(mat)]=0
mat$Winner15=data$Winner15
for (i in 1:nrow(mat)) {
   mat$Winner17[i]=colnames(mat)[which.max(mat[i,1:5])]
}
levels(mat$Winner15)[4]="Lib Dem"
levels(mat$Winner15)[6]="SNP"
for (i in 1:nrow(mat)) {
   mat$swing[i]=!grepl(mat$Winner15[i],mat$Winner17[i])
}
swingers=mat[which(mat$swing==TRUE),]
col.bar=c("red","blue","gold","olivedrab4","darkorange1")
partycol=data.frame(cols=c("blue","mediumpurple3","olivedrab4","gold","red","darkorange1","green"),
              names=c("Conservative","UKIP","Plaid Cymru","SNP","Labour","Lib Dem","Green"))
col.lab=as.character(partycol$cols[match(swingers$Winner15,partycol$names)])
par(mar=c(5.1,15.1,4.1,8.5),xpd=TRUE)
bp=barplot(t(swingers[,1:5]),horiz=T,col=col.bar,xlab="Probability of winning seat",axes=F,
   names.arg=rep("",nrow(swingers)),main="Swinging seats")
axis(1)
axis(2,at=bp,labels=paste0(rownames(swingers)," (",swingers$Winner15,")"),cex.axis=.7,las=1,tick=FALSE)
legend("topright", inset=c(-0.2,0), legend=colnames(swingers)[1:5],lty=1,lwd=5,title="Winner 2017",bty="n",col=col.bar,cex=.8)
text(-.015,49,"Constituency (Winner 2015)",cex=.8,pos=2)
par(mar=c(5.1,4.1,4.1,2.1),xpd=FALSE)

# Plots the results on the UK map --- can do overall or by area
#[1] "East Midlands"            "East of England"         
#[3] "London"                   "North East"              
#[5] "North West"               "Scotland"                
#[7] "South East"               "South West"              
#[9] "Wales"                    "West Midlands"           
#[11] "Yorkshire and The Humber"
map.areas("London")
map.areas()
areas=levels(data$Region); areas=areas[c(6,5,4,11,10,1,2,3,7,8,9)]
p=list()
for (i in 1:length(areas)) {
p[[i]]=map.areas(areas[i])
}
# This removes the box around the maps
trellis.par.set(axis.line=list(col=NA))
# This would select different height & width for the panels: 
# panel.height=list(8,"cm"),panel.width=list(8,"cm")
print(p[[1]], position = c(0,.75,.3,1),more=T)
print(p[[2]], position = c(.3,.75,.6,1),more = T)
print(p[[3]], position = c(.6,.75,1,1),more=T)
print(p[[4]], position = c(0,.5,.3,.75),more=T)
print(p[[5]], position = c(.3,.5,.6,.75),more = T)
print(p[[6]], position = c(.6,.5,1,.75),more=T)
print(p[[7]], position = c(0,.25,.3,.5),more=T)
print(p[[8]], position = c(.3,.25,.6,.5),more = T)
print(p[[9]], position = c(.6,.25,1,.5),more=T)
print(p[[10]], position = c(0,0,.3,.25),more=T)
print(p[[11]], position = c(.3,0,.6,.25))

## Analysis of coalitions
# Computed probability (vote share) in each constituency and for all simulations
m=model[[last]]$m
pi=lapply(1:m$BUGSoutput$n.sims,function(i) m$BUGSoutput$sims.list$pi17[i,,])
for (i in 1:m$BUGSoutput$n.sims) {
    colnames(pi[[i]])=c("con","lab","ukip","ld","snp","grn","pcy","oth")
    rownames(pi[[i]])=data$ConstituencyName
}
labs=c("Conservative","Labour","UKIP","Lib Dem","SNP","Green","PCY","Coalition")
# Adds together the progressives (& then could also add together the right)
for (i in 1:length(pi)) {
   pi[[i]]=cbind(pi[[i]],pi[[i]][,2]+pi[[i]][,4]+pi[[i]][,6])
}
for (i in 1:length(pi)) {
   colnames(pi[[i]])[9]=c("progs")
}
# Now computes the winner & seats
Winner17=matrix(NA,C,m$BUGSoutput$n.sims)
for (i in 1:m$BUGSoutput$n.sims) {
    for (c in 1:C) {
         Winner17[c,i]=labs[which.max(pi[[i]][c,-8])]
    }
}
res = matrix(NA,length(labs),m$BUGSoutput$n.sims)
for (i in 1:m$BUGSoutput$n.sims) {
   for (j in 1:length(labs)) {
      res[j,i] = sum(Winner17[,i]==labs[j])
   }
}
rownames(res)=labs
coalition=list(tab=stats(t(res)),res=res,Winner17=Winner17)

# Seats
par(mar=c(5.1,4.1,4.1,2.1))
partycol=data.frame(cols=c("blue","mediumpurple3","olivedrab4","gold","pink"),
              names=c("Conservative","UKIP","PCY","SNP","Coalition"))
cols=as.character(partycol$cols[match(rownames(coalition$res),partycol$names)]); cols=cols[!is.na(cols)]
labs=c("Conservative","UKIP","SNP","PCY","Progressive")
index=matrix(seq(1:(length(labs)*m$BUGSoutput$n.sims)),nrow=length(labs),byrow=T)
coalition$res=coalition$res[c(1,3,5,7,8),]
plot(index[1,],coalition$res[1,],xlim=c(0,m$BUGSoutput$n.sims*length(labs)),main="Seats won in England/Scotland/Wales (median and 95% interval)",
ylim=range(coalition$res),axes=F,pch=20,col=cols[1],cex=.4,ylab="Number of seats",xlab="Party")
for (i in 2:length(labs)) {
points(index[i,],coalition$res[i,],pch=20,col=cols[i],cex=.4)
}
axis(1,at=apply(index,1,median),labels=toupper(c("con","ukip","snp","pcy","prog")),cex.axis=.8)
axis(2,las=2,cex.axis=.8)
points(apply(index,1,median),apply(coalition$res,1,median),pch=20,cex=1.3)
abline(h=325,lty=2,lwd=1); text(.85*(m$BUGSoutput$n.sims*length(labs)),325,"325 seats",pos=3,cex=.6,offset=.15)
meds=round(apply(coalition$res,1,median))
lows=round(apply(coalition$res,1,quantile,.025))
upps=round(apply(coalition$res,1,quantile,.975))
text(apply(index,1,median),apply(coalition$res,1,max),paste0(meds," (",lows," - ",upps,")"),cex=.65,pos=3)

# Analysis of the vote share
pi=lapply(1:m$BUGSoutput$n.sims,function(i) m$BUGSoutput$sims.list$pi17[i,,])
for (i in 1:m$BUGSoutput$n.sims) {
    colnames(pi[[i]])=c("con","lab","ukip","ld","snp","grn","pcy","oth")
    rownames(pi[[i]])=data$ConstituencyName
}
partycol=data.frame(cols=c("blue","green","red","darkorange1","olivedrab4","gold","mediumpurple3"),
                      names=c("Conservative","Green","Labour","Lib Dem","PCY","SNP","UKIP"))
cols=as.character(partycol$cols[match(rownames(res),partycol$names)]); cols[is.na(cols)]="dark grey"
labs=c("Conservative","Labour","UKIP","Lib Dem","SNP","Green","PCY","Other")
probs = matrix(NA,length(labs),m$BUGSoutput$n.sims)
for (i in 1:m$BUGSoutput$n.sims) {
   probs[,i] = apply(pi[[i]],2,mean)
}
rownames(probs)=labs
index=matrix(seq(1:(length(labs)*m$BUGSoutput$n.sims)),nrow=length(labs),byrow=T)
plot(index[1,],probs[1,],xlim=c(0,m$BUGSoutput$n.sims*length(labs)),main="Proportion of vote share",
     ylim=c(0,1),axes=F,pch=20,col=cols[1],cex=.6,ylab="Vote share",xlab="Party")
for (i in 2:length(labs)) {
   points(index[i,],probs[i,],pch=20,col=cols[i],cex=.6)
}
axis(1,at=apply(index,1,median),labels=toupper(c("con","lab","ukip","ld","snp","grn","pcy","oth")),cex.axis=.8)
axis(2,las=2,cex.axis=.8)
meds=format(apply(probs,1,median),digits=1,nsmall=2)
lows=format(apply(probs,1,quantile,.025),digits=1,nsmall=2)
upps=format(apply(probs,1,quantile,.975),digits=1,nsmall=2)
text(apply(index,1,median),apply(probs,1,max),paste0(meds," (",lows," - ",upps,")"),cex=.65,pos=3)


