# Function to rescale the estimated constituency- and party-specific vote shares so that they sum to 1
rescale.pi=function(m) {
  for (i in 1:m$BUGSoutput$n.sims) {
    m$BUGSoutput$sims.list$pi17[i,,]=m$BUGSoutput$sims.list$pi17[i,,]/apply(m$BUGSoutput$sims.list$pi17[i,,],1,sum)
  }
  return(m)
}


# Function to simulate the seats allocation
make.election=function(m) {
# m = the BUGS object with the simulations for the votes 
   pi=lapply(1:m$BUGSoutput$n.sims,function(i) m$BUGSoutput$sims.list$pi17[i,,])
   for (i in 1:m$BUGSoutput$n.sims) {
       colnames(pi[[i]])=c("con","lab","ukip","ld","snp","grn","pcy","oth")
       rownames(pi[[i]])=data$ConstituencyName
   }
   labs=c("Conservative","Labour","UKIP","Lib Dem","SNP","Green","PCY","Other")
   Winner17=matrix(NA,C,m$BUGSoutput$n.sims)
   for (i in 1:m$BUGSoutput$n.sims) {
       for (c in 1:C) {
            Winner17[c,i]=labs[which.max(pi[[i]][c,1:(P-1)])]
       }
   }
   rownames(Winner17)=data$ConstituencyName
   res = matrix(NA,length(labs),m$BUGSoutput$n.sims)
   for (i in 1:m$BUGSoutput$n.sims) {
      for (j in 1:length(labs)) {
         res[j,i] = sum(Winner17[,i]==labs[j])
      }
   }
   rownames(res)=labs
   list(tab=stats(t(res)),res=res,Winner17=Winner17)
}

# Function to plot the outcomes of the simulations in terms of seats
plot.seats=function(res,m) {
  # Analysis of all simulations (not just the mean values)
  par(mar=c(5.1,4.1,4.1,2.1))
  partycol=data.frame(cols=c("blue","green","red","darkorange1","olivedrab4","gold","mediumpurple3"),
                      names=c("Conservative","Green","Labour","Lib Dem","PCY","SNP","UKIP"))
  cols=as.character(partycol$cols[match(rownames(res$res),partycol$names)]); cols[is.na(cols)]="dark grey"
  labs=c("Conservative","Labour","UKIP","Lib Dem","SNP","Green","PCY","Other")
  index=matrix(seq(1:(length(labs)*m$BUGSoutput$n.sims)),nrow=length(labs),byrow=T)
  plot(index[1,],res$res[1,],xlim=c(0,m$BUGSoutput$n.sims*length(labs)),main="Seats won in England/Scotland/Wales (median and 95% interval)",
       ylim=range(res$res),axes=F,pch=20,col=cols[1],cex=.4,ylab="Number of seats",xlab="Party")
  for (i in 2:length(labs)) {
    points(index[i,],res$res[i,],pch=20,col=cols[i],cex=.4)
  }
  axis(1,at=apply(index,1,median),labels=toupper(c("con","lab","ukip","lib dem","snp","grn","pcy","oth")),cex.axis=.8)
  axis(2,las=2,cex.axis=.8)
  points(apply(index,1,median),apply(res$res,1,median),pch=20,cex=1.3)
  abline(h=325,lty=2,lwd=1); text(.85*(m$BUGSoutput$n.sims*length(labs)),325,"325 seats",pos=3,cex=.6,offset=.15)
  meds=round(apply(res$res,1,median))
  lows=round(apply(res$res,1,quantile,.025))
  upps=round(apply(res$res,1,quantile,.975))
  text(apply(index,1,median),apply(res$res,1,max),paste0(meds," (",lows," - ",upps,")"),cex=.65,pos=3)
}


# Restricts the analysis to a certain numbers of polls (depending on "today")
elec.today=function(today=Sys.Date()) {
t=as.numeric(today-polls$Date)
ystar=y
for (i in 1:nrow(ystar)) {
   ystar[i,]=round(ystar[i,]/(1+disc)^t[i])
}
ystar=ystar[t>0,]
dataJags$y=ystar; dataJags$N=apply(ystar,1,sum,na.rm=T); dataJags$Npolls=nrow(ystar); dataJags$ref=polls$EU_Ref[t>0]
# Defines the parameters to be monitored
params <- c("alpha","beta","pi","pi17","rho")
m <- jags(dataJags, inits=inits, params, model.file=model.code,n.chains=2, n.iter, n.burnin, n.thin,
           DIC=TRUE, working.directory=working.dir, progress.bar="text")
m=rescale.pi(m)
list(m=m)
}

map.areas=function(area=NULL) {
	if(is.null(area)) {res.temp=res; toplot=uk} else {res.temp=res[res$Region==area,]; toplot=uk[uk$Region==area,]}
	res.temp$Winner17=factor(res.temp$Winner17)
	data.temp=data.frame(pcon14cd=res.temp$PCON15CD,majority=res.temp$Winner17)
	map.temp=attr(toplot, "data"); map.temp$num=1:nrow(map.temp)
	tmp = merge(map.temp, data.temp, by="pcon14cd")
	tmp = tmp[order(tmp$num),]
	attr(toplot,"data") = tmp
	cols=c(colorRampPalette(c("white","blue"))(length(grep("Conservative",levels(res.temp$Winner17)))+1),
		   colorRampPalette(c("white","green"))(length(grep("Green",levels(res.temp$Winner17)))+1),
		   colorRampPalette(c("white","red"))(length(grep("Labour",levels(res.temp$Winner17)))+1),
		   colorRampPalette(c("white","darkorange1"))(length(grep("Lib Dem",levels(res.temp$Winner17)))+1),
		   colorRampPalette(c("white","olivedrab4"))(length(grep("PCY",levels(res.temp$Winner17)))+1),
		   colorRampPalette(c("white","gold"))(length(grep("SNP",levels(res.temp$Winner17)))+1),
		   colorRampPalette(c("white","mediumpurple3"))(length(grep("UKIP",levels(res.temp$Winner17)))+1)
	)
	cols=cols[cols!="#FFFFFF"]
	print(spplot(toplot,zcol="majority",col.regions=cols,colorkey=list(cex.label=4),main=area))
}

