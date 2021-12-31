#Two plots on same graph  ####################################
TwoVarPlot<-function(plotdata,title="",leg1="",leg2="",Handle_NAs=F){
  if (Handle_NAs) plotdata<-na.omit(na.locf(plotdata))
  
  oldpar<-par()
  par(mar=c(3,4,3,4))
  plot(plotdata[,1],main=title,xlab="",ylab="",col="red",las=1)
  axis(2,col.axis="red",col="red",las=1)
  par(new=TRUE)
  plot(plotdata[,2],xaxt="n", yaxt="n",col="blue",ylab="",xlab="")
  axis(4,col.axis="blue",col="blue",las=1)
  #par(xpd=FALSE)
  legend("topleft", c(leg1,leg2), text.col=c("red","blue"),cex=0.8,bty="n")
  options(warn=-1)
  try(suppressWarnings(par(oldpar)),silent = T)
  options(warn=0)
}
