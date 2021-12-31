Best.Lambdas.X<-function(Y,X,
                                   hp_lambda_1=10^c(1:10),
                                   hp_lambda_2=10^c(1:10),
                                   lags=c(-12:6),
                                   metric="cor", # cor or rss
                                   type = "trend", # cycle or trend
                                   ifStationary = F){
  
  X <- Convert.To.Monthly(X)
  X <- X[index(X) >= "2002-12-31"]
  Df <- na.omit(merge(Y,X))
  Y <-Df[,1]
  X_Omitted <-Df[,2]
  
  
if(type == "trend"){
  
  min.RSS<-function(lambda,lag, multiple){
    temp<-zoo(HP.Filter(X_Omitted,lambda),time(X_Omitted))
    temp <- temp * as.numeric(multiple)
    temp<-stats::lag(temp,lag,na.pad=T)
    temp.all<-na.omit(merge(temp,Y))
    sum((temp.all[,1]-temp.all[,2])^2)
  }
  
  neg.cor<-function(lambda,lag, multiple){
    temp<-zoo(HP.Filter(X_Omitted,lambda),time(X_Omitted))
    temp <- temp * as.numeric(multiple)
    temp<-stats::lag(temp,lag,na.pad=T)
    temp.all<-na.omit(merge(temp,Y))
    -cor(na.omit(merge(temp.all[,1],temp.all[,2])))[2,1]
  }
  
  if (metric=="cor"){
    obj<-neg.cor
  } else {
    obj<-min.RSS
  }
  
  multiple = c(-1,1)
  mygrid<-expand.grid(hp_lambda_1,lags, multiple)
  all.rss<-sapply(1:nrow(mygrid),
                  function(i) obj(mygrid[i,1],mygrid[i,2], mygrid[i,3]))
  p.star<-mygrid[which.min(all.rss),]
  temp = zoo(HP.Filter(X, as.numeric(p.star[1])), time(X))
  temp = temp * as.numeric(p.star[3])
  final_dat<-merge(Y,stats::lag(temp, p.star[2],na.pad=T))
  Trend = zoo(HP.Filter(X, as.numeric(p.star[1])), time(X))
  Trend = Trend * as.numeric(p.star[3])
  
  if (metric=="cor"){
    metric<-cor(final_dat,use = "complete")[2,1]
  } else {
    metric<-all.rss[which.min(all.rss)]
  }
  
  Results <- list(best_lambda=p.star[1],
       best_lag_length=p.star[2],
       best_multiple = p.star[3],
       final_dat=final_dat,
       Trend = Trend,
       metric=metric)}
  
if(type == "cycle" & ifStationary == FALSE){

  min.RSS<-function(lambda_1, lambda_2, lag, multiple){
    temp<-zoo(HP.Filter(X_Omitted,lambda_1),time(X_Omitted))
    temp_res <- X_Omitted - temp
    temp <- zoo(HP.Filter(temp_res, lambda_2), time(X_Omitted))
    temp <- temp * as.numeric(multiple)
    temp<-stats::lag(temp,lag,na.pad=T)
    temp.all<-na.omit(merge(temp,Y))
    sum((temp.all[,1]-temp.all[,2])^2)
  }
  
  neg.cor<-function(lambda_1, lambda_2, lag, multiple){
    temp<-zoo(HP.Filter(X_Omitted,lambda_1),time(X_Omitted))
    temp_res <- X_Omitted - temp
    temp <- zoo(HP.Filter(temp_res, lambda_2), time(X_Omitted))
    temp = temp * as.numeric(multiple)
    temp<-stats::lag(temp,lag,na.pad=T)
    temp.all<-na.omit(merge(temp,Y))
    -(cor(na.omit(merge(temp.all[,1],temp.all[,2])))[2,1])
  }
  
  if (metric=="cor"){
    obj<-neg.cor
  } else {
    obj<-min.RSS
  }
  
  multiple <- c(-1,1)
  mygrid<-expand.grid(hp_lambda_1,hp_lambda_2,lags, multiple)
  all.rss<-sapply(1:nrow(mygrid),
                  function(i) obj(mygrid[i,1],mygrid[i,2], mygrid[i,3], mygrid[i,4]))
  p.star<-mygrid[which.min(all.rss),]
  best_multiple = p.star[4]
  Trend = HP.Filter(X, as.numeric(p.star[1]))
  Residual = X - Trend
  Cycle = HP.Filter(Residual, as.numeric(p.star[2]))
  Cycle = Cycle * as.numeric(best_multiple)
  
  final_dat<-merge(Y,stats::lag(
    zoo(Cycle,time(X)),
    p.star[3],na.pad=T))
  
  if (metric=="cor"){
    metric<-cor(final_dat,use = "complete")[2,1]
  } else {
    metric<-all.rss[which.min(all.rss)]
  }
  
  Results <- list(best_lambda_1=p.star[1],
       best_lambda_2=p.star[2],
       best_lag_length=p.star[3],
       best_multiple = p.star[4],
       final_dat=final_dat,
       Trend = Trend,
       Cycle = Cycle,
       metric = metric)}
  
  if(type == "cycle" & ifStationary == TRUE){
  
  min.RSS<-function(lambda_1, lag, multiple){
    temp<-zoo(HP.Filter(X_Omitted,lambda_1),time(X_Omitted))
    temp <- temp * as.numeric(multiple)
    temp<-stats::lag(temp,lag,na.pad=T)
    temp.all<-na.omit(merge(temp,Y))
    sum((temp.all[,1]-temp.all[,2])^2)
  }
  
  neg.cor<-function(lambda_1, lag, multiple){
    temp<-zoo(HP.Filter(X_Omitted,lambda_1),time(X_Omitted))
    temp = temp * as.numeric(multiple)
    temp<-stats::lag(temp,lag,na.pad=T)
    temp.all<-na.omit(merge(temp,Y))
    -(cor(na.omit(merge(temp.all[,1],temp.all[,2])))[2,1])
  }
  
  if (metric=="cor"){
    obj<-neg.cor
  } else {
    obj<-min.RSS
  }
  
  multiple <- c(-1,1)
  mygrid<-expand.grid(hp_lambda_1,lags, multiple)
  all.rss<-sapply(1:nrow(mygrid),
                  function(i) obj(mygrid[i,1],mygrid[i,2], mygrid[i,3]))
  p.star<-mygrid[which.min(all.rss),]
  best_multiple = p.star[3]
  Trend = 0
  Residual = X - Trend
  Cycle = HP.Filter(Residual, as.numeric(p.star[1]))
  Cycle = Cycle * as.numeric(best_multiple)
  
  final_dat<-merge(Y,stats::lag(
    zoo(Cycle,time(X)),
    p.star[2],na.pad=T))
  
  if (metric=="cor"){
    metric<-cor(final_dat,use = "complete")[2,1]
  } else {
    metric<-all.rss[which.min(all.rss)]
  }
  
  Results <-  list(best_lambda_1=NA,
       best_lambda_2=p.star[1],
       best_lag_length=p.star[2],
       best_multiple = p.star[3],
       final_dat=final_dat,
       Trend = Trend,
       Cycle = Cycle,
       metric = metric)}
  return(Results)
}
