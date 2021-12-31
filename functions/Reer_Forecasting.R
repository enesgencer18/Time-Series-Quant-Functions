library(zoo)
library(xts)
library(tidyverse)
library(lubridate)
library(glmnet)
library(caret)
library(pls)
library(plsdof)

##################################################
# FUNCTION START
##################################################
Forecast_Trend_Cycle<-function(Rebal_Date,
                               Lookback,
                               ifRolling, #TRUE/FALSE, if FALSE full sample analysis
                               Start_Date,
                               Y_Name,
                               X_Name,
                               X_Stationary,
                               Predict_Cycle,#If TRUE, cycle is predicted using its own fourier series 
                               Predict_Trend,#If TRUE, trend is predicted using its own fourier series
                               Forecast_Method, #TBATS or ARIMA
                               Fit_Method, # OLS or LASSO
                               freq, #Monthly=12, daily=365, weekly=52, etc
                               TopX, # Use TopX explantory variables in forecasting cycles
                               TopN, #Use TopN fourier terms
                               H){ #Forecast horizon
  
  
  # Generate data 
  if(ifRolling == T){
    
    Period.Length <- as.numeric(Rebal_Date - (Rebal_Date - Lookback))/30
    mydat<-window(get(Y_Name),start=Rebal_Date-Lookback,end=Rebal_Date)
    tmp <- c()
    selected_names <- c()
    Stationary_Selected <- c()
    for (i in 1:length(X_Name)){
      
      tmp <- window(get(X_Name[i]),
                    start=Rebal_Date-Lookback,
                    end=Rebal_Date)
      if (!all(is.na(tmp)) & length(tmp) >= Period.Length/3){
        mydat<-merge(mydat,
                     window(get(X_Name[i]),
                            start=Rebal_Date-Lookback,
                            end=Rebal_Date))
        selected_names <- c(selected_names, X_Name[i])
        Stationary_Selected <- c(Stationary_Selected, X_Stationary[i])
      }}
  }
  if(ifRolling == F){
    Period.Length <- as.numeric(Rebal_Date - Start_Date)/30
    mydat<-window(get(Y_Name),start=Start_Date,end=Rebal_Date)
    tmp <- c()
    selected_names <- c()
    Stationary_Selected <- c()
    for (i in 1:length(X_Name)){
      
      tmp <- window(get(X_Name[i]),
                    start=Start_Date,
                    end=Rebal_Date)
      if (!all(is.na(tmp)) & length(tmp) >= Period.Length/3){
        mydat<-merge(mydat,
                     window(get(X_Name[i]),
                            start=Start_Date,
                            end=Rebal_Date))
        selected_names <- c(selected_names, X_Name[i])
        Stationary_Selected <- c(Stationary_Selected, X_Stationary[i])
      }}
  }
  
  names(mydat)<-c(Y_Name,selected_names)
  plot(mydat)
  Y<-mydat[,1]
  if(Fit_Method %in% c("lasso", "ridge", "pls")){TopX <- ncol(mydat)-1}
  
  Best.Lambdas.Y <- function(Y,
                             hp_lambda_1 = 10 ^ c(1:10),
                             hp_lambda_2 = 10 ^ c(1:10),
                             metric = "cor"
                             # cor or rss
                             
  ){
    
    Rolling_10Y = rollapplyr(
      data = Return.calculate(exp(Y)),
      width = 120,
      FUN = Return.annualized,
      scale = 12
    )
    Rolling_1Y = rollapplyr(
      data = Return.calculate(exp(Y)),
      width = 12,
      FUN = Return.annualized,
      scale = 12
    ) 
    
    min.RSS<-function(lambda_1, lambda_2){
      temp<-zoo(HP.Filter(Y,lambda_1),time(Y))
      Rolling_10Y_Trend = rollapplyr(
        data = Return.calculate(exp(temp)),
        width = 120,
        FUN = Return.annualized,
        scale = 12
      )
      temp.1<-na.omit(merge(temp,Rolling_10Y))
      rss.1 = sum((temp.1[,1]-temp.1[,2])^2)
      
      res = Y - temp
      cycle = zoo(HP.Filter(res, lambda_2), time(Y))
      Rolling_1Y_Cycle = rollapplyr(
        data = Return.calculate(exp(cycle)),
        width = 12,
        FUN = Return.annualized,
        scale = 12
      ) 
      temp.2 = na.omit(merge(cycle, Rolling_1Y))
      rss.2 = sum((temp.2[,1]-temp.2[,2])^2)
      
      return(rss.1 * rss.2)
      
    }
    
    neg.cor<-function(lambda_1, lambda_2){
      temp<-zoo(HP.Filter(Y,lambda_1),time(Y))
      Rolling_10Y_Trend = rollapplyr(
        data = Return.calculate(exp(temp)),
        width = 120,
        FUN = Return.annualized,
        scale = 12
      )
      temp.1<-na.omit(merge(temp,Rolling_10Y))
      cor.1 = -abs(cor(na.omit(merge(temp.1[,1],temp.1[,2])))[2,1])
      
      res = Y - temp
      cycle = zoo(HP.Filter(res, lambda_2), time(Y))
      Rolling_1Y_Cycle = rollapplyr(
        data = Return.calculate(exp(cycle)),
        width = 12,
        FUN = Return.annualized,
        scale = 12
      ) 
      temp.2 = na.omit(merge(cycle, Rolling_1Y))
      cor.2 = -abs(cor(na.omit(merge(temp.2[,1],temp.2[,2])))[2,1])
      return(-(cor.1 * cor.2))
    }
    
    if (metric=="cor"){
      obj<-neg.cor
    } else {
      obj<-min.RSS
    }
    
    mygrid<-expand.grid(hp_lambda_1,hp_lambda_2)
    all.rss<-sapply(1:nrow(mygrid),
                    function(i) obj(mygrid[i,1],mygrid[i,2]))
    lambda1.star <- mygrid[which.min(all.rss), 1]
    lambda2.star <- mygrid[which.min(all.rss), 2]
    
    Trend = zoo(HP.Filter(Y, as.numeric(lambda1.star)), time(Y))
    Res = Y - Trend
    Cycle = zoo(HP.Filter(Res, as.numeric(lambda2.star)), time(Y))
    Irregular = Y - Trend - Cycle
    
    
    
    final_dat <- merge(Y, Trend, Cycle, Irregular)
    
    
    list(best_lambda_1=lambda1.star,
         best_lambda_2=lambda2.star,
         final_dat=final_dat)}
  
  #Y.Filtered <- Best.Lambdas.Y(Y,
                               #hp_lambda_1 = 10 ^ c(1:10),
                               #hp_lambda_2 = 10 ^ c(1:10),
                               #metric = "rss")
  
  #Y_trend<-zoo(Y.Filtered$final_dat[,2],time(mydat))
  #Y_cycle<-zoo(Y.Filtered$final_dat[,3],time(mydat))
  #Y_Irregular<-zoo(Y.Filtered$final_dat[,4],time(mydat))
  
  Y_trend = zoo(HP.Filter(x = Y, lambda = 1e+05), time(mydat))
  Residual = Y - Y_trend
  Y_cycle = zoo(HP.Filter(Residual, lambda = 10), time(mydat))
  Y_Irregular = zoo((Y - Y_trend - Y_cycle), time(mydat))
  
  plot(merge(mydat[,1],Y_trend,Y_cycle,Y_Irregular))
  
  #Shift dates for forecasting
  if (freq==365)
    newtime<-c(time(mydat),
               time(mydat)[nrow(mydat)] %m+% days(1:H))
  if (freq==52)
    newtime<-c(time(mydat),
               time(mydat)[nrow(mydat)] %m+% weeks(1:H))
  if (freq==12)
    newtime<-c(time(mydat),
               time(mydat)[nrow(mydat)] %m+% months(1:H))
  
  newmydat<-zoo(matrix(NA,nrow=length(newtime),ncol=ncol(mydat)),newtime)
  window(newmydat,time(mydat))<-mydat
  
  
  names(newmydat) <- c(Y_Name,selected_names)
  plot(newmydat)
  
  # Find best filter and lag
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
  
  
  
  # Find best explanatory variables
  newmydat_cycle<-zoo(matrix(data = NA, nrow = nrow(newmydat), ncol = ncol(newmydat)), time(newmydat))
  names(newmydat_cycle)<-c(Y_Name,selected_names)
  window(newmydat_cycle[,1],time(Y_cycle))<-Y_cycle
  
  all_fits<-lapply(2:ncol(mydat),
                   function(i) Best.Lambdas.X(Y_cycle,X=mydat[,i],
                                              hp_lambda_1=10^c(1:10),
                                              hp_lambda_2=10^c(1:10),
                                              lags=c(-12:3),
                                              metric="cor", # cor or rss
                                              type = "cycle",
                                              ifStationary = Stationary_Selected[i-1]))
  metrics_ordered <- order(sapply(all_fits,function(L) L$metric),
        decreasing = T)
  threshold = sapply(all_fits,function(L) L$metric)[metrics_ordered[TopX]]
  selected_fits <- sapply(all_fits,function(L) L$metric)>=threshold
  for (j in 1:length(selected_fits)){
    if (selected_fits[j]){
      window(newmydat_cycle[,1+j],time(all_fits[[j]]$final_dat)) <- zoo(all_fits[[j]]$Cycle,time(all_fits[[j]]$final_dat))
      newmydat_cycle[,1+j]<-stats::lag(newmydat_cycle[,1+j],
                                       as.numeric(all_fits[[j]][[3]]),na.pad=T)
    } else {
      newmydat_cycle[,1+j]<-NA*newmydat_cycle[,1+j]
    }}
  newmydat_cycle<-newmydat_cycle[,c(TRUE,selected_fits)]
  plot(newmydat_cycle)
  
  
  # Forecast X
  Forecast_TBATS<-function(myzoo,freq,TopN,H,ifPlot=T){
    timeseries<-Convert_Zoo_To_ts(myzoo,freq)
    
    pgram <- spec.pgram(as.vector(timeseries),plot = T)
    pgram_df <- data.frame(freq = pgram$freq, spec = pgram$spec)
    freqs <- head(1/pgram_df[order(pgram_df$spec, decreasing = TRUE),1],n=TopN)
    # build multi-seasonal time series
    mts <- msts(timeseries, seasonal.periods = freqs, ts.frequency =
                  frequency(timeseries))
    fit <- tbats(mts, use.trend = FALSE)
    fore<-forecast::forecast(fit,lead = H)
    
    if (ifPlot)  plot(fore)
    
    fore
  }
  Forecast_ARIMA <- function(myzoo, ifPlot, H){
    fit = auto.arima(as.numeric(myzoo), stationary = TRUE, allowmean = TRUE)
    fore<-forecast::forecast(fit, h = H)
    
    if (ifPlot)  plot(fore)
    
    fore
}
  
  if(Forecast_Method == "tbats"){
    for (j in 2:ncol(newmydat_cycle)){
      no_missing_obs<-nrow(newmydat_cycle)-max(which(!is.na(newmydat_cycle[,j])))
      if (no_missing_obs>0) 
        window(newmydat_cycle[,j],
               time(newmydat_cycle)[(nrow(newmydat_cycle)-no_missing_obs+1):
                                      nrow(newmydat_cycle)])<-as.numeric(Forecast_TBATS(na.omit(newmydat_cycle[,j]),
                                                                                        freq,TopN,no_missing_obs,F)$mean)
    }}
  
  if(Forecast_Method == "arima"){
    for (j in 2:ncol(newmydat_cycle)){
      no_missing_obs<-nrow(newmydat_cycle)-max(which(!is.na(newmydat_cycle[,j])))
      if (no_missing_obs>0) 
        window(newmydat_cycle[,j],
               time(newmydat_cycle)[(nrow(newmydat_cycle)-no_missing_obs+1):
                                      nrow(newmydat_cycle)])<-as.numeric(Forecast_ARIMA(na.omit(newmydat_cycle[,j]), ifPlot = F, H = no_missing_obs)$mean)
    }}
  
  plot(newmydat_cycle)
  
  # Find FV residual
  lmdf<-window(newmydat_cycle,end=end(mydat)) %>% as.data.frame 
  names(lmdf)<-paste0("Var_",1:ncol(lmdf))
  
  if(Fit_Method == "ols"){
  fit<-lm(Var_1~.,data=lmdf)
  fitted_values<-fitted(fit)
  }
  if(Fit_Method == "lasso"){
    lmdf.standardized <- scale(lmdf)
    lmdf.standardized <- na.omit(lmdf.standardized)
    y = as.matrix(lmdf.standardized[,1])
    x = as.matrix(lmdf.standardized[,-1])
    fit.lasso <- glmnet(x = x, y = y, family = "gaussian", alpha = 1, intercept = FALSE, nlambda = 10000)
    tLL <- fit.lasso$nulldev - deviance(fit.lasso)
    k <- fit.lasso$df
    n <- fit.lasso$nobs
    AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
    best.model <- which.min(AICc)
    best.lambda <- fit.lasso$lambda[best.model]
    lasso.coef <- as.numeric(coef(fit.lasso, s = best.lambda))
    lasso.coef <- lasso.coef[-1]
    selected.coef <- ifelse(lasso.coef == 0, FALSE, TRUE)
    lmdf <- lmdf[,c(TRUE, selected.coef)]
    fit<-lm(Var_1~.,data=lmdf)
    fitted_values<-fitted(fit)
  }
  if(Fit_Method == "ridge"){
    lmdf <- na.omit(lmdf)
    y = as.matrix(lmdf[,1])
    x = as.matrix(lmdf[,-1])
    fit.ridge <- glmnet(x = x, y = y, family = "gaussian", alpha = 0, intercept = FALSE, nlambda = 10000, standardize = TRUE)
    tLL <- fit.ridge$nulldev - deviance(fit.ridge)
    k <- fit.ridge$df
    n <- fit.ridge$nobs
    AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
    best.model <- which.min(AICc)
    best.lambda <- fit.ridge$lambda[best.model]
    fit <- glmnet(x = x, y = y, family = "gaussian", alpha = 0, intercept = FALSE, lambda = as.numeric(best.lambda), standardize = TRUE)
    fitted_values<-predict(fit, newx = x)
  }
  if(Fit_Method == "pls"){
    pls.data <- na.omit(lmdf)
    pls.data <- scale(pls.data)
    pls <- plsdof::pls.ic(X = as.matrix(pls.data[,-1]), y = as.matrix(pls.data[,1]), criterion = "aic")
    optimal.ncomp = as.numeric(pls$m.opt)
    fit <- plsr(Var_1 ~., data = lmdf, scale = TRUE)
    fitted_values<-predict(fit, ncomp = optimal.ncomp)
  }
  
  if (length(fitted_values)<nrow(lmdf)) fitted_values<-c(
    rep(NA,nrow(lmdf)-length(fitted_values)),
    fitted_values)
  if(Fit_Method %in% c("ols", "lasso")){Std_FV_Resid_Cycle<-last(residuals(fit))/summary(fit)$sigma}
  
  plot(lmdf$Var_1,type="l");lines(fitted_values,col=2)
  
  # Forecast Y cycle
  if (Predict_Cycle==TRUE){
    if(Forecast_Method == "tbats"){
      prd<-as.numeric(Forecast_TBATS(Y_cycle,freq,TopN,H,ifPlot=T)$mean)
    }
    if(Forecast_Method == "arima"){
      prd<-as.numeric(Forecast_ARIMA(Y_cycle,ifPlot=T, H = H)$mean)
    }
  } else {
    lmdf_new<-window(newmydat_cycle,start=end(mydat)+1) %>% as.data.frame 
    names(lmdf_new)<-paste0("Var_",1:ncol(lmdf_new))
    if(Fit_Method == "pls"){
      prd<-predict(fit,newdata =lmdf_new, ncomp = optimal.ncomp)
    }
    if(Fit_Method == "ridge"){
      x_new = as.matrix(lmdf_new[,-1])
      prd <- as.numeric(predict(fit, newx = x_new))
    }else{
     prd<-predict(fit,newdata =lmdf_new)
}}
  
  plot(c(lmdf$Var_1,prd*NA),type="l");lines(c(fitted_values,prd),col=2)
  init_gap<-first(as.numeric(prd)) - last(lmdf$Var_1)
  gap<-seq(init_gap,0,length.out = H+1)[-1]
  prd_cycle<-prd-gap
  plot(c(lmdf$Var_1,prd*NA),type="l")
  lines(c(fitted_values,prd),col=2)
  lines(c(fitted_values*NA,prd_cycle),col=4)
  
  # df_ecm<-data.frame(dy=diff(lmdf$Var_1),
  #                    dx=diff(fitted_values),
  #                    ec=(lmdf$Var_1-fitted_values)[-length(fitted_values)])
  # ecm.fit<-lm(dy~.,data=df_ecm)
  # predict(ecm.fit,newdata=data.frame(dx=diff(c(last(fitted_values),prd)),
  #                                    ec=))
  
  
  # Forecast trend
  newmydat_trend<-zoo(matrix(data = NA, nrow = nrow(newmydat), ncol = ncol(newmydat)), time(newmydat))
  names(newmydat_trend)<-c(Y_Name,selected_names)
  window(newmydat_trend[,1],time(Y_trend))<-Y_trend
  newmydat_trend <- newmydat_trend[,c(TRUE,!X_Stationary)]
  mydat_stationary <- mydat[, c(TRUE, !X_Stationary)]
  if(Fit_Method %in% c("lasso", "ridge", "pls")){TopX <- ncol(mydat_stationary)-1}
  
  all_fits_trend<-lapply(2:ncol(mydat_stationary),
                         function(i) Best.Lambdas.X(Y_trend,X=mydat_stationary[,i],
                                                    hp_lambda_1= 10^c(1:10),
                                                    lags=c(-12:6),
                                                    metric="cor", # cor or rss
                                                    type = "trend"))
  metrics_ordered_trend <- order(sapply(all_fits_trend,function(L) L$metric),
                           decreasing = T)
  threshold_trend = sapply(all_fits_trend,function(L) L$metric)[metrics_ordered_trend[TopX]]
  selected_fits_trend <- sapply(all_fits_trend,function(L) L$metric)>=threshold_trend
  
  for (j in 1:length(selected_fits_trend)){
    if (selected_fits_trend[j]){
      window(newmydat_trend[,1+j],time(all_fits_trend[[j]]$final_dat)) <- zoo(all_fits_trend[[j]]$Trend,time(all_fits_trend[[j]]$final_dat))
      newmydat_trend[,1+j]<-stats::lag(newmydat_trend[,1+j],
                                       as.numeric(all_fits[[j]][[3]]),na.pad=T)
    } else {
      newmydat_trend[,1+j]<-NA*newmydat_trend[,1+j]
    }}
  
  newmydat_trend<-newmydat_trend[,c(TRUE,selected_fits_trend)]
  plot(newmydat_trend)
  
  
  for (j in 2:ncol(newmydat_trend)){
    no_missing_obs<-nrow(newmydat_trend)-max(which(!is.na(newmydat_trend[,j])))
    if (no_missing_obs>0) 
      window(newmydat_trend[,j],
             time(newmydat_trend)[(nrow(newmydat_trend)-no_missing_obs+1):
                                    nrow(newmydat_trend)])<-as.numeric(Forecast_TBATS(na.omit(newmydat_trend[,j]),
                                                                                      freq,TopN,no_missing_obs,F)$mean)
  }
  
  
  
  lmdf_trend<-window(newmydat_trend,end=end(mydat)) %>% as.data.frame 
  names(lmdf_trend)<-paste0("Var_",1:ncol(lmdf_trend))
  fit_trend<-lm(Var_1~.,data=lmdf_trend)
  fitted_values_trend<-fitted(fit_trend)
  if (length(fitted_values_trend)<nrow(lmdf_trend)) fitted_values_trend<-c(
    rep(NA,nrow(lmdf_trend)-length(fitted_values_trend)),
    fitted_values_trend)
  Std_FV_Resid_Trend<-last(residuals(fit_trend))/summary(fit_trend)$sigma
  
  plot(lmdf_trend$Var_1,type="l");lines(fitted_values_trend,col=2)
  
  # Forecast trend
  if (Predict_Trend==TRUE){
    prd_trend<-as.numeric(Forecast_TBATS(Y_trend,freq,TopN,H,ifPlot=T)$mean)[1:H]
  } else {
    lmdf_new_trend<-window(newmydat_trend,start=end(mydat)+1) %>% as.data.frame 
    names(lmdf_new_trend)<-paste0("Var_",1:ncol(lmdf_new_trend))
    prd_trend<-predict(fit_trend,newdata =lmdf_new_trend)
  }
    #plot(c(lmdf_trend$Var_1,prd*NA),type="l");lines(c(fitted_values_trend,prd_trend),col=2)
    
    init_gap<-first(prd_trend) - last(lmdf_trend$Var_1)
    gap<-seq(init_gap,0,length.out = H+1)[-1]
    prd_trend<-prd_trend-gap
    
    plot(c(lmdf_trend$Var_1,prd_trend*NA),type="l")
    lines(c(fitted_values_trend,prd_trend),col=2)
    lines(c(fitted_values_trend*NA,prd_trend),col=4)
  
  allvars<-merge(newmydat[,1],Y,Y_trend,Y_trend+Y_cycle,
                 zoo(prd_cycle+prd_trend,
                     as.Date(setdiff(as.character(index(newmydat)),
                                     as.character(index(mydat))))))[,-1]
  
  plot(allvars,screens=1,col=1:4)
  plot(exp(allvars),screens=1,col=1:4)
  
  # Forecast of one period return
  newdts<-as.Date(setdiff(as.character(index(newmydat)),
                          as.character(index(mydat))))
  cycle_forecast<-zoo(prd_cycle,newdts)
  trend_forecast<-zoo(prd_trend,newdts)
  combined_forecast<-cycle_forecast+trend_forecast
  forecast_return<-(as.numeric(last(allvars[,4]))-as.numeric(last(Y)))/H 
  
  Levels <- c()
  Momentums <- c()
  Predicted_Momentum <- c()
  for(i in 2:ncol(newmydat_cycle)){
    Denom <- (coredata(newmydat_cycle[which.max(newmydat_cycle[,i]),i]) - coredata(newmydat_cycle[which.min(newmydat_cycle[,i]),i]))/2
    Levels[i-1] <- coredata(newmydat_cycle[nrow(mydat),i]) / Denom
    Momentums[i-1] <- (coredata(newmydat_cycle[nrow(mydat),i]) - coredata(newmydat_cycle[nrow(mydat)-1,i]))/ Denom
    Predicted_Momentum[i-1] <- (coredata(newmydat_cycle[nrow(newmydat_cycle), i]) - coredata(newmydat_cycle[nrow(mydat), i]))/Denom  
    }
  names(Levels) <- paste0(names(newmydat_cycle)[-1], "_Level")
  names(Momentums) <- paste0(names(newmydat_cycle)[-1], "_Momentum")
  names(Predicted_Momentum) <- paste0(names(newmydat_cycle)[-1], "_Predicted_Momentum")
  
  Trend_Momentum <- as.numeric(diff(newmydat_trend)[nrow(mydat),-1]) 
  Trend_Acc <- as.numeric(diff(diff(newmydat_trend))[nrow(mydat), -1])

  names(Trend_Momentum) <- paste0(names(newmydat_trend)[-1], "_Trend_Momentum")
  names(Trend_Acc) <- paste0(names(newmydat_trend)[-1], "_Trend_Acc")
  
  ML_Data <-window(newmydat, end(mydat)) %>% 
    fortify %>% 
    rename(Date = Index) %>% 
    as_tibble %>% 
    bind_cols(as_tibble_row(Momentums)) %>% 
    bind_cols(as_tibble_row(Levels)) %>% 
    bind_cols(as_tibble_row(Predicted_Momentum)) %>% 
    bind_cols(as_tibble_row(Trend_Momentum)) %>% 
    bind_cols(as_tibble_row(Trend_Acc)) %>% 
    dplyr::select(-Reer_Df_Zoo) %>% 
    mutate(Y_Cycle_Pred = as.numeric(prd_cycle[1])) %>% 
    mutate(Forecasted_Return = as.numeric(forecast_return)) %>% 
    select(-c(2:ncol(newmydat)))
  
  
  list(ML_Data = ML_Data,
       cycle_forecast=cycle_forecast,
       trend_forecast=trend_forecast,
       combined_forecast=combined_forecast,
       forecast_return=forecast_return,
       Y_cycle = Y_cycle,
       Y_trend = Y_trend,
       Cycle_Fits = all_fits,
       Trend_Fits = all_fits_trend)
}





