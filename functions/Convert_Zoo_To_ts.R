Convert_Zoo_To_ts<-function(myzoo,freq){
  ts(myzoo,
     c(lubridate::year(start(myzoo)),
       lubridate::month(start(myzoo))),
     c(lubridate::year(end(myzoo)),
       lubridate::month(end(myzoo))),
     frequency = freq)
}

