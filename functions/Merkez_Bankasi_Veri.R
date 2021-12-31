#Get Inflation Survey Data from CBRT
library(methods)
library(xts)
library(tidyverse)
####################################################################################################
## Imalat sanayi kapasite kullanim orani
## Odemeler dengesi cari islemler hesabi
## Dis borc cevirme orani (banka)
## Yurtdisi yerlesik hisse
## Yurtdisi yerlesik DIBS
## Kisa vadeli dis borc stoku

MB_Series <- c("TP.KKO.MA", "TP.ODEAYRSUNUM6.Q1", "TP.ODEROLL.K1", "TP.PYUK3-8", "TP.KALANVADE.K20")  
for(i in 1:length(MB_Series)){

  series <- MB_Series[i]
  startDate <- "01-01-2012"
  endDate <- lubridate::today()
  API <- "QGeXzOxn4l"
  #construct URL
  URL <-
    paste0(
      "https://evds2.tcmb.gov.tr/service/evds/series=",
      paste0(series, collapse = "-"),
      "&startDate=",
      startDate,
      "&endDate=",
      format(endDate, "%d-%m-%Y"),
      "&type=xml&key=",
      API
    )
  doc <- xmlTreeParse(GET(URL))
  root <- xmlRoot(doc)
  my_data <- xmlSApply(root, function(x)
    xmlSApply(x, xmlValue))
  my_data <- data.frame(my_data, row.names = NULL)
  my_data <- t(my_data)
  colnames(my_data) <- unique(names(root[2]$items))
  my_data <- my_data[-1, c(1:(ncol(my_data) - 1))]
  rownames(my_data) <- NULL
  my_data <- as_tibble(my_data)
  assign(
    paste0("TR_",series),
    zoo(my_data[, 2] %>% pull %>% as.numeric, as.Date(as.yearmon(my_data$Tarih))) %>% Push_To_Month_End()
  )
  print(i)
}
TR_TP.KALANVADE.K20


