get_data <- function(DF=DF,TYPE,REGION=NULL,CODE=NULL,NAME=NULL){
  x <- DF %>% filter(TYPE==!!enquo(TYPE)) 
  
  if (!is.null(REGION))
    x <- x %>% filter(REGION %in% !!enquo(REGION)) 
  
  if (!is.null(CODE))
    x <- x %>% filter(CODE %in% !!enquo(CODE)) 
  
  if (!is.null(NAME))
    x <- x %>% filter(NAME %in% !!enquo(NAME)) 
  
  x %>% select(DATE,CODE,P) %>% 
    spread(CODE,P) %>%
    Tibble_To_Zoo()
}

Tibble_To_Zoo<-function(df) zoo(as.data.frame(df)[,-1],as.Date(as.data.frame(df)[,1]))
