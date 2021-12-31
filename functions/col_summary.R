col_summary <- function(data, col_names, na.rm=TRUE){
  
  data %>% summarise(across({{ col_names }},
                    list(
                       min=min,
                       max=max,
                       median=median,
                       mean=mean
                     ),
                     na.rm=na.rm,
                     .names="{col}_{fn}"
                    ))
  
}
