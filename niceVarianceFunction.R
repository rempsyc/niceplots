niceVariance <- function(variable, group, data, colours, groups.labels, grid=TRUE, shapiro=FALSE, ytitle=variable) {
  if(!require(dplyr)){install.packages("dplyr")}
  if(!require(ggplot2)){install.packages("ggplot2")}
  if(!require(ggrepel)){install.packages("ggrepel")}
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  data[[group]] <- as.factor(data[[group]])
  {if (!missing(groups.labels)) levels(data[[group]]) <- groups.labels}
  # Calculate variance
  var <- data %>%
    group_by(.data[[group]]) %>%
    summarize(var=var(.data[[variable]]))
  diff <- max(var[,"var"])/min(var[,"var"])
  # Make annotation dataframe
  dat_text <- var %>% 
    mutate(text=paste0("var = ", round(var,2)))
  # Make plot
  niceScatter(data=data,
              predictor=.data[[group]],
              response=.data[[variable]],
              group.variable=data[[group]],
              colours=colours,
              groups.names=groups.labels,
              xtitle=NULL,
              ytitle=ytitle) +
    annotate(geom="text", 
             x=median(1:length(levels(data[[group]]))), 
             y=max(data[[variable]]), 
             label=paste0("max/min = ", 
                          round(diff, 2),
                          "x bigger"),
             hjust=0.5, 
             size=6) +
    geom_text_repel(data=dat_text,
                    mapping=aes(x=.data[[group]], 
                                  y=-Inf, 
                                  label=text),
                    inherit.aes=FALSE,
                    size=6)
}