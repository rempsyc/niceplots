niceDensity <- function(variable, group, data, colours, ytitle="Density", xtitle=waiver(), groups.labels=NULL, grid=TRUE, shapiro.p) {
  if(!require(dplyr)){install.packages("dplyr")}
  if(!require(ggplot2)){install.packages("ggplot2")}
  library(dplyr)
  library(ggplot2)
  data$variable <- data[,variable]
  data$group <- factor(data[,group])
  data[,group] <- factor(data[,group])
  {if (!missing(groups.labels)) levels(data$group) <- groups.labels}
    norm.1 <- data %>%
    filter(data$group==levels(data$group)[1]) %>%
    with(data.frame(x = seq(min(variable), 
                            max(variable), 
                            length.out=100), 
                    y = dnorm(seq(min(variable), 
                                  max(variable), 
                                  length.out=100), mean(variable), 
                              sd(variable)))) %>%
    mutate(group = factor(levels(data$group)[1],levels = levels(data$group)))
  norm.2 <- data %>%
    filter(data$group==levels(data$group)[2]) %>%
    with(data.frame(x = seq(min(variable), 
                            max(variable), 
                            length.out=100), 
                    y = dnorm(seq(min(variable), 
                                  max(variable), 
                                  length.out=100), mean(variable), 
                              sd(variable)))) %>%
    mutate(group = factor(levels(data$group)[2],levels = levels(data$group)))
  norm.3 <- data %>%
    filter(data$group==levels(data$group)[3]) %>%
    with(data.frame(x = seq(min(variable), 
                            max(variable), 
                            length.out=100), 
                    y = dnorm(seq(min(variable), 
                                  max(variable), 
                                  length.out=100), mean(variable), 
                              sd(variable)))) %>%
    mutate(group = factor(levels(data$group)[3],levels = levels(data$group)))
  ggplot(data, aes(x=variable, fill=group)) +
    geom_density(alpha=0.6, size=1, colour="gray25") +
    theme_bw(base_size = 24) +
    facet_grid(group ~ .) +
    geom_line(data = norm.1, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    geom_line(data = norm.2, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    geom_line(data = norm.3, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    ylab(ytitle) +
    xlab(xtitle) +
    {if (!missing(colours)) scale_fill_manual(values=colours)} +
    {if (grid == FALSE) theme(panel.grid.major=element_blank(),
                             panel.grid.minor=element_blank())} +
    theme(legend.position = "none",
          axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black"),
          panel.border=element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks=element_line(colour = "black"))
}