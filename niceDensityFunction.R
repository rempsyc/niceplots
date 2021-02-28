niceDensity <- function(variable, Group, data, colours, ytitle="Density", xtitle=waiver(), groups.labels=NULL, grid=TRUE, shapiro.p) {
  if(!require(dplyr)){install.packages("dplyr")}
  if(!require(ggplot2)){install.packages("ggplot2")}
  library(dplyr)
  library(ggplot2)
  var.name <- names(data)[which(names(data)==variable)]
  data$variable <- data[,variable]
  data$Group <- factor(data[,Group])
  {if (!missing(groups.labels)) levels(data$Group) <- groups.labels}
  x <- seq(min(data$variable), max(data$variable), length.out=100)
  y = dnorm(x, mean(data$variable), sd(data$variable))
  norm.1 <- data %>%
    filter(data$Group==levels(data$Group)[1]) %>%
    with(data.frame(x = x, y = y)) %>%
    mutate(Group = factor(levels(data$Group)[1],levels = levels(data$Group)))
  norm.2 <- data %>%
    filter(data$Group==levels(data$Group)[2]) %>%
    with(data.frame(x = x, y = y)) %>%
    mutate(Group = factor(levels(data$Group)[2],levels = levels(data$Group)))
  norm.3 <- data %>%
    filter(data$Group==levels(data$Group)[3]) %>%
    with(data.frame(x = x, y = y)) %>%
    mutate(Group = factor(levels(data$Group)[3],levels = levels(data$Group)))
  ggplot(data, aes(x=variable, fill=Group)) +
    geom_density(alpha=0.6, size=1, colour="gray25") +
    theme_bw(base_size = 24) +
    facet_grid(Group ~ .) +
    geom_line(data = norm.1, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    geom_line(data = norm.2, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    geom_line(data = norm.3, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    ylab(ytitle) +
    xlab(xtitle) +
    xlab(var.name) +
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