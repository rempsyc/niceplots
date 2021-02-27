niceDensity <- function(variable, Group, data, colours) {
  if(!require(dplyr)){install.packages("dplyr")}
  library(dplyr)
  var.name <- names(data)[which(names(data)==variable)]
  variable <- data[,variable]
  Group <- data[,Group]
  Group <- factor(Group)
  x <- seq(min(variable), max(variable), length.out=100)
  norm.1 <- data %>%
    filter(Group==levels(factor(data[,"Group"]))[1]) %>%
    with(data.frame(x = x, y = dnorm(x, mean(variable), sd(variable)))) %>%
    mutate(Group = factor(levels(factor(data[,"Group"]))[1],levels = levels(factor(data[,"Group"]))))
  norm.2 <- data %>%
    filter(Group==levels(factor(data[,"Group"]))[2]) %>%
    with(data.frame(x = x, y = dnorm(x, mean(variable), sd(variable)))) %>%
    mutate(Group = factor(levels(factor(data[,"Group"]))[2],levels = levels(factor(data[,"Group"]))))
  norm.3 <- data %>%
    filter(Group==levels(factor(data[,"Group"]))[3]) %>%
    with(data.frame(x = x, y = dnorm(x, mean(variable), sd(variable)))) %>%
    mutate(Group = factor(levels(factor(data[,"Group"]))[3],levels = levels(factor(data[,"Group"]))))
  ggplot(data, aes(x=variable, fill=Group)) +
    geom_density(alpha=0.6, size=1, colour="gray25") +
    theme_bw(base_size = 24) +
    facet_grid(Group ~ .) + 
    {if (!missing(colours)) scale_fill_manual(values=colours)} +
    geom_line(data = norm.1, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    geom_line(data = norm.2, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    geom_line(data = norm.3, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    xlab(var.name) +
    theme(legend.position = "none")
}