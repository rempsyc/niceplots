niceDensity <- function(variable, group, data, colours, ytitle="Density", xtitle=waiver(), groups.labels=NULL, grid=TRUE, shapiro=FALSE) {
  if(!require(dplyr)){install.packages("dplyr")}
  if(!require(ggplot2)){install.packages("ggplot2")}
  if(!require(ggrepel)){install.packages("ggrepel")}
  library(dplyr)
  library(ggplot2)
  require(ggrepel)
  data$variable <- data[,variable]
  data$group <- factor(data[,group])
  data[,group] <- factor(data[,group])
  {if (!missing(groups.labels)) levels(data$group) <- groups.labels}
  # Make data for normally distributed lines
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
  # Make data for the Shapiro-Wilk tests
  if (shapiro == TRUE) { 
    format.p <- function(p, precision = 0.001) {
      digits <- -log(precision, base = 10)
      p <- formatC(p, format = 'f', digits = digits)
      if (p < .001) {
        p = paste0('< ', precision, " (Shapiro-Wilk)")}
      if (p >= .001) {
        p = paste0('= ', p, " (Shapiro-Wilk)")    }
      sub("0", "", p)
    }
  data %>%
    filter(data$group==levels(data$group)[1]) %>%
    select(variable) %>%
    unlist %>%
    shapiro.test() -> shap
  shapiro.1 <- sprintf("italic('p')~'%s'", format.p(shap$p.value))
  data %>%
    filter(data$group==levels(data$group)[2]) %>%
    select(variable) %>%
    unlist %>%
    shapiro.test() -> shap
  shapiro.2 <- sprintf("italic('p')~'%s'", format.p(shap$p.value))
  data %>%
    filter(data$group==levels(data$group)[3]) %>%
    select(variable) %>%
    unlist %>%
    shapiro.test() -> shap
  shapiro.3 <- sprintf("italic('p')~'%s'", format.p(shap$p.value))
  # Make annotations dataframes
  dat_text <- data.frame(
    group = levels(data$group),
    text = c(shapiro.1, shapiro.2, shapiro.3))
  }
  # Make plot
  ggplot(data, aes(x=variable, fill=group)) +
    geom_density(alpha=0.6, size=1, colour="gray25") +
    theme_bw(base_size = 24) +
    facet_grid(group ~ .) +
    geom_line(data = norm.1, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    geom_line(data = norm.2, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    geom_line(data = norm.3, aes(x = x, y = y), color = "darkslateblue", size=1.2, alpha=0.9) +
    ylab(ytitle) +
    xlab(xtitle) +
    {if (shapiro == TRUE) geom_text_repel(data = dat_text,
                                          mapping = aes(x = Inf, 
                                                        y = Inf, 
                                                        label = text),
                                          inherit.aes = FALSE,
                                          size = 6,
                                          force = 0,
                                          parse = TRUE)} +
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