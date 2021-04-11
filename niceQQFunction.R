niceQQ <- function(variable, group, data, colours, groups.labels=NULL, grid=TRUE, shapiro=FALSE, title=variable) {
  if(!require(dplyr)){install.packages("dplyr")}
  if(!require(ggplot2)){install.packages("ggplot2")}
  if(!require(ggrepel)){install.packages("ggrepel")}
  if(!require(qqplotr)){install.packages("qqplotr")}
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(qqplotr)
  data[[group]] <- as.factor(data[[group]])
  gform <- reformulate(".", response=group)
  {if (!missing(groups.labels)) levels(data[[group]]) <- groups.labels}
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
    suppressWarnings(
      dat_text <- data %>% group_by(.data[[group]]) %>% 
        summarise(text=shapiro.test(.data[[variable]])$p.value) %>% 
        mutate(text=sprintf("italic('p')~'%s'", format.p(text)))
    )
  }
  # Make plot
  ggplot(data = data, mapping = aes_string(fill=group, sample=variable)) +
    stat_qq_band() +
    stat_qq_line() +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    facet_grid(gform) +
    ggtitle(title) +
    theme_bw(base_size = 24) +
    {if (shapiro == TRUE) geom_text_repel(data = dat_text,
                                          mapping = aes(x = Inf, 
                                                        y = -Inf, 
                                                        label = text),
                                          inherit.aes = FALSE,
                                          size = 6,
                                          force = 0,
                                          parse = TRUE)} +
    {if (!missing(colours)) scale_fill_manual(values=colours)} +
    {if (grid == FALSE) theme(panel.grid.major=element_blank(),
                             panel.grid.minor=element_blank())} +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black"),
          panel.border=element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks=element_line(colour = "black"))
}