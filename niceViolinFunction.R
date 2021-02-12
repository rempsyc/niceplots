niceViolin <- function (group,response,boot=T,bootstraps=2000,colours,xlabels=NULL,ytitle=waiver(),xtitle=NULL,has.ylabels=T,has.xlabels=T,comp1=1,comp2=2,signif_annotation=NULL,signif_yposition=NULL,signif_xmin=NULL,signif_xmax=NULL) {
  Data <- data.frame(group, response)
  class(Data$response) <- "numeric"
  if(!require(rcompanion)){install.packages("rcompanion")}
  if(!require(ggplot2)){install.packages("ggplot2")}
  if(!require(ggsignif)){install.packages("ggsignif")}
  library(rcompanion)
  library(ggplot2)
  library(ggsignif)
  dataSummary <- groupwiseMean(response ~ group, 
                               data   = Data, 
                               conf   = 0.95, 
                               digits = 3,
                               R      = bootstraps,
                               boot        = TRUE,
                               traditional = !boot,
                               normal      = FALSE,
                               basic       = FALSE,
                               percentile  = FALSE,
                               bca         = boot)
  ggplot(Data, aes(x = factor(group), 
                   y = response,
                   fill = factor(group))) + 
    theme_bw(base_size = 24) +
    {if (!missing(colours)) scale_fill_manual(values=colours)} +
    {if (!missing(xlabels)) scale_x_discrete(labels=c(xlabels))} +
    ylab(ytitle) +
    xlab(xtitle) +
    geom_violin(color = "white", alpha = 0.7) +
    geom_point(aes(y = Mean), 
               color = "black", 
               size = 4, 
               data = dataSummary) + 
    geom_errorbar(aes(y = Mean, 
                      ymin = dataSummary[,6], 
                      ymax = dataSummary[,7]),
                  color = "black", 
                  size = 0.5, 
                  width = 0, 
                  data = dataSummary) + 
    theme(legend.position = "none", 
          axis.text.x = element_text(colour="black"), 
          axis.text.y = element_text(colour="black"),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks=element_line(colour = "black")) +
    {if (has.ylabels == FALSE) theme(axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank())} +
    {if (has.xlabels == FALSE) theme(axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank())} +
    {if (!missing(comp1)) geom_signif(comparisons = list(c(comp1, comp2)), 
                                      map_signif_level=TRUE, 
                                      size= 1.3, 
                                      textsize=8)} +
    {if (!missing(signif_annotation)) geom_signif(annotation=signif_annotation, 
                                             y_position=signif_yposition, 
                                             xmin=signif_xmin, 
                                             xmax=signif_xmax,
                                             size=1.3, 
                                             textsize=8)}
}
