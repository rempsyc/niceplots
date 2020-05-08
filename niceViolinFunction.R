niceViolin <- function (Group,Response,boot=T,bootstraps=2000,what.Colours,xlabels=NULL,ytitle=waiver(),xtitle=waiver(),has.ylabels = T,has.xlabels = T,comp1=NULL,comp2=NULL,signif_annotation=NULL,signif_yposition=NULL,signif_xmin=NULL,signif_xmax=NULL) {
  Data <- data.frame(Group, Response)
  class(Data$Response) <- "numeric"
  if(!require(rcompanion)){install.packages("rcompanion") + library(rcompanion)}
  dataSummary <- groupwiseMean(Response ~ Group, 
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
  if(!require(ggplot2)){install.packages("ggplot2") + library(ggplot2)}
  if(!require(ggsignif)){install.packages("ggsignif") + library(ggsignif)}
  ggplot(Data, aes(x = factor(Group), 
                   y = Response,
                   fill = factor(Group))) + 
    theme_bw(base_size = 24) +
    {if (!missing(what.Colours)) scale_fill_manual(values=what.Colours)} +
    {if (!missing(xlabels)) scale_x_discrete(labels=c(xlabels))} +
    ylab(ytitle) +
    xlab(xtitle) +
    geom_violin() +
    geom_point(aes(y = Mean), 
               color = "black", 
               size = 4, 
               data = dataSummary) + 
    geom_errorbar(aes(y = Mean, 
                      ymin = dataSummary[,6], 
                      ymax = dataSummary[,7]),
                  color = "black", 
                  size = 0.5, 
                  width = 0.2, 
                  data = dataSummary) + 
    theme(legend.position = "none", 
          axis.text.x = element_text(colour="black"), 
          axis.text.y = element_text(colour="black"),
          # axis.title.x= {if (has.xtitle == FALSE) element_blank()},
          # axis.title.y = switch((has.ytitle == FALSE) + 1,
          #                      element_text(angle = 90), 
          #                      element_blank()),
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
