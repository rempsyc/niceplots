niceViolin <- function (Group,Response,boot=T,bootstraps=2000,Manual.Colour=F,what.Colours,has.ylabels=T,has.xlabels=T,Manual.xlabels=F,xlabels=NULL,has.xtitle=T,has.ytitle=T,ytitle=waiver(),xtitle=waiver(),compare=F,comp1=NULL,comp2=NULL,Manual.compare=F,signif_annotation=NULL,signif_yposition=NULL,signif_xmin=NULL,signif_xmax=NULL) {
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
    {if (Manual.Colour == TRUE) scale_fill_manual(values=what.Colours)} +
    {if (Manual.xlabels == TRUE) scale_x_discrete(labels=c(xlabels))} +
    ylab(ytitle) +
    xlab(xtitle) +
    geom_violin() +
    geom_point(aes(y = dataSummary$Mean), 
               color = "black", 
               size = 4, 
               data = dataSummary) + 
    geom_errorbar(aes(y = dataSummary$Mean, 
                      ymin = dataSummary[,6], 
                      ymax = dataSummary[,7]),
                  color = "black", 
                  size = 0.5, 
                  width = 0.2, 
                  data = dataSummary) + 
    theme(legend.position = "none", 
          axis.text.x = element_text(colour="black"), 
          axis.text.y = element_text(colour="black"),
          axis.title.x= {if (has.xtitle == FALSE) element_blank()},
          axis.title.y = switch((has.ytitle == FALSE) + 1,
                                element_text(angle = 90), 
                                element_blank()),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks=element_line(colour = "black")) +
    {if (has.ylabels == FALSE) theme(axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank())} +
    {if (has.xlabels == FALSE) theme(axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank())} +
    {if (compare == TRUE) geom_signif(comparisons = list(c(comp1, comp2)), 
                                      map_signif_level=TRUE, 
                                      size= 1.3, 
                                      textsize=8)} +
    {if (Manual.compare == TRUE) geom_signif(annotation=signif_annotation, 
                                             y_position=signif_yposition, 
                                             xmin=signif_xmin, 
                                             xmax=signif_xmax,
                                             size=1.3, 
                                             textsize=8)}
}
