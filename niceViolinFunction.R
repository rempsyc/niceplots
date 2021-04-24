niceViolin <- function (data, group, response, boot=TRUE, bootstraps=2000, 
                        colours, xlabels=NULL, ytitle=waiver(), xtitle=NULL, 
                        has.ylabels=TRUE, has.xlabels=TRUE, comp1=1, comp2=2,
                        signif_annotation=NULL, signif_yposition=NULL, signif_xmin=NULL,
                        signif_xmax=NULL, ymin, ymax, yby=1, CIcap.width=0.1, obs=FALSE,
                        alpha=.70, border.colour="white") {
  data[[group]] <- as.factor(data[[group]])
  gform <- reformulate(group, response)
  class(data[[response]]) <- "numeric"
  if(!require(rcompanion)){install.packages("rcompanion")}
  if(!require(ggplot2)){install.packages("ggplot2")}
  if(!require(ggsignif)){install.packages("ggsignif")}
  library(rcompanion)
  library(ggplot2)
  library(ggsignif)
  dataSummary <- groupwiseMean(gform,
                               data = data,
                               conf = 0.95,
                               digits = 3,
                               R = bootstraps,
                               boot = TRUE,
                               traditional = !boot,
                               normal = FALSE,
                               basic = FALSE,
                               percentile = FALSE,
                               bca = boot)
  ggplot(data, aes(x = .data[[group]],
                   y = .data[[response]],
                   fill = .data[[group]])) + 
    theme_bw(base_size = 24) +
    {if (!missing(colours)) scale_fill_manual(values=colours)} +
    {if (!missing(xlabels)) scale_x_discrete(labels=c(xlabels))} +
    ylab(ytitle) +
    xlab(xtitle) +
    geom_violin(color = border.colour, alpha = alpha) +
    geom_point(aes(y = Mean), 
               color = "black", 
               size = 4, 
               data = dataSummary) + 
    geom_errorbar(aes(y = Mean, 
                      ymin = dataSummary[,6], 
                      ymax = dataSummary[,7]),
                  color = "black", 
                  size = 1, 
                  width = CIcap.width, 
                  data = dataSummary) +
    theme(legend.position = "none", 
          axis.text.x = element_text(colour="black"), 
          axis.text.y = element_text(colour="black"),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks=element_line(colour = "black")) +
    {if (obs == TRUE) geom_dotplot(binaxis = "y", 
                                   stackdir = "center", 
                                   position = "dodge",
                                   color = NA,
                                   fill = "black",
                                   alpha = 0.3,
                                   dotsize = 0.5)} +
    {if (has.ylabels == FALSE) theme(axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank())} +
    {if (has.xlabels == FALSE) theme(axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank())} +
    {if (!missing(ymin)) scale_y_continuous(limits=c(ymin, ymax),
                                            breaks = seq(ymin, ymax, by = yby))} +
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