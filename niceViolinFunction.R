# Nice Violin Plot Function
niceViolin <- function (Group,Response,ManualColour=F,ylabel,xlabel=T,compare=F,comp1=NULL,comp2=NULL,compareManual=F,signif_annotation=NULL,signif_yposition=NULL,signif_xmin=NULL,signif_xmax=NULL) {
  Data <- data.frame(Group, Response)
  class(Data$Response) <- "numeric"
  library(rcompanion)
  dataSummary <- groupwiseMean(Response ~ Group, 
                               data   = Data, 
                               conf   = 0.95, 
                               digits = 3,
                               R      = 2000,
                               boot        = TRUE,
                               traditional = FALSE,
                               normal      = FALSE,
                               basic       = FALSE,
                               percentile  = FALSE,
                               bca         = TRUE)
  library(ggplot2)
  library(ggsignif)
  ggplot(Data, aes(x = factor(Group), 
                   y = Response,
                   fill = factor(Group))) + 
    theme_grey(base_size = 24) +
    {if (ManualColour == TRUE) scale_fill_manual(values=c("#00BA38",
                                                          "#619CFF",
                                                          "#F8766D"))} +
    scale_x_discrete(labels=c("EPT" = "Embodied",
                              "MPT" = "Mental",
                              "CTR" = "Control")) +
    ylab(ylabel) +
    geom_violin() +
    geom_point(aes(y = dataSummary$Mean), 
               color = "black", 
               size = 4, 
               data = dataSummary) + 
    geom_errorbar(aes(y = dataSummary$Mean, 
                      ymin = dataSummary$Bca.lower, 
                      ymax = dataSummary$Bca.upper),
                  color = "black", 
                  size = 0.5, 
                  width = 0.2, 
                  data = dataSummary) + 
    theme(legend.position = "none", 
          axis.title.x=element_blank(),
          axis.text.x = element_text(colour="black"), 
          axis.text.y = element_text(colour="black")) +
    {if (xlabel == FALSE) theme(axis.text.x=element_blank(),
                               axis.ticks.x=element_blank())} +
    {if (compare == TRUE) geom_signif(comparisons = list(c(comp1, comp2)), 
                                          map_signif_level=TRUE, 
                                          size= 1.3, 
                                          textsize=8)} +
    {if (compareManual == TRUE) geom_signif(annotation=signif_annotation, 
                                            y_position=signif_yposition, 
                                            xmin=signif_xmin, 
                                            xmax=signif_xmax,
                                            size=1.3, 
                                            textsize=8)}
}
# Dots  = Means; Error bars = 95% bootstrapped confidence Intervals; Width = Distribution Density (Frequency)
