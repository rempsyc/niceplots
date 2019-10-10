function (Data,Predictor,Response,xtitle=waiver(),ytitle=waiver(),has.points=T,has.jitter=F,alpha=0.7,has.ConfBand=F,has.Fullrange=F,has.linetype=F,has.shape=F,x_scale_Manual=F,y_scale_Manual=F,xmin,xmax,ymin,ymax,has.Legend=F,legend.Title="",has.Groups=F,Group.variable=NULL,Manual.colour=F,what.Colour="#619CFF",Groups.order=NULL,Groups.names=NULL,manual.slope.alpha=NULL) {
  library(ggplot2)
  if (!missing(Groups.order)) {Group.variable <- factor(Group.variable, levels=Groups.order)}
  if (!missing(Groups.names)) {levels(Group.variable) = Groups.names}
  if (has.Groups == F) {
   smooth <- stat_smooth(geom="line", method="lm", fullrange=has.Fullrange, color = what.Colour, size = 1)}
  if (has.Groups == T) {
    smooth <- stat_smooth(geom="line", method="lm", fullrange=has.Fullrange, size = 1)}
  if (has.ConfBand == T & has.Groups == F) {
    band <- geom_smooth(method="lm",colour=NA,fill=what.Colour)}
  if (has.ConfBand == T & has.Groups == T) {
    band <- geom_smooth(method="lm",colour=NA)}
  if (has.points == T & has.Groups == F & Manual.colour == F) {
    point <- geom_point(size = 2, alpha = alpha, shape = 16)}
  if (has.points == T & has.Groups == F & Manual.colour == T) {
    point <- geom_point(size = 2, alpha = alpha, colour = what.Colour, shape = 16)}
  if (has.points == T & has.Groups & has.shape == F) {
    point <- geom_point(size = 2, alpha = alpha, shape = 16)}
  if (has.points == T & has.Groups == T & has.shape == T) {
    point <- geom_point(size = 2, alpha = alpha)}
  if (has.jitter == T & has.Groups == F & Manual.colour == T) {
    jitter <- geom_jitter(size = 2, alpha = alpha, colour = what.Colour, shape = 16)}
  if (has.jitter == T & has.Groups == F & Manual.colour == F) {
    jitter <- geom_jitter(size = 2, alpha = alpha, shape = 16)}
  if (has.jitter == T & has.Groups == T & has.shape == F) {
    jitter <- geom_jitter(size = 2, alpha = alpha, shape = 16)}
  if (has.jitter == T & has.Groups == T & has.shape == T) {
    jitter <- geom_jitter(size = 2, alpha = alpha)}
  ggplot(Data,aes(x={{Predictor}},y={{Response}}, colour = switch(has.Groups==T, Group.variable), fill = switch(has.Groups==T, Group.variable), linetype = switch(has.Groups==T & has.linetype==T, Group.variable), shape = switch(has.Groups==T & has.shape==T, Group.variable), alpha = switch(!is.null(manual.slope.alpha),Group.variable))) +
    xlab(xtitle) +
    ylab(ytitle) +
    smooth +
    theme_bw(base_size = 24) +
    {if (has.ConfBand == TRUE) band} +
    {if (has.jitter == TRUE) jitter} +
    {if (has.points == TRUE) point} +
    {if (x_scale_Manual == TRUE) scale_x_continuous(limits=c(xmin, xmax), breaks = xmin:xmax)} +
    {if (y_scale_Manual == TRUE) scale_y_continuous(limits=c(ymin, ymax), breaks = ymin:ymax)} +
    {if (Manual.colour == TRUE & has.Groups == TRUE) scale_color_manual(values=what.Colour, name = legend.Title)} +
    {if (Manual.colour == TRUE & has.Groups == TRUE) scale_fill_manual(values=what.Colour, name = legend.Title)} +
    {if (Manual.colour == TRUE) guides(fill = guide_legend(override.aes=list(colour = what.Colour)))} +
    {if (has.Legend == FALSE) theme(legend.position = "none")} +
    labs(legend.title = legend.Title, colour = legend.Title, fill = legend.Title, linetype = legend.Title, shape = legend.Title) +
    {if (!missing(manual.slope.alpha)) scale_alpha_manual(values=manual.slope.alpha, guide=FALSE)} +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line=element_line(colour = "black"), axis.ticks=element_line(colour = "black"))
}
