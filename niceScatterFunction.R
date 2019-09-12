# Nice ggplot scatter plot with groups
niceScatter <- function (Data,Predictor,Response,xtitle=waiver(),ytitle=waiver(),has.points=T,has.jitter=F,alpha=0.7,has.ConfBand=F,has.Fullrange=F,has.linetype=F,has.shape=F,x_scale_Manual=F,y_scale_Manual=F,xmin,xmax,ymin,ymax,has.Legend=F,legend.Title="",has.Groups=F,Group.variable=NULL,Manual.colour=F,what.Colour="#619CFF",Groups.order=NULL,Groups.names=NULL) {
  library(ggplot2)
  if (!missing(Groups.order)) {Group.variable <- factor(Group.variable, levels=Groups.order)}
  if (!missing(Groups.names)) {levels(Group.variable) = Groups.names}
  if (has.Groups == F) {
    smooth <- geom_smooth(method="lm", se = has.ConfBand, fullrange=has.Fullrange, color = what.Colour)}
  if (has.Groups == T) {
    smooth <- geom_smooth(method="lm", se = has.ConfBand, fullrange=has.Fullrange)}
  if (has.points == T & has.Groups == F & Manual.colour == T) {
    point <- geom_point(size = 2, alpha = alpha, colour = what.Colour)}
  if (has.points == T & has.Groups == F & Manual.colour == F) {
    point <- geom_point(size = 2, alpha = alpha)}
  if (has.points == T & has.Groups == T) {
    point <- geom_point(size = 2, alpha = alpha)}
  if (has.jitter == T & has.Groups == F & Manual.colour == T) {
    jitter <- geom_jitter(size = 2, alpha = alpha, colour = what.Colour)}
  if (has.jitter == T & has.Groups == F & Manual.colour == F) {
    jitter <- geom_jitter(size = 2, alpha = alpha)}
  if (has.jitter == T & has.Groups == T) {
    jitter <- geom_jitter(size = 2, alpha = alpha)}
  ggplot(Data,aes(x=Predictor,y=Response, colour = switch(has.Groups==T, Group.variable), fill = switch(has.Groups==T, Group.variable), linetype = switch(has.Groups==T & has.linetype==T, Group.variable), shape = switch(has.Groups==T & has.shape==T, Group.variable))) +
    xlab(xtitle) +
    ylab(ytitle) +
    smooth +
    theme_bw(base_size = 24) +
    {if (has.jitter==T) jitter} +
    {if (has.points==T) point} +
    {if (x_scale_Manual == TRUE) scale_x_continuous(limits=c(xmin, xmax), breaks = xmin:xmax)} +
    {if (y_scale_Manual == TRUE) scale_y_continuous(limits=c(ymin, ymax), breaks = ymin:ymax)} +
    {if (Manual.colour == TRUE & has.Groups == TRUE) scale_color_manual(values=what.Colour, name = legend.Title)} +
    {if (Manual.colour == TRUE & has.Groups == TRUE) scale_fill_manual(values=what.Colour, name = legend.Title)} +
    {if (Manual.colour == TRUE) guides(fill = guide_legend(override.aes=list(colour = what.Colour)))} +
    {if (has.Legend == FALSE) theme(legend.position = "none")} +
    labs(legend.title = legend.Title, colour = legend.Title, fill = legend.Title, linetype = legend.Title, shape = legend.Title) +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line=element_line())
}

