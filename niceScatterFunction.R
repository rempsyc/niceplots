niceScatter <- function(data,predictor,response,xtitle=waiver(),ytitle=waiver(),has.points=T,has.jitter=F,alpha=0.7,has.confband=F,has.fullrange=F,has.linetype=F,has.shape=F,xmin,xmax,xby=1,ymin,ymax,yby=1,has.legend=F,legend.title="",group.variable=NULL,colours="#619CFF",groups.order=NULL,groups.names=NULL,manual.slope.alpha=NULL,has.r=FALSE, r.x = Inf, r.y = -Inf, has.p=FALSE, p.x = Inf, p.y = -Inf) {
  cat("Important: This function is now deprecated. A better version has migrated to the rempsyc package. \n Please install and use the rempsyc package here: https://github.com/rempsyc/rempsyc/ \n")
  if(!require(ggplot2)){install.packages("ggplot2")}
  library(ggplot2)
  has.groups=!missing(group.variable)
  if (has.r == T) { 
    format.r <- function(r, precision = 0.01) {
      digits <- -log(precision, base = 10)
      r <- formatC(r, format = 'f', digits = digits)
      sub("0", "", r)}
    r = format.r(cor.test(data[,deparse(substitute(predictor))],data[,deparse(substitute(response))], use="complete.obs",)$estimate) 
    }
  if (has.p == T) { 
    format.p <- function(p, precision = 0.001) {
      digits <- -log(precision, base = 10)
      p <- formatC(p, format = 'f', digits = digits)
      if (p < .001) {
        p = paste0('< ', precision)}
      if (p >= .001) {
        p = paste0('= ', p)    }
      sub("0", "", p)
    }
    p = format.p(cor.test(data[,deparse(substitute(predictor))],data[,deparse(substitute(response))], use="complete.obs",)$p.value) 
    }
  if (!missing(groups.order)) {group.variable <- factor(group.variable, levels=groups.order)}
  if (!missing(groups.names)) {levels(group.variable) = groups.names}
  if (missing(group.variable)) {
    smooth <- stat_smooth(geom="line", method="lm", fullrange=has.fullrange, color = colours, size = 1)}
  if (!missing(group.variable)) {
    smooth <- stat_smooth(geom="line", method="lm", fullrange=has.fullrange, size = 1)}
  if (has.confband == T & missing(group.variable)) {
    band <- geom_smooth(method="lm",colour=NA,fill=colours)}
  if (has.confband == T & !missing(group.variable)) {
    band <- geom_smooth(method="lm",colour=NA)}
  if (has.points == T & missing(group.variable) & missing(colours)) {
    observations <- geom_point(size = 2, alpha = alpha, shape = 16)}
  if (has.points == T & !missing(group.variable) & has.shape == F) {
    observations <- geom_point(size = 2, alpha = alpha, shape = 16)}
  if (has.points == T & missing(group.variable) & !missing(colours)) {
    observations <- geom_point(size = 2, alpha = alpha, colour = colours, shape = 16)}
  if (has.points == T & !missing(group.variable) & has.shape == T) {
    observations <- geom_point(size = 2, alpha = alpha)}
  if (has.jitter == T & missing(group.variable) & missing(colours)) {
    observations <- geom_jitter(size = 2, alpha = alpha, shape = 16)
    has.points=F}
  if (has.jitter == T & !missing(group.variable) & has.shape == F) {
    observations <- geom_jitter(size = 2, alpha = alpha, shape = 16)
    has.points=F}
  if (has.jitter == T & missing(group.variable) & !missing(colours)) {
    observations <- geom_jitter(size = 2, alpha = alpha, colour = colours, shape = 16)
    has.points=F}
  if (has.jitter == T & !missing(group.variable) & has.shape == T) {
    observations <- geom_jitter(size = 2, alpha = alpha)
    has.points=F}
  ggplot(data,aes(x={{predictor}},y={{response}}, colour = switch(has.groups==T, group.variable), fill = switch(has.groups==T, group.variable), linetype = switch(has.groups==T & has.linetype==T, group.variable), shape = switch(has.groups==T & has.shape==T, group.variable), alpha = switch(!is.null(manual.slope.alpha),group.variable))) +
    xlab(xtitle) +
    ylab(ytitle) +
    smooth +
    theme_bw(base_size = 24) +
    {if (has.confband == TRUE) band} +
    {if (exists("observations")) observations} +
    {if (!missing(xmin)) scale_x_continuous(limits=c(xmin, xmax), breaks = seq(xmin, xmax, by = xby))} +
    {if (!missing(ymin)) scale_y_continuous(limits=c(ymin, ymax), breaks = seq(ymin, ymax, by = yby))} +
    {if (!missing(colours) & !missing(group.variable)) scale_color_manual(values=colours, name = legend.title)} +
    {if (!missing(colours) & !missing(group.variable)) scale_fill_manual(values=colours, name = legend.title)} +
    {if (!missing(colours)) guides(fill = guide_legend(override.aes=list(colour = colours)))} +
    {if (has.legend == FALSE) theme(legend.position = "none")} +
    labs(legend.title = legend.title, colour = legend.title, fill = legend.title, linetype = legend.title, shape = legend.title) +
    {if (!missing(manual.slope.alpha)) scale_alpha_manual(values=manual.slope.alpha, guide=FALSE)} +
    {if (has.r == TRUE) annotate(geom="text", x=r.x, y=r.y, label=sprintf("italic('r =')~'%s'", r), parse = TRUE, hjust=1, vjust=-3, size=7)} +
    {if (has.p == TRUE) annotate(geom="text", x=p.x, y=p.y, label=sprintf("italic('p')~'%s'", p), parse = TRUE, hjust=1, vjust=-1, size=7)} +
    theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line=element_line(colour = "black"), axis.ticks=element_line(colour = "black"))
}