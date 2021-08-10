if(!require(bootES)){install.packages("bootES")}
if(!require(lmSupport)){install.packages("lmSupport")}
library(bootES)
library(lmSupport)

simpleSlopes <- function(outcome, pred, modx, data) {
  df <- data
  formula <- paste(outcome, "~", pred, "*", modx)
  mod <- lm(formula, data=df, na.action="na.exclude")
  models.list <- list(mod)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,sums.list,ES.list,SIMPLIFY=FALSE)
  table.stats <- do.call(rbind.data.frame, stats.list)
  predictor.names <- row.names(table.stats)
  table.stats <- cbind(predictor.names, table.stats)
  names(table.stats) <- c("Predictor", "b", "t", "p", "sr2")
  table.stats
}

simpleSlopes_lows <- function(outcome, pred, modx, data) {
  df <- data
  df$lows <- unlist(df[,modx]+sd(unlist(df[,modx])))
  formula <- paste(outcome, "~", pred, "* lows")
  mod <- lm(formula, data=df, na.action="na.exclude")
  models.list <- list(mod)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,sums.list,ES.list,SIMPLIFY=FALSE)
  table.stats <- do.call(rbind.data.frame, stats.list)
  predictor.names <- row.names(table.stats)
  table.stats <- cbind(predictor.names, table.stats)
  names(table.stats) <- c("Predictor", "b", "t", "p", "sr2")
  table.stats
}

simpleSlopes_highs <- function(outcome, pred, modx, data) {
  df <- data
  df$lows <- unlist(df[,modx]-sd(unlist(df[,modx])))
  formula <- paste(outcome, "~", pred, "* lows")
  mod <- lm(formula, data=df, na.action="na.exclude")
  models.list <- list(mod)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,sums.list,ES.list,SIMPLIFY=FALSE)
  table.stats <- do.call(rbind.data.frame, stats.list)
  predictor.names <- row.names(table.stats)
  table.stats <- cbind(predictor.names, table.stats)
  names(table.stats) <- c("Predictor", "b", "t", "p", "sr2")
  table.stats
}