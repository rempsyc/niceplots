simpleSlopes <- function(response, predictor, moderator, data) {
  if(!require(bootES)){install.packages("bootES")}
  if(!require(lmSupport)){install.packages("lmSupport")}
  library(bootES)
  library(lmSupport)
  df <- data
  formulas <- paste(response, "~", predictor, "*", moderator)
  models.list <- sapply(formulas, lm, data = df, simplify = FALSE, USE.NAMES = TRUE)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,sums.list,ES.list,SIMPLIFY=FALSE)
  table.stats <- do.call(rbind.data.frame, stats.list)
  response.names <- rep(response, each=3)
  predictor.names <- row.names(table.stats)
  row.names(table.stats) <- NULL
  predictor.names <- gsub(".*\\.", "", predictor.names)
  table.stats <- cbind(response.names, predictor.names, table.stats)
  names(table.stats) <- c("Dependent Variable", "Predictor", "b", "t", "p", "sr2")
  table.stats
}

simpleSlopes_lows <- function(response, predictor, moderator, data) {
  if(!require(bootES)){install.packages("bootES")}
  if(!require(lmSupport)){install.packages("lmSupport")}
  library(bootES)
  library(lmSupport)
  df <- data
  df$lows <- unlist(df[,moderator]+sd(unlist(df[,moderator])))
  formulas <- paste(response, "~", predictor, "* lows")
  models.list <- sapply(formulas, lm, data = df, simplify = FALSE, USE.NAMES = TRUE)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,sums.list,ES.list,SIMPLIFY=FALSE)
  stats.list <- lapply(stats.list, function(x) x[predictor,])
  table.stats <- do.call(rbind.data.frame, stats.list)
  predictor.names <- paste0(predictor, " (Low-", moderator, ")")
  table.stats <- cbind(response, predictor.names, table.stats)
  names(table.stats) <- c("Dependent Variable", "Predictor (-1 SD)", "b", "t", "p", "sr2")
  table.stats
}

simpleSlopes_highs <- function(response, predictor, moderator, data) {
  if(!require(bootES)){install.packages("bootES")}
  if(!require(lmSupport)){install.packages("lmSupport")}
  library(bootES)
  library(lmSupport)
  df <- data
  df$highs <- unlist(df[,moderator]-sd(unlist(df[,moderator])))
  formulas <- paste(response, "~", predictor, "* highs")
  models.list <- sapply(formulas, lm, data = df, simplify = FALSE, USE.NAMES = TRUE)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,sums.list,ES.list,SIMPLIFY=FALSE)
  stats.list <- lapply(stats.list, function(x) x[predictor,])
  table.stats <- do.call(rbind.data.frame, stats.list)
  predictor.names <- paste0(predictor, " (High-", moderator, ")")
  table.stats <- cbind(response, predictor.names, table.stats)
  names(table.stats) <- c("Dependent Variable", "Predictor (+1 SD)", "b", "t", "p", "sr2")
  table.stats
}