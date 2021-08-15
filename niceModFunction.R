niceMod <- function(response, predictor, moderator, covariates=NULL, data) {
  if(!require(bootES)){install.packages("bootES")}
  if(!require(lmSupport)){install.packages("lmSupport")}
  library(bootES)
  library(lmSupport)
  if(!missing(covariates)) {
    covariates.term <- paste("+", covariates, collapse = " ") 
  } else {covariates.term <- ""}
  formulas <- paste(response, "~", predictor, "*", moderator, covariates.term)
  models.list <- sapply(formulas, lm, data = data, simplify = FALSE, USE.NAMES = TRUE)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  df.list <- lapply(models.list, function(x) x[["df.residual"]])
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,df.list,sums.list,ES.list,SIMPLIFY=FALSE)
  table.stats <- do.call(rbind.data.frame, stats.list)
  response.names <- rep(response, each=nrow(sums.list[[1]]))
  predictor.names <- row.names(table.stats)
  row.names(table.stats) <- NULL
  predictor.names <- gsub(".*\\.", "", predictor.names)
  table.stats <- cbind(response.names, predictor.names, table.stats)
  names(table.stats) <- c("Dependent Variable", "Predictor", "df", "b", "t", "p", "sr2")
  table.stats
}

simpleSlopes <- function(response, predictor, moderator, covariates=NULL, data) {
  if(!require(bootES)){install.packages("bootES")}
  if(!require(lmSupport)){install.packages("lmSupport")}
  library(bootES)
  library(lmSupport)
  if(!missing(covariates)) {
    covariates.term <- paste("+", covariates, collapse = " ") 
  } else {covariates.term <- ""}

  # Calculate simple slopes for LOWS
  data$lows <- unlist(data[,moderator]+sd(unlist(data[,moderator])))
  formulas <- paste(response, "~", predictor, "* lows", covariates.term)
  models.list <- sapply(formulas, lm, data = data, simplify = FALSE, USE.NAMES = TRUE)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  df.list <- lapply(models.list, function(x) x[["df.residual"]])
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,df.list,sums.list,ES.list,SIMPLIFY=FALSE)
  stats.list <- lapply(stats.list, function(x) x[predictor,])
  table.stats1 <- do.call(rbind.data.frame, stats.list)
  predictor.names <- paste0(predictor, " (LOW-", moderator, ")")
  table.stats1 <- cbind(response, predictor.names, table.stats1)
  names(table.stats1) <- c("Dependent Variable", "Predictor (+/-1 SD)", "df", "b", "t", "p", "sr2")
  
  # Calculate simple slopes for HIGHS
  data$highs <- unlist(data[,moderator]-sd(unlist(data[,moderator])))
  formulas <- paste(response, "~", predictor, "* highs", covariates.term)
  models.list <- sapply(formulas, lm, data = data, simplify = FALSE, USE.NAMES = TRUE)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  df.list <- lapply(models.list, function(x) x[["df.residual"]])
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,df.list,sums.list,ES.list,SIMPLIFY=FALSE)
  stats.list <- lapply(stats.list, function(x) x[predictor,])
  table.stats2 <- do.call(rbind.data.frame, stats.list)
  predictor.names <- paste0(predictor, " (HIGH-", moderator, ")")
  table.stats2 <- cbind(response, predictor.names, table.stats2)
  names(table.stats2) <- c("Dependent Variable", "Predictor (+/-1 SD)", "df", "b", "t", "p", "sr2")
  
  # Combine both dataframes for both LOWS and HIGHS
  table.stats <- rbind(table.stats1,table.stats2)
  correct.order <- c(aperm(array(1:nrow(table.stats), c(1,nrow(table.stats)/2,2)),c(1,3,2)))
  table.stats <- table.stats[correct.order,]
  table.stats
}