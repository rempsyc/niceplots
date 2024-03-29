niceMod <- function(response, predictor, moderator, moderator2=NULL, covariates=NULL, data, ...) {
  cat("Important: This function is now deprecated. A better version has migrated to the rempsyc package. \n Please install and use the rempsyc package here: https://github.com/rempsyc/rempsyc/ \n")
  if(!require(lmSupport)){install.packages("lmSupport")}
  library(lmSupport)
  if(!missing(covariates)) {
    covariates.term <- paste("+", covariates, collapse = " ") 
  } else {covariates.term <- ""}
  if(!missing(moderator2)) {
    moderator2.term <- paste("*", moderator2, collapse = " ") 
  } else {moderator2.term <- ""}
  formulas <- paste(response, "~", predictor, "*", moderator, moderator2.term, covariates.term)
  models.list <- sapply(formulas, lm, data = data, ..., simplify = FALSE, USE.NAMES = TRUE)
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

simpleSlopes <- function(response, predictor, moderator, moderator2=NULL, covariates=NULL, data, ...) {
  
  if(!require(lmSupport)){install.packages("lmSupport")}
  library(lmSupport)
  if(!missing(covariates)) {
    covariates.term <- paste("+", covariates, collapse = " ") 
  } else {covariates.term <- ""}
  if(!missing(moderator2)) {
    moderator2.term <- paste("*", moderator2, collapse = " ")
  } else {moderator2.term <- ""}
  
  # Calculate simple slopes for LOWS
  data$lows <- unlist(data[,moderator]+sd(unlist(data[,moderator])))
  formulas <- paste(response, "~", predictor, "* lows", moderator2.term, covariates.term)
  models.list <- sapply(formulas, lm, data = data, ..., simplify = FALSE, USE.NAMES = TRUE)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  df.list <- lapply(models.list, function(x) x[["df.residual"]])
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,df.list,sums.list,ES.list,SIMPLIFY=FALSE)
  stats.list <- lapply(stats.list, function(x) x[predictor,])
  table.stats1 <- do.call(rbind.data.frame, stats.list)
  predictor.names <- paste0(predictor, " (LOW-", moderator, ")")
  table.stats1 <- cbind(response, predictor.names, table.stats1)
  names(table.stats1) <- c("Dependent Variable", "Predictor (+/-1 SD)", "df", "b", "t", "p", "sr2")
  
  # Calculate simple slopes for mean-level
  formulas <- paste(response, "~", predictor, "*", moderator, moderator2.term, covariates.term)
  models.list <- sapply(formulas, lm, data = data, ..., simplify = FALSE, USE.NAMES = TRUE)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  df.list <- lapply(models.list, function(x) x[["df.residual"]])
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,df.list,sums.list,ES.list,SIMPLIFY=FALSE)
  stats.list <- lapply(stats.list, function(x) x[predictor,])
  table.stats2 <- do.call(rbind.data.frame, stats.list)
  predictor.names <- paste0(predictor, " (MEAN-", moderator, ")")
  table.stats2 <- cbind(response, predictor.names, table.stats2)
  names(table.stats2) <- c("Dependent Variable", "Predictor (+/-1 SD)", "df", "b", "t", "p", "sr2")
  
  # Calculate simple slopes for HIGHS
  data$highs <- unlist(data[,moderator]-sd(unlist(data[,moderator])))
  formulas <- paste(response, "~", predictor, "* highs", moderator2.term, covariates.term)
  models.list <- sapply(formulas, lm, data = data, ..., simplify = FALSE, USE.NAMES = TRUE)
  sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
  df.list <- lapply(models.list, function(x) x[["df.residual"]])
  ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
  stats.list <- mapply(cbind,df.list,sums.list,ES.list,SIMPLIFY=FALSE)
  stats.list <- lapply(stats.list, function(x) x[predictor,])
  table.stats3 <- do.call(rbind.data.frame, stats.list)
  predictor.names <- paste0(predictor, " (HIGH-", moderator, ")")
  table.stats3 <- cbind(response, predictor.names, table.stats3)
  names(table.stats3) <- c("Dependent Variable", "Predictor (+/-1 SD)", "df", "b", "t", "p", "sr2")
  
  # Combine both dataframes for both LOWS and HIGHS
  table.stats <- rbind(table.stats1,table.stats2,table.stats3)
  correct.order <- c(aperm(array(1:nrow(table.stats), 
                                 c(1,nrow(table.stats)/3,3)),
                           c(1,3,2)))
  table.stats <- table.stats[correct.order,] # 1, 4, 7, 2, 5, 8, 3, 6, 9
  
  if(missing(moderator2)){ return(table.stats) }
  
  if(!missing(moderator2)) { # Repeat steps for other level of the moderator
    
    # Add a column about moderator to the first column
    table.stats[moderator2] <- 0
    table.stats <- cbind(table.stats[1], table.stats[8], table.stats[2:7])
    
    # Recode dichotomic group variable moderator2
    data[moderator2] <- ifelse(data[moderator2] =="0", 1, 0)
    
    # Calculate simple slopes for LOWS
    data$lows <- unlist(data[,moderator]+sd(unlist(data[,moderator])))
    formulas <- paste(response, "~", predictor, "* lows", moderator2.term, covariates.term)
    models.list <- sapply(formulas, lm, data = data, ..., simplify = FALSE, USE.NAMES = TRUE)
    sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
    df.list <- lapply(models.list, function(x) x[["df.residual"]])
    ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
    stats.list <- mapply(cbind,df.list,sums.list,ES.list,SIMPLIFY=FALSE)
    stats.list <- lapply(stats.list, function(x) x[predictor,])
    table.stats1 <- do.call(rbind.data.frame, stats.list)
    predictor.names <- paste0(predictor, " (LOW-", moderator, ")")
    table.stats1 <- cbind(response, predictor.names, table.stats1)
    names(table.stats1) <- c("Dependent Variable", "Predictor (+/-1 SD)", "df", "b", "t", "p", "sr2")
    
    # Calculate simple slopes for mean-level
    formulas <- paste(response, "~", predictor, "*", moderator, moderator2.term, covariates.term)
    models.list <- sapply(formulas, lm, data = data, ..., simplify = FALSE, USE.NAMES = TRUE)
    sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
    df.list <- lapply(models.list, function(x) x[["df.residual"]])
    ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
    stats.list <- mapply(cbind,df.list,sums.list,ES.list,SIMPLIFY=FALSE)
    stats.list <- lapply(stats.list, function(x) x[predictor,])
    table.stats2 <- do.call(rbind.data.frame, stats.list)
    predictor.names <- paste0(predictor, " (MEAN-", moderator, ")")
    table.stats2 <- cbind(response, predictor.names, table.stats2)
    names(table.stats2) <- c("Dependent Variable", "Predictor (+/-1 SD)", "df", "b", "t", "p", "sr2")
    
    # Calculate simple slopes for HIGHS
    data$highs <- unlist(data[,moderator]-sd(unlist(data[,moderator])))
    formulas <- paste(response, "~", predictor, "* highs", moderator2.term, covariates.term)
    models.list <- sapply(formulas, lm, data = data, ..., simplify = FALSE, USE.NAMES = TRUE)
    sums.list <- lapply(models.list, function(x) {summary(x)$coefficients[-1,-2]})
    df.list <- lapply(models.list, function(x) x[["df.residual"]])
    ES.list <- lapply(models.list, function(x) {modelEffectSizes(x, Print=FALSE)$Effects[-1,4]})
    stats.list <- mapply(cbind,df.list,sums.list,ES.list,SIMPLIFY=FALSE)
    stats.list <- lapply(stats.list, function(x) x[predictor,])
    table.stats3 <- do.call(rbind.data.frame, stats.list)
    predictor.names <- paste0(predictor, " (HIGH-", moderator, ")")
    table.stats3 <- cbind(response, predictor.names, table.stats3)
    names(table.stats3) <- c("Dependent Variable", "Predictor (+/-1 SD)", "df", "b", "t", "p", "sr2")
    
    # Combine both dataframes for both LOWS and HIGHS
    table2.stats <- rbind(table.stats1,table.stats2,table.stats3)
    correct.order <- c(aperm(array(1:nrow(table2.stats), 
                                   c(1,nrow(table2.stats)/3,3)),
                             c(1,3,2)))
    table2.stats <- table2.stats[correct.order,] # 1, 4, 7, 2, 5, 8, 3, 6, 9
    
    # Add a column for moderator2
    table2.stats[moderator2] <- 1
    table2.stats <- cbind(table2.stats[1], table2.stats[8], table2.stats[2:7])
    
    # Merge with the first table
    final.table <- rbind(table.stats, table2.stats)
    final.table
    
  }
  
}