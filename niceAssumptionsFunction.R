niceAssumptions <- function(model, interpretation = TRUE) {
  cat("Important: This function is now deprecated. A better version has migrated to the rempsyc package. \n Please install and use the rempsyc package here: https://github.com/rempsyc/rempsyc/ \n")
  if(!require(lmtest)){install.packages("lmtest")}
  if(!require(crayon)){install.packages("crayon")}
  library(lmtest)
  library(crayon)
  model.name <- format(model$terms)
  shapiro <- round(shapiro.test(model$residuals)$p.value, 3)
  bp <- round(bptest(model)$p.value, 3)
  dw <- round(dwtest(model)$p.value, 3)
  dg <- sum(shapiro < .05, bp < .05, dw < .05)
  df <- data.frame("Model..." = model.name, "Normality (Shapiro-Wilk)..." = shapiro, "Homoscedasticity (Breusch-Pagan)..." = bp, "Autocorrelation of residuals (Durbin-Watson)..." = dw, "Diagnostic..." = dg, check.names = FALSE)
  row.names(df) <- NULL
  if(interpretation == TRUE) { cat(yellow("Interpretation: (p) values < .05 imply assumptions are not respected. \nDiagnostic is how many assumptions are not respected for a given model or variable. \n  \n")) }
  df
}