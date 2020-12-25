niceAss <- function(model) {
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
  cat(yellow("Interpretation: (p) values < .05 imply assumptions are not respected. \nDiagnostic is how many assumptions are not respected for a given model or variable. \n  \n"))
  df
}