nice_t_test <- function(response, group, data) {
  if(!require(effsize)){install.packages("effsize")}
  library(effsize)
  formulas <- paste0(response, " ~ ", group)
  formulas <- sapply(formulas, as.formula)
  mod.list <- sapply(formulas, t.test, data = data, simplify = FALSE, USE.NAMES = TRUE)
  list.names <- c("statistic", "parameter", "p.value")
  sums.list <- lapply(mod.list, function(x) {(x)[list.names]})
  sapply(formulas, function (x) {
    cohen.d(x, 
            data = data)},
    simplify = FALSE, 
    USE.NAMES = TRUE) -> boot.lists
  list.stats <- list()
  for (i in 1:length(list.names)) {
    list.stats[[list.names[i]]] <- c(t((sapply(sums.list, `[[`, i))))
  }
  d <- unlist(sapply(boot.lists, function(x) {(x)["estimate"]}))
  ci <- sapply(boot.lists, function(x) {(x)["conf.int"]})
  CI_lower <- sapply(ci, `[`, "lower")
  CI_higher <- sapply(ci, `[`, "upper")
  table.stats <- data.frame(response,
                            list.stats,
                            d,
                            CI_lower,
                            CI_higher)
  names(table.stats) <- c("Variable", "t", "df", "p", "d", "CI_lower", "CI_upper")
  table.stats
}