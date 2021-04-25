niceVar <- function(variable, group, data) {
  if(!require(dplyr)){install.packages("dplyr")}
  library(dplyr)
  # Make basic frame
  var.table <- data %>%
    group_by(.data[[group]]) %>%
    summarise(var=var(.data[[variable]])) %>%
    t %>%
    as.data.frame
  # Format table in an acceptable format
  var.table <- cbind(variable, var.table)
  var.table <- var.table[-1,]
  rownames(var.table) <- NULL
  # Make all relevant variables numeric
  var.table <- var.table %>%
    mutate(across(-variable, as.numeric))
  # Add the ratio and hetero columns
  var.table %>%
    rowwise() %>%
    mutate(`Max/Min Ratio` = max(c(V1, V2, V3))/min(c(V1, V2, V3)),
           `Heteroscedastic (four times bigger)?` = `Max/Min Ratio` > 4) -> var.table
  # Change names to something meaningful
  for (i in 1:length(levels(data[[group]]))) {
    names(var.table)[1+i] <- levels(data[[group]])[i]
  }
  # Capitalize first letters
  var.table <- var.table %>%
    rename_with(tools::toTitleCase, everything())
  # Get resulting table
  var.table
}