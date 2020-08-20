niceTable <- function (dataframe) {
  if(!require(flextable)){install.packages("flextable") + library(flextable)}
  if(!require(dplyr)){install.packages("dplyr") + library(dplyr)}
  dataframe %>%
    flextable %>%
    theme_vanilla %>%
    fontsize(part = "all", size = 12) %>%
    font(part = "all", fontname = "Times New Roman") %>%
    align(align = "center", part = "all") %>%
    set_table_properties(layout = "autofit")
}