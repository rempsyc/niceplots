niceTable <- function (dataframe, special = FALSE) {
  if(!require(flextable)){install.packages("flextable") + library(flextable)}
  if(!require(dplyr)){install.packages("dplyr") + library(dplyr)}
  dataframe %>%
    flextable %>%
    theme_vanilla %>%
    fontsize(part = "all", size = 12) %>%
    font(part = "all", fontname = "Times New Roman") %>%
    align(align = "center", part = "all") %>%
    set_table_properties(layout = "autofit") -> table
  if(special == TRUE) {
    table %>%
    italic(j = 3:6, part = "header") %>%
    compose(i = 1, j = 2, part = "header",
            value = as_paragraph("β")) %>%
    compose(i = 1, j = 6, part = "header",
            value = as_paragraph("sr", as_sup("2"))) %>% 
    compose(i = 1, j = 5, part = "header",
            value = as_paragraph("η", as_sub("p"), as_sup("2")))
    }
}
