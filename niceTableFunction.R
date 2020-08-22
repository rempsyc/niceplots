niceTable <- function (dataframe, italics = NULL, special = FALSE, highlight = FALSE) {
  if(!require(flextable)){install.packages("flextable") + library(flextable)}
  if(!require(dplyr)){install.packages("dplyr") + library(dplyr)}
  dataframe %>%
    flextable %>%
    theme_vanilla %>%
    fontsize(part = "all", size = 12) %>%
    font(part = "all", fontname = "Times New Roman") %>%
    align(align = "center", part = "all") %>%
    set_table_properties(layout = "autofit") -> table
  if(!missing(italics)) {
    table %>%
    italic(j = italics, part = "header") -> table
  }
  if("B" %in% names(dataframe)) {
    table %>%
    compose(i = 1, j = "B", part = "header",
            value = as_paragraph("β")) -> table
    }
  if("sr2" %in% names(dataframe)) {
    table %>%
    compose(i = 1, j = "sr2", part = "header",
            value = as_paragraph("sr", as_sup("2"))) -> table
    }
  if("np2" %in% names(dataframe)) {
    table %>%
    compose(i = 1, j = "np2", part = "header",
            value = as_paragraph("η", as_sub("p"), as_sup("2"))) -> table
    }
  if(highlight == TRUE) {
    table %>%
    bold(i = ~ signif == TRUE,
         j = ~ Model + B + t + p + np2 + sr2 + signif) %>%
    bg(i = ~ signif == TRUE,
       j = ~ Model + B + t + p + np2 + sr2 + signif,
       bg = "#CFCAC2") -> table
    }
  table
}
