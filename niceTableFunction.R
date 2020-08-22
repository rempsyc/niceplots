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
  if("p" %in% names(dataframe)) {
    format.p <- function(p, precision = 0.001) {
      digits <- -log(precision, base = 10)
      p <- formatC(p, format = 'f', digits = digits)
      p[p == formatC(0, format = 'f', digits = digits)] <- paste0('< ', precision)
      sub("0", "", p)
    }
    table %>%
      italic(j = "p", part = "header") %>%
      set_formatter(p = function(x)
        format.p(x)) -> table
  }  
  if("t" %in% names(dataframe)) {
    table %>%
      italic(j = "t", part = "header") -> table
  }
  if("SE" %in% names(dataframe)) {
    table %>%
      italic(j = "SE", part = "header") -> table
  }
  if("SD" %in% names(dataframe)) {
    table %>%
      italic(j = "SD", part = "header") -> table
  }
  if("F" %in% names(dataframe)) {
    table %>%
      italic(j = "F", part = "header") -> table
  }
  if("df" %in% names(dataframe)) {
    table %>%
      italic(j = "df", part = "header") -> table
  }
  if("r" %in% names(dataframe)) {
    table %>%
      italic(j = "r", part = "header") -> table
  }
  if("b" %in% names(dataframe)) {
    table %>%
      italic(j = "b", part = "header") -> table
  }
  if("M" %in% names(dataframe)) {
    table %>%
      italic(j = "M", part = "header") -> table
  }
  if("B" %in% names(dataframe)) {
    table %>%
      compose(i = 1, j = "B", part = "header",
              value = as_paragraph("β")) -> table
  }
  if("R2" %in% names(dataframe)) {
    table %>%
      compose(i = 1, j = "R2", part = "header",
              value = as_paragraph("R", as_sup("2"))) %>%
      italic(j = "R2", part = "header") -> table
  }
  if("sr2" %in% names(dataframe)) {
    table %>%
      compose(i = 1, j = "sr2", part = "header",
              value = as_paragraph("sr", as_sup("2"))) %>%
      italic(j = "sr2", part = "header") -> table
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
