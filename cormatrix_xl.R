cormatrix_xl <- function(data, filename = "mycormatrix", overwrite = TRUE) {
  if(!require(openxlsx)){install.packages("openxlsx")}
  library(openxlsx)
  
  my.cox.matrix <- cor(data, use = "na.or.complete")
  my.cox.matrix <- as.data.frame(my.cox.matrix)
  
  print(round(my.cox.matrix,2))
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  writeData(wb, sheet = 1, my.cox.matrix, rowNames = TRUE)
  
  my.style <- createStyle(numFmt = "0.00")
  addStyle(wb, 
           sheet = 1, 
           style = my.style, 
           rows = 1:ncol(my.cox.matrix), 
           cols = 1:ncol(my.cox.matrix), 
           gridExpand = TRUE)
  
  diagonalStyle <- createStyle(bgFill = "gray")
  mediumStyle <- createStyle(bgFill = "#FBCAC0") # blue
  largeStyle <- createStyle(bgFill = "#F65534") # red
  
  all.columns <- 1:(ncol(my.cox.matrix)+1)
  
  conditionalFormatting(wb, 
                        "Sheet 1",
                        cols = all.columns,
                        rows = all.columns, 
                        rule = "==1", 
                        style = diagonalStyle
  )
  conditionalFormatting(wb, 
                        "Sheet 1",
                        cols = all.columns,
                        rows = all.columns, 
                        rule = c(0.3, 0.59999999), 
                        style = mediumStyle,
                        type = "between"
  )
  conditionalFormatting(wb, 
                        "Sheet 1",
                        cols = all.columns,
                        rows = all.columns, 
                        rule = c(-0.3, -0.59999999), 
                        style = mediumStyle,
                        type = "between"
  )
  conditionalFormatting(wb, 
                        "Sheet 1",
                        cols = all.columns,
                        rows = all.columns, 
                        rule = c(0.6, 0.9999999), 
                        style = largeStyle,
                        type = "between"
  )
  conditionalFormatting(wb, 
                        "Sheet 1",
                        cols = all.columns,
                        rows = all.columns, 
                        rule = c(-0.6, -0.9999999), 
                        style = largeStyle,
                        type = "between"
  )
  freezePane(wb, "Sheet 1", firstCol = TRUE, firstRow = TRUE)
  
  saveWorkbook(wb, 
               file = paste(filename, ".xlsx"),
               overwrite = overwrite)
  cat(paste0("\n [Correlation matrix '", filename, ".xlsx' has been saved to working directory.]"))
}