randomize <- function (design="between",Ncondition=3,n=9,condition.names=c("a","b","c"),col.names=c("id", "Condition")) {
  cat("Important: This function is now deprecated. A better version has migrated to the rempsyc package. \n Please install and use the rempsyc package here: https://github.com/rempsyc/rempsyc/ \n")
  Condition <- data.frame() # to precreate dataframe  
  if(!require(plyr)){install.packages("plyr")}
  library(plyr)
  if (design=="between") {
    if (!n%%Ncondition==0) {cat("Warning(!): sample size needs to be a multiple of your number of groups if using 'between'!")}
    for (i in 1:(n/Ncondition)){ # Repeat this for number of participants divided by Ncondition (number of complete combinations)
      x <- sample(1:Ncondition, replace=F) # (Choose a number between 1 and Ncondition; repeat this Ncondition times with no replacement)
      Condition <- rbind(Condition, t(t(x)), # Add new stats to dataframe
                         stringsAsFactors = FALSE)
    }
    }
  if (design=="within") {
    Condition <- Condition
    for (i in 1:n){ # Generate the random values for n participants and Nconditions
      x <- sample(1:Ncondition, replace=F) # Choose a number between 1 and Nconditions; repeat this Nconditions times with no replacement
      Condition <- rbind(Condition, x, # Adds new conditions to dataframe row by row
                         stringsAsFactors = FALSE) # Not as factors as this can create problems
    }
    }
  Condition <- mapvalues(as.matrix(Condition), 1:Ncondition, condition.names) # Converts numbers to condition labels
  for (i in 1:n) {
    Condition[i,1] <- paste(Condition[i,], collapse=" - ") # Adds hyphen between conditions for easier read
  }
  if(ncol(Condition) > 2) { Condition[,2:ncol(Condition)] <- NA}
  Condition <- as.data.frame(Condition)
  id <- t(t(1:n))
  final_table <- data.frame(id, Condition, matrix(NA, ncol = length(col.names)))[,1:length(col.names)]
  names(final_table) <- col.names
  final_table
}
