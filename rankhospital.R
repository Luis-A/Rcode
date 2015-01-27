rankhospital <- function(state, outcome, num = "best") {
  
  # reading outcome data
  data <- read.csv("outcome-of-care-measures.csv", 
                   colClasses ="character")
  
  # checking state is correct
  if (!state %in% data[,7]) 
    stop('Error in rankhospital("' , state, '", "',outcome,'", "',num,'") : invalid state')
  
  # checking outcome is correct  
  if (outcome == "heart attack") col <- 11
  else if (outcome =="heart failure") col <- 17
  else if (outcome == "pneumonia") col <- 23
  else 
    stop('Error in rankhospital("' , state, '", "',outcome,'", "',num,'") : invalid outcome')
  
  # draw the hospital data of hospitals from param state
  sdata <- data[data[,7]==state,]
  sdata[,col] <- as.numeric(sdata[,col])
  sinNA <- sdata[!is.na(sdata[,col]),]
  
  if (num == "best") pos <- 1
  else if (num == "worst") pos <- nrow(sinNA)
  else if (is.numeric(num) & num <= nrow(sinNA) & num > 0) pos <- num
  else  
    return(NA)
  
  # everything is going to work fine
  # return the element [pos,2] after ordering by column col and
  #   breaking the ties by column 2
  
  sinNA[order(sinNA[,col], sinNA[,2]),][pos,2]
  
}