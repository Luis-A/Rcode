best <- function(state, outcome) {
  
  # reading outcome data
  data <- read.csv("outcome-of-care-measures.csv", 
                   colClasses ="character")
  
  # checking state is correct
  if (!state %in% data[,7]) 
     stop('Error in best("',state,'", ', '"',outcome,'") : invalid state\n')

  # checking outcome is correct  
  if (outcome == "heart attack") col <- 11
  else if (outcome =="heart failure") col <- 17
  else if (outcome == "pneumonia") col <- 23
  else 
    stop('Error in best("',state,'", ', '"',outcome,'") : invalid outcome\n')
  
  # draw the hospital data of hospitals from param state
  sdata <- data[data[,7]==state,]
  sdata[,col] <- as.numeric(sdata[,col])
  
  sort(sdata[which( sdata[,col] %in% min(sdata[,col], na.rm=TRUE)),2])[1]
  
}