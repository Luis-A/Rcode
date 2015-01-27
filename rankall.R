rankall <- function(outcome, num = "best") {
 
  # checking outcome is correct  
  if (outcome == "heart attack") col <- 11
  else if (outcome =="heart failure") col <- 17
  else if (outcome == "pneumonia") col <- 23
  else 
    stop('Error in rankhospital("' , state, '", "',
         outcome,'", "',num,'") : invalid outcome')
  
  # reading outcome data
  data <- read.csv("outcome-of-care-measures.csv", 
                   colClasses ="character")
  
  # convert the column col to numeric
  data[,col] <- as.numeric(data[,col])  
  
  # drawn just those rows without NA in the column col
  sinNA <- data[!is.na(data[,col]),]
  
  # First split sinNA by state, this return a list object
  df <- split(sinNA, sinNA[,7])

  # Go through each state. In each state add the row pos
  #   to the previously data.frame for the response resp
  resp <- data.frame(hospital = character(0), 
                     state = character(0), 
                     stringsAsFactors = FALSE)
  for(state in df) { 
    # Compute column to sort on
    if (num == "best") pos <- 1
    else if (num == "worst") pos <- nrow(state)
         else if (is.numeric(num) & num > 0) pos <- num
              else return(NA)  # parameter num incorrect
    if (nrow(state)>= pos){
      # compute columns 2 and 7 in row pos after ordering
      sortState <- state[order(state[,col], state[,2]),][pos, c(2,7)]
      names(sortState) <- c("hospital", "state")
      resp <- rbind(resp, sortState)
    }
    else # There is no pos row in this state
      resp <- rbind(resp, data.frame(hospital=NA,state=state[1,7]))
  }
  resp
}