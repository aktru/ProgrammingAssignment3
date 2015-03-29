rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character", na.strings = "Not Available")
  c1 <- colnames(data[2])
  c2 <- colnames(data[7]) 
  cf <- 0
  if (outcome == "heart attack") {
    fColName <- "Attack"
    cf <- colnames(data[11])
  }
  if (outcome == "heart failure") {
    fColName <- "Failure"
    cf <- colnames(data[17])
  }
  if (outcome == "pneumonia") {
    fColName <- "Pneumonia"
    cf <- colnames(data[23])
  }
  if (cf == 0) {
    stop("invalid outcome")
  }
  # filter data containing 3 columns:
  # - hospital name,
  # - death rate based on outcome, and
  # - state (useful for rankall function)
  columns <- c(c1,c2,cf)
  # re-assign data frame with only 3 columns
  data2 <- subset(data, select = columns)
  colnames(data2)[3] <- fColName
  # remove rows with NAs in column 2
  data2 <- data2[!is.na(data2[3]),]
  # convert death rate (column 2) to numeric
  data2[, 3] <- sapply(data2[, 3], as.numeric)
  data3 <- split(data2, data2$State)
  data3 <- lapply(data3, function(df) df[order(df[3]), ])
  ##print(length(data3))
  data4 <- data3[eval(state)]
  ##print(data4)
  hosp <- data4[[1]][1,1]
  if (is.null(hosp)) {stop("invalid state")}
  data4 <- as.data.frame(data4)
  data4 <- data4[order(data4[,3], data4[,1]),]
  if (num == "best") {
    hosp <- data4[1,1]
  } else if (num == "worst") {
    hosp <- data4[nrow(data4),1]
  } else {
    hosp <- data4[eval(num),1]
  }
  
  ##print(data4)
  return(hosp)
}