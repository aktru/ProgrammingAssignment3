rankall <- function(outcome, num = "best") {
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
  data3 <- lapply(data3, function(df) df[order(df[,3], df[,1]),])
  ##print(length(data3))
  columns2 <- c(c1,c2)
  qStates <- length(data3)
  ## checking max length of elements of list
  i1 <- 1
  for (i in 1:qStates) {
    data4 <- as.data.frame(data3[i])
    i2 <- nrow(data4)
    if (i2 > i1) {i1 <- i2}
  }
  if (num == "best") {
    hosp <- 1
    } else if (num == "worst") {
      hosp <- i1
    } else {hosp <- num}
  df_all <- data3[[1]][hosp,]
  state_name_0 <- data3[[1]][1,2]
  if (is.na(data3[[1]][hosp,1])) {
    df_all[1,2] <- state_name_0
  }
  for (j in 2:qStates) {
    state_name <- data3[[j]][1,2]
    if (num == "worst") {
      data5 <- as.data.frame(data3[j])
      hosp <- nrow(data5)
    } 
    if (is.na(data3[[j]][hosp,1])) {
      tempDataFrame <- c(data3[[j]][hosp,1],state_name)
    } else {
      tempDataFrame <- c(data3[[j]][hosp,1],data3[[j]][hosp,2])
    }
    df_all <- rbind(df_all,tempDataFrame)
  }
  df_all <- subset(df_all, select = columns2)
  new_row_names <- df_all$State
  rownames(df_all) <- new_row_names
  colnames(df_all)[1] <- "hospital"
  colnames(df_all)[2] <- "state"
  ##data4 <- data3[eval(state)]
  ##print(data4)
  ##hosp <- data4[[1]][1,]
  ##if (is.null(hosp)) {stop("invalid state")}
  ##data4 <- as.data.frame(data4)
  ##data4 <- data4[order(data4[,3], data4[,1]),]
  ##if (num == "best") {
  ##  hosp <- data4[1,1]
  ##} else if (num == "worst") {
  ##  hosp <- data4[nrow(data4),1]
  ##} else {
  ##  hosp <- data4[eval(num),1]
  ##}
  
  ##print(data4)
  return(df_all)
  ##return(data3)
}