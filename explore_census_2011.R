library(psych)
library(plyr)
library(data.table)

# Set working directory to current folder script is running from
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
}
set_wd() # set working directory to folder running script from

source("functions.R")

# Using table: 2011 Census - Employment, Income and Unpaid Work
# Columns: Indigenous Status, Sex, Age in 10 year blocks, Personal Weekly Income
# Import: Indigenous Status by SA2 Level for each state (2011 Census)

  sa2Data <- importSA2Data("2011")

# Import: SA2 Level by Indigenous Status, Sex, Age in 10 year blocks, and Weekly Personal Income (2011 Census)  
  
  census <- importCensusData("2011", "2011-Census-Analysis/data/2011_Census.csv", "D:/2011_Census.csv")

# Summary Statistics - SA2Data
  
  sum(!complete.cases(sa2Data)) # number of rows of data that have missing values 
  
  dim(sa2Data) # data frame dimensions
  str(sa2Data) # structure of data
  head(sa2Data, n=5) 
  summary(sa2Data)
  describe(sa2Data[c(2:7)], IQR=TRUE)
  
  count(sa2Data, 'State')
  sum(count(sa2Data, 'State')$freq) # Number of SA2 areas
  
# Summary Statistics - 2011 Census Data
  
  sum(!complete.cases(census)) # number of rows of data that have missing values 
  
  dim(census) # data frame dimensions
  str(census) # structure of data
  head(census, n=5) 
  summary(census)   # When transforming the data counts of zero were excluded
  
  describe(census$Count, IQR=TRUE) 
  
  sum(census$Count) # Number of people counted in 2011 Census
  length(unique(census$SA2)) # Number of SA2 areas with counts > 0
  length(unique(sa2Data$SA2)) - length(unique(census$SA2)) # Number of SA2 areas with no population recorded
  
  # Number of people counted in each state during 2011  Census
  total <- sum(census$Count)
  for (state in unique(census$State))
  {
    count <- sum(census[which(census$State == state),]$Count)
    percentage <- (count / total) * 100
    print(paste(state, "-", count, " : ", percentage))
  }
  
  # Number of people by indigenous status
  for (status in unique(census$Indigenous.Status))
  {
    count <- sum(census[which(census$Indigenous.Status == status),]$Count)
    percentage <- (count / total) * 100
    print(paste(status, "-", count, " : ", percentage))
  }
  
  # Number of people who are indigenous or not
  for (indigenous in unique(census$Indigenous))
  {
    count <- sum(census[which(census$Indigenous == indigenous),]$Count)
    percentage <- (count / total) * 100
    print(paste(indigenous, "-", count, " : ", percentage))
  }
  
  # Number of people per state by indigenous status
  for (status in unique(census$Indigenous.Status))
  {
    for (state in unique(census$State))
    {
      stateTotal <- sum(census[which(census$State == state),]$Count)
      count <- sum(census[which(census$Indigenous.Status == status & census$State == state),]$Count)
      percentage <- (count / stateTotal) * 100
      print(paste(status, "-", state, "-", count, " : ", percentage))
    }
  }
  
  # Number of people by sex
  for (sex in unique(census$Sex))
  {
    count <- sum(census[which(census$Sex == sex),]$Count)
    percentage <- (count / total) * 100
    print(paste(sex, "-", count, " : ", percentage))
  }
  
  # Number of people by age range
  for (age in unique(census$Age))
  {
    count <- sum(census[which(census$Age == age),]$Count)
    percentage <- (count / total) * 100
    print(paste(age, "-", count, " : ", percentage))
  }
  
  # Number of people per Indigenous Status by age range
  for (status in unique(census$Indigenous.Status))
  {
    statusTotal <- sum(census[which(census$Indigenous.Status == status),]$Count)
    for (age in unique(census$Age))
    {
      count <- sum(census[which(census$Indigenous.Status == status & census$Age == age),]$Count)
      percentage <- (count / statusTotal) * 100
      print(paste(status, "-", age, "-", count, " : ", percentage))
    }
  }
  
  # Number of people by Weekly Personal Income
  for (income in unique(census$Weekly.Personal.Income))
  {
    count <- sum(census[which(census$Weekly.Personal.Income == income),]$Count)
    percentage <- (count / total) * 100
    print(paste(income, "-", count, " : ", percentage))
  }
  
  # Number of people per Indigenous Status by Weekly Personal Income
  for (status in unique(census$Indigenous.Status))
  {
    statusTotal <- sum(census[which(census$Indigenous.Status == status),]$Count)
    for (income in unique(census$Weekly.Personal.Income))
    {
      count <- sum(census[which(census$Indigenous.Status == status & census$Weekly.Personal.Income == income),]$Count)
      percentage <- (count / statusTotal) * 100
      print(paste(status, "-", income, "-", count, " : ", percentage))
    }
  }
 
  rm(state, status, age, income, sex, indigenous, count, percentage, stateTotal, statusTotal, total)

