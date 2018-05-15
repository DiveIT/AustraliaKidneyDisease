library(psych)
library(plyr)
library(data.table)

# Set working directory to current folder script is running from
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  #print( getwd() )
}

set_wd() # set working directory to folder running script from

source("functions.R")

# Using table: 2011 Census - Employment, Income and Unpaid Work
#   Columns: Indigenous Status, Sex, Age in 10 year blocks
#   TODO: Total Family Income, Education/Qualification?, Employment?

# Indigenous Status by SA2 Level (2011 Census)
  #sa2Data <- read.csv(file="data/Census 2011 - Indigenous Status by SA2 Level.csv", skip = 9, header = TRUE)
  #sa2Data$X <- NULLW
  #names(sa2Data) <- c("SA2", "Non-Indigenous", "Aboriginal", "Torres Strait Islander", 
  #                 "Both Aboriginal and Torres Strait Islander", "Not stated", "Total")
  #sa2Data$`Non-Indigenous` <- as.integer(sa2Data$`Non-Indigenous`)
  #sa2Data <- sa2Data[-c(1, 2216, 2217, 2218, 2219), ]

# Import: Indigenous Status by SA2 Level for each state (2011 Census)

  act <- importSA2Data("data/Census 2011 - Indigenous Status by SA2 Level - ACT.csv", "ACT")
  nsw <- importSA2Data("data/Census 2011 - Indigenous Status by SA2 Level - NSW.csv", "NSW")
  nt <- importSA2Data("data/Census 2011 - Indigenous Status by SA2 Level - NT.csv", "NT")
  other <- importSA2Data("data/Census 2011 - Indigenous Status by SA2 Level - Other.csv", "OTHER")
  qld <- importSA2Data("data/Census 2011 - Indigenous Status by SA2 Level - QLD.csv", "QLD")
  sa <- importSA2Data("data/Census 2011 - Indigenous Status by SA2 Level - SA.csv", "SA")
  tas <- importSA2Data("data/Census 2011 - Indigenous Status by SA2 Level - TAS.csv", "TAS")
  vic <- importSA2Data("data/Census 2011 - Indigenous Status by SA2 Level - VIC.csv", "VIC")
  wa <- importSA2Data("data/Census 2011 - Indigenous Status by SA2 Level - WA.csv", "WA")
  
  sa2Data <- rbind(act, nsw, nt, other, qld, sa, tas, vic, wa)
  sa2Data$State <- as.factor(sa2Data$State)
  rm(act, nsw, nt, other, qld, sa, tas, vic, wa)

  
# Import: SA2 Level by Indigenous Status, Sex, Age in 10 year blocks, and Weekly Personal Income (2011 Census)  
  
  filePath = "data/2011_Census.csv"
  statusList <- c("Non-Indigenous", "Aboriginal", "Torres Strait Islander", 
                  "Both Aboriginal and Torres Strait Islander", "Not stated")
  
  if (file.exists(filePath)) {
    
    census <- read.csv(file=filePath, header=TRUE)
    # TODO: Add new column that codes everyone as Non-Indigenous; Indigenous; or Not Stated
    census$Status <- 
    rm(filePath)
    
  } else 
  {
    
    data <- read.csv(file="data/Census 2011 - SA2 by IndigenousStatus Sex Age10 WeeklyIncome.csv", 
                     skip = 12, header = TRUE)
    nrows <- nrow(data)
    data <- data[-c(1, nrows-3, nrows-2, nrows-1, nrows), ] # Remove first row and last 4 rows of dataset
    data$Negative.income <- as.integer(as.character(data$Negative.income))
    
    names(data)[1] <- "SA2"
    data <- merge(sa2Data[c("SA2", "State")], data, by="SA2") # Add state SA2 region belongs to
    head(data[c("SA2", "State")])
    
    # Get rid of columns for "Total" status as this can be calculated from other data (last 308 columns)
    ncols <- ncol(data)
    data <- data[, -c((ncols-307):ncols)]
    
    # Rename Columns headings
    # New Columns:  SA2  
    #               State
    #               Indigenous Status 
    #               Sex 
    #               Age
    #               Weekly Income
    
    nrows <- nrow(data)
    census <- data.frame(matrix(ncol = 7, nrow = 0))
    column_names <- c("SA2", "State", "Indigenous Status", "Sex", "Age", "Weekly Personal Income", "Count")
    colnames(census) <- column_names
    census$`Indigenous Status` <- census$Sex <- census$Age <- census$`Weekly Personal Income` <- factor()
    census$SA2 <- factor(levels = unique(data$SA2))
    census$State <- factor(levels = unique(data$State))
    census$Count <- integer()
    
    row <- 1
    col <- 3
    start_time <- Sys.time()
    while (row <= nrows) 
    {
      for (status in statusList)
      {
        for (gender in c("Male", "Female")) 
        {
          for (age in c("0-9 years", "10-19 years", "20-29 years", "30-39 years", "40-49 years", "50-59 years", 
                        "60-69 years", "70-79 years", "80-89 years", "90-99 years", "100 years and over"))
          {
            for (weekly_income in c("Negative income", "Nil income", "$1-$199", "$200-$299", "$300-$399", "$400-$599", 
                                    "$600-$799", "$800-$999", "$1,000-$1,249", "$1,250-$1,499", "$1,500-$1,999", 
                                    "$2,000 or more", "Not stated", "Not applicable"))
            {
              count <- data[row, col]
              if (count == 0) 
              {
                col <- col+1
                next
              }
              new_row <- c(as.character(data[row, "SA2"]), as.character(data[row, "State"]), status, gender, 
                           age, weekly_income, count)
              new_row <- data.frame(t(new_row))
              colnames(new_row) <- column_names
              #census <- rbind(census, new_row) # 1.846581 min 
              census <- rbindlist(list(census, new_row)) #34.37751 sec
              col <- col+1
            }
          }
        }
      }
      row <- row+1
      col <- 3
    }
    end_time <- Sys.time()
    end_time - start_time
    
    census$Count <- as.integer(as.character(census$Count))
    rm(nrows, ncols, age, gender, status, weekly_income, column_names, row, col, new_row, count, start_time, end_time)
    
    # 172 obs took 1.820187 hours to produce 264,880 x 7 data frame. Only takes ~2.5 minutes if skip 0 values
    # 6.666625 hours to run
    write.table(census, "D:/2011_Census.csv", sep=",", col.names = TRUE, row.names = FALSE)
    
  }
  
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
  
  # Number of people counted in each state during 2011  Census
  for (state in unique(census$State))
  {
    print(paste(state, "-", sum(census[which(census$State == state),]$Count)))
  }
  
  # Number of people by indigenous status
  for (status in unique(census$Indigenous.Status))
  {
    print(paste(status, "-", sum(census[which(census$Indigenous.Status == status),]$Count)))
  }
  
  # Number of people per state by indigenous status
  for (status in unique(census$Indigenous.Status))
  {
    for (state in unique(census$State))
    {
      print(paste(status, "-", state, "-", 
                  sum(census[which(census$Indigenous.Status == status & census$State == state),]$Count)))
    }
  }
  
  # TODO: Sex
  for (sex in unique(census$Sex))
  {
    print(paste(sex, "-", sum(census[which(census$Sex == sex),]$Count)))
  }
  
  # TODO: Age
  for (age in unique(census$Age))
  {
    print(paste(age, "-", sum(census[which(census$Age == age),]$Count)))
  }
  
  # TODO: Weekly Income
  for (income in unique(census$Weekly.Personal.Income))
  {
    print(paste(income, "-", sum(census[which(census$Weekly.Personal.Income == income),]$Count)))
  }

  
  rm(state, status, age, income, sex)
  
# TODO: Plots
  
  plot(sa2Data$State, main="Number of SA2 Levels per State", ylab="SA2 Levels")

# TODO: Correlation
# https://stats.stackexchange.com/questions/108007/correlations-with-unordered-categorical-variables
  
  
