
importSA2byState <- function(filePath, state) {
  data <- read.csv(file=filePath, skip = 9, header = TRUE)
  data$X <- NULL
  names(data) <- c("SA2", "Non-Indigenous", "Aboriginal", "Torres Strait Islander", 
                   "Both Aboriginal and Torres Strait Islander", "Not stated", "Total")
  data$`Non-Indigenous` <- as.integer(data$`Non-Indigenous`)
  data$State <- state
  nrows <- nrow(data)
  if (grepl("2011", filePath)) {
    data <- data[-c(1, nrows-3, nrows-2, nrows-1, nrows), ]
  }
  else if (grepl("2016", filePath)) {
    data <- data[-c(1, nrows-2, nrows-1, nrows), ]
  }
  return(data)
}

# Import: Indigenous Status by SA2 Level for each state
importSA2Data <- function(census_year = "2011") {
  
  act <- importSA2byState(paste(census_year, "-Census-Analysis/data/Census ", census_year, " - Indigenous Status by SA2 Level - ACT.csv", sep=""), "ACT")
  nsw <- importSA2byState(paste(census_year, "-Census-Analysis/data/Census ", census_year, " - Indigenous Status by SA2 Level - NSW.csv", sep=""), "NSW")
  nt <- importSA2byState(paste(census_year, "-Census-Analysis/data/Census ", census_year, " - Indigenous Status by SA2 Level - NT.csv", sep=""), "NT")
  other <- importSA2byState(paste(census_year, "-Census-Analysis/data/Census ", census_year, " - Indigenous Status by SA2 Level - Other.csv", sep=""), "OTHER")
  qld <- importSA2byState(paste(census_year, "-Census-Analysis/data/Census ", census_year, " - Indigenous Status by SA2 Level - QLD.csv", sep=""), "QLD")
  sa <- importSA2byState(paste(census_year, "-Census-Analysis/data/Census ", census_year, " - Indigenous Status by SA2 Level - SA.csv", sep=""), "SA")
  tas <- importSA2byState(paste(census_year, "-Census-Analysis/data/Census ", census_year, " - Indigenous Status by SA2 Level - TAS.csv", sep=""), "TAS")
  vic <- importSA2byState(paste(census_year, "-Census-Analysis/data/Census ", census_year, " - Indigenous Status by SA2 Level - VIC.csv", sep=""), "VIC")
  wa <- importSA2byState(paste(census_year, "-Census-Analysis/data/Census ", census_year, " - Indigenous Status by SA2 Level - WA.csv", sep=""), "WA")
  
  sa2Data <- rbind(act, nsw, nt, other, qld, sa, tas, vic, wa)
  sa2Data$State <- as.factor(sa2Data$State)
  rm(act, nsw, nt, other, qld, sa, tas, vic, wa)
  return(sa2Data)
}

# Import Census data
importCensusData <- function(census_year = "2011", 
                             importFilePath = "2011-Census-Analysis/data/2011_Census.csv", 
                             exportFilePath = "D:/2011_Census.csv") {
  
  if (file.exists(importFilePath)) {
    
    census <- read.csv(file=importFilePath, header=TRUE)
    rm(importFilePath)
    
  } else 
  {
    sa2Data <- importSA2Data(census_year)
    
    data <- read.csv(file=paste(census_year, "-Census-Analysis/data/Census ", census_year, " - SA2 by IndigenousStatus Sex Age10 WeeklyIncome.csv", sep=""), 
                     skip = 12, header = TRUE)
    nrows <- nrow(data)
    
    if (grepl("2011", census_year)) {
      data <- data[-c(1, nrows-3, nrows-2, nrows-1, nrows), ] # Remove first row and last 4 rows of dataset
    }
    else if (grepl("2016", census_year)) {
      data <- data[-c(1, nrows-2, nrows-1, nrows), ] # Remove first row and last 3 rows of dataset
    }
    #data <- data[-c(1, nrows-3, nrows-2, nrows-1, nrows), ] 
    data$Negative.income <- as.integer(as.character(data$Negative.income))
    
    names(data)[1] <- "SA2"
    data <- merge(sa2Data[c("SA2", "State")], data, by="SA2") # Add state SA2 region belongs to
    head(data[c("SA2", "State")])
    
    # Get rid of columns for "Total" status as this can be calculated from other data (
    # last 308 columns for 2011 Census data and last 374 for 2016 Census data
    ncols <- ncol(data)
    if (grepl("2011", census_year)) {
      data <- data[, -c((ncols-307):ncols)] 
    }
    else if (grepl("2016", census_year)) {
      data <- data[, -c((ncols-373):ncols)]
    }
    #data <- data[, -c((ncols-307):ncols)] 
    #data <- data[, -c((ncols-373):ncols)] # 374 vs 308
    
    length(which(data == 0)) # number of zero counts in data
    
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
    
    income_options <- c("Negative income", "Nil income", "$1-$199", "$200-$299", "$300-$399", "$400-$599", 
                        "$600-$799", "$800-$999", "$1,000-$1,249", "$1,250-$1,499", "$1,500-$1,999", 
                        "$2,000 or more", "Not stated", "Not applicable")
    if (grepl("2016", census_year)) {
      income_options <- c("Negative income", "Nil income", "$1-$149", "$150-$299", "$300-$399", "$400-$499", 
                          "$500-$649", "$650-$799", "$800-$999", "$1,000-$1,249", "$1,250-$1,499", "$1,500-$1,749", 
                          "$1,750-$1,999", "$2,000-$2,999", "$3,000 or more", "Not stated", "Not applicable")
    }
    
    row <- 1
    col <- 3
    start_time <- Sys.time()
    while (row <= nrows) 
    {
      for (status in c("Non-Indigenous", "Aboriginal", "Torres Strait Islander", 
                       "Both Aboriginal and Torres Strait Islander", "Not stated"))
      {
        for (gender in c("Male", "Female")) 
        {
          for (age in c("0-9 years", "10-19 years", "20-29 years", "30-39 years", "40-49 years", "50-59 years", 
                        "60-69 years", "70-79 years", "80-89 years", "90-99 years", "100 years and over"))
          {
            for (weekly_income in income_options)
            {
              count <- data[row, col]
              if (count == 0) 
              {
                # Skipping counts of 0 speeds up the processing considerably
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
    rm(income_options, data)
    
    # Add new column that codes everyone as Non-Indigenous or Indigenous
    census$Indigenous <- 1
    census$Indigenous[census$`Indigenous Status`=='Non-Indigenous'] <- 0
    census$Indigenous[census$`Indigenous Status`=='Not stated'] <- 0
    
    # Import: SA2 Level by Remoteness Areas. This is used to determine whether a SA2 area is considered rural or not.
    remoteness <- read.csv(file=paste(census_year, "-Census-Analysis/data/Census ", census_year, " - SA2 by Remoteness Area.csv", sep=""), skip=9, header=TRUE)
    remoteness$Total <- remoteness$X <- NULL # Remove unrequired variables
    nrows <- nrow(remoteness)
    
    if (grepl("2011", census_year)) {
      remoteness <- remoteness[-c(1, nrows-3, nrows-2, nrows-1, nrows), ] # Remove unrequired rows
    }
    else if (grepl("2016", census_year)) {
      remoteness <- remoteness[-c(1, nrows-2, nrows-1, nrows), ] # Remove unrequired rows
    }
    
    colnames(remoteness)[1] <- "SA2"
    remoteness$Major.Cities.of.Australia..NSW. <- as.integer(as.character(remoteness$Major.Cities.of.Australia..NSW.))
    remoteness$SA2 <- as.character(remoteness$SA2)
    
    remoteness <- merge(sa2Data[c("SA2", "State")], remoteness, by="SA2") # Add state SA2 region belongs to
    
    # Create dummy categories
    remoteness$nonzeros <- simplify2array(
      apply(
        remoteness[-1:-2], 1, 
        function(x) paste(names(remoteness[-1:-2])[x != 0], collapse = " ")
      )
    )
    
    remoteness$City <- 0
    remoteness$InnerRegional <- 0
    remoteness$OuterRegional <- 0
    remoteness$Remote <- 0
    remoteness$VeryRemote <- 0
    remoteness$MigratoryOffshoreShipping <- 0
    remoteness$NoUsualAddress <- 0
    remoteness$City[grepl("Cities", remoteness$nonzeros)] <- 1
    remoteness$InnerRegional[grepl("Inner.Regional", remoteness$nonzeros)] <- 1
    remoteness$OuterRegional[grepl("Outer.Regional", remoteness$nonzeros)] <- 1
    remoteness$Remote[grepl("Remote.Australia", remoteness$nonzeros)] <- 1
    remoteness$VeryRemote[grepl("Very.Remote", remoteness$nonzeros)] <- 1
    remoteness$MigratoryOffshoreShipping[grepl("Migratory", remoteness$nonzeros)] <- 1
    remoteness$NoUsualAddress[grepl("No.usual.address", remoteness$nonzeros)] <- 1
    
    remoteness$nonzeros <- NULL
    remoteness <- remoteness[c(1:2, 56:62)]
    
    # Merge Remoteness dummy variables
    census <- merge(remoteness, census, by=c("SA2", "State")) 
    rm(remoteness)
    
    # 172 obs took 1.820187 hours to produce 264,880 x 7 data frame. Only takes ~2.5 minutes if skip 0 values
    # 6.666625 hours to run for 2011 Census data
    write.table(census, exportFilePath, sep=",", col.names = TRUE, row.names = FALSE)
    
  }
  
  return (census) 
}

