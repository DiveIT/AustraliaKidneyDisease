
importSA2Data <- function(filePath, state) {
  data <- read.csv(file=filePath, skip = 9, header = TRUE)
  data$X <- NULL
  names(data) <- c("SA2", "Non-Indigenous", "Aboriginal", "Torres Strait Islander", 
                   "Both Aboriginal and Torres Strait Islander", "Not stated", "Total")
  data$`Non-Indigenous` <- as.integer(data$`Non-Indigenous`)
  data$State <- state
  nrows <- nrow(data)
  data <- data[-c(1, nrows-3, nrows-2, nrows-1, nrows), ]
  return(data)
}