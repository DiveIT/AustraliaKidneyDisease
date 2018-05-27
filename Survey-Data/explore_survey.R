###########################################################################

# explore_survey.R
# ETL of Australian Health Survey data has already been completed in TableBuilder

###########################################################################

# Setup
library(dplyr)
library(tidyr)
library(visdat) # make sure you have it installed
library(psych)
library(ggplot2)
# Set working directory to current folder script is running from
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  #print( getwd() )
}

set_wd()

# Import data ----
# Limited information is available. Data is sparse.
# Review each dataset for 
survey_geo <- read.csv("KidneySexStateRemote.csv")
survey_state <- read.csv("KidneySexState.csv")
survey_age <- read.csv("KidneySexAge.csv")
survey_incm <- read.csv("KidneySexIncome.csv")



# Transform data ----
survey_geo <- survey_geo %>% 
  mutate(Total = Not.reported+Kidney.Disease+No.Kidney.Disease,
         Geography = paste0(State.or.Territory,"-",ASGS.2011))

# Summarise data ----
# For all survey sets
dim(survey_ds)

vis_dat(survey_ds) #factor and numeric features
vis_miss(survey_ds) #no data missing

summary(survey_ds)

describe(survey_ds$Not.reported, IQR=TRUE) 
describe(survey_ds$Kidney.Disease, IQR=TRUE) 
describe(survey_ds$No.Kidney.Disease, IQR=TRUE) 

#investigate missing values by setting 0 to NA
survey_ds_miss <- survey_ds %>% 
  mutate(Not.reported = ifelse(Not.reported==0, NA, Not.reported),
         Kidney.Disease = ifelse(Kidney.Disease==0, NA, Kidney.Disease),
         No.Kidney.Disease = ifelse(No.Kidney.Disease==0, NA, No.Kidney.Disease))

vis_miss(survey_ds_miss) # 12% 0 values

# TODO: Plot values

# TODO: Select most appropriate data


