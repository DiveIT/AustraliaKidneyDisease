###########################################################################

# sae_models.R
# Examples of Small Area Approximation using different packages

###########################################################################


# Setup ----
library(plyr)
library(data.table)# Set working directory to current folder script is running from
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path))
  #print( getwd() )
}

set_wd()
source("functions.R")

# Load data ----
load("./2011-Census-Analysis/censusIndigenous2011.RData")
load("./2016-Census-Analysis/censusIndigenous2016.RData")
load("./Survey-Data/SurveyData.RData")


# Models ----

# Survey data is aggregated, and smaller areas have large errors.
# Indirect domain estimation ----
# Compute post-stratified synthetic estimators of kidney disease
# for SA2 areas considering the income levels as post-strata.

# Survey dataset contains strata and value to estimate for population
survey_incm <- survey_incm %>% 
  mutate(strata=0)
survey_incm$strata[survey_incm$incm =="$1-$299"] <- 1
survey_incm$strata[survey_incm$incm =="$300-$399"] <- 2
survey_incm$strata[survey_incm$incm =="$400-$799"] <- 3
survey_incm$strata[survey_incm$incm =="$800-$999"] <- 4
survey_incm$strata[survey_incm$incm =="$1,000-$1,249"] <- 5
survey_incm$strata[survey_incm$incm =="$1,250-$1,499"] <- 6
survey_incm$strata[survey_incm$incm =="$1,500-$1,999"] <- 7
survey_incm$strata[survey_incm$incm =="$2,000 or more"] <- 8

#Census data
census <- censusIndigenous2011 %>% 
  dplyr::select(State,Income,Count) %>% 
  group_by(State,Income) %>% 
  summarise(count=sum(Count)) %>% 
  filter(State %in% c("ACT","NSW","QLD","NT","VIC","SA","WA","TAS")) %>% 
  spread(Income,count) %>% 
  mutate(incm=NA)

census <- census %>% dplyr::select(c(1:9))
colnames(census) <- c("State", "1","2","3","4","5","6","7","8")

# Compute post-stratified synthetic estimators with state
# as domain codes
result1 <- pssynt(y=ppt, sweight=Total, ps=strata,
                  domsizebyps=census, data=survey_incm)

#Compare against original values:
survey_avg <- survey_incm %>% dplyr::select(State=State.or.Territory,Kidney.Disease,Total) %>% 
  group_by(State) %>% 
  summarise(ppt=sum(Kidney.Disease)/sum(Total))

result1
survey_avg

# TODO: Model-based MSE estimates ----
