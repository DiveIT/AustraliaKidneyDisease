###########################################################################

# sae_hb_model.R
# Examples of Small Area Approximation using HB model

###########################################################################


# Setup ----
library(dplyr)
library(sae)
library(data.table)
library(sp)
library(rgdal)
library(broom)
library(leaflet)
library(diseasemapping)
library(geostatsp)
library(mapmisc)
# Set working directory to current folder script is running from
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path))
  #print( getwd() )
}

set_wd()
source("functions.R")

# Load data ----
load("./Processed-Data/CensusData.RData")
load("./Survey-Data/SurveyData.RData")

survey_geo <- read.csv("./Survey-Data/data/KidneySexRemoteness.csv")
# Transform data ----

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

#Roll up to larger area - if R and VR, then R
censusIndigenous2011 <- censusIndigenous2011 %>% 
  mutate(InnerRegional=ifelse(City>0 & InnerRegional>0,0,InnerRegional),
         OuterRegional=ifelse(InnerRegional>0 & OuterRegional>0,0,OuterRegional),
         Remote=ifelse(OuterRegional>0 & Remote>0,0,Remote),
         VeryRemote=ifelse(Remote>0&VeryRemote>0,0,VeryRemote))

census <- censusIndigenous2011 %>% 
  gather("Remoteness","value",3:8) %>% 
  filter(value>0)

# rename variables back to match polygons
# add poverty measure
census <- census %>% 
  mutate(State = case_when(State=="NSW" ~ "New South Wales",
                           State=="QLD" ~ "Queensland",
                           State=="VIC" ~ "Victoria",
                           State=="TAS" ~ "Tasmania",
                           State=="SA" ~ "South Australia",
                           State=="WA" ~ "Western Australia",
                           State=="NT"~"Northern Territory",
                           State=="ACT" ~ "Australian Capital Territory"),
         Remoteness = case_when(Remoteness=="City" ~ "Major Cities of Australia",
                                Remoteness=="InnerRegional" ~ "Inner Regional Australia",
                                Remoteness=="OuterRegional" ~ "Outer Regional Australia",
                                Remoteness=="Remote" ~ "Remote Australia",
                                Remoteness=="VeryRemote" ~ "Very Remote Australia"),
         weekly_income = case_when(Income=="$1-$299" ~ 299,
                                   Income=="$300-$399" ~ 399,
                                   Income=="$400-$799" ~ 799,
                                   Income=="$800-$999" ~ 999,
                                   Income=="$1,000-$1,249" ~ 1249,
                                   Income=="$1,250-$1,499" ~ 1499,
                                   Income=="$1,500-$1,999" ~ 1999,
                                   Income=="$2000 or more" ~ 3000,
                                   TRUE ~ 0))

census$Geography <- paste0(census$State,"-",census$Remoteness)
census <- census  %>% 
  group_by(Geography) %>% 
  spread(Sex, Count) %>% 
  mutate(Female=ifelse(is.na(Female),0,Female),
         Male=ifelse(is.na(Male),0,Male)) %>% 
  summarise(M_10=sum(`0-9 years`*Male),
            F_10=sum(`0-9 years`*Female),
            M_20=sum(`10-19 years`*Male),
            F_20=sum(`10-19 years`*Female),
            M_30=sum(`20-29 years`*Male),
            F_30=sum(`20-29 years`*Female),
            M_40=sum(`30-39 years`*Male),
            F_40=sum(`30-39 years`*Female),
            M_50=sum(`40-49 years`*Male),
            F_50=sum(`40-49 years`*Female),
            M_60=sum(`50-59 years`*Male),
            F_60=sum(`50-59 years`*Female),
            M_70=sum(`60-69 years`*Male),
            F_70=sum(`60-69 years`*Female),
            M_80=sum(`70-79 years`*Male)+sum(`80-89 years`*Male),
            F_80=sum(`70-79 years`*Female)+sum(`80-89 years`*Female),
            weekly_income=sum(weekly_income)) 

remote_poly = readOGR("/Users/sarahhepworth/Documents/R Sandbox/data/remoteness_shape/RA_2011_AUST.shp", stringsAsFactors=FALSE)
remote_poly@data$Geography = paste0(remote_poly@data$STE_NAME11,"-",remote_poly@data$RA_NAME11)

# Join the CSV data to the shapefile:
remote_poly@data <- remote_poly@data %>% dplyr::select(Geography)
census_poly = merge(x=remote_poly, y=census, by.x="Geography",by.y="Geography")
census_poly@data <- census_poly@data %>% 
  mutate(M_10 = ifelse(is.na(M_10),0,M_10),
         F_10 = ifelse(is.na(F_10),0,F_10),
         M_20 = ifelse(is.na(M_20),0,M_20),
         F_20 = ifelse(is.na(F_20),0,F_20),
         M_30 = ifelse(is.na(M_30),0,M_30),
         F_30 = ifelse(is.na(F_30),0,F_30),
         M_40 = ifelse(is.na(M_40),0,M_40),
         F_40 = ifelse(is.na(F_40),0,F_40),
         M_50 = ifelse(is.na(M_50),0,M_50),
         F_50 = ifelse(is.na(F_50),0,F_50),
         M_60 = ifelse(is.na(M_60),0,M_60),
         F_60 = ifelse(is.na(F_60),0,F_60),
         M_70 = ifelse(is.na(M_70),0,M_70),
         F_70 = ifelse(is.na(F_70),0,F_70),
         M_80 = ifelse(is.na(M_80),0,M_80),
         F_80 = ifelse(is.na(F_80),0,F_80),
         weekly_income = ifelse(is.na(weekly_income),0,weekly_income))

all_sex_age <- all_sex_age %>% 
  group_by(Category) %>% 
  summarise(rate=sum(Kidney.Disease)/sum(Total))
kidneyRates = structure(all_sex_age$rate, .Names=all_sex_age$Category)


survey_geo <- survey_geo %>% 
  mutate(Geography = paste0(State,"-",Remoteness))

kidney = survey_geo %>% 
  gather("sex","Cases",c("Male","Female")) %>% 
  mutate(age=NA,
         sex = ifelse(sex=="Male","M","F")) %>% dplyr::select(Geography,Cases,sex,age)

# HB BYM area model ----

# creating SMR's
census_poly2 = getSMR(census_poly, kidneyRates, kidney,
                   regionCode="Geography")

if(require('mapmisc', quietly=TRUE)) {
  mycol = colourScale(census_poly2$SMR, breaks=9,
                      dec=-log10(0.5), style='equal', transform='sqrt')
  plot(census_poly2, col=mycol$plot)
  legendBreaks('topleft', mycol)
}

kBYM = bym(observed ~ offset(logExpected) + weekly_income, census_poly2,
           priorCI = list(sdSpatial=c(0.1, 5), sdIndep=c(0.1, 5)),
           control.mode=list(theta=c(3.52, 3.35),restart=TRUE))
kBYM$par$summary


kBYM$data$exc1 = geostatsp::excProb(
    kBYM$inla$marginals.fitted.bym, log(1.2)
  )


# plot model
if(require('mapmisc', quietly=TRUE) & length(kBYM$data$fitted.exp)){
  thecol = colourScale(kBYM$data$fitted.exp,
                       breaks=5, dec=5, opacity = 0.7)
  map.new(kBYM$data)
  plot(kBYM$data, col=thecol$plot,add=TRUE)
  legendBreaks("topleft", thecol)
}
