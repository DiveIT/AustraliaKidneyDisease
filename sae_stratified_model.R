###########################################################################

# sae_stratified_model.R
# Examples of Small Area Approximation using different packages

###########################################################################


# Setup ----
library(dplyr)
library(sae)
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
load("./Processed-Data/CensusData.RData")
load("./Survey-Data/SurveyData.RData")

# Transform data ----
# Survey data is aggregated, and smaller areas have large errors.
# Compute post-stratified synthetic estimators of kidney disease
# for SA2 areas considering the income levels as post-strata.

# Survey dataset contains strata and value to estimate for population
survey_incm <- survey_incm %>%
  mutate(incm_strata=NA,
         rate = Kidney.Disease/Total) %>% 
  #filter(deciles>0) %>% 
  dplyr::select(deciles,rate,Kidney.Disease, Total)

# Census data

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
                                Remoteness=="VeryRemote" ~ "Very Remote Australia")) %>% 
  filter(!is.na(State))

census$Geography <- paste0(census$State,"-",census$Remoteness)

census <- census  %>% 
  dplyr::select(Geography,Income,Count) %>% 
  group_by(Geography,Income) %>% 
  summarise(count=sum(Count)) %>% 
  spread(Income,count) %>% 
  mutate(incm=NA) %>% 
  dplyr::select(-3,-4,-5,-14)

census <- census %>% dplyr::select(1:10)
colnames(census) <- c("Geography","0","1","2","3","4","5","6","7","8")

census$`0`[is.na(census$`0`)]<-0
census$`1`[is.na(census$`1`)]<-0
census$`2`[is.na(census$`2`)]<-0
census$`3`[is.na(census$`3`)]<-0
census$`4`[is.na(census$`4`)]<-0
census$`5`[is.na(census$`5`)]<-0
census$`6`[is.na(census$`6`)]<-0
census$`7`[is.na(census$`7`)]<-0
census$`8`[is.na(census$`8`)]<-0

row <- c(1,2,3,4,5,6,7,8,9)
weights <- c(sum(census$`0`),sum(census$`1`),sum(census$`2`),sum(census$`3`),sum(census$`4`),sum(census$`5`),sum(census$`6`),sum(census$`7`),sum(census$`8`))
weightings <- data.frame(row,weights)

survey_incm <- survey_incm %>% 
  left_join(weightings, by=c("deciles"="row")) %>% 
  mutate(weights=weights/Total)

# Indirect domain estimation model with plot ----

# Compute post-stratified synthetic estimators with state
# as domain codes
result1 <- pssynt(y=rate, sweight=weights, ps=deciles,
                  domsizebyps=census, data=survey_incm)

#get rate in total percentage
result1$PsSyntheticPercent <- result1$PsSynthetic*100
#Map
library(sp)
library(rgdal)
library(broom)
library(data.table)
library(leaflet)

remote_poly = readOGR("/Users/sarahhepworth/Documents/R Sandbox/data/remoteness_shape/RA_2011_AUST.shp", stringsAsFactors=FALSE)
remote_poly@data$Geography = paste0(remote_poly@data$STE_NAME11,"-",remote_poly@data$RA_NAME11)

# Join the CSV data to the shapefile:
results_poly = merge(x=remote_poly, y=result1, by.x="Geography",by.y="Geography")

#proj4string(survey_phn) <- CRS("+init=epsg:4283")
#survey_poly2 <- spTransform(survey_phn, prj.LatLong)

## define a palette for the colour
pal <- colorNumeric(palette = "YlOrRd",
                    domain = results_poly$PsSyntheticPercent)
#polygons
leaflet(data=results_poly) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.1,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(PsSyntheticPercent),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(position = "bottomleft", pal = pal, values = ~PsSyntheticPercent, title="Kidney Disease (percentage)") 

# TODO: Model-based MSE estimates ----

error_set <- result1 %>% left_join(survey_geo, by=("Geography"="Geography"))
error_set <- error_set %>% mutate(rate=Kidney.Disease/Total)
error_set$rate[is.na(error_set$rate)]<-0
rmse(error_set$PsSynthetic,error_set$rate)
