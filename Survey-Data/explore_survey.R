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
survey_geo <- read.csv("KidneyStateRemoteness.csv")
survey_strata <- read.csv("KidneySexStateRemote.csv")
survey_state <- read.csv("KidneySexState.csv")
survey_age <- read.csv("KidneySexAge.csv")
survey_incm <- read.csv("KidneySexIncome.csv")
survey_incm <- read.csv("KidneyStateIncome.csv")
survey_phn <- read.csv("KidneyPHN.csv")


# Transform data ----
survey_geo$Remoteness <- as.character(survey_geo$Remoteness)
survey_geo$Remoteness[survey_geo$Remoteness =="City"] <- "Major Cities of Australia"
survey_geo$Remoteness[survey_geo$Remoteness =="InnerRegional"] <- "Inner Regional Australia"
survey_geo$Remoteness[survey_geo$Remoteness =="OuterRegional"] <- "Outer Regional Australia"
survey_geo$Remoteness[survey_geo$Remoteness =="Remote"] <- "Remote Australia"
survey_geo$Remoteness[survey_geo$Remoteness =="VeryRemote"] <- "Very Remote Australia"
survey_geo <- survey_geo %>% 
  mutate(Total = Not.Reported+Kidney.Disease+No.Kidney.Disease,
         Geography = paste0(State,"-",Remoteness))



survey_strata <- survey_strata %>% 
  mutate(Total = Kidney.Disease+No.Kidney.Disease)
survey_strata$Remoteness <- as.character(survey_geo$Remoteness)
survey_strata$Remoteness[survey_geo$Remoteness =="Major Cities of Australia"] <- 1
survey_strata$Remoteness[survey_geo$Remoteness =="Inner Regional Australia"] <-2
survey_strata$Remoteness[survey_geo$Remoteness =="Outer Regional Australia"]<-3
survey_strata$Remoteness[survey_geo$Remoteness =="Remote Australia"]<-4
survey_strata$Remoteness[survey_geo$Remoteness =="Very Remote Australia"]<-5
survey_strata <- survey_strata %>% 
  mutate(Remoteness=as.numeric(Remoteness),
         ppt=Kidney.Disease/Total*1000)



survey_age <- survey_age %>% 
  mutate(Total = Not.reported+Kidney.Disease+No.Kidney.Disease)
survey_age$Age <- as.character(survey_age$Age)
survey_age$Age[survey_age$Age %in% c("18","19")] <- "10-19 years"
survey_age$Age[survey_age$Age %in% c("20","21","22","23","24","25","26","27","28","29")] <- "20-29 years"
survey_age$Age[survey_age$Age %in% c("30","31","32","33","34","35","36","37","38","39")] <- "30-39 years"
survey_age$Age[survey_age$Age %in% c("30","31","32","33","34","35","36","37","38","39")] <- "30-39 years"
survey_age$Age[survey_age$Age %in% c("40","41","42","43","44","45","46","47","48","49")] <- "40-49 years"
survey_age$Age[survey_age$Age %in% c("50","51","52","53","54","55","56","57","58","59")] <- "50-59 years"
survey_age$Age[survey_age$Age %in% c("60","61","62","63","64","65","66","67","68","69")] <- "60-69 years"
survey_age$Age[survey_age$Age %in% c("70","71","72","73","74","75","76","77","78","79")] <- "70-79 years"
survey_age$Age[survey_age$Age %in% c("80","81","82","83","84","85","86","87","88","89")] <- "80-89 years"
survey_age$Age[survey_age$Age %in% c("90","91","92","93","94","95","96","97","98","99")] <- "90-99 years"

# Income translated from deciles
# $1-$299 (less than $390)
# $300-$399 (309-573)
# $400-$799 ($574-782)
# $800-$999 (783-1,019)
# $1,000-$1,249 ($1,020-$1,330)
# $1,250-$1,499 ($1,331-$1,663)
# $1,500-$1,999 ($1,664-$2,027)
# $2,000 or more ($2,028 or more)
survey_incm$incm <- as.character(survey_incm$Gross.weekly.cash.income.of.household..deciles)
survey_incm$incm[survey_incm$incm =="First decile"] <- "$1-$299"
survey_incm$incm[survey_incm$incm =="Second decile"] <- "$300-$399"
survey_incm$incm[survey_incm$incm =="Third decile"] <- "$400-$799"
survey_incm$incm[survey_incm$incm =="Fourth decile"] <- "$800-$999"
survey_incm$incm[survey_incm$incm =="Fifth decile"] <- "$1,000-$1,249"
survey_incm$incm[survey_incm$incm =="Sixth decile"] <- "$1,250-$1,499"
survey_incm$incm[survey_incm$incm =="Seventh decile"] <- "$1,500-$1,999"
survey_incm$incm[survey_incm$incm =="Eighth decile"|survey_incm$incm =="Ninth decile"|survey_incm$incm =="Tenth decile"] <- "$2,000 or more"
survey_incm <- survey_incm %>% 
  mutate(Total = Not.reported+Kidney.Disease+No.Kidney.Disease)


survey_phn <- survey_phn %>% 
  mutate(Total = Not.Reported+Kidney.Disease+No.Kidney.Disease)


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
## Map survey ----
library(sp)
library(rgdal)
library(broom)
library(data.table)
library(leaflet)

phn_poly = readOGR("/Users/sarahhepworth/Documents/R Sandbox/data/phn_shape/PHN_boundaries_AUS_Sep2015_V5.shp", stringsAsFactors=FALSE)
remote_poly = readOGR("/Users/sarahhepworth/Documents/R Sandbox/data/remoteness_shape/RA_2011_AUST.shp", stringsAsFactors=FALSE)

remote_poly@data$STE_RA = paste0(remote_poly@data$STE_NAME11,"-",remote_poly@data$RA_NAME11)

# Join the CSV data to the shapefile:
survey_poly = merge(x=phn_poly, y=survey_phn, by.x="PHN_Name", by.y="PHN")
survey_remote_poly = merge(x=remote_poly, y=survey_geo, by.x="STE_RA",by.y="Geography")

#proj4string(survey_phn) <- CRS("+init=epsg:4283")
#survey_poly2 <- spTransform(survey_phn, prj.LatLong)

# Extracting point locations for each polygon 
phn_poly@data$id = rownames(phn_poly@data)
phn_poly.points = tidy(phn_poly)
phn_poly.df = inner_join(phn_poly.points, phn_poly@data, by = "id")
phn_poly.df = subset(phn_poly.df, select = c(long, lat, group, PHN_Name))
names(phn_poly.df) = c("long", "lat", "group", "PHN_Name")

survey.data = inner_join(survey_phn, phn_poly.df, by = c("PHN"="PHN_Name"))

#Transform to take centroid (for fast processing)
survey.data.point <- survey.data %>% 
  group_by(PHN, Kidney.Disease) %>% 
  summarise(long=median(long),
            lat=median(lat))

label_points = coordinates(survey_poly)
x_label = label_points[,1]
y_label = label_points[,2]
label_text = sprintf("%s", survey.data$PHN)

labels.df = data.frame(x_label, y_label, label_text, stringsAsFactors=FALSE)



## define a palette for the colour
pal <- colorNumeric(palette = "YlOrRd",
                    domain = survey_remote_poly$Kidney.Disease)

leaflet(data = survey.data.point) %>%
  addTiles() %>%
  addCircleMarkers(lat = ~lat, lng = ~long, popup = ~PHN, 
                   color = ~pal(Kidney.Disease), stroke = FALSE, fillOpacity = 0.6) %>%
  addLegend(position = "bottomleft", pal = pal, values = ~Kidney.Disease)

#polygons
leaflet(data=survey_poly) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(Kidney.Disease),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(position = "bottomleft", pal = pal, values = ~Kidney.Disease) 


leaflet(data=survey_remote_poly) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(Kidney.Disease),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(position = "bottomleft", pal = pal, values = ~Kidney.Disease) 


# TODO: Select most appropriate data

# Save data
save(survey_age,survey_geo,survey_incm,survey_phn,survey_state, file="./SurveyData.RData")

