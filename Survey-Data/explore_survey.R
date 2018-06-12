
###########################################################################

# explore_survey.R
# ETL of Australian Health Survey data has already been completed in TableBuilder

###########################################################################

# Setup ----
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
# Indigenous Health Survey
survey_age <- read.csv("./data/KidneyAge.csv")
survey_geo <- read.csv("./data/KidneyStateRemoteness.csv")
survey_incm <- read.csv("./data/KidneyIncome.csv")
survey_health <- read.csv("./data/comorbidity.csv")
# Australian Health Survey
all_survey_incm <- read.csv("./data/AllKidneyIncome.csv")
all_survey_geo <- read.csv("./data/AllKidneyStateRemoteness.csv")
all_sex_age <- read.csv("./data/AllKidneySexAge.csv")

# Transform data ----
#Age into deciles
survey_age <- survey_age %>% 
  mutate(ppn = Kidney.Disease/Total)
survey_age$Age <- as.character(survey_age$Age)
survey_age$Age[survey_age$Age %in% c("1","2","3","4","5","6","7","8","9")] <- "0-9 years"
survey_age$Age[survey_age$Age %in% c("10","11","12","13","14","15","16","17","18","19")] <- "10-19 years"
survey_age$Age[survey_age$Age %in% c("20","21","22","23","24","25","26","27","28","29")] <- "20-29 years"
survey_age$Age[survey_age$Age %in% c("30","31","32","33","34","35","36","37","38","39")] <- "30-39 years"
survey_age$Age[survey_age$Age %in% c("30","31","32","33","34","35","36","37","38","39")] <- "30-39 years"
survey_age$Age[survey_age$Age %in% c("40","41","42","43","44","45","46","47","48","49")] <- "40-49 years"
survey_age$Age[survey_age$Age %in% c("50","51","52","53","54","55","56","57","58","59")] <- "50-59 years"
survey_age$Age[survey_age$Age %in% c("60","61","62","63","64","65","66","67","68","69")] <- "60-69 years"
survey_age$Age[survey_age$Age %in% c("70","71","72","73","74","75","76","77","78","79")] <- "70-79 years"
survey_age$Age[survey_age$Age %in% c("80","81","82","83","84","85","86","87","88","89")] <- "80-89 years"
survey_age$Age[survey_age$Age %in% c("90","91","92","93","94","95","96","97","98","99")] <- "90-99 years"

#All Age into deciles
all_sex_age <- all_sex_age %>% 
  mutate(rate = Kidney.Disease/Total)
all_sex_age$Category <- as.character(all_sex_age$Age)
all_sex_age$Category[all_sex_age$Category %in% c("1","2","3","4","5","6","7","8","9") & all_sex_age$Sex=="Male"] <- "M_10"
all_sex_age$Category[all_sex_age$Category %in% c("1","2","3","4","5","6","7","8","9") & all_sex_age$Sex=="Female"] <- "M_10"
all_sex_age$Category[all_sex_age$Category %in% c("10","11","12","13","14","15","16","17","18","19")& all_sex_age$Sex=="Male"] <- "M_20"
all_sex_age$Category[all_sex_age$Category %in% c("10","11","12","13","14","15","16","17","18","19")& all_sex_age$Sex=="Female"] <- "F_20"
all_sex_age$Category[all_sex_age$Category %in% c("20","21","22","23","24","25","26","27","28","29") & all_sex_age$Sex=="Male"] <- "M_30"
all_sex_age$Category[all_sex_age$Category %in% c("20","21","22","23","24","25","26","27","28","29") & all_sex_age$Sex=="Female"] <- "F_30"
all_sex_age$Category[all_sex_age$Category %in% c("30","31","32","33","34","35","36","37","38","39") & all_sex_age$Sex=="Male"] <- "M_40"
all_sex_age$Category[all_sex_age$Category %in% c("30","31","32","33","34","35","36","37","38","39") & all_sex_age$Sex=="Female"] <- "F_40"
all_sex_age$Category[all_sex_age$Category %in% c("30","31","32","33","34","35","36","37","38","39") & all_sex_age$Sex=="Male"] <- "M_50"
all_sex_age$Category[all_sex_age$Category %in% c("30","31","32","33","34","35","36","37","38","39") & all_sex_age$Sex=="Female"] <- "F_50"
all_sex_age$Category[all_sex_age$Category %in% c("40","41","42","43","44","45","46","47","48","49") & all_sex_age$Sex=="Male"] <- "M_60"
all_sex_age$Category[all_sex_age$Category %in% c("40","41","42","43","44","45","46","47","48","49") & all_sex_age$Sex=="Female"] <- "F_60"
all_sex_age$Category[all_sex_age$Category %in% c("50","51","52","53","54","55","56","57","58","59") & all_sex_age$Sex=="Male"] <- "M_70"
all_sex_age$Category[all_sex_age$Category %in% c("50","51","52","53","54","55","56","57","58","59") & all_sex_age$Sex=="Female"] <- "F_70"
all_sex_age$Category[all_sex_age$Category %in% c("60","61","62","63","64","65","66","67","68","69",
                                                 "70","71","72","73","74","75","76","77","78","79",
                                                 "80","81","82","83","84","85","86","87","88","89",
                                                 "90","91","92","93","94","95","96","97","98","99","75 or over") & all_sex_age$Sex=="Male"]  <- "M_80"
all_sex_age$Category[all_sex_age$Category %in% c("60","61","62","63","64","65","66","67","68","69",
                                                 "70","71","72","73","74","75","76","77","78","79",
                                                 "80","81","82","83","84","85","86","87","88","89",
                                                 "90","91","92","93","94","95","96","97","98","99","75 or over") & all_sex_age$Sex=="Female"]  <- "F_80"





#Geographic info into strata
survey_geo <- survey_geo %>% 
  mutate(Geography = paste0(State,"-",Remoteness))

survey_strata <- survey_geo 
survey_strata$Remoteness <- as.character(survey_geo$Remoteness)
survey_strata$Remoteness[survey_geo$Remoteness =="Major Cities of Australia"] <- 1
survey_strata$Remoteness[survey_geo$Remoteness =="Inner Regional Australia"] <-2
survey_strata$Remoteness[survey_geo$Remoteness =="Outer Regional Australia"]<-3
survey_strata$Remoteness[survey_geo$Remoteness =="Remote Australia"]<-4
survey_strata$Remoteness[survey_geo$Remoteness =="Very Remote Australia"]<-5
survey_strata <- survey_strata %>% 
  mutate(Remoteness=as.numeric(Remoteness),
         ppt=Kidney.Disease/Total*1000)

all_survey_geo <- all_survey_geo %>% 
  mutate(Geography = paste0(State,"-",Remoteness))

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
survey_incm$deciles <- NA
survey_incm$deciles[survey_incm$incm =="Not known"] <- 0
survey_incm$deciles[survey_incm$incm =="First decile"] <- 1
survey_incm$deciles[survey_incm$incm =="Second decile"] <- 2
survey_incm$deciles[survey_incm$incm =="Third decile"] <- 3
survey_incm$deciles[survey_incm$incm =="Fourth decile"] <- 4
survey_incm$deciles[survey_incm$incm =="Fifth decile"] <- 5
survey_incm$deciles[survey_incm$incm =="Sixth decile"] <- 6
survey_incm$deciles[survey_incm$incm =="Seventh decile"] <- 7
survey_incm$deciles[survey_incm$incm =="Eighth decile"|survey_incm$incm =="Ninth decile"|survey_incm$incm =="Tenth decile"] <- 8
survey_incm <- survey_incm %>% 
  filter(incm!="Not applicable") 
survey_incm <- survey_incm %>% 
  filter(incm!="Not stated")

survey_incm$incm[survey_incm$incm =="First decile"] <- "$1-$299"
survey_incm$incm[survey_incm$incm =="Second decile"] <- "$300-$399"
survey_incm$incm[survey_incm$incm =="Third decile"] <- "$400-$799"
survey_incm$incm[survey_incm$incm =="Fourth decile"] <- "$800-$999"
survey_incm$incm[survey_incm$incm =="Fifth decile"] <- "$1,000-$1,249"
survey_incm$incm[survey_incm$incm =="Sixth decile"] <- "$1,250-$1,499"
survey_incm$incm[survey_incm$incm =="Seventh decile"] <- "$1,500-$1,999"
survey_incm$incm[survey_incm$incm =="Eighth decile"|survey_incm$incm =="Ninth decile"|survey_incm$incm =="Tenth decile"] <- "$2,000 or more"
survey_incm <- survey_incm %>% 
  dplyr::select(incm,deciles,Kidney.Disease,rse,Total)


#Total Kidney Disease data
all_survey_incm$incm <- as.character(all_survey_incm$Gross.weekly.cash.income.of.household..deciles)
all_survey_incm$deciles <- NA
all_survey_incm$deciles[all_survey_incm$incm =="Not known"] <- 0
all_survey_incm$deciles[all_survey_incm$incm =="First decile"] <- 1
all_survey_incm$deciles[all_survey_incm$incm =="Second decile"] <- 2
all_survey_incm$deciles[all_survey_incm$incm =="Third decile"] <- 3
all_survey_incm$deciles[all_survey_incm$incm =="Fourth decile"] <- 4
all_survey_incm$deciles[all_survey_incm$incm =="Fifth decile"] <- 5
all_survey_incm$deciles[all_survey_incm$incm =="Sixth decile"] <- 6
all_survey_incm$deciles[all_survey_incm$incm =="Seventh decile"] <- 7
all_survey_incm$deciles[all_survey_incm$incm =="Eighth decile"|all_survey_incm$incm =="Ninth decile"|all_survey_incm$incm =="Tenth decile"] <- 8
all_survey_incm <- all_survey_incm %>% 
  filter(incm!="Not applicable") 
all_survey_incm <- all_survey_incm %>% 
  filter(incm!="Not stated")

all_survey_incm$incm[all_survey_incm$incm =="First decile"] <- "$1-$299"
all_survey_incm$incm[all_survey_incm$incm =="Second decile"] <- "$300-$399"
all_survey_incm$incm[all_survey_incm$incm =="Third decile"] <- "$400-$799"
all_survey_incm$incm[all_survey_incm$incm =="Fourth decile"] <- "$800-$999"
all_survey_incm$incm[all_survey_incm$incm =="Fifth decile"] <- "$1,000-$1,249"
all_survey_incm$incm[all_survey_incm$incm =="Sixth decile"] <- "$1,250-$1,499"
all_survey_incm$incm[all_survey_incm$incm =="Seventh decile"] <- "$1,500-$1,999"
all_survey_incm$incm[all_survey_incm$incm =="Eighth decile"|all_survey_incm$incm =="Ninth decile"|all_survey_incm$incm =="Tenth decile"] <- "$2,000 or more"
all_survey_incm <- all_survey_incm %>% 
  dplyr::select(incm,deciles,All.Kidney.Disease,rse,Total)

# Summarise data ----

# Correlation of health factors

comord <- as.vector(survey_health %>% filter(d1d2type=="Kidney.Disease and Diabetes"))
comorbidity(comord$d1,comord$d2,comord$com, labels = c("Kidney","Diabetes"))

comord <- as.vector(survey_health %>% filter(d1d2type=="Kidney.Disease and CVD"))
comorbidity(comord$d1,comord$d2,comord$com, labels = c("Kidney","CVD"))

comord <- as.vector(survey_health %>% filter(d1d2type=="Kidney.Disease and Diabetes"))
comorbidity(comord$d1,comord$d2,comord$com, labels = c("Kidney","Diabetes"))


# For all survey sets
dim(survey_age)
vis_dat(survey_age) #factor and numeric features
#investigate missing values by setting 0 to NA
survey_age_miss <- survey_age %>% 
  mutate(Kidney.Disease = ifelse(Kidney.Disease==0, NA, Kidney.Disease))
vis_miss(survey_age_miss) # 10.1% 0 values
summary(survey_age)

dim(survey_geo)
vis_dat(survey_geo) #factor and numeric features
survey_geo_miss <- survey_geo %>% 
  mutate(Kidney.Disease = ifelse(Kidney.Disease==0, NA, Kidney.Disease))
vis_miss(survey_geo_miss) # 9.1% 0 values
summary(survey_geo)

dim(survey_incm)
vis_dat(survey_incm) #factor and numeric features
survey_incm_miss <- survey_incm %>% 
  mutate(Kidney.Disease = ifelse(Kidney.Disease==0, NA, Kidney.Disease))
vis_miss(survey_incm_miss) # 6.2% 0 values
summary(survey_incm)

describe(survey_age$Kidney.Disease, IQR=TRUE) 
describe(survey_geo$Kidney.Disease, IQR=TRUE) 
describe(survey_incm$Kidney.Disease, IQR=TRUE) 

# Histograms of data ----
# age
total <- sum(survey_age$Kidney.Disease)
age_plot <- survey_age %>% 
  group_by(Age) %>% 
  summarise(Kidney.Disease=sum(Kidney.Disease),
            rse = mean(rse),
            proportion = Kidney.Disease/total)

ggplot(age_plot, aes(Age,Kidney.Disease)) + geom_bar(stat="identity", fill="#999999") + 
  labs(title="Indigenous Persons with Kidney disease", 
       subtitle="Age distribution with error bars") + theme_classic() +
  geom_errorbar(aes(ymin=Kidney.Disease+Kidney.Disease*(rse/2), ymax=Kidney.Disease-Kidney.Disease*(rse/2)), colour="black", width=.1) 

#incm
incm_plot <- survey_incm %>% 
  group_by(incm,deciles) %>% 
  summarise(Kidney.Disease=sum(Kidney.Disease),
            rse = mean(rse))
incm_plot$deciles <- reorder(incm_plot$incm, incm_plot$deciles)
g <- ggplot(incm_plot, aes(deciles,Kidney.Disease))
g + geom_bar(stat="identity", colour="black") + 
  labs(title="Indigenous persons with Kidney disease", 
       subtitle="Distribution of Weekly Income level with RSE") +
  theme_classic() +
  geom_errorbar(aes(ymin=Kidney.Disease+Kidney.Disease*(rse/200), ymax=Kidney.Disease-Kidney.Disease*(rse/200)), colour="black", width=.1) 

all_incm_plot <- all_survey_incm %>% 
  group_by(incm,deciles) %>% 
  summarise(Kidney.Disease=sum(All.Kidney.Disease),
            rse = mean(rse))
all_incm_plot$deciles <- reorder(all_incm_plot$incm, all_incm_plot$deciles)
g <- ggplot(all_incm_plot, aes(deciles,Kidney.Disease))
g + geom_bar(stat="identity", colour="black") + 
  labs(title="All persons with Kidney disease", 
       subtitle="Distribution of Weekly Income level") + 
  theme_classic() +
  geom_errorbar(aes(ymin=Kidney.Disease+Kidney.Disease*(rse/200), ymax=Kidney.Disease-Kidney.Disease*(rse/200)), colour="black", width=.1) 

#combined income
incm_plot <- left_join(survey_incm,all_survey_incm, by=c("incm","deciles"))

incm_plot <- incm_plot %>% gather("Type","Kidney.Disease",c(Kidney.Disease,All.Kidney.Disease))
totals = incm_plot %>% group_by(Type) %>% summarise(total=sum(Kidney.Disease))
incm_plot <- left_join(incm_plot,totals, by=c("Type"))
incm_plot <- incm_plot %>% mutate(Kidney.Disease = Kidney.Disease/total)
incm_plot$deciles <- reorder(incm_plot$incm, incm_plot$deciles)
incm_plot <- incm_plot %>% select(incm,Type,Kidney.Disease)
ggplot(incm_plot, aes(x=incm,y=Kidney.Disease, colour=Type)) + geom_bar(stat="identity") + 
  labs(title="Persons with Kidney disease", 
       subtitle="Distribution of Weekly Income level") + theme_classic()


#remoteness - leaflet map
library(sp)
library(rgdal)
library(broom)
library(data.table)
library(leaflet)
library(mapview)
remote_poly = readOGR("/Users/sarahhepworth/Documents/R Sandbox/data/remoteness_shape/RA_2011_AUST.shp", stringsAsFactors=FALSE)
remote_poly@data$STE_RA = paste0(remote_poly@data$STE_NAME11,"-",remote_poly@data$RA_NAME11)

total <- sum(survey_geo$Kidney.Disease)
geo_plot <- survey_geo %>% 
  group_by(Geography) %>% 
  summarise(Kidney.Disease=sum(Kidney.Disease),
            proportion = Kidney.Disease/total)
# Join the CSV data to the shapefile:
survey_remote_poly = merge(x=remote_poly, y=geo_plot, by.x="STE_RA",by.y="Geography")

#proj4string(survey_phn) <- CRS("+init=epsg:4283")
#survey_poly2 <- spTransform(survey_phn, prj.LatLong)

## define a palette for the colour
pal <- colorNumeric(palette = "YlOrRd",
                    domain = survey_remote_poly$proportion)
#polygons
leaflet(data=survey_remote_poly) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(proportion),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(position = "bottomleft", pal = pal, values = ~proportion, title="Kidney Disease (proportion)") 
#mapshot(m, file = "~/Rplot.png")
# Extracting point locations for each polygon 
#remote_poly@data$id = rownames(remote_poly@data)
#remote_poly.points = tidy(remote_poly)
#remote_poly.df = inner_join(remote_poly.points, remote_poly@data, by = "id")
#remote_poly.df = subset(remote_poly.df, select = c(long, lat, group, STE_RA))
#names(remote_poly.df) = c("long", "lat", "group", "STE_RA")

#survey.data = merge(survey_remote_poly, remote_poly.df, by = c("STE_RA"="STE_RA"))

#Transform to take centroid (for fast processing)
#survey.data.point <- survey.data %>% 
#  group_by(PHN, Kidney.Disease) %>% 
#  summarise(long=median(long),
 #           lat=median(lat))

#label_points = coordinates(survey_poly)
#x_label = label_points[,1]
#y_label = label_points[,2]
#label_text = sprintf("%s", survey.data$PHN)

#labels.df = data.frame(x_label, y_label, label_text, stringsAsFactors=FALSE)


#leaflet(data = survey.data.point) %>%
#  addTiles() %>%
#  addCircleMarkers(lat = ~lat, lng = ~long, popup = ~PHN, 
#                   color = ~pal(Kidney.Disease), stroke = FALSE, fillOpacity = 0.6) %>%
#  addLegend(position = "bottomleft", pal = pal, values = ~Kidney.Disease)


#compare to all data distribution
# Join the CSV data to the shapefile:
all_survey_remote_poly = merge(x=remote_poly, y=all_survey_geo, by.x="STE_RA",by.y="Geography")

#proj4string(survey_phn) <- CRS("+init=epsg:4283")
#survey_poly2 <- spTransform(survey_phn, prj.LatLong)

## define a palette for the colour
pal <- colorNumeric(palette = "PuRd",
                    domain = all_survey_remote_poly$Kidney.Disease)
#polygons
leaflet(data=all_survey_remote_poly) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(Kidney.Disease),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(position = "bottomleft", pal = pal, values = ~Kidney.Disease) 



# TODO: Select most appropriate data

# Save data
save(survey_age,survey_geo,survey_incm,all_sex_age,file="./SurveyData.RData")
