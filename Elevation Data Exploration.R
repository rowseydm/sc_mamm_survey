rm(list=ls())
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(purrr)
library(gsubfn)
library(ggpubr)

list.files(pattern = ".xlsx")

#####Read in data, there are three separate sources and four files
####1. UAZ Records
uaz_elev<-read.csv(file = "UAZ_SC_elev.csv") %>% #DEM Elevations from UAZ specimens
  rename(catalogNumber = UAZ_NUMBER)
uaz_data<-read_xlsx(path = "UAZ_Refined.xlsx") %>% #UAZ occurrence records sourced from M. Bucci
  mutate_at(.vars = vars(LATITUDE, LONGITUDE), .funs = as.numeric) %>%
  mutate_at(.vars = vars(COLLECTING_DATE), .funs = as.Date, format = "%d %B %Y") %>%
  #Select only needed variables
  select(UAZ_NUMBER, ORDER_, FAMILY, GENUS, SPECIES, SEX, COLLECTOR, 
         COLLECTOR_NUMBER, COLLECTING_DATE, COUNTY, LOCATION, LATITUDE, 
         LONGITUDE) %>%
  #Rename variables to conform to other sources
  rename(catalogNumber = UAZ_NUMBER, order = ORDER_, family = FAMILY, 
         genus = GENUS, species = SPECIES, sex = SEX, recordedBy = COLLECTOR,
         recordNumber = COLLECTOR_NUMBER, eventDate = COLLECTING_DATE, 
         county = COUNTY, locality = LOCATION, 
         decimalLatitude = LATITUDE, decimalLongitude = LONGITUDE)
uaz_data_elev <- uaz_data %>% #Combine occurrence records with DEM elevation
  mutate(scientificName = paste(genus, species),
         verbatimElevationInMeters = 
           as.numeric(stringr::str_extract(uaz_data$locality,
                                           pattern = "[0-9]{4}"))*0.3048,
         elevationDeviation = verbatimElevationInMeters,
         recordSource = "UAZ",
         basisOfRecord = "PRESERVED_SPECIMEN") %>%
  left_join(y = uaz_elev, by = "catalogNumber") 

####2. Records from our survey
our_data<-read_xlsx(path = "AllMammalRecords_2021-23_Refined.xlsx") %>%
  mutate(recordSource = "ASU post-2020") %>%
  mutate_at(.vars = vars(eventDate), .funs = as.Date) %>%
  mutate_at(.vars = vars(decimalLatitude, decimalLongitude), .funs = as.numeric) %>%
  #select only needed variables
  select(catalogNumber, order_, family, genus, species, verbatimScientificName, 
         sex, recordedBy, recordNumber, eventDate, locality, decimalLatitude, 
         decimalLongitude, elevation, elevationDEM, basisOfRecord, recordSource) %>%
  #rename variables to conform to other data sources
  rename(order = order_, scientificName = verbatimScientificName, 
         verbatimElevationInMeters = elevation, DEMElevationInMeters = elevationDEM)

####3. All other records, including observations
old_data<-read_xlsx(path = "AllMammalRecords_pre2021_Refined.xlsx", guess_max = 2500) %>% 
  #remove duplicate UAZ records with less necessary data than exist in uaz_data 
  filter(!grepl("UAZ .", catalogNumber)) %>%
  mutate_at(.vars = vars(eventDate), .funs = as.Date) %>%
  mutate_at(.vars = vars(decimalLatitude, decimalLongitude, elevation, 
                         elevationDEM), .funs = as.numeric) %>%
  #select only needed variables
  select(catalogNumber, order_, family, genus, species, verbatimScientificName,
         recordedBy, recordNumber, eventDate, locality, decimalLatitude,
         decimalLongitude, elevation, elevationDEM, basisOfRecord) %>%
  #rename variables to conform to other data sources
  rename(order = order_, scientificName = verbatimScientificName, 
         verbatimElevationInMeters = elevation, DEMElevationInMeters = elevationDEM) %>%
  mutate(recordSource = "non-UAZ pre-2021")
        
#####Combine data frames from all sources into one                      
all_data<-full_join(uaz_data_elev, old_data) %>%
  full_join(our_data)

#####Construct linear Model to analyze difference between tag and DEM elevation
elev_lm<-lm(DEMElevationInMeters ~ verbatimElevationInMeters, data = all_data)

####Store residuals and percent of standard deviation in all_data object
###(some values are NA)
all_data$residuals<-NA
all_data$residuals[as.numeric(names(elev_lm$residuals))] <- elev_lm$residuals
all_data$percentDeviation<-NA
all_data$percentDeviation[as.numeric(names(elev_lm$residuals))] <- 
  (abs(elev_lm$residuals)/sd(elev_lm$residuals)*100)

#####Plot DEM and Verbatim elevation while removing high-residual outliers
#1 sd removed (most strict)
  all_data %>%
    filter(!is.na(residuals)) %>%
    ggplot(aes(x = verbatimElevationInMeters, y = DEMElevationInMeters, color = recordSource)) +
    geom_point() +
    theme(aspect.ratio = 1) +
    scale_x_continuous(limits = c(500, 3000)) +
    scale_y_continuous(limits = c(500, 3000)) +
    geom_smooth(method = lm, se = F) +
    facet_wrap(facets = vars(percentDeviation > 100)) +
    theme_minimal() +
    stat_cor(label.y = c(3000, 2900, 2800), digits = 4) +
    stat_regline_equation(label.y = c(2200, 2100, 2000)) +
    labs(title = "verbatim vs DEM elevation, > 1 sd from mean")
#2 sd removed 
  all_data %>%
    filter(!is.na(residuals)) %>%
    ggplot(aes(x = verbatimElevationInMeters, y = DEMElevationInMeters, color = recordSource)) +
    geom_point() +
    theme(aspect.ratio = 1) +
    scale_x_continuous(limits = c(500, 3000)) +
    scale_y_continuous(limits = c(500, 3000)) +
    geom_smooth(method = lm, se = F) +
    facet_wrap(facets = vars(percentDeviation > 200)) +
    theme_minimal() +
    stat_cor(label.y = c(3000, 2900, 2800), digits = 4) +
    stat_regline_equation(label.y = c(2200, 2100, 2000)) +
    labs(title = "verbatim vs DEM elevation, > 2 sd from mean")
#3 sd removed (most lenient)
  all_data %>%
    filter(!is.na(residuals)) %>%
    ggplot(aes(x = verbatimElevationInMeters, y = DEMElevationInMeters, color = recordSource)) +
    geom_point() +
    theme(aspect.ratio = 1) +
    scale_x_continuous(limits = c(500, 3000)) +
    scale_y_continuous(limits = c(500, 3000)) +
    geom_smooth(method = lm, se = F) +
    facet_wrap(facets = vars(percentDeviation > 300)) +
    theme_minimal() +
    stat_cor(label.y = c(3000, 2900, 2800), digits = 4) +
    stat_regline_equation(label.y = c(2200, 2100, 2000)) +
    labs(title = "verbatim vs DEM elevation, > 3 sd from mean")

