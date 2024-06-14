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
           as.numeric(stringr::str_extract(locality,
                                           pattern = "[0-9]{4}"))*0.3048,
         recordSource = "UAZ",
         institutionCode = "UAZ",
         basisOfRecord = "PRESERVED_SPECIMEN") %>%
  select(-species) %>%
  left_join(y = uaz_elev, by = "catalogNumber") 

####2. Records from our survey
our_data<-read_xlsx(path = "AllMammalRecords_2021-23_Refined.xlsx") %>%
  mutate(recordSource = "ASU post-2020") %>%
  mutate_at(.vars = vars(eventDate), .funs = as.Date) %>%
  mutate_at(.vars = vars(decimalLatitude, decimalLongitude), .funs = as.numeric) %>%
  #select only needed variables
  select(institutionCode, catalogNumber, order_, family, genus, species, 
         sex, recordedBy, recordNumber, eventDate, locality, decimalLatitude, 
         decimalLongitude, elevation, elevationDEM, basisOfRecord, recordSource) %>%
  #rename variables to conform to other data sources
  rename(order = order_, scientificName = species, 
         verbatimElevationInMeters = elevation, DEMElevationInMeters = elevationDEM)

our_data_vouchers<-our_data %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN")
our_data_uniqueTissues<-our_data %>%
  filter(basisOfRecord == "MATERIAL_SAMPLE") %>%
  distinct(recordNumber, .keep_all = TRUE)

our_data_unique<-our_data_vouchers %>%
  rbind(our_data_uniqueTissues) %>%
  distinct(recordNumber, .keep_all = TRUE)

####3. All other records, including observations
old_data<-read_xlsx(path = "AllMammalRecords_pre2021_Refined.xlsx", guess_max = 2500) %>% 
  #remove duplicate UAZ records with less necessary data than exist in uaz_data 
  filter(!grepl("UAZ .", catalogNumber)) %>%
  mutate_at(.vars = vars(eventDate), .funs = as.Date) %>%
  mutate_at(.vars = vars(decimalLatitude, decimalLongitude, elevation, 
                         elevationDEM), .funs = as.numeric) %>%
  #select only needed variables
  select(institutionCode, catalogNumber, order_, family, genus, species,
         recordedBy, recordNumber, eventDate, locality, decimalLatitude,
         decimalLongitude, elevation, elevationDEM, basisOfRecord) %>%
  #rename variables to conform to other data sources
  rename(order = order_, scientificName = species, 
         verbatimElevationInMeters = elevation, DEMElevationInMeters = elevationDEM) %>%
  mutate(recordSource = "non-UAZ pre-2021", basisOfRecord = replace(basisOfRecord, basisOfRecord == "OCCURRENCE", "PRESERVED_SPECIMEN"))
        
#####Combine data frames from all sources into one                      
all_data<-full_join(our_data_unique, old_data) %>%
  full_join(uaz_data_elev) %>%
  filter(str_detect(scientificName, " ") & !str_detect(scientificName, " NA")) #Keep records that are identified to species


#####Construct linear Model to analyze difference between tag and DEM elevation
elev_lm<-lm(DEMElevationInMeters ~ verbatimElevationInMeters, data = all_data)

####Store residuals and percent of standard deviation in all_data object
###(some values are NA)
all_data$residuals<-NA
all_data$residuals[as.numeric(names(elev_lm$residuals))] <- elev_lm$residuals
all_data$percentDeviation<-NA
all_data$percentDeviation[as.numeric(names(elev_lm$residuals))] <- 
  ((abs(elev_lm$residuals)/sd(elev_lm$residuals))*100)

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

#####export to new file
all_data %>%
  write.csv(file = "SC_allmamm_georef.csv")


sd_prune<-all_data%>% 
  filter(percentDeviation<300|is.na(percentDeviation))


#####Exploratory plots
sd_prune %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN" & family %in% c("Heteromyidae", "Geomyidae")) %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN" & family == "Heteromyidae") %>%
  ggplot(mapping = aes(x = DEMElevationInMeters, y = scientificName)) +
  geom_boxplot() +
  theme_minimal()

sd_prune %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN" & family == "Cricetidae") %>%
  ggplot(mapping = aes(x = DEMElevationInMeters, y = scientificName)) +
  geom_boxplot() +
  theme_minimal()

sd_prune %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN" & family == "Sciuridae") %>%
  ggplot(mapping = aes(x = DEMElevationInMeters, y = scientificName)) +
  geom_boxplot() +
  theme_minimal()

sd_prune %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN" & order == "Carnivora") %>% 
  ggplot(mapping = aes(x = DEMElevationInMeters, y = scientificName)) +
  geom_boxplot() +
  theme_minimal()

sd_prune %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN" & 
           order %in% c("Artiodactyla", "Lagomorpha") ) %>%
    ggplot(mapping = aes(x = DEMElevationInMeters, y = scientificName)) +
  geom_boxplot() +
  theme_minimal()

sd_prune %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN" & order == "Eulipotyphla") %>% #Need to change vagrans and monticola to monticolus
  ggplot(mapping = aes(x = DEMElevationInMeters, y = scientificName)) +
  geom_boxplot() +
  theme_minimal()

sd_prune %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN" & order == "Chiroptera") %>% 
  ggplot(mapping = aes(x = DEMElevationInMeters, y = scientificName)) +
  geom_boxplot() +
  theme_minimal()

all_data %>%
  filter(scientificName %in% c("Perognathus flavus", "Dipodomys ordii") |
           DEMElevationInMeters > 2000 & scientificName %in% 
           c("Chaetodipus penicillatus", "Chaetodipus baileyi") |
           DEMElevationInMeters < 2200 & scientificName == "Peromyscus maniculatus" |
           DEMElevationInMeters < 2000 & scientificName %in% 
           c("Neotoma mexicana", "Reithrodontomys megalotis") |
           DEMElevationInMeters > 2000 & scientificName %in% 
           c("Peromyscus eremicus", "Onychomys torridus", "Neotoma albigula") |
           DEMElevationInMeters < 1000 & scientificName %in% 
           c("Sciurus aberti", "Otospermophilus variegatus") | 
           DEMElevationInMeters > 1500 & scientificName == "Ammospermophilus harrisii" |
           DEMElevationInMeters > 1200 & scientificName == "Parastrellus hesperus" |
           DEMElevationInMeters < 1000 & scientificName == "Myotis auriculus" |
           grepl("Virginia", locality) |
           scientificName == "Sylvilagus floridanus" | 
           scientificName == "Spilogale putorius"
         ) #%>%
  #write.csv(file = "Elevational_Outliers.csv") #Otospermophilus in Sabino Canyon, interestingly enough

basisCols<-c(HUMAN_OBSERVATION = "deepskyblue", 
             MATERIAL_SAMPLE = "green3", 
             PRESERVED_SPECIMEN = "darkorange")

sd_prune %>%
  filter(family %in% c("Heteromyidae", "Geomyidae")) %>%
  ggplot(mapping = aes(x = DEMElevationInMeters,
                       y = reorder(scientificName, as.numeric(as.factor(paste(family, scientificName)))), fill = basisOfRecord)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  labs(title = "Elevational Distributions of Santa Catalinas Mammals",
       subtitle = "Families Geomyidae and Heteromyidae",
       x = "DEM Elevation (m)", y = "Species",
       fill = "Basis of Record") +
  scale_fill_manual(values = basisCols, drop = FALSE, limits = rev, 
                    labels = c("Preserved specimen", "Material sample", "Human observation")) +
  scale_y_discrete(limits = rev) +
  theme_minimal() +
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y")+
  theme(axis.text.y = element_text(face = "italic"),
        strip.placement = "outside")

sd_prune %>%
  filter(family == "Cricetidae") %>%
  ggplot(mapping = aes(x = DEMElevationInMeters,
                       y = reorder(scientificName, as.numeric(as.factor(paste(family, scientificName)))), fill = basisOfRecord)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  labs(title = "Elevational Distributions of Santa Catalinas Mammals",
       subtitle = "Family Cricetidae",
       x = "DEM Elevation (m)", y = "Species",
       fill = "Basis of Record") +
  scale_fill_manual(values = basisCols, drop = FALSE, limits = rev, 
                    labels = c("Preserved specimen", "Material sample", "Human observation")) +
  scale_y_discrete(limits = rev) +
  theme_minimal() +
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y")+
  theme(axis.text.y = element_text(face = "italic"),
        strip.placement = "outside")

sd_prune %>%
  filter(family == "Sciuridae") %>%
  ggplot(mapping = aes(x = DEMElevationInMeters,
                       y = reorder(scientificName, as.numeric(as.factor(paste(family, scientificName)))), fill = basisOfRecord)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  labs(title = "Elevational Distributions of Santa Catalinas Mammals",
       subtitle = "Family Sciuridae",
       x = "DEM Elevation (m)", y = "Species",
       fill = "Basis of Record") +
  scale_fill_manual(values = basisCols, drop = FALSE, limits = rev, 
                    labels = c("Preserved specimen", "Material sample", "Human observation")) +
  scale_y_discrete(limits = rev) +
  theme_minimal() +
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y")+
  theme(axis.text.y = element_text(face = "italic"),
        strip.placement = "outside")

sd_prune %>%
  filter(order %in% c("Artiodactyla", "Lagomorpha") ) %>%
  ggplot(mapping = aes(x = DEMElevationInMeters,
                       y = reorder(scientificName, as.numeric(as.factor(paste(family, scientificName)))), fill = basisOfRecord)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  labs(title = "Elevational Distributions of Santa Catalinas Mammals",
       subtitle = "Orders Artiodactyla and Lagomorpha",
       x = "DEM Elevation (m)", y = "Species",
       fill = "Basis of Record") +
  scale_fill_manual(values = basisCols, limits = rev, 
                    labels = c("Preserved specimen", "Human observation")) +
  scale_y_discrete(limits = rev) +
  theme_minimal() +
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y")+
  theme(axis.text.y = element_text(face = "italic"),
        strip.placement = "outside")

sd_prune %>%
  filter(order %in% c("Carnivora","Eulipotyphla")) %>%
  ggplot(mapping = aes(x = DEMElevationInMeters,
                       y = reorder(scientificName, as.numeric(as.factor(paste(family, scientificName)))), fill = basisOfRecord)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  labs(title = "Elevational Distributions of Santa Catalinas Mammals",
       subtitle = "Orders Carnivora and Eulipotyphla",
       x = "DEM Elevation (m)", y = "Species",
       fill = "Basis of Record") +
  scale_fill_manual(values = basisCols, drop = FALSE, limits = rev, 
                    labels = c("Preserved specimen", "Human observation")) +
  scale_y_discrete(limits = rev) +
  theme_minimal() +
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y")+
  theme(axis.text.y = element_text(face = "italic"),
        strip.placement = "outside")

sd_prune %>%
  filter(order == "Chiroptera") %>% 
  ggplot(mapping = aes(x = DEMElevationInMeters,
                       y = reorder(scientificName, as.numeric(as.factor(paste(family, scientificName)))), fill = basisOfRecord)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  labs(title = "Elevational Distributions of Santa Catalinas Mammals",
       subtitle = "Order Chiroptera",
       x = "DEM Elevation (m)", y = "Species",
       fill = "Basis of Record") +
  scale_fill_manual(values = basisCols, drop = FALSE, limits = rev, 
                    labels = c("Preserved specimen", "Material sample", "Human observation")) +
  scale_y_discrete(limits = rev) +
  theme_minimal() +
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y")+
  theme(axis.text.y = element_text(face = "italic"),
        strip.placement = "outside")

sd_prune %>%
  ggplot(mapping = aes(x = as.numeric(format(eventDate, "%Y")), fill = basisOfRecord)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
  scale_x_continuous(breaks = seq(1875, 2025, 20)) +
  labs(title = "Santa Catalinas Mammal Occurrence Records Per Year",
       xlab = "Year", ylab = "Number of Records",
       fill = "Basis of Record") +
  scale_fill_manual(values = basisCols, drop = FALSE, limits = rev,
                    labels = c("Preserved specimen", "Material sample", "Human observation")) +
  theme_minimal()

sd_prune %>%
  ggplot(mapping = aes(x = as.numeric(format(eventDate, "%Y")), fill = basisOfRecord)) +
  geom_density(bw = 1, position = "identity", alpha = 0.7) +
  scale_x_continuous(breaks = seq(1875, 2025, 20)) +
  theme_minimal()
