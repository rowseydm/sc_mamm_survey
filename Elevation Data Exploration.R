# ###reminder that github credentials will need to be set and refreshed every so often:
# library(usethis)
# library(gitcreds)
# create_github_token() #will open window to create github access token; copy token from
# gitcreds_set()

rm(list=ls())
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(purrr)
library(gsubfn)
library(ggpubr)
library(sampbias)
library(terra)

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
         decimalLongitude, elevation, elevationDEM, preparations, habitat, burnStatus, basisOfRecord, recordSource, site) %>%
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
  distinct(recordNumber, .keep_all = TRUE) %>%
  filter(str_detect(scientificName, " ") & !str_detect(scientificName, " NA"))
  

####3. Most other records, including observations but excluding USNM and AMNH
old_data<-read_xlsx(path = "AllMammalRecords_pre2021_Refined2024-07.xlsx", guess_max = 2500) %>% 
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
  mutate(recordSource = "non-UAZ pre-2021", 
         basisOfRecord = replace(basisOfRecord, basisOfRecord == "OCCURRENCE", 
                                 "PRESERVED_SPECIMEN"))

####4. USNM and AMNH records
usnm_amnh_data<-read.csv(file = "AMNH_USNM_Records.csv") %>%
  mutate_at(.vars = vars(eventDate), .funs = as.Date) %>%
  mutate_at(.vars = vars(decimalLatitude, decimalLongitude, elevation, 
                         elevationDEM), .funs = as.numeric) %>%
  select(institutionCode, catalogNumber, order, family, genus, species,
         recordedBy, recordNumber, eventDate, locality, decimalLatitude,
         decimalLongitude, elevation, elevationDEM, basisOfRecord) %>%
  rename(scientificName = species, 
         verbatimElevationInMeters = elevation, DEMElevationInMeters = elevationDEM) %>%
  mutate(recordSource = "non-UAZ pre-2021", 
         basisOfRecord = replace(basisOfRecord, basisOfRecord == "PreservedSpecimen", 
                                 "PRESERVED_SPECIMEN"))
        
#####Combine data frames from all sources into one                      
all_data<-full_join(our_data_unique, old_data) %>%
  full_join(uaz_data_elev) %>%
  full_join(usnm_amnh_data) %>%
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

# all_data %>%
#   filter(scientificName %in% c("Perognathus flavus") | #Dipodomys ordii not recovered in most recent version of database
#            DEMElevationInMeters < 2000 & scientificName %in% 
#            c("Neotoma mexicana", "Peromyscus maniculatus") |
#            DEMElevationInMeters > 2000 & scientificName == "Neotoma albigula" |
#            DEMElevationInMeters < 1000 & scientificName %in% 
#            c("Sciurus aberti", "Otospermophilus variegatus") | 
#            DEMElevationInMeters > 1200 & scientificName == "Parastrellus hesperus" |
#            DEMElevationInMeters < 1000 & scientificName == "Myotis auriculus" |
#            scientificName == "Lepus californicus" |
#            scientificName == "Lepus alleni" |
#            grepl("Virginia", locality)
#          ) %>%
#   write.csv(file = "Elevational_Outliers_2024-07-06.csv") #Otospermophilus in Sabino Canyon, interestingly enough

outliers <- read.csv("Elevational_Outliers_2024-07-06.csv") %>%
  filter(institutionCode != "ASU", institutionCode != "iNaturalist", 
        family != "Vespertilionidae", 	scientificName != "Otospermophilus variegatus",
        catalogNumber != "UAZ 15492", catalogNumber != "UAZ 20835", catalogNumber != "UAZ 03820")

outliers <- subset(outliers, select = -c(X))
outliers$eventDate <- as.Date(outliers$eventDate)
outliers2<- sd_prune%>%
  filter(scientificName=="Felis catus" | scientificName=="Peromyscus truei")
sd_prune_no_outliers_pero <- sd_prune%>%
  filter(!catalogNumber %in% outliers$catalogNumber) %>%
  filter(!catalogNumber %in% outliers2$catalogNumber)
##sd_prune_no_outliers <- filter(sd_prune, catalogNumber != match(sd_prune$catalogNumber, outliers$catalogNumber))
sd_prune_no_outliers <- sd_prune_no_outliers_pero
sd_prune_no_outliers['scientificName'][sd_prune_no_outliers['scientificName'] == "Peromyscus leucopus" |
                                         sd_prune_no_outliers['scientificName'] == "Peromyscus melanotis" |
                                         sd_prune_no_outliers['scientificName'] == "Peromyscus sonoriensis" |
                                         sd_prune_no_outliers['scientificName'] == "Peromyscus maniculatus" ] <- "Peromyscus sonoriensis.melanotis"
  
# write.csv(sd_prune_no_outliers, file = "SC_All_Records_No_Outliers.csv")


basisCols<-c(HUMAN_OBSERVATION = "deepskyblue", 
             MATERIAL_SAMPLE = "green3", 
             PRESERVED_SPECIMEN = "darkorange")

sd_prune_no_outliers %>%
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

sd_prune_no_outliers %>%
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

sd_prune_no_outliers %>%
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

sd_prune_no_outliers %>%
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

sd_prune_no_outliers %>%
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

sd_prune_no_outliers %>%
  filter(order == "Chiroptera") %>% 
  ggplot(mapping = aes(x = DEMElevationInMeters,
                       y = reorder(scientificName, as.numeric(as.factor(paste(family, scientificName)))), fill = basisOfRecord)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  labs(title = "Elevational Distributions of Santa Catalinas Mammals",
       subtitle = "Order Chiroptera",
       x = "DEM Elevation (m)", y = "Species",
       fill = "Basis of Record") +
  scale_fill_manual(values = basisCols, drop = FALSE, limits = rev, 
                    labels = c("Preserved specimen", "Human observation")) +
  scale_y_discrete(limits = rev) +
  theme_minimal() +
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y")+
  theme(axis.text.y = element_text(face = "italic"),
        strip.placement = "outside")

sd_prune_no_outliers %>%
  ggplot(mapping = aes(x = as.numeric(format(eventDate, "%Y")), fill = basisOfRecord)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
  scale_x_continuous(breaks = seq(1875, 2025, 20)) +
  labs(title = "Santa Catalinas Mammal Occurrence Records Per Year",
       xlab = "Year", ylab = "Number of Records",
       fill = "Basis of Record") +
  scale_fill_manual(values = basisCols, drop = FALSE, limits = rev,
                    labels = c("Preserved specimen", "Material sample", "Human observation")) +
  theme_minimal()

sd_prune_no_outliers %>%
  ggplot(mapping = aes(x = as.numeric(format(eventDate, "%Y")), fill = basisOfRecord)) +
  geom_density(bw = 1, position = "identity", alpha = 0.7) +
  scale_x_continuous(breaks = seq(1875, 2025, 20)) +
  theme_minimal()

##For Savage to analyze:
###Ectoparasite prevalence from our survey
our_data %>%
  select(recordNumber, preparations) %>%
  filter(preparations == 'ectos') %>%
  filter(!duplicated(incomparables = FALSE, recordNumber))
  ###123 unique ecto entries
  ###369 unique recordNumbers

####Burn status
our_data %>%
  select(recordNumber, burnStatus) %>%
  filter(burnStatus == 'unburned') %>%
  filter(!duplicated(incomparables = FALSE, recordNumber))
  ###55 unique "burned" records
  ###314 unique "unburned" records
  ###369 unique recordNumbers

####Min, max, med elevation (and standard devation) for each species (or just summary statistics on elevation)
  ##parallel with just "our_data" and one with all species "sd_prune_no_outliers", create table of data
sd_prune_no_outliers %>%
  dplyr::select(scientificName, DEMElevationInMeters) %>%
  summarise(n = n(), min = min(DEMElevationInMeters), max = max(DEMElevationInMeters), mean = mean(DEMElevationInMeters), sd = sd(DEMElevationInMeters),.by = scientificName) %>%
  arrange(scientificName) %>%
  print(n = 66)
  
###Trap success by site and habitat
table(our_data_unique$scientificName, our_data_unique$site)
table(our_data_unique$scientificName, our_data_unique$habitat)


# Calculate mean habitat elevation lumping burned/unburned
aggregate(our_data_unique$verbatimElevationInMeters, list(our_data_unique$habitat), mean)
aggregate(our_data_unique$verbatimElevationInMeters, list(our_data_unique$site), mean)
# Various by-species summary statistics
our_data_unique_p %>%
  summarise(n= n(), min = min(DEMElevationInMeters), max = max(DEMElevationInMeters), .by = scientificName) %>%
  print()

# Historical records only
historic_data <- sd_prune_no_outliers %>%
  filter(recordSource != "ASU post-2020")
# records of historical specimens by basis of record
historic_data %>%
  summarise(n=n(), .by = basisOfRecord)
  
historic_data %>%
  summarise(n=n(), .by = scientificName) %>%
  print(n = 72)

sd_prune_no_outliers %>%
  dplyr::select(scientificName, DEMElevationInMeters) %>%
  summarise(n = n(), min = min(DEMElevationInMeters), 
            max = max(DEMElevationInMeters), 
            mean = mean(DEMElevationInMeters), 
            sd = sd(DEMElevationInMeters), 
            range = max(DEMElevationInMeters)-min(DEMElevationInMeters), 
            .by = scientificName) %>%
  arrange(sd) %>%
  filter(n>10) %>%
    print(n = 66) %>%
  ggplot(mapping = aes(y = sd, x = rev(seq_along(1:length(sd))))) +
  geom_line() +
  scale_x_continuous(breaks = seq(1875, 2025, 20)) +
  theme_minimal()


# Density plot of samples relative to elevation
sd_prune_no_outliers %>%
  ggplot(mapping = aes(x = (DEMElevationInMeters))) +
  geom_density() +
  scale_x_continuous(limits = c(800, 3000)) +
  theme_minimal() # holy sampling bias batman!

#our samples only
our_data_unique %>%
  ggplot(mapping = aes(x = log(DEMElevationInMeters))) +
  geom_density() +
  scale_x_continuous(limits = c(6.625, 8)) +
  theme_minimal() # holy sampling bias batman!

###At this point we need to split the recent samples back out of the no-outliers
####because we have changed the names of the montane Peromyscus
our_data_unique_p <-sd_prune_no_outliers %>%
  filter(recordSource=="ASU post-2020")
  
#filter historic records to the 15 species sampled in our survey
match_data <- historic_data %>%
  filter(scientificName %in% unique(our_data_unique_p$scientificName)) #1208 records

left_join(match_data %>%
  summarise(min.hist = min(DEMElevationInMeters), 
            max.hist = max(DEMElevationInMeters), 
            .by = scientificName),
  our_data_unique_p %>%
    summarise(min.rec = min(DEMElevationInMeters),
              max.rec = max(DEMElevationInMeters),
              .by = scientificName)) %>%
  mutate(min.ext = min.hist>min.rec, 
         max.ext = max.hist<max.rec,
         elev.diff.min = min.hist - min.rec, 
         elev.diff.max = max.rec - max.hist) %>%
  filter(min.ext == TRUE | max.ext == TRUE) %>%
  write.csv(file = "SC elevational extensions.csv")
  
###Quantify sampling bias in historical and contemporary records
scboundary<-as.data.frame(terra::vect(basename("Santa_Catalina_StudyArea_Polygon_FINAL.shp"), 
                                      crs = "epsg:4326"), geom = "WKT") #read SC Polygon
# smamm.sampbias<-calculate_bias(x = sd_prune_no_outliers %>%
#                                  rename(species = scientificName), 
#                                res = 0.035,
#                                restrict_sample = sancat) #need a custom gazetteer with roads if we want to make this work...
