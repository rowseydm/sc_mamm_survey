library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
list.files(pattern = ".xlsx")
uaz_data<-read_xlsx(path = "UAZ_Refined.xlsx")
our_data<-read_xlsx(path = "AllMammalRecords_2021-23_Refined.xlsx")
old_data<-read_xlsx(path = "AllMammalRecords_pre2021_Refined.xlsx", guess_max = 2500)

old_data %>%
  filter(!grepl("UAZ .", old_data$catalogNumber)) %>%
  filter(collectionCode == "ASUMAC") %>%
  View()

