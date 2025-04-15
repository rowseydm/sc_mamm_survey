#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
#~#~# Steph's figure stuff: a supplement to "Elevation Data Exploration"#~#~#
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

# Note: data objects used here are loaded in Elevation Data Exploration.R

# DR Needed to run the following code chunk to be able to install ggord
#  options(repos = c(
#    fawda123 = 'https://fawda123.r-universe.dev',
#    CRAN = 'https://cloud.r-project.org'))
# 
# # Install ggord
# install.packages('ggord')


pacman::p_load(vegan, elevatr, ggbeeswarm, ggnewscale, ggord, ggplot2, ggrepel, grid, gridExtra, raster, reshape2, sf)
library(tidyverse)
library(ggord)
library(BiocManager)

# colors
# In this order: loud pink, berry, blue, olive green, light aqua
# c("#f178fa", "#62255c", "#4d6180", "#758150", "#bafdca")
cols <- c("#f178fa", "#62255c", "#416fb4", "#799943", "#bafdca")
colsplus <- c("#f178fa", "#62255c", "#416fb4", "#799943", "#bafdca", "#505050", "#959595", rep("#000000", 15))

#### Map Figures ####
# Read in Santa Catalinas area polygon
sancat <- st_read("../../Resources/Santa_Catalina_StudyArea_Polygon_FINAL.shp")
sancat <- st_set_crs(sancat, value = 4326) # This sets the CRS to WGS84

# Obtain elevation raster for the Santa Catalinas polygon
elevation_data <- elevatr::get_elev_raster(locations = sancat, z = 9, clip = "locations")
elevation_data <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data)[3] <- "elevation"
# remove rows of data frame with one or more NA's,using complete.cases
elevation_data <- elevation_data[complete.cases(elevation_data), ]

# Plot: elevation map: 2 versions. 
# Version 1, commented out: with new (our) data in pink and old in aqua
# Version 2, not commented out: all grey dots with black outlines
# To use one version, comment out the other!
allplot <- ggplot(data = sancat) +
  geom_sf() + 
  coord_sf(xlim = c(-110.9643, -110.5513), ylim = c(32.27945, 32.67916), expand = FALSE) +
  geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) + 
  #scale_fill_viridis_c(option = "plasma")+
  scale_fill_distiller(type = "seq",
                        direction = -1,
                        palette = "Greys")+
  #geom_point(data = old_data, mapping = aes(y = decimalLatitude, x = decimalLongitude), fill = "#bafdca", color = "#000000", pch = 21, size = 2) +
  #geom_point(data = our_data_unique, mapping = aes(y = decimalLatitude, x = decimalLongitude), fill = "#f178fa", color = "#000000", pch = 21, size = 2)+
  geom_point(data = sd_prune, mapping = aes(y = decimalLatitude, x = decimalLongitude), fill = "#d0d0d0", color = "#000000", pch = 21, size = 2)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(legend.position = "none")
  

# Plot: elevation map with our data colored by habitat
# you might need this: https://gradientdescending.com/how-to-use-multiple-color-scales-in-ggplot-with-ggnewscale/
newplot_hab <- ggplot(data = sancat) +
  geom_sf() + 
  coord_sf(xlim = c(-110.9643, -110.5513), ylim = c(32.27945, 32.67916), expand = FALSE) +
  geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) + 
  scale_fill_distiller(type = "seq",
                       direction = -1,
                       palette = "Greys")+
  new_scale_fill()+
  geom_point(data = our_data_unique, mapping = aes(y = decimalLatitude, x = decimalLongitude, fill = habitat), pch = 21, color = "#000000", size = 3)+
  scale_fill_manual(values = cols)+
  xlab("Longitude")+
  ylab("Latitude")

# function for extracting legend: https://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram 
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

# extract legend: you have to do this when you have the legend included in the plot, and then remove the legend from the final grid.arrange version.
legs <- g_legend(newplot_hab)

# Final plot with no legend for grid.arrange:
newplot_hab <- newplot_hab +
  theme(legend.position = "none") 

# plot two maps and legend
map_fig <- grid.arrange(allplot, newplot_hab, legs, nrow = 1)


#### Boxplot figures ####
# In Dakota's exploratory code, there is code for 7 different clade-based groupings of specimens:
# Heteromyidae, Cricetidae, Sciuridae, Carnivora, Artiodactyla+Lagomorpha, Eulipotyphla, and Chiroptera.
sd_prune %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN" & family %in% c("Heteromyidae", "Geomyidae")) %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN" & family == "Heteromyidae") %>%
  ggplot(mapping = aes(y = DEMElevationInMeters, x = scientificName)) +
  geom_boxplot() +
  theme_minimal()
  

#### Beeswarm plots ####
# These are made with two types of records: Preserved specimens and material samples. 

# Not all of the species we have include reps from both our data and historical data, but that's what we want to compare with our beeswarm plots. First, we have to make a version of the data only including species that are present in both old and new data sets. 

oldie.sp <- sd_prune %>% 
  filter(recordSource !="ASU post-2020") %>% 
  distinct(scientificName)

new.sp <- sd_prune %>% 
  filter(recordSource =="ASU post-2020") %>% 
  distinct(scientificName)

overlap <- intersect(oldie.sp, new.sp) # 15 species because we found R. fulvescens which is a new record

# Make a new object of only the species that both have, from the pruned object with all the data
both_prune <- sd_prune[grepl(paste(overlap$scientificName, collapse = "|"), sd_prune$scientificName),]
#check - there should be 15 species
length(unique(both_prune$scientificName)) # POW CRASH BANG!! It worked!

# Make a column for ASM or not in the pruned data. "new" is ASU post-2020, "old" is everything else.
both_prune <- both_prune %>% 
   mutate(newOrNot = recode(recordSource, `ASU post-2020` = "new", `non-UAZ pre-2021` = "old", `UAZ` = "old"))
  

# Now we make the beeswarm plots:
cri_plot <- both_prune %>% 
  filter(basisOfRecord == "PRESERVED_SPECIMEN"|basisOfRecord=="MATERIAL_SAMPLE") %>% 
  filter(order == "Rodentia"&family=="Cricetidae") %>% 
  ggplot(mapping = aes(x = DEMElevationInMeters, y = scientificName))+
  geom_quasirandom(aes(color = newOrNot), method = "pseudorandom", size = 2)+
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y")+
  #geom_beeswarm(aes(color = newOrNot), method = "hex")+
  scale_color_manual(values = c("#62255c", "#f178fa"))+
  labs(#title = "Elevational Range Comparisons",
       #subtitle = "Historical surveys vs. 2020s resurvey",
       x = "DEM Elevation (m)", y = "Species", color = "Record Type")

# get lengend

legs2 <- g_legend(cri_plot)
cri_plot <- cri_plot +
  theme(legend.position = "none")
  
notcri_plot <- both_prune %>% 
  filter(basisOfRecord == "PRESERVED_SPECIMEN"|basisOfRecord=="MATERIAL_SAMPLE") %>% 
  filter(family != "Cricetidae") %>% 
  ggplot(mapping = aes(x = DEMElevationInMeters, y = scientificName))+
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y")+
  geom_quasirandom(aes(color = newOrNot), method = "pseudorandom", size = 2)+
  #geom_beeswarm(aes(color = newOrNot), method = "swarm")+
  #geom_jitter(aes(color = newOrNot))+
  #geom_violin(aes(fill = newOrNot))
  scale_color_manual(values = c("#62255c", "#f178fa")) +
  labs(#title = "Elevational Range Comparisons",
       #subtitle = "Historical surveys vs. 2020s resurvey",
       x = "DEM Elevation (m)", y = "Species", color = "Record Type") +
  theme(legend.position = "none")

oldNewBees <- grid.arrange(notcri_plot, cri_plot, legs2,  nrow = 1, widths = c(2,2,0.5), top = textGrob("Elevational Range Comparisons: Historical vs. 2020s resurvey",gp=gpar(fontsize=20,font=1)))


                           

#### Corespondence analysis ####
# Make a combo item: burn + habitat, and prep the data frame to make it into a presence-absence matrix
burn <- our_data_unique %>% 
  mutate(burnStatus = recode(burnStatus, `Unburned` = "unburned")) %>% 
  mutate(habburn = paste(habitat, burnStatus, sep = " ")) %>% 
  dplyr::select(order, family, genus, scientificName, habburn)

# convert to presence-absence matrix
sp_pres_ab <- dcast(burn, habburn~scientificName, length) 
rownames(sp_pres_ab) <- sp_pres_ab$habburn


sp_pres_ab <- sp_pres_ab %>%
  dplyr::select(where(is.numeric)) %>% 
  mutate(across(everything(), ~ifelse(.x > 0, 1, 0)))


# Compute distance among habitats: 
dis <- vegdist(sp_pres_ab, method = "bray")
pcoa <- cmdscale(dis, k = 2, eig = T)

library(ca)
corsp <- ca(sp_pres_ab)
corsp_plot <- plot(corsp)


# Make your ca plot object into a thing you can put in ggplot. From here: https://www.r-bloggers.com/2019/08/correspondence-analysis-visualization-using-ggplot/ 

make.ca.plot.df <- function (ca.plot.obj,
                             row.lab = "Rows",
                             col.lab = "Columns") {
  df <- data.frame(Label = c(rownames(ca.plot.obj$rows),
                             rownames(ca.plot.obj$cols)),
                   Dim1 = c(ca.plot.obj$rows[,1], ca.plot.obj$cols[,1]),
                   Dim2 = c(ca.plot.obj$rows[,2], ca.plot.obj$cols[,2]),
                   Variable = c(rep(row.lab, nrow(ca.plot.obj$rows)),
                                rep(col.lab, nrow(ca.plot.obj$cols))))
  rownames(df) <- 1:nrow(df)
  df
}

# Shapes and colors. Shapes by family, colors by habitat.
col_corsp <- c("IntChapUB", "MadOakB", "MadOakUB", "PetConB", "PetConUB", "SDGrassUB", "EcotoneUB", rep("species", 15))
shape_corsp <- c(rep("B_hab", 7), "Sciuridae", rep("Heteromyidae", 3), "Sciuridae", rep("Cricetidae", 9), "Soricidae")

threehab <- as.data.frame(corsp$rowcoord[,3]) %>% 
  rename(Dim3 = "corsp$rowcoord[, 3]")
threesp <- as.data.frame(corsp$colcoord[,3]) %>% 
  rename(Dim3 = "corsp$colcoord[, 3]")
three <- rbind(threehab, threesp)
corsp_df <- make.ca.plot.df(corsp_plot, row.lab = "habitat", col.lab = "species")
corsp_df <-corsp_df %>% 
  mutate(Dim3 = as.numeric(three[,]), 
         shape = shape_corsp, 
         color = col_corsp)



# NB: This looks very crappy at the moment, maybe fix it up.
corsp_niceplot <- ggplot(corsp_df, mapping = aes(x = Dim2, y = Dim3, pch = shape, color = color, label = Label))+
  geom_point(cex = 4)+
  scale_color_manual(values = c("#f178fa", "#62255c", "#416fb4", "#799943", "#bafdca", "#505050", "#959595", rep("#000000", 15)))+
  geom_label_repel(cex =3)


corsp_niceplot




#~#~---S-A-N-D-B-O-X---~#~#-------------------
# testing out an AZ map strategy from here:
# https://rspatialdata.github.io/elevation.html
usa <- rgeoboundaries::geoboundaries("United States of America", adm_lvl = "adm1", type = "simplified")
az <- usa[usa$shapeName=="Arizona",]
az_elev <- elevatr::get_elev_raster(locations = az, z = 9, clip = "locations")

az_elev <- as.data.frame(az_elev, xy = TRUE)
colnames(az_elev)[3] <- "elevation"
# remove rows of data frame with one or more NA's,using complete.cases
az_elev <- az_elev[complete.cases(az_elev),]
ggplot() +
  geom_raster(data = az_elev, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = az, color = "white", fill = NA) +
  geom_sf(data = sancat, color = "white", fill = NA) +
  coord_sf() +
  scale_fill_distiller(type = "seq",
                       direction = -1,
                       palette = "Greys")+
  labs(title = "Elevation in Arizona", x = "Longitude", y = "Latitude", fill = "Elevation (meters)")
