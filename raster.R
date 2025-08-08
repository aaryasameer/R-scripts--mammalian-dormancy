####spatial distribution
library(sf)       # for reading shapefiles
library(dplyr) 

iucn <- st_read("C:\\Users\\AARYA\\Downloads\\MAMMALS_TERRESTRIAL_ONLY\\MAMMALS_TERRESTRIAL_ONLY.shp")

library(readxl)
dormant_data <- read_excel("C:\\Users\\AARYA\\Downloads\\kissling_modified_dataset.xlsx")
head(dormant_data$Species_SHP.x)
names(iucn)

iucn$sci_name <- trimws(iucn$sci_name)
dormant_data$Species_SHP.x <- trimws(dormant_data$Species_SHP.x)

iucn$sci_name <- tolower(iucn$sci_name)
dormant_data$Species_SHP.x <- tolower(dormant_data$Species_SHP.x)

iucn_dormant <- iucn %>%
  filter(sci_name %in% dormant_data$Species_SHP.x)
nrow(iucn_dormant)      # number of matching polygons
length(unique(iucn_dormant$sci_name))  # number of unique dormant species

library(terra)
# Convert sf to SpatVector (terra format)
iucn_vect <- vect(iucn_dormant)

# Reproject to Behrmann projection
iucn_behrmann <- project(iucn_vect, "ESRI:54017")

# Create an empty raster with 2° resolution in Behrmann projection
r_template <- rast(ext(iucn_behrmann), res = 220000, crs = "ESRI:54017")

# Optional: create one raster layer per species
r_stack <- rasterize(iucn_behrmann[iucn_behrmann$sci_name == species_list[1], ],
                     r_template, field = 1, touches = TRUE)
names(r_stack) <- gsub(" ", "_", species_list[1])

for (sp in species_list[-1]) {
  sp_shape <- iucn_behrmann[iucn_behrmann$sci_name == sp, ]
  sp_raster <- rasterize(sp_shape, r_template, field = 1, touches = TRUE)
  names(sp_raster) <- gsub(" ", "_", sp)
  r_stack <- c(r_stack, sp_raster)
}

# One raster layer showing how many species fall in each cell
richness_raster <- rasterize(iucn_behrmann, r_template, fun = "sum", field = 1)

plot(richness_raster, main = "Dormant Mammal Richness (Behrmann Projection)")

##CARNIVORES
carnivores <- dormant_data %>%
  filter(TrophicLevel == "Carnivore")

length(unique(carnivores$Species_SHP.x))

# Subset polygons
iucn_carnivores <- iucn_dormant %>%
  filter(sci_name %in% carnivores$Species_SHP.x)

# Convert to SpatVector
iucn_carn_v <- vect(iucn_carnivores)

iucn_carn_behr <- project(iucn_carn_v, "ESRI:54017")

# Rasterize into the existing 2° grid
r_carnivores <- rasterize(iucn_carn_behr, r_template, fun = "sum", field = 1)

plot(r_carnivores, main = "Dormant Carnivore Species Richness")

###HERBIVORES

herbivores <- dormant_data %>%
  filter(TrophicLevel == "Herbivore")

length(unique(herbivores$Species_SHP.x))

# Subset polygons
iucn_herbivores <- iucn_dormant %>%
  filter(sci_name %in% herbivores$Species_SHP.x)

# Convert to SpatVector
iucn_herb_v <- vect(iucn_herbivores)

# Reproject to Behrmann
iucn_herb_behr <- project(iucn_herb_v, "ESRI:54017")

# Rasterize into the 2° template
r_herbivores <- rasterize(iucn_herb_behr, r_template, fun = "sum", field = 1)

# Plot richness
plot(r_herbivores, main = "Dormant Herbivore Species Richness")

###OMNIVORE
omnivores <- dormant_data %>%
  filter(TrophicLevel == "Omnivore")

# Number of unique omnivore species
length(unique(omnivores$Species_SHP.x))

# Subset polygons
iucn_omnivores <- iucn_dormant %>%
  filter(sci_name %in% omnivores$Species_SHP.x)

# Convert to SpatVector
iucn_omn_v <- vect(iucn_omnivores)

# Reproject to Behrmann
iucn_omn_behr <- project(iucn_omn_v, "ESRI:54017")

# Rasterize into the 2° template
r_omnivores <- rasterize(iucn_omn_behr, r_template, fun = "sum", field = 1)

# Plot richness
plot(r_omnivores, main = "Dormant Omnivore Species Richness")


