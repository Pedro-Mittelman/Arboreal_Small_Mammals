#Stacked Occupancy model edible dormouse 3 seasosn

#packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(unmarked)


#filtering only edible dormouse records
data.glis.occu <-  filter(camera_data_ta, animal_species=="Glis glis")

#filter by 2023 where we had all plots and southern sites where we found Glis glis
data.glis.occu <- data.glis.occu %>% filter(year=="2023", Site_location=="Southern")

# Ensure Date column is in Date format
data.glis.occu$full_date <- as.Date(data.glis.occu$full_date, format = "%Y-%m-%d")
camera_data_ta$full_date <- as.Date(camera_data_ta$full_date, format = "%Y-%m-%d")

# Define the start and end date for the survey period
camera_data_ta2023s <- camera_data_ta %>% filter(year=="2023",Site_location=="Southern")
start_date <- min(camera_data_ta2023s$full_date)
end_date <- max(data.glis.occu$full_date)


# Create weekly intervals
survey_occasions <- seq.Date(start_date, end_date, by = "7 days")

# Add a column for survey occasion
data.glis.occu <- data.glis.occu %>%
  mutate(Occasion = findInterval(full_date, survey_occasions))

# Get unique sites and plot types
all.trees_glis <- camera_data_ta %>% filter(year=="2023", Site_location=="Southern") %>% select (id_occasion)
all.trees_glis <- as.factor(unique( all.trees_glis$id_occasion))
all_sites_glis <- levels(data.glis.occu$id_plot)
#sites_glis <- unique(data.glis.occu$id_plot)
plot_types <- data.frame(id_plot=all_sites_glis,
                         plot_type= as.factor(substr(all_sites_glis,2,2)))

# Create an empty matrix for detection/non-detection data
detection_data.glis.occu <- matrix(0, nrow = length(all.trees_glis), ncol = length(survey_occasions))
rownames(detection_data.glis.occu) <- all.trees_glis

# Fill the matrix with detections
for (i in 1:nrow(data.glis.occu)) {
  site <- data.glis.occu$id_occasion[i]
  occasion <- data.glis.occu$Occasion[i]
  detection_data.glis.occu[as.character(site), occasion] <- 1
}


# Convert matrix to data frame
detection_data.glis.occu <- as.data.frame(detection_data.glis.occu)
#make sure the order of occasion remain the same (aplphabeitcal)
# important for when joining with site covariable
detection_data.glis.occu <-detection_data.glis.occu[order(rownames(detection_data.glis.occu)), ]

## adding start and end occasions for each tree/camera
startdfs<- camera_data_ta2023s %>% filter(animal_species=="Start start")
startdfs$full_date <- as.Date(startdfs$full_date, format = "%Y-%m-%d")
startdfs <- startdfs %>%
  mutate(Occasion = findInterval(full_date, survey_occasions))

#change non surveyed dates to NA
startdfsc <- startdfs %>% filter(Occasion>1)

for (i in 1:length(startdfsc$id_occasion)) {
  detection_data.glis.occu[startdfsc$id_occasion[i],1:(startdfsc$Occasion[i]-1)]  <- NA
}
#End
enddfs<- camera_data_ta2023s %>% filter(animal_species=="End end")
enddfs$full_date <- as.Date(enddfs$full_date, format = "%Y-%m-%d")
enddfs <- enddfs %>%
  mutate(Occasion = findInterval(full_date, survey_occasions))

#change non surveyed dates to NA
max.endoccasion <- max(enddfs$Occasion)
enddfsc <- enddfs %>% filter(Occasion<max.endoccasion)

for (i in 1:length(enddfsc$id_occasion)) {
  detection_data.glis.occu[enddfsc$id_occasion[i],(enddfsc$Occasion[i]+1):max.endoccasion]  <- NA
}


# #adding weeks of Nas because each season must have the same number of weeks
# detection_data.glis.occu[,29:30] <- NA
detection_data.glis.occu[,27:30] <- NA
spring_glis2 <- detection_data.glis.occu[, 1:10]
summer_glis2 <- detection_data.glis.occu[, 11:20]
autumn_glis2 <- detection_data.glis.occu[, 21:30]


# Add site names to each season
sites <- rownames(spring_glis2)

spring_glis2_df <- spring_glis2 %>%
  mutate(site = sites, season = "spring_glis2")

summer_glis2_df <- summer_glis2 %>%
  mutate(site = sites, season = "summer_glis2")

autumn_glis2_df <- autumn_glis2 %>%
  mutate(site = sites, season = "autumn_glis2")

# Stack the three seasons together
stacked_glis2 <- bind_rows(spring_glis2_df, summer_glis2_df, autumn_glis2_df)

# Create a unique ID for each site-season combination
stacked_glis2$site_season <- paste0(stacked_glis2$site, "_", stacked_glis2$season)

# Move ID columns to the front
stacked_glis2 <- stacked_glis2 %>% relocate(site_season, site, season)


# Extract id_occasion from rownames
spring_glis2_df$id_occasion <- rownames(spring_glis2)
summer_glis2_df$id_occasion <- rownames(summer_glis2)
autumn_glis2_df$id_occasion <- rownames(autumn_glis2)

# Stack and keep id_occasion
stacked_glis2 <- bind_rows(spring_glis2_df, summer_glis2_df, autumn_glis2_df) %>%
  mutate(season = as.factor(season),
         site_season = paste0(id_occasion, "_", season))


site_covariates <- camera_data_ta %>% filter(year=="2023", Site_location=="Southern") %>% group_by(id_occasion, id_plot,plot_type, platform, tree_species_c, Site, Site_location) %>%
  summarise(beech_prop_ba=mean(beech_prop_ba),spruce_prop_ba=mean(spruce_prop_ba),douglas_prop_ba=mean(douglas_prop_ba),
            mean(effort_days), Height= mean(Height),DBH=mean(DBH), mean(Max_tree_height),Crown_radius=mean(Crown_radius95),
            Mass_branches=mean(Mass_branches), mean(NDIV), beech_quad=(mean(beech_prop_ba)^2),s.age=mean(s.age_2025))
site_covariates$beech_quad <- (scale(site_covariates$beech_prop_ba)[,1])^2
site_covariates$Site <- droplevels(site_covariates$Site)
site_covariates$platform <- as.factor(as.character(site_covariates$platform))
site_covariates$id_occasion <- as.factor((site_covariates$id_occasion))


# Join site-level covariates
stacked_glis2 <- left_join(stacked_glis2, site_covariates, by = c("id_occasion" = "id_occasion"))


# Detection matrix (drop non-detection columns)
detection_matrix_glis2 <- stacked_glis2 %>%
  select(starts_with("V")) %>%  # or select those 30 weekly columns if they don’t start with "X"
  as.matrix()

# Build site covariate table
site_covs_glis2 <- stacked_glis2 %>%
  select(id_plot,site_season, season, plot_type, tree_species_c, platform, Site, Site_location,
         beech_prop_ba, spruce_prop_ba, douglas_prop_ba, Height, DBH, Crown_radius,
         Mass_branches, s.age) %>%
  distinct()

# Set rownames to match the detection matrix
rownames(site_covs_glis2) <- site_covs_glis2$site_season
