#Stacked occupancy model red squrrel



#packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(unmarked)
library(broom)

#filtering only squirrel records
data.sciurus.occu <-  filter(camera_data_ta, animal_species=="Sciurus vulgaris")

#only using 2023 data, where we had all plots
data.sciurus.occu <- data.sciurus.occu %>% filter(year=="2023")

table.sciurus <- data.sciurus.occu %>% group_by(plot_type,) %>% summarise(n()) 
table.sciurus2 <- data.sciurus.occu %>% group_by(id_plot) %>% summarise(n()) 

table.sciurus <- data.sciurus.occu %>% group_by(plot_type,id_occasion,Date_m_d) %>% summarise(n()) %>% 
  group_by(plot_type) %>% summarise(n())


# Ensure Date column is in Date format
data.sciurus.occu$full_date <- as.Date(data.sciurus.occu$full_date, format = "%Y-%m-%d")
camera_data_ta$full_date <- as.Date(camera_data_ta$full_date, format = "%Y-%m-%d")
# data.sciurus.occu$full_date <- as.Date(data.sciurus.occu$full_date, format = "%d/%m/%Y")
# camera_data_ta$full_date <- as.Date(camera_data_ta$full_date, format = "%d/%m/%y")

# Define the start and end date for the survey period
camera_data_ta2023eff20 <- camera_data_ta %>% filter(year=="2023")
start_date <- min(camera_data_ta2023eff20$full_date)
end_date <- max(data.sciurus.occu$full_date)


# Create weekly intervals
survey_occasions <- seq.Date(start_date, end_date, by = "7 days")

# Add a column for survey occasion
data.sciurus.occu <- data.sciurus.occu %>%
  mutate(Occasion = findInterval(full_date, survey_occasions))

# Get unique sites and plot types
all.trees <- camera_data_ta %>% filter(year=="2023") %>% select (id_occasion)
all.trees <- as.factor(unique( all.trees$id_occasion))
all_sites <- levels(data.sciurus.occu$id_plot)
#sites <- unique(data.sciurus.occu$id_plot)
plot_types <- data.frame(id_plot=all_sites,
                         plot_type= as.factor(substr(all_sites,2,2)))

# Create an empty matrix for detection/non-detection data
detection_data.sciurus.occu <- matrix(0, nrow = length(all.trees), ncol = length(survey_occasions))
rownames(detection_data.sciurus.occu) <- all.trees

# Fill the matrix with detections
for (i in 1:nrow(data.sciurus.occu)) {
  site <- data.sciurus.occu$id_occasion[i]
  occasion <- data.sciurus.occu$Occasion[i]
  detection_data.sciurus.occu[as.character(site), occasion] <- 1
}


# Convert matrix to data frame
detection_data.sciurus.occu <- as.data.frame(detection_data.sciurus.occu)
#make sure the order of occasion remain the same (aplphabeitcal)
# important for when joining with site covariable
detection_data.sciurus.occu <-detection_data.sciurus.occu[order(rownames(detection_data.sciurus.occu)), ]

## adding start and end occasions for each tree/camera so the rest is filled with NAs
startdf<- camera_data_ta2023eff20 %>% filter(animal_species=="Start start")
startdf$full_date <- as.Date(startdf$full_date, format = "%Y-%m-%d")
startdf <- startdf %>%
  mutate(Occasion = findInterval(full_date, survey_occasions))

#change non surveyed dates to NA
startdfc <- startdf %>% filter(Occasion>1)

for (i in 1:length(startdfc$id_occasion)) {
  
  detection_data.sciurus.occu[startdfc$id_occasion[i],1:(startdfc$Occasion[i]-1)]  <- NA
}
#End
enddf<- camera_data_ta2023eff20 %>% filter(animal_species=="End end")
enddf$full_date <- as.Date(enddf$full_date, format = "%Y-%m-%d")
enddf <- enddf %>%
  mutate(Occasion = findInterval(full_date, survey_occasions))

#change non surveyed dates to NA
max.endoccasion <- max(enddf$Occasion)
enddfc <- enddf %>% filter(Occasion<max.endoccasion)

for (i in 1:length(enddfc$id_occasion)) {
  
  detection_data.sciurus.occu[enddfc$id_occasion[i],(enddfc$Occasion[i]+1):max.endoccasion]  <- NA
}
#starting at first detection
# detection_data.sciurus.occu <- detection_data.sciurus.occu[,2:29]
# 
# # assuming detection_matrix_sciurus has 29 columns
# #adding weeks of Nas because each season must have the same number of weeks
# detection_data.sciurus.occu[,29:30] <- NA
detection_data.sciurus.occu[,30] <- NA
spring_sciurus <- detection_data.sciurus.occu[, 1:10]
summer_sciurus <- detection_data.sciurus.occu[, 11:29]
autumn_sciurus <- detection_data.sciurus.occu[, 21:30]


# Add site names to each season
sites <- rownames(spring_sciurus)

spring_sciurus_df <- spring_sciurus %>% 
  mutate(site = sites, season = "spring_sciurus")

summer_sciurus_df <- summer_sciurus %>% 
  mutate(site = sites, season = "summer_sciurus")

autumn_sciurus_df <- autumn_sciurus %>% 
  mutate(site = sites, season = "autumn_sciurus")

# Stack the three seasons together
stacked_sciurus <- bind_rows(spring_sciurus_df, summer_sciurus_df, autumn_sciurus_df)

# Create a unique ID for each site-season combination
stacked_sciurus$site_season <- paste0(stacked_sciurus$site, "_", stacked_sciurus$season)

# Move ID columns to the front
stacked_sciurus <- stacked_sciurus %>% relocate(site_season, site, season)


# Extract id_occasion from rownames
spring_sciurus_df$id_occasion <- rownames(spring_sciurus)
summer_sciurus_df$id_occasion <- rownames(summer_sciurus)
autumn_sciurus_df$id_occasion <- rownames(autumn_sciurus)

# Stack and keep id_occasion
stacked_sciurus <- bind_rows(spring_sciurus_df, summer_sciurus_df, autumn_sciurus_df) %>%
  mutate(season = as.factor(season),
         site_season = paste0(id_occasion, "_", season))

# #Site covariates
site_covariates <- camera_data_ta %>% filter(year=="2023") %>% group_by(id_occasion, id_plot,plot_type,
                                                                        tree_species_c,
                                                                        platform,
                                                                        Site, Site_location) %>% 
  summarise(beech_prop_ba=mean(beech_prop_ba),spruce_prop_ba=mean(spruce_prop_ba),douglas_prop_ba=mean(douglas_prop_ba),
            mean(effort_days), Height= mean(Height), DBH=mean(DBH), mean(Max_tree_height),Crown_radius=mean(Crown_radius95),
            Mass_branches=mean(Mass_branches), mean(NDIV), s.age=mean(s.age_2025)) 
site_covariates$platform <- as.factor(as.character(site_covariates$platform))
site_covariates$id_occasion <- as.factor((site_covariates$id_occasion))

# Join site-level covariates
stacked_sciurus <- left_join(stacked_sciurus, site_covariates, by = c("id_occasion" = "id_occasion"))
stacked_sciurus_sciurus <- stacked_sciurus

# Detection matrix (drop non-detection columns)
detection_matrix_sciurus <- stacked_sciurus %>%
  select(starts_with("V")) %>%  # or select those 30 weekly columns if they don’t start with "X"
  as.matrix()

# Build site covariate table
site_covs_sciurus <- stacked_sciurus %>%
  select(id_plot,site_season, season, plot_type, tree_species_c, platform, Site, Site_location,
         beech_prop_ba, spruce_prop_ba, douglas_prop_ba, Height, DBH, Crown_radius,
         Mass_branches, s.age) %>%
  distinct()

# Set rownames to match the detection matrix
rownames(site_covs_sciurus) <- site_covs_sciurus$site_season

#Create unmarkedFrameOccu

umf.sciurus.occu_stacked_sciurus <- unmarkedFrameOccu(
  y = detection_matrix_sciurus,
  siteCovs = site_covs_sciurus %>% select(-site_season)
)

# Fit the Occupancy Model
#finding best model with beech
# Full model with id_plot as random effect
model1befull2 <- occu(~ DBH + Mass_branches+ Height + Crown_radius + s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + tree_species_c + s.age + (1|id_plot)  , data = umf.sciurus.occu_stacked_sciurus)
summary(model1befull2)

#removing non-significant detection variables one by one by least significant
modelsbe1 <- occu(~ DBH +Mass_branches+ Height  + s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + (1|id_plot)  , data = umf.sciurus.occu_stacked_sciurus)
summary(modelsbe1)

modelsbe2 <- occu(~ Mass_branches+ Height  + s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + (1|id_plot)  , data = umf.sciurus.occu_stacked_sciurus)
summary(modelsbe2)

modelsbe3 <- occu(~  Height  + s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + (1|id_plot)  , data = umf.sciurus.occu_stacked_sciurus)
summary(modelsbe3)

modelsbe4 <- occu(~  s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + (1|id_plot)  , data = umf.sciurus.occu_stacked_sciurus)
summary(modelsbe4)


#selecting only significant variables

modelsbest <- modelsbe4
summary(modelsbest)
#best model

#using Site as random structure
modelsbe4s <- occu(~  s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + (1|Site)  , data = umf.sciurus.occu_stacked_sciurus)
summary(modelsbe4s)
#same results


#plotting
beech_prop_ba_range <- 0:1
beech_prop_ba_seq <- seq(beech_prop_ba_range[1], beech_prop_ba_range[2], length.out=101)
nd <- data.frame(beech_prop_ba = rep(beech_prop_ba_seq,3))
#nd$tree_species_c <- c(rep("Dougl",101),rep("Beech",101),rep("Spruc",101))
occ_beech_prop_ba_sciss <- predict(modelsbest, type="state", newdata=nd, re.form=NA)
occ_beech_prop_ba_sciss$Species <- "Sciurus"
occ_beech_prop_ba_sciss$beech_prop_ba <- rep(beech_prop_ba_seq,3)
#occ_beech_prop_ba_sciss_p <- cbind(nd,occ_beech_prop_ba_sciss)



ggplot(data= occ_beech_prop_ba_sciss, aes(x=beech_prop_ba, y=Predicted))+
  geom_line(linewidth=2)+
  ylim(0,1)+
  geom_ribbon( aes(ymin = lower, ymax = upper, fill=Species), fill="#F8766D", alpha = 0.1)+
  labs(x="Beech proportional basal area", y="Occupancy")+
  theme_minimal()


summary(modelsbest)
#best model