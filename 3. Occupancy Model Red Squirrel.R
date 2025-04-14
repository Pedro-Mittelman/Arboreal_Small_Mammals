#Occupancy Model Red Squirrel


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


#Site covariates
site_covariates <- camera_data_ta %>% filter(year=="2023") %>% group_by(id_occasion, id_plot,plot_type,
                                                                                        tree_species_c,
                                                                                        platform,
                                                                                        Site, Site_location) %>% 
  summarise(beech_prop_ba=mean(beech_prop_ba),spruce_prop_ba=mean(spruce_prop_ba),douglas_prop_ba=mean(douglas_prop_ba),
            mean(effort_days), Height= mean(Height), DBH=mean(DBH), mean(Max_tree_height),Crown_radius=mean(Crown_radius95),
            Mass_branches=mean(Mass_branches), mean(NDIV), s.age=mean(s.age_2025)) 
site_covariates$platform <- as.factor(as.character(site_covariates$platform))
site_covariates$id_occasion <- as.factor((site_covariates$id_occasion))

str(site_covariates)
# # Merge detection data with plot types
# detection_data.sciurus.occu$id_plot <- rownames(detection_data.sciurus.occu)
# detection_data.sciurus.occu <- detection_data.sciurus.occu %>%
#   left_join(plot_types, by = "id_plot")

# Create unmarkedFrameOccu object
umf.sciurus.occu <- unmarkedFrameOccu(y = detection_data.sciurus.occu, siteCovs = site_covariates)

# # Create unmarkedFrameOccu object
# umf.sciurus.occu <- unmarkedFrameOccu(y = detection_data.sciurus.occu[, -ncol(detection_data.sciurus.occu)], 
#                          siteCovs = detection_data.sciurus.occu[, c("id_plot", "plot_type")])

# Fit the Occupancy Model
#finding best model with beech
# Full model with id_plot as random effect
model1befull2 <- occu(~ DBH + Mass_branches+ Height + Crown_radius + s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + tree_species_c + s.age + (1|id_plot)  , data = umf.sciurus.occu)
summary(model1befull2)

#removing non-significant detection variables one by one by least significant
modelsbe1 <- occu(~ DBH +Mass_branches+ Height  + s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + tree_species_c + s.age + (1|id_plot)  , data = umf.sciurus.occu)
summary(modelsbe1)

modelsbe2 <- occu(~ DBH +Mass_branches  + s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + tree_species_c + s.age + (1|id_plot)  , data = umf.sciurus.occu)
summary(modelsbe2)

modelsbe3 <- occu(~ Mass_branches  + s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + tree_species_c + s.age + (1|id_plot)  , data = umf.sciurus.occu)
summary(modelsbe3)

modelsbe4 <- occu(~  s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + tree_species_c + s.age + (1|id_plot)  , data = umf.sciurus.occu)
summary(modelsbe4)

modelsbe5 <- occu(~  tree_species_c + beech_prop_ba ~ beech_prop_ba + tree_species_c + s.age + (1|id_plot)  , data = umf.sciurus.occu)
summary(modelsbe5)

#removing non significant occupancy variables
modelsbe6 <- occu(~  tree_species_c + beech_prop_ba ~ beech_prop_ba + tree_species_c  + (1|id_plot)  , data = umf.sciurus.occu)
summary(modelsbe6)

modelsbe7 <- occu(~  tree_species_c + beech_prop_ba ~ beech_prop_ba +  (1|id_plot)  , data = umf.sciurus.occu)
summary(modelsbe7)
# tree species affect detection ( but it did not affect occupancy)

#selecting only significant variables

modelsbest <- modelsbe7
summary(modelsbest)
#best model

#using Site as random structure
modelsbe7s <- occu(~  tree_species_c + beech_prop_ba ~ beech_prop_ba +  (1|Site)  , data = umf.sciurus.occu)
summary(modelsbe7s)
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
