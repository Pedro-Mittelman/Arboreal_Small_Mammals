#Occupancy model edible dormouse

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

#Site covariates
site_covariates <- camera_data_ta %>% filter(year=="2023", Site_location=="Southern") %>% group_by(id_occasion, id_plot,plot_type, platform, tree_species_c, Site, Site_location) %>% 
  summarise(beech_prop_ba=mean(beech_prop_ba),spruce_prop_ba=mean(spruce_prop_ba),douglas_prop_ba=mean(douglas_prop_ba),
            mean(effort_days), Height= mean(Height),DBH=mean(DBH), mean(Max_tree_height),Crown_radius=mean(Crown_radius95),
            Mass_branches=mean(Mass_branches), mean(NDIV), beech_quad=(mean(beech_prop_ba)^2),s.age=mean(s.age_2025)) 
site_covariates$beech_quad <- (scale(site_covariates$beech_prop_ba)[,1])^2
site_covariates$Site <- droplevels(site_covariates$Site)
site_covariates$platform <- as.factor(as.character(site_covariates$platform))
site_covariates$id_occasion <- as.factor((site_covariates$id_occasion))

# Create unmarkedFrameOccu object
umf.glis.occu <- unmarkedFrameOccu(y = detection_data.glis.occu, siteCovs = site_covariates)



#FULL MODEL
modelglisfull <- occu(~ DBH +Mass_branches+ Height + Crown_radius +s.age +tree_species_c + beech_prop_ba ~ beech_prop_ba + tree_species_c +s.age +(1|id_plot), data = umf.glis.occu)
summary(modelglisfull)
#model failed to converge, too many variables, removing least significnat ones

modelglisfull2 <- occu(~ DBH +Mass_branches+ Height + Crown_radius +s.age + beech_prop_ba ~ beech_prop_ba + tree_species_c +s.age +(1|id_plot), data = umf.glis.occu)
summary(modelglisfull2)

##removing non-significant detection variables one by one, startng by least significant
modelglis1 <- occu(~ DBH +Mass_branches+ Height +s.age + beech_prop_ba ~ beech_prop_ba + tree_species_c +s.age +(1|id_plot), data = umf.glis.occu)
summary(modelglis1)

modelglis2<- occu(~ DBH + Height +s.age + beech_prop_ba ~ beech_prop_ba + tree_species_c +s.age +(1|id_plot), data = umf.glis.occu)
summary(modelglis2)

##removing non-significant occupancy variables one by one, starting by least significant
modelglis3<- occu(~ DBH + Height +s.age + beech_prop_ba ~ beech_prop_ba  +s.age +(1|id_plot), data = umf.glis.occu)
summary(modelglis3)

modelglis4<- occu(~ DBH + Height +s.age + beech_prop_ba ~ beech_prop_ba  +(1|id_plot), data = umf.glis.occu)
summary(modelglis4)

modelgbest <- modelglis4
summary(modelgbest)
#best model
# positive, marginaly siginifcant impact of beech ba on Glis glis


#using site as random factor
modelglis4s<- occu(~ DBH + Height +s.age + beech_prop_ba ~ beech_prop_ba  +(1|Site), data = umf.glis.occu)
summary(modelglis4s)


#plotting
beech_prop_ba_range <- 0:1
beech_prop_ba_seq <- seq(beech_prop_ba_range[1], beech_prop_ba_range[2], length.out=101)
nd <- data.frame(beech_prop_ba = rep(beech_prop_ba_seq,3))
#nd$tree_species_c <- c(rep("Dougl",101),rep("Beech",101),rep("Spruc",101))
occ_beech_prop_ba_gliss <- predict(modelgbest, type="state", newdata=nd, re.form=NA)
occ_beech_prop_ba_gliss$Species <- "Glis glis"
occ_beech_prop_ba_gliss$beech_prop_ba <- rep(beech_prop_ba_seq,3)
#occ_beech_prop_ba_gliss_p <- cbind(nd,occ_beech_prop_ba_gliss)




ggplot(data= occ_beech_prop_ba_gliss, aes(x=beech_prop_ba, y=Predicted))+
  geom_line(linewidth=2, linetype="twodash")+
  ylim(0,1)+
  geom_ribbon( aes(ymin = lower, ymax = upper), fill="#619CFF",alpha = 0.1)+
  labs(x="Beech proportional basal area", y="Occupancy")+
  theme_minimal()


summary(modelgbest)
#best model

