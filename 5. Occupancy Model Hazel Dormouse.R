#Ocuppancy model hazel dormouse

#packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(unmarked)

#filtering only hazel dormouse records
data.muscardinus.occu <-  filter(camera_data_ta, animal_species=="Muscardinus avellanarius")

#filter by 2023 where we had all plots and southern sites where we found muscardinus muscardinus
data.muscardinus.occu <- data.muscardinus.occu %>% filter(year=="2023", Site_location=="Southern")

table.mus <- data.muscardinus.occu %>% group_by(plot_type) %>% summarise(n())
table.mus2 <- data.muscardinus.occu %>% group_by(id_plot) %>% summarise(n())


# Ensure Date column is in Date format
data.muscardinus.occu$full_date <- as.Date(data.muscardinus.occu$full_date, format = "%Y-%m-%d")
camera_data_ta$full_date <- as.Date(camera_data_ta$full_date, format = "%Y-%m-%d")

# Define the start and end date for the survey period
camera_data_ta2023s <- camera_data_ta %>% filter(year=="2023",Site_location=="Southern")
start_date <- min(camera_data_ta2023s$full_date)
end_date <- max(data.muscardinus.occu$full_date)


# Create weekly intervals
survey_occasions <- seq.Date(start_date, end_date, by = "7 days")

# Add a column for survey occasion
data.muscardinus.occu <- data.muscardinus.occu %>%
  mutate(Occasion = findInterval(full_date, survey_occasions))

# #fixing occasions because it was not a continous survey gap between 2022 end and 2023 start
# data.muscardinus.occu$Occasion <- if_else(data.muscardinus.occu$year=="2022",data.muscardinus.occu$Occasion, as.integer( data.muscardinus.occu$Occasion-23))

# Get unique sites and plot types
all.trees_muscardinus <- camera_data_ta %>% filter(year=="2023", Site_location=="Southern") %>% select (id_occasion)
all.trees_muscardinus <- as.factor(unique( all.trees_muscardinus$id_occasion))
all_sites_muscardinus <- levels(data.muscardinus.occu$id_plot)
#sites_muscardinus <- unique(data.muscardinus.occu$id_plot)
plot_types <- data.frame(id_plot=all_sites_muscardinus,
                         plot_type= as.factor(substr(all_sites_muscardinus,2,2)))

# Create an empty matrix for detection/non-detection data
detection_data.muscardinus.occu <- matrix(0, nrow = length(all.trees_muscardinus), ncol = length(survey_occasions))
rownames(detection_data.muscardinus.occu) <- all.trees_muscardinus

# Fill the matrix with detections
for (i in 1:nrow(data.muscardinus.occu)) {
  site <- data.muscardinus.occu$id_occasion[i]
  occasion <- data.muscardinus.occu$Occasion[i]
  detection_data.muscardinus.occu[as.character(site), occasion] <- 1
}


# Convert matrix to data frame
detection_data.muscardinus.occu <- as.data.frame(detection_data.muscardinus.occu)
#make sure the order of occasion remain the same (aplphabeitcal)
# important for when joining with site covariable
detection_data.muscardinus.occu <-detection_data.muscardinus.occu[order(rownames(detection_data.muscardinus.occu)), ]


## adding start and end occasions for each tree/camera
startdfs<- camera_data_ta2023s %>% filter(animal_species=="Start start")
startdfs$full_date <- as.Date(startdfs$full_date, format = "%Y-%m-%d")
startdfs <- startdfs %>%
  mutate(Occasion = findInterval(full_date, survey_occasions))

#change non surveyed dates to NA
startdfsc <- startdfs %>% filter(Occasion>1)

for (i in 1:length(startdfsc$id_occasion)) {
  detection_data.muscardinus.occu[startdfsc$id_occasion[i],1:(startdfsc$Occasion[i]-1)]  <- NA
}
#End
enddfs<- camera_data_ta2023s %>% filter(animal_species=="End end")
enddfs$full_date <- as.Date(enddfs$full_date, format = "%Y-%m-%d")
enddfs <- enddfs %>%
  mutate(Occasion = findInterval(full_date, survey_occasions))

##change non surveyed dates to NA
max.endoccasion <- max(enddfs$Occasion)
enddfsc <- enddfs %>% filter(Occasion<max.endoccasion)

for (i in 1:length(enddfsc$id_occasion)) {
  detection_data.muscardinus.occu[enddfsc$id_occasion[i],(enddfsc$Occasion[i]+1):max.endoccasion]  <- NA
}

#Site covariates
site_covariates <- camera_data_ta %>% filter(year=="2023", Site_location=="Southern") %>% group_by(id_occasion, id_plot,plot_type, platform, tree_species_c, Site, Site_location) %>% 
  summarise(beech_prop_ba=mean(beech_prop_ba),spruce_prop_ba=mean(spruce_prop_ba),douglas_prop_ba=mean(douglas_prop_ba),
            mean(effort_days), Height= mean(Height),DBH=mean(DBH), mean(Max_tree_height),Crown_radius=mean(Crown_radius95),
            Mass_branches=mean(Mass_branches), mean(NDIV), s.age=mean(s.age_2025)) 
site_covariates$Site <- droplevels(site_covariates$Site)
site_covariates$platform <- as.factor(as.character(site_covariates$platform))
site_covariates$id_occasion <- as.factor((site_covariates$id_occasion))

str(site_covariates)


# Create unmarkedFrameOccu object
umf.muscardinus.occu <- unmarkedFrameOccu(y = detection_data.muscardinus.occu, siteCovs = site_covariates)
                

# Fit the Occupancy Model
#Full Model with id_plot as random effect
modelmusfull <- occu(~ DBH +Mass_branches+ Height + s.age + Crown_radius + tree_species_c + beech_prop_ba ~ beech_prop_ba + tree_species_c +s.age + (1|id_plot) , data = umf.muscardinus.occu)
summary(modelmusfull)

###removing non-significant detection variables one by one, starting by least significant
modelmus1 <- occu(~ DBH +Mass_branches+ Height + s.age + Crown_radius + tree_species_c ~ beech_prop_ba + tree_species_c +s.age + (1|id_plot) , data = umf.muscardinus.occu)
summary(modelmus1)

modelmus2 <- occu(~ Mass_branches+ Height + s.age + Crown_radius + tree_species_c ~ beech_prop_ba + tree_species_c +s.age + (1|id_plot) , data = umf.muscardinus.occu)
summary(modelmus2)

###removing non-significant occupancy  variables one by one, starting by least significant
modelmus3 <- occu(~ Mass_branches+ Height + s.age + Crown_radius + tree_species_c ~ beech_prop_ba + tree_species_c  + (1|id_plot) , data = umf.muscardinus.occu)
summary(modelmus3)

modelmus4 <- occu(~ Mass_branches+ Height + s.age + Crown_radius + tree_species_c ~ beech_prop_ba + (1|id_plot) , data = umf.muscardinus.occu)
summary(modelmus4)

modelmus5 <- occu(~ Mass_branches+ Height  + Crown_radius + tree_species_c ~ beech_prop_ba + (1|id_plot) , data = umf.muscardinus.occu)
summary(modelmus5)


modelmbest <- modelmus5
summary(modelmbest)
#best model


#using Site as the random factor
modelmus5s <- occu(~ Mass_branches+ Height  + Crown_radius + tree_species_c ~ beech_prop_ba + (1|Site) , data = umf.muscardinus.occu)
summary(modelmus5s)
# 


beech_prop_ba_range <- 0:1
beech_prop_ba_seq <- seq(beech_prop_ba_range[1], beech_prop_ba_range[2], length.out=101)
nd <- data.frame(beech_prop_ba = rep(beech_prop_ba_seq,3))
#nd$tree_species_c <- c(rep("Dougl",101),rep("Beech",101),rep("Spruc",101))
occ_beech_prop_ba_musss <- predict(modelmbest, type="state", newdata=nd, re.form=NA)
occ_beech_prop_ba_musss$Species <- "Muscardinus\navellanarius"
occ_beech_prop_ba_musss$beech_prop_ba <- rep(beech_prop_ba_seq,3)
#occ_beech_prop_ba_musss_p <- cbind(nd,occ_beech_prop_ba_musss)



ggplot(data= occ_beech_prop_ba_musss, aes(x=beech_prop_ba, y=Predicted))+
  geom_line(linewidth=2)+
  ylim(0,1)+
  geom_ribbon( aes(ymin = lower, ymax = upper, fill=Species), fill="#00BA38", alpha = 0.1)+
  #scale_fill_manual(values = "darkturquoise")+
  labs(x="Beech proportional basal area", y="Occupancy")+
  theme_minimal()

# View Model Results
summary(modelmbest)
#best model