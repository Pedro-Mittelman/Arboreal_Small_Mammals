#Multi species occupancy model

#only southern sites and 2023

# Load necessary packages
library(tidyverse)
library(unmarked)
library(reshape2)

#here I am only using Southern sites as there the three samll mammal species were found
stacked_sciurus$id_plot2 <- as.numeric(as.character(stacked_sciurus$id_plot))

stacked_sciurus.s<- stacked_sciurus %>% filter(id_plot2<50)
# Detection matrix (drop non-detection columns)
detection_matrix_sciurus.s <- stacked_sciurus.s %>%
  select(starts_with("V")) %>%  # or select those 30 weekly columns if they don’t start with "X"
  as.matrix()

# Build site covariate table
site_covs_sciurus.s <- stacked_sciurus.s %>%
  select(id_plot,site_season, season, plot_type, tree_species_c, platform, Site, Site_location,
         beech_prop_ba, spruce_prop_ba, douglas_prop_ba, Height, DBH, Crown_radius,
         Mass_branches, s.age) %>%
  distinct()

# 
# 
# #making all detections the same length to run MS occupancy model
# new_columns_needed <- length(detection_data.muscardinus.occu) - length(detection_data.sciurus.occu.s)
# new_columns_needed.b <- length(detection_data.muscardinus.occu) - length(detection_data.glis.occu)
# 
# # Create a data frame of NA values with the same number of rows as the original data frame
# new_columns <- as.data.frame(matrix(NA, nrow = nrow(detection_data.sciurus.occu.s), ncol = new_columns_needed))
# new_columns.b <- as.data.frame(matrix(NA, nrow = nrow(detection_data.glis.occu), ncol = new_columns_needed.b))
# 
# # Renaming extra columns
# #colnames(new_columns) <- paste0("EXTRA", 1:new_columns_needed)
# colnames(new_columns.b) <- paste0("EXTRA", 1:new_columns_needed.b)
# 
# 
# # Combine the original data frame with the new columns
# expanded_sciurus.df <- cbind(detection_data.sciurus.occu.s, new_columns)
# expanded_glis.df <- cbind( new_columns.b, detection_data.glis.occu)

#defining each species data 
colnames(detection_matrix_sciurus.s) <- paste0("sciurus_visit",
                                        1:length(colnames(detection_matrix_sciurus.s)))
rownames(detection_matrix_sciurus.s) <- stacked_sciurus.s$site_season

colnames(detection_matrix_glis2) <- paste0("glis_visit",
                                               1:length(colnames(detection_matrix_glis2)))
rownames(detection_matrix_glis2) <- stacked_glis2$site_season

colnames(detection_matrix_muscardinus) <- paste0("muscardinus_visit",
                                           1:length(colnames(detection_matrix_muscardinus)))
rownames(detection_matrix_muscardinus) <- stacked_muscardinus$site_season


# Combine detection histories into a list
det_histories.ms.list <- list(sciurus = detection_matrix_sciurus.s, glis = detection_matrix_glis2,
                              muscardinus = detection_matrix_muscardinus)

class(det_histories.ms.list$sciurus)
#Site covariates
site_covariates.s <- camera_data_ta %>% filter(year=="2023", Site_location=="Southern") %>% group_by(id_occasion, id_plot,plot_type, tree_species_c, Site, Site_location) %>% 
  summarise(beech_prop_ba=mean(beech_prop_ba),spruce_prop_ba=mean(spruce_prop_ba),douglas_prop_ba=mean(douglas_prop_ba),
            mean(effort_days), Height= mean(Height),DBH=mean(DBH),Crown_radius=mean(Crown_radius95),
            Mass_branches=mean(Mass_branches)) 

# Create unmarkedFrameOccu object
umf_ms <- unmarkedFrameOccuMulti(y = det_histories.ms.list, 
                                 siteCovs = site_covs_muscardinus)

plot(umf_ms)


# Specify the occupancy and detection formulas
#this model does not support a random structure
#null model
occ_formula <- c("~1","~1","~1","~1","~1","~1","0")

det_formula <- c("~ 1","~ 1","~ 1")  # Assuming constant detection probability for simplicity

# Fit the multi-species occupancy model


ms_occu_model_null <- occuMulti(det_formula, occ_formula, umf_ms)
sites_tob_removed<-c( 2, 10, 33, 44, 45, 46, 49, 51, 52, 54, 59, 60, 61, 62, 66, 67, 69, 71, 72,
75, 82, 83, 84, 87, 89, 90, 92, 94, 97, 98, 99, 100, 104, 105, 106, 107, 109, 110, 112, 113)


#removing sites with Na only
detection_matrix_sciurus.s2 <- detection_matrix_sciurus.s[-sites_tob_removed,]

detection_matrix_glis2b <- detection_matrix_glis2[-sites_tob_removed,]

detection_matrix_muscardinus2 <- detection_matrix_muscardinus[-sites_tob_removed,]


# Combine detection histories into a list
det_histories.ms.list2 <- list(sciurus = detection_matrix_sciurus.s2, glis = detection_matrix_glis2b,
                              muscardinus = detection_matrix_muscardinus2)

# Create unmarkedFrameOccu object
umf_ms <- unmarkedFrameOccuMulti(y = det_histories.ms.list2, 
                                 siteCovs = site_covs_muscardinus[-sites_tob_removed,])

plot(umf_ms)


# Specify the occupancy and detection formulas
#this model does not support a random structure
#null model
occ_formula <- c("~1","~1","~1","~1","~1","~1","0")

det_formula <- c("~ 1","~ 1","~ 1")  # Assuming constant detection probability for simplicity

# Fit the multi-species occupancy model
ms_occu_model_null <- occuMulti(det_formula, occ_formula, umf_ms)


# Summarize the model results
summary(ms_occu_model_null)

# #Null model 2, just modelling detection
# det_formulan2 <- c("~ 1","~  DBH + Height + Mass_branches",
#                    "~ Mass_branches+ Crown_radius+ Height  + tree_species_c + beech_prop_ba")
# #this formula was derived from the best model
# 
# # Fit the multi-species occupancy model
# ms_occu_model_null2 <- occuMulti(det_formulan2, occ_formula, umf_ms)
# 
# # Summarize the model results
# summary(ms_occu_model_null2)

##with covariables
#detection and occupancy formulas derived from best single species models
#squirrel
modelsbe4 <- occu(~  s.age + tree_species_c + beech_prop_ba ~ beech_prop_ba + (1|id_plot)  , data = umf.sciurus.occu_stacked_sciurus)
summary(modelsbe4)

#edible dormouse
modelbeglis4 <- occu(~ DBH    ~ beech_prop_ba + (1|id_plot)  , data = umf.glis.occu_stacked_glis)
summary(modelbeglis4)

#hazel dormouse
modelbemus2 <- occu(~ DBH + Crown_radius + tree_species_c  ~ beech_prop_ba + (1|id_plot)  , data = umf.muscardinus.occu_stacked_muscardinus)
summary(modelbemus2)


occ_formula2 <- c("~beech_prop_ba","~beech_prop_ba","~beech_prop_ba","~1","~1","~1","0")

det_formula2 <- c("~ s.age + tree_species_c + beech_prop_ba","~  DBH",
                  "~ DBH + Crown_radius + tree_species_c")
# Fit the multi-species occupancy model
ms_occu_model_2 <- occuMulti(det_formula2, occ_formula2, umf_ms)

# Summarize the model results
summary(ms_occu_model_2)

#removing non significant covariables

occ_formula3 <- c("~beech_prop_ba  ","~beech_prop_ba","~beech_prop_ba","~1","~1","~1","0")

det_formula3 <- c("~ 1","~  DBH ",
                  "~ 1")
# Fit the multi-species occupancy model
ms_occu_model_3 <- occuMulti(det_formula3, occ_formula3, umf_ms)
summary(ms_occu_model_3)

# #removing non significant covariables
# 
# occ_formula4 <- c("~beech_prop_ba  ","~beech_prop_ba","~beech_prop_ba","~1","~1","~1","0")
# 
# det_formula4 <- c("~ tree_species_c + beech_prop_ba","~  DBH ",
#                   "~ DBH + tree_species_c")
# # Fit the multi-species occupancy model
# ms_occu_model_4 <- occuMulti(det_formula4, occ_formula4, umf_ms)
# summary(ms_occu_model_4)

#removing non significant covariables

occ_formula4 <- c("~beech_prop_ba  ","~beech_prop_ba","~beech_prop_ba","~1","~1","~1","0")

det_formula4 <- c("~ 1","~  DBH ",
                  "~ DBH + tree_species_c")
# Fit the multi-species occupancy model
ms_occu_model_4 <- occuMulti(det_formula4, occ_formula4, umf_ms)
summary(ms_occu_model_4)

# Conditional occupancy=-----
## Using the null model
#using the null model does not takes into account habitat preferences
#so species might be 'excluding' or faicliatating each other just because tehy share
#the same niche or habitat preferences

sciurus_glis <- predict(ms_occu_model_null, type="state", species="sciurus", cond="glis")
sciurus_noglis <- predict(ms_occu_model_null, type="state", species="sciurus", cond="-glis")
#plotting
cond_data <- rbind(sciurus_glis[1,], sciurus_noglis[1,])
cond_data$glis_status <- c("Present","Absent")

sciurus_glis_plot <- ggplot(data=cond_data, aes(y=Predicted, x=glis_status, col=glis_status))+
  #geom_crossbar(aes(ymin=lower, ymax=upper))+
  geom_pointrange(aes(ymin=lower, ymax=upper), fatten=10, linewidth=1.3, show.legend=F)+
  ylim(0,1)+
  labs(x="Edible dormouse",y="Red squirrel occupancy")+
  theme_minimal(); sciurus_glis_plot

sciurus_muscardinus <- predict(ms_occu_model_null, type="state", species="sciurus", cond="muscardinus")
sciurus_nomuscardinus <- predict(ms_occu_model_null, type="state", species="sciurus", cond="-muscardinus")
cond_datasciurus_muscardinus <- rbind(sciurus_muscardinus[1,], sciurus_nomuscardinus[1,])
cond_datasciurus_muscardinus$muscardinus_status <- c("Present","Absent")

sciurus_muscardinus_plot <- ggplot(data=cond_datasciurus_muscardinus, aes(y=Predicted, x=muscardinus_status, col=muscardinus_status))+
  #geom_crossbar(aes(ymin=lower, ymax=upper))+
  geom_pointrange(aes(ymin=lower, ymax=upper), fatten=10, linewidth=1.3, show.legend=F)+
  ylim(0,1)+
  labs(x="Hazel dormouse",y="Red squirrel occupancy")+
  theme_minimal(); sciurus_muscardinus_plot


#together
sciurus_glis_plot2 <- sciurus_glis_plot +
  theme(axis.title.y = element_text(size = 14))
sciurus_muscardinus_plot2 <- sciurus_muscardinus_plot +
  ylab(" ")
sciurus_dormice_plot<- ggpubr::ggarrange(sciurus_glis_plot2,
                                         sciurus_muscardinus_plot2);sciurus_dormice_plot

glis_muscardinus <- predict(ms_occu_model_null, type="state", species="glis", cond="muscardinus")
glis_nomuscardinus <- predict(ms_occu_model_null, type="state", species="glis", cond="-muscardinus")
cond_dataglis_muscardinus <- rbind(glis_muscardinus[1,], glis_nomuscardinus[1,])
cond_dataglis_muscardinus$muscardinus_status <- c("Present","Absent")


glis_muscardinus_plot <- ggplot(data=cond_dataglis_muscardinus, aes(y=Predicted, x=muscardinus_status, col=muscardinus_status))+
  #geom_crossbar(aes(ymin=lower, ymax=upper))+
  geom_pointrange(aes(ymin=lower, ymax=upper), fatten=10, linewidth=1.3, show.legend=F)+
  ylim(0,1)+
  labs(x="Hazel dormouse",y="Edible dormouse occupancy")+
  theme_minimal(); glis_muscardinus_plot

#using model that take into account habitat and forest carachteristics----

sciurus_glis <- predict(ms_occu_model_4, type="state", species="sciurus", cond="glis") %>% mutate(glis_status="Present")
sciurus_noglis <- predict(ms_occu_model_4, type="state", species="sciurus", cond="-glis")%>% mutate(glis_status="Absent")
#plotting
cond_data <- rbind(sciurus_glis, sciurus_noglis) %>% group_by(glis_status) %>% 
  summarise(Predicted=mean(Predicted), lower=mean(lower), upper=mean(upper),SE=mean(SE))

sciurus_glis_plota <- ggplot(data=cond_data, aes(y=Predicted, x=glis_status, col=glis_status))+
  #geom_crossbar(aes(ymin=lower, ymax=upper))+
  geom_pointrange(aes(ymin=lower, ymax=upper), fatten=10, linewidth=1.3, show.legend=F)+
  ylim(0,1)+
  labs(x="Edible dormouse",y="Red squirrel occupancy")+
  theme_minimal(); sciurus_glis_plota


sciurus_muscardinus <- predict(ms_occu_model_4, type="state", species="sciurus", cond="muscardinus") %>% mutate(muscardinus_status="Present")
sciurus_nomuscardinus <- predict(ms_occu_model_4, type="state", species="sciurus", cond="-muscardinus")%>% mutate(muscardinus_status="Absent")
#plotting
cond_datasciurus_muscardinus <- rbind(sciurus_muscardinus, sciurus_nomuscardinus) %>% group_by(muscardinus_status) %>% 
  summarise(Predicted=mean(Predicted), lower=mean(lower), upper=mean(upper),SE=mean(SE))

sciurus_muscardinus_plota <- ggplot(data=cond_datasciurus_muscardinus, aes(y=Predicted, x=muscardinus_status, col=muscardinus_status))+
  #geom_crossbar(aes(ymin=lower, ymax=upper))+
  geom_pointrange(aes(ymin=lower, ymax=upper), fatten=10, linewidth=1.3, show.legend=F)+
  ylim(0,1)+
  labs(x="Hazel dormouse",y="Red squirrel occupancy")+
  theme_minimal(); sciurus_muscardinus_plota

glis_muscardinus <- predict(ms_occu_model_4, type="state", species="glis", cond="muscardinus") %>% mutate(muscardinus_status="Present")
glis_nomuscardinus <- predict(ms_occu_model_4, type="state", species="glis", cond="-muscardinus")%>% mutate(muscardinus_status="Absent")
#plotting
cond_dataglis_muscardinus <- rbind(glis_muscardinus, glis_nomuscardinus) %>% group_by(muscardinus_status) %>% 
  summarise(Predicted=mean(Predicted), lower=mean(lower), upper=mean(upper),SE=mean(SE))

glis_muscardinus_plota <- ggplot(data=cond_dataglis_muscardinus, aes(y=Predicted, x=muscardinus_status, col=muscardinus_status))+
  #geom_crossbar(aes(ymin=lower, ymax=upper))+
  geom_pointrange(aes(ymin=lower, ymax=upper), fatten=10, linewidth=1.3, show.legend=F)+
  ylim(0,1)+
  labs(x="Hazel dormouse",y="\nEdible dormouse occupancy")+
  theme_minimal() +
  theme(axis.title.y = element_text(size = 14)); glis_muscardinus_plota



#together

sciurus_glis_plot2a <- sciurus_glis_plota +
  theme(axis.title.y = element_text(size = 14))


sciurus_muscardinus_plot2a <- sciurus_muscardinus_plota +
  ylab(" ")+
  labs(y = NULL) 
sciurus_dormice_plota<- ggpubr::ggarrange(sciurus_glis_plot2a,
                                          sciurus_muscardinus_plot2a);sciurus_dormice_plota
all3species_plota<- ggpubr::ggarrange(sciurus_glis_plot2a,
                                      sciurus_muscardinus_plot2a,glis_muscardinus_plota, ncol=3);all3species_plota
#ggsave("multispecies_comp.jpg",plot=all3species_plota, width = 24, height = 13, unit = "cm", dpi=200)

