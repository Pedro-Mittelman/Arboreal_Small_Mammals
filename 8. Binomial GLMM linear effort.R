
#packages
library(lme4)


#only southern sites and 2023

# Load necessary packages
library(tidyverse)
library(unmarked)
library(reshape2)

#here I am only using Southern sites as there the three samll mammal species were found
#Southern sites starts with 3 or 4 in their names
Sites <- as.numeric(substr(row.names(detection_data.sciurus.occu), 1, 1))
# Filter rows where the first digit is less than 5
detection_data.sciurus.occu.s<- detection_data.sciurus.occu[Sites < 5,]


#making all detections the same length to run MS occupancy model
new_columns_needed <- length(detection_data.muscardinus.occu) - length(detection_data.sciurus.occu.s)
new_columns_needed.b <- length(detection_data.muscardinus.occu) - length(detection_data.glis.occu)

# Create a data frame of NA values with the same number of rows as the original data frame
new_columns <- as.data.frame(matrix(NA, nrow = nrow(detection_data.sciurus.occu.s), ncol = new_columns_needed))
new_columns.b <- as.data.frame(matrix(NA, nrow = nrow(detection_data.glis.occu), ncol = new_columns_needed.b))

# Renaming extra columns
#colnames(new_columns) <- paste0("EXTRA", 1:new_columns_needed)
colnames(new_columns.b) <- paste0("EXTRA", 1:new_columns_needed.b)


# Combine the original data frame with the new columns
expanded_sciurus.df <- cbind(detection_data.sciurus.occu.s, new_columns)
expanded_glis.df <- cbind(detection_data.glis.occu, new_columns.b)

#defining each species data 
colnames(expanded_sciurus.df) <- paste0("sciurus_visit",
                                        1:length(expanded_sciurus.df))
colnames(expanded_glis.df) <- paste0("glis_visit",
                                     1:length(expanded_glis.df))
colnames(detection_data.muscardinus.occu) <- paste0("muscardinus_visit",
                                                    1:length(colnames(detection_data.muscardinus.occu)))
n_visits <- length(detection_data.muscardinus.occu)

# Combine detection histories into a list
det_histories.ms.list <- list(sciurus = as.matrix(expanded_sciurus.df), glis = as.matrix(expanded_glis.df),
                              muscardinus = as.matrix( detection_data.muscardinus.occu))


#Site covariates
site_covariates.s <- camera_data_ta %>% filter(year=="2023", Site_location=="Southern") %>% group_by(id_occasion, id_plot,plot_type, tree_species_c, Site, Site_location) %>% 
  summarise(beech_prop_ba=mean(beech_prop_ba),spruce_prop_ba=mean(spruce_prop_ba),douglas_prop_ba=mean(douglas_prop_ba),
            mean(effort_days), Height= mean(Height),DBH=mean(DBH),Crown_radius=mean(Crown_radius95),
            Mass_branches=mean(Mass_branches)) 



#Creating data frames where squirrels and dormouse were seen



#number of recorders per tree/ocassion
sciurus.ir <- camera_data_ta %>% filter(animal_species=="Sciurus vulgaris") %>% group_by(id_plot,Site, Site_location, plot_type, tree_number, tree_species_c, year, id_occasion, Date_m_d) %>% 
  summarise(n())
sciurus.irt <- sciurus.ir %>% group_by(id_plot, id_occasion, Site, Site_location, plot_type, tree_number, tree_species_c,year) %>% 
  summarise(independent_records=n())
#aading TRUE to all trees that edlible dormouse was seen
sciurus.irt <- sciurus.irt %>% mutate(Sciurus_seen=TRUE)

#number of recorders per tree/ocassion
glis.ir <- camera_data_ta %>% filter(animal_species=="Glis glis") %>% group_by(id_plot,Site, Site_location, plot_type, tree_number, tree_species_c, year, id_occasion, Date_m_d) %>% 
  summarise(n())
glis.irt <- glis.ir %>% group_by(id_plot, id_occasion, Site, Site_location, plot_type, tree_number, tree_species_c,year) %>% 
  summarise(independent_records=n())
#aading TRUE to all trees that edlible dormouse was seen
glis.irt <- glis.irt %>% mutate(Glis_seen=TRUE)

#number of recorders per tree/ocassion
muscardinus.ir <- camera_data_ta %>% filter(animal_species=="Muscardinus avellanarius") %>% group_by(id_plot,Site, Site_location, plot_type, tree_number, tree_species_c, year, id_occasion, Date_m_d) %>% 
  summarise(n())
muscardinus.irt <- muscardinus.ir %>% group_by(id_plot, id_occasion, Site, Site_location, plot_type, tree_number, tree_species_c,year) %>% 
  summarise(independent_records=n())
#aading TRUE to all trees that edlible dormouse was seen
muscardinus.irt <- muscardinus.irt %>% mutate(Muscardinus_seen=TRUE)

##Adding all arboreal mammals and all trees together into a dataframes
glis.irt$Glis_id_records <-glis.irt$independent_records 
#adding all id_ocasions
occasionsdf <- camera_data_ta %>% group_by(id_plot,Site, Site_location, plot_type, tree_number, tree_species_c, year, id_occasion,beech_prop_ba, effort_days) %>% 
  summarise(mockn=n()) %>% dplyr::select(-mockn)
arbmam.df1 <- left_join(occasionsdf,glis.irt) %>% dplyr::select(-independent_records )
muscardinus.irt$Muscardinus_id_records <-muscardinus.irt$independent_records 
arbmam.df2 <- left_join(occasionsdf,muscardinus.irt) %>% dplyr::select(-independent_records )
sciurus.irt$Sciurus_id_records <-sciurus.irt$independent_records 
arbmam.df3 <- left_join(occasionsdf,sciurus.irt) %>% dplyr::select(-independent_records )
arbmam.df <- left_join(arbmam.df1,arbmam.df2) %>% left_join(arbmam.df3)
#adding FALSE and 0 s to where animals were no tssen
arbmam.df$Glis_seen[is.na(arbmam.df$Glis_seen)] <- FALSE
arbmam.df$Muscardinus_seen[is.na(arbmam.df$Muscardinus_seen)] <- FALSE
arbmam.df$Sciurus_seen[is.na(arbmam.df$Sciurus_seen)] <- FALSE
arbmam.df$Glis_id_records[is.na(arbmam.df$Glis_id_records)] <- 0
arbmam.df$Muscardinus_id_records[is.na(arbmam.df$Muscardinus_id_records)] <- 0
arbmam.df$Sciurus_id_records[is.na(arbmam.df$Sciurus_id_records)] <- 0

#filtering only southern plots for edible and hazel dormouse
arbmam.df_s <- arbmam.df %>% filter(Site_location=="Southern")



#preparing data for analyses
#adding which species as a column and only one TRUE or FALSE COLUMN

arbmam.df_s <- arbmam.df_s %>% mutate(allspecies_id_records=sum(Glis_id_records, Muscardinus_id_records, Sciurus_id_records),
                                      anyspecies_seen=if_else(allspecies_id_records>0, TRUE,FALSE)) %>% left_join(site_covariates.s)

sciurustojoin <- arbmam.df_s %>%  select(id_plot, id_occasion, Sciurus_seen, Sciurus_id_records, beech_prop_ba,effort_days) %>% 
  mutate(Seen=Sciurus_seen, ind_records=Sciurus_id_records, Species="Sciurus") %>% select(-Sciurus_seen, -Sciurus_id_records)
glistojoin <- arbmam.df_s %>%  select(id_plot, id_occasion, Glis_seen, Glis_id_records, beech_prop_ba,effort_days) %>% 
  mutate(Seen=Glis_seen, ind_records=Glis_id_records, Species="Glis") %>% select(-Glis_seen, -Glis_id_records)
muscardinustojoin <- arbmam.df_s %>%  select(id_plot, id_occasion, Muscardinus_seen, Muscardinus_id_records, beech_prop_ba,effort_days) %>% 
  mutate(Seen=Muscardinus_seen, ind_records=Muscardinus_id_records,Species="Muscardinus") %>% select(-Muscardinus_seen, -Muscardinus_id_records)

allspeciescolum <- rbind(sciurustojoin, glistojoin,muscardinustojoin)


# #filtering only southern plots for edible and hazel dormouse
# allspeciescolum <- allspeciescolum %>% filter(Site_location=="Southern"|
#                                       (Site_location=="Northern" & Species=="Sciurus"))
#
#checking effort per plot
arbmam.df_seffort <-arbmam.df_s %>%  group_by(plot_type) %>% summarise(sum(effort_days))

ggplot(data=arbmam.df_s, aes(x=beech_prop_ba, y=effort_days))+
  geom_point()+
  geom_smooth()
#not exactly evenly distributed
#proabbly I should consider add weights to the points
#weight will be inversally proportional to the effort (camera days)
allspeciescolum <-allspeciescolum %>%  mutate(weights_per_effort= as.integer( round((1/(effort_days+1))*10000,0)))
# I think it's best to use a logarithmic effort because that means taht adding a few camera days
#when the effort is low makes a lot of differnce, which is not true when you already have acamera active for >3 months
#this is specially true in TRUE or FALSE scenario where adding more camera days will only decrease the value
#of the recorded effort
#allspeciescolum <-allspeciescolum %>%  mutate(logweights_per_effort= as.integer(round((1/(log10(effort_days+1)))*10000,0)))

 allspeciescolum$Year <- substr(allspeciescolum$id_occasion,8,11)
 allspeciescolum <- allspeciescolum %>% filter(Year==2023)

#MODEls-----

# #without log effort----



#linear models binomial
modeltospecies_seen <- glmer(Seen~beech_prop_ba+ (1|id_plot),
                             data=allspeciescolum, family = "binomial",weights = weights_per_effort)
summary(modeltospecies_seen)

#quadratic model
modeltospecies_seen2 <- glmer(Seen~(I(scale(beech_prop_ba)^2))+ (1|id_plot),
                              data=allspeciescolum, family = "binomial",weights = weights_per_effort)
summary(modeltospecies_seen2)

#quadratic model and linear
modeltospecies_seen3 <- glmer(Seen~scale(beech_prop_ba)+ scale(I(beech_prop_ba)^2)+ (1|id_plot),
                              data=allspeciescolum, family = "binomial",  weights = weights_per_effort)
summary(modeltospecies_seen3)

#comparing models
bbmle::AICctab(modeltospecies_seen,modeltospecies_seen2,modeltospecies_seen3, base=TRUE, weights=TRUE)
#Quadratic  model is the best model
summary(modeltospecies_seen2)


#PLotting best model
general.grid<-expand.grid(beech_prop_ba = seq(0, 1, 0.005))
pred.tospecies_seen3<-predict(modeltospecies_seen2, newdata = general.grid, type="response", re.form=NA)
df.predict.tospecies_seen3 <- data.frame(pred = pred.tospecies_seen3, general.grid)

#Plotting the graphs considereng effort
set.seed(25)


#filtering for geom_rug
allspeciescolumn19 <- allspeciescolum %>% filter(effort_days>20)
allspeciescolumn19T <- allspeciescolumn19 %>% filter(Seen=="TRUE")
allspeciescolumn19F <- allspeciescolumn19 %>% filter(Seen=="FALSE")


ad.quadratic.binomial.plot<- ggplot(data = allspeciescolumn19, aes(x = jitter(beech_prop_ba, 190),
                                                                 y = jitter(as.numeric(Seen),0.19))) +
  geom_point(data = allspeciescolumn19F, aes(size = I((weights_per_effort/30)*1.5), col=Species), alpha=0.2) +
  geom_point(data=allspeciescolumn19T, aes(size = I((weights_per_effort/30)*1.5),col=Species, x = jitter(beech_prop_ba, 90), y=jitter(rep(0.370,length(as.numeric(allspeciescolumn19T$Seen))),0.65)),
             alpha=0.2)+
  geom_line(data = df.predict.tospecies_seen3, aes(y = pred, x = beech_prop_ba),
            col= "black", linewidth=3, alpha=0.65) +
  labs(y="Probability of spotting\n all 3 arboreal small mammals' species", x = "Beech Proportional Basal Area")+
  xlim(-0.05,1.05)+
  ylim(-0.01,0.380)+
  #scale_y_continuous(breaks = c(0.0,0.10, 0.20, 0.30))+
  scale_color_manual(values = c("#619CFF","#00BA38", "#F8766D")) +
  #geom_rug(data=allspeciescolumn19T, aes(col=Species, shape=Species), sides="t", position="jitter")+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13));ad.quadratic.binomial.plot


ad.quadratic.binomial.plot<- ad.quadratic.binomial.plot+
  scale_color_discrete(guide = guide_legend(override.aes = list(size = 6, alpha=0.2))) +
  theme(#legend.position = "none",
    legend.position = c(0.82,0.68),
    axis.text = element_text(size = 13),
    axis.title.x = element_text( size=14),
    axis.title.y = element_text( size=14),
    legend.title = element_text(size = 14),  # Increase legend title size
    legend.text = element_text(size = 12),   # Increase legend text size
    legend.key.size = unit(1.75, "lines"),    # Increase legend key size
  );ad.quadratic.binomial.plot

#ggsave("S.Figure_2_binomial.plot.linearb.jpg", plot=ad.quadratic.binomial.plot, width =22, height = 16, units = "cm", dpi=200)

