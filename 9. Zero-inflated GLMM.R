#fitting zero0inflated model with Site_location as the variable
#predicting zero-inflation proabbility

#pacakges
library(glmmTMB)
library(tidyverse)

##testing zero-inflated model

#number of independend records per camera per species

camera_data_ta$tree_number <- as.factor(camera_data_ta$tree_number)
camera_data_ta$id_occasion <- as.factor(camera_data_ta$id_occasion)
#Sciurus
sciurus.df<- filter(camera_data_ta, animal_species=="Sciurus vulgaris")
sciurus.ir <- sciurus.df %>% group_by(id_plot,Site, Site_location, plot_type, tree_number, tree_species_c, year, id_occasion, Date_m_d,) %>%
  summarise(count=n())

sciurus.ir2 <- sciurus.ir %>% group_by(id_plot,Site, Site_location, tree_number, id_occasion) %>%
  summarise(count=n(), .groups = "drop") %>%
  complete( id_occasion, fill = list(count = 0))
sciurus.ir2$id_plot <- as.factor(substr(sciurus.ir2$id_occasion, 1, 2))
sciurus.ir2$plot_type <- as.factor(substr(sciurus.ir2$id_occasion, 2, 2))
sciurus.ir2$Site <- substr(sciurus.ir2$id_occasion, 1, 1)
sciurus.ir2$tree_number<- substr(sciurus.ir2$id_occasion, 4, 6)
sciurus.ir2$Site_location <- as.factor(if_else(as.numeric(sciurus.ir2$Site)>4, "Northern","Southern"))
sciurus.ir2$Site <- as.factor(sciurus.ir2$Site)
sciurus.ir2 <- left_join(sciurus.ir2,bas)
sciurus.ir2$Species <- "Sciurus"

#Glis
glis.df<- filter(camera_data_ta, animal_species=="Glis glis")
glis.ir <- glis.df %>% group_by(id_plot,Site, Site_location, plot_type, tree_number, tree_species_c, year, id_occasion, Date_m_d) %>%
  summarise(count=n())

glis.ir2 <- glis.ir %>% group_by(id_plot,Site, Site_location, tree_number, id_occasion) %>%
  summarise(count=n(), .groups = "drop") %>%
  complete( id_occasion, fill = list(count = 0))
glis.ir2$id_plot <- as.factor(substr(glis.ir2$id_occasion, 1, 2))
glis.ir2$plot_type <- as.factor(substr(glis.ir2$id_occasion, 2, 2))
glis.ir2$Site <- substr(glis.ir2$id_occasion, 1, 1)
glis.ir2$tree_number<- substr(glis.ir2$id_occasion, 4, 6)
glis.ir2$Site_location <- as.factor(if_else(as.numeric(glis.ir2$Site)>4, "Northern","Southern"))
glis.ir2$Site <- as.factor(glis.ir2$Site)
glis.ir2 <- left_join(glis.ir2,bas)
glis.ir2$Species <- "Glis"

#Muscardinus
muscardinus.df<- filter(camera_data_ta, animal_species=="Muscardinus avellanarius")
muscardinus.ir <- muscardinus.df %>% group_by(id_plot,Site, Site_location, plot_type, tree_number, tree_species_c, year, id_occasion, Date_m_d) %>%
  summarise(count=n())

muscardinus.ir2 <- muscardinus.ir %>% group_by(id_plot,Site, Site_location, tree_number, id_occasion) %>%
  summarise(count=n(), .groups = "drop") %>%
  complete( id_occasion, fill = list(count = 0))
muscardinus.ir2$id_plot <- as.factor(substr(muscardinus.ir2$id_occasion, 1, 2))
muscardinus.ir2$plot_type <- as.factor(substr(muscardinus.ir2$id_occasion, 2, 2))
muscardinus.ir2$Site <- substr(muscardinus.ir2$id_occasion, 1, 1)
muscardinus.ir2$tree_number<- substr(muscardinus.ir2$id_occasion, 4, 6)
muscardinus.ir2$Site_location <- as.factor(if_else(as.numeric(muscardinus.ir2$Site)>4, "Northern","Southern"))
muscardinus.ir2$Site <- as.factor(muscardinus.ir2$Site)
muscardinus.ir2 <- left_join(muscardinus.ir2,bas)
muscardinus.ir2$Species <- "Muscardinus"

## putting everythingtogether
all3spdf <- sciurus.ir2 %>% rbind(glis.ir2) %>% rbind(muscardinus.ir2)

## adding  effort
effortds.per.ocasion <- camera_data_ta %>% group_by(id_occasion) %>%  summarise( effort_days=mean(effort_days))
all3spdfe <- all3spdf %>% left_join(effortds.per.ocasion)
all3spdfe$count_w_effort <- if_else(all3spdfe$count==0,0,round((all3spdfe$count/all3spdfe$effort_days)*1000))


#models

modelzeroalle <- glmmTMB(count_w_effort ~ scale(beech_prop_ba)+(1|Site/id_plot) ,
                        data = all3spdfe,
                        ziformula = ~Site_location ,    # Zero-inflation model
                        family = poisson)
summary(modelzeroalle)


#adding quadratic funtion
modelzeroall2e <- glmmTMB(count_w_effort ~ (I(scale(beech_prop_ba)^2)) +(1|Site/id_plot) ,
                         data = all3spdfe,
                         ziformula = ~Site_location,    # Zero-inflation model
                         family = poisson)
summary(modelzeroall2e)

#adding linear and quadratic funtion
modelzeroall3e <- glmmTMB(count_w_effort ~ scale(beech_prop_ba) + (I(scale(beech_prop_ba)^2)) +(1|Site/id_plot) ,
                         data = all3spdfe,
                         ziformula = ~Site_location,    # Zero-inflation model
                         family = poisson)
summary(modelzeroall3e)

bbmle::AICctab(modelzeroalle,modelzeroall2e,modelzeroall3e,base=TRUE, weights=TRUE)
anova(modelzeroalle,modelzeroall2e,modelzeroall3e)
#linear and qaudratic funtion has the lowest AIC
summary(modelzeroall3e)


modelzeroall3es <- glmmTMB(count_w_effort ~ scale(beech_prop_ba) + (I(scale(beech_prop_ba)^2)) +Site_location +(1|Site/id_plot) ,
                          data = all3spdfe,
                          ziformula = ~Site_location,    # Zero-inflation model
                          family = poisson)
summary(modelzeroall3es)
#Site location has an effect on the zero-inflation process but not on the count
