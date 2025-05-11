
# ##WITH LOG EFFORT-----


#linear models binomial
modeltospecies_seenelog <- glmer(Seen~beech_prop_ba+ (1|Site/id_plot),
                                 data=allspeciescolum, family = "binomial",,weights = round((log10(weights_per_effort)*10)))
summary(modeltospecies_seenelog)

#quadratic model
modeltospecies_seen2loge <- glmer(Seen~(I(scale(beech_prop_ba)^2))+ (1|Site/id_plot),
                                  data=allspeciescolum, family = "binomial",weights = round((log10(weights_per_effort)*10)))
summary(modeltospecies_seen2loge)

modeltospecies_seen2loge2 <- glmer(Seen~(I(scale(beech_prop_ba)^2))+ (1|id_plot),
                                   data=allspeciescolum, family = "binomial",weights = round((log10(weights_per_effort)*10)))
summary(modeltospecies_seen2loge2)

modeltospecies_seen2loge3 <- glmer(Seen~(I(scale(beech_prop_ba)^2))+ (1|Site/plot_type),
                                   data=allspeciescolum, family = "binomial",weights = round((log10(weights_per_effort)*10)))
summary(modeltospecies_seen2loge3)

#adding 'Site' to the random structure does not change the model at all, because 
#either id_plot already incorporates the Sites structure or Site does not have any effect
#btw: you can remove site from the random structure
#it does not change the model and makes it give singular warning
# but it was added as a Reviewer request


#quadratic model and linear
modeltospecies_seen3loge <- glmer(Seen~scale(beech_prop_ba)+ scale(I(beech_prop_ba)^2)+ (1|Site/id_plot),
                                  data=allspeciescolum, family = "binomial",  weights = round((log10(weights_per_effort)*10)))
summary(modeltospecies_seen3loge)

#comparing models
bbmle::AICtab(modeltospecies_seenelog,modeltospecies_seen2loge,modeltospecies_seen3loge, base=TRUE, weights=TRUE)
#Quadratic model is the best model
summary(modeltospecies_seen2loge)



#PLotting best model
general.grid<-expand.grid(beech_prop_ba = seq(-0.01, 1.01, 0.005))
pred.tospecies_seenlog3<-predict(modeltospecies_seen2loge, newdata = general.grid, type="response", re.form=NA)
df.predict.tospecies_seenlog3 <- data.frame(pred = pred.tospecies_seenlog3, general.grid)

#Plotting the graphs considering effort
set.seed(25)

#filtering for geom_rug
allspeciescolumT <- allspeciescolum %>% filter(Seen=="TRUE")
allspeciescolumF <- allspeciescolum %>% filter(Seen=="FALSE")


ad.quadratic.binomial.plot.3.species.logeffort <- ggplot(data = allspeciescolum, aes(x = jitter(beech_prop_ba, 190),
                                                                                 y = jitter(as.numeric(Seen),0.19))) +
  geom_point(data = allspeciescolumF, aes(size = I(log(weights_per_effort/2)*1.5), col=Species), alpha=0.2) +
  geom_point(data=allspeciescolumT, aes(size = I(log(weights_per_effort/2)*1.5),col=Species, x = jitter(beech_prop_ba, 90), y=jitter(rep(0.525,length(as.numeric(allspeciescolumT$Seen))),0.65)),
             alpha=0.2)+
  geom_line(data = df.predict.tospecies_seenlog3, aes(y = pred, x = beech_prop_ba),
            col= "black", linewidth=3, alpha=0.65) +
  labs(y="Probability of spotting\n all 3 arboreal small mammal species", x = "Beech Proportional Basal Area")+
  xlim(-0.05,1.05)+
  ylim(-0.01,0.54)+
  scale_y_continuous(breaks = c(0.0,0.15, 0.3, 0.45))+
  scale_color_manual(values = c("#619CFF","#00BA38", "#F8766D")) +
  #geom_rug(data=allspeciescolumT, aes(col=Species, shape=Species), sides="t", position="jitter")+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13));ad.quadratic.binomial.plot.3.species.logeffort


# plot1 <- ad.linearquadratic.binomial.plot.3.species.logeffort+
#   theme(#legend.position = "none",
#     axis.text = element_text(size = 13),
#     axis.title.x = element_text( size=14),
#     axis.title.y = element_text( size=14)
#   )
# 
# plot1.2 <- plot1+
#   scale_color_discrete(guide = guide_legend(override.aes = list(size = 5, alpha=0.2))) +
#   theme(
#     #legend.position = c(0.85,0.65),
#     legend.title = element_text(size = 14),  # Increase legend title size
#     legend.text = element_text(size = 12),   # Increase legend text size
#     legend.key.size = unit(1.75, "lines"),    # Increase legend key size
#   )


#ggsave("cooccurrence.plot.jpg",plot=plot1, width = 20, height = 10, unit = "cm", dpi=200)
# 
# 
# ##doing the analyses with all sites
# 
# #preparing data for analyses
# #adding which species as a column and only one TRUE or FALSE COLUMN
# 
# arbmam.df <- arbmam.df %>% mutate(allspecies_id_records=sum(Glis_id_records, Muscardinus_id_records, Sciurus_id_records),
#                                       anyspecies_seen=if_else(allspecies_id_records>0, TRUE,FALSE)) %>% left_join(site_covariates.s)
# 
# sciurustojoin2 <- arbmam.df %>%  select(id_plot, id_occasion, Sciurus_seen, Sciurus_id_records, beech_prop_ba,effort_days) %>% 
#   mutate(Seen=Sciurus_seen, ind_records=Sciurus_id_records, Species="Sciurus") %>% select(-Sciurus_seen, -Sciurus_id_records)
# glistojoin2 <- arbmam.df%>%  select(id_plot, id_occasion, Glis_seen, Glis_id_records, beech_prop_ba,effort_days) %>% 
#   mutate(Seen=Glis_seen, ind_records=Glis_id_records, Species="Glis") %>% select(-Glis_seen, -Glis_id_records)
# muscardinustojoin2 <- arbmam.df %>%  select(id_plot, id_occasion, Muscardinus_seen, Muscardinus_id_records, beech_prop_ba,effort_days) %>% 
#   mutate(Seen=Muscardinus_seen, ind_records=Muscardinus_id_records,Species="Muscardinus") %>% select(-Muscardinus_seen, -Muscardinus_id_records)
# 
# allspeciescolumas <- rbind(sciurustojoin2, glistojoin2,muscardinustojoin2)
# allspeciescolumas <-allspeciescolumas %>%  mutate(weights_per_effort= as.integer( round((1/(effort_days+1))*10000,0)))
# 
# 
# modeltospecies_seen2loge3b <- glmer(Seen~(I(scale(beech_prop_ba)^2))+ Site_location + (1|Site/id_plot),
#                                    data=allspeciescolumas, family = "binomial",weights = round((log10(weights_per_effort)*10)))
# summary(modeltospecies_seen2loge3b)
# 
# # modeltospecies_seen2loge3by <- glmer(Seen~(I(scale(beech_prop_ba)^2))+  (1|Site/id_plot),
# #                                     data=allspeciescolumas, family = "binomial",weights = round((log10(weights_per_effort)*10)))
# # summary(modeltospecies_seen2loge3by)
# # 
# # modeltospecies_seen2loge3bx <- glmer(Seen~(I(scale(beech_prop_ba)^2)): Site + (1|Site/plot_type),
# #                                     data=allspeciescolumas, family = "binomial",weights = round((log10(weights_per_effort)*10)))
# # summary(modeltospecies_seen2loge3bx)
# 
# #PLotting best model
# general.grid2<-data.frame(beech_prop_ba = rep(seq(-0.01, 1.01, 0.005),4), Site= c(rep("3",205),rep("4",205),rep("6",205),rep("8",205)))
# pred.tospecies_seenlog3b<-predict(modeltospecies_seen2loge3b, newdata = general.grid2, type="response", re.form = NA)
# #argument, re.form=NULL takes into account the random effect structure
# df.predict.tospecies_seenlog3b <- data.frame(pred = pred.tospecies_seenlog3b, general.grid2)
# 
# general.grid2<-data.frame(beech_prop_ba = rep(seq(-0.01, 1.01, 0.005),4), Site_location= c(rep("Northern",410),rep("Southern",410)))
# pred.tospecies_seenlog3b<-predict(modeltospecies_seen2loge3b, newdata = general.grid2, type="response", re.form = NA)
# #argument, re.form=NULL takes into account the random effect structure
# df.predict.tospecies_seenlog3b <- data.frame(pred = pred.tospecies_seenlog3b, general.grid2)
# 
# #for each different SIte
# predSite3 <- df.predict.tospecies_seenlog3b %>% filter(Site=="3")
# predSite4 <- df.predict.tospecies_seenlog3b %>% filter(Site=="4")
# predSite6 <- df.predict.tospecies_seenlog3b %>% filter(Site=="6")
# predSite8 <- df.predict.tospecies_seenlog3b %>% filter(Site=="8")
# 
# predSiteN <- df.predict.tospecies_seenlog3b %>% filter(Site_location=="Northern")
# predSiteS <- df.predict.tospecies_seenlog3b %>% filter(Site_location=="Southern")
# 
# #Plotting the graphs considering effort
# set.seed(25)
# 
# #filtering for geom_rug
# allspeciescolumasT <- allspeciescolumas %>% filter(Seen=="TRUE")
# allspeciescolumasF <- allspeciescolumas %>% filter(Seen=="FALSE")
# 
# 
# ad.quadratic.binomial.plot.3.species.logeffortb <- ggplot(data = allspeciescolumas, aes(x = jitter(beech_prop_ba, 190),
#                                                                                      y = jitter(as.numeric(Seen),0.19))) +
#   geom_point(data = allspeciescolumasF, aes(size = I(log(weights_per_effort/2)*1.5), col=Species), alpha=0.11) +
#   geom_point(data=allspeciescolumasT, aes(size = I(log(weights_per_effort/2)*1.5),col=Species, x = jitter(beech_prop_ba, 90), y=jitter(rep(0.525,length(as.numeric(allspeciescolumasT$Seen))),0.65)),
#              alpha=0.2)+
#   geom_line(data = df.predict.tospecies_seenlog3b, aes(y = pred, x = beech_prop_ba, linetype =Site_location),
#              linewidth=3, alpha=0.65) +
#   labs(y="Probability of spotting\n all 3 arboreal small mammal species", x = "Beech Proportional Basal Area")+
#   xlim(-0.05,1.05)+
#   ylim(-0.01,0.54)+
#   scale_y_continuous(breaks = c(0.0,0.15, 0.3, 0.45))+
#   scale_color_manual(values = c("#619CFF","#00BA38", "#F8766D")) +
#   #geom_rug(data=allspeciescolumT, aes(col=Species, shape=Species), sides="t", position="jitter")+
#   theme_minimal()+
#   theme(
#     axis.text = element_text(size = 15),
#     axis.title = element_text(size = 13));ad.quadratic.binomial.plot.3.species.logeffortb
# 
# 
# 
# ad.quadratic.binomial.plot.3.species.logeffortb <- ggplot(data = allspeciescolumas, aes(x = jitter(beech_prop_ba, 190),
#                                                                                         y = jitter(as.numeric(Seen),0.19))) +
#   geom_point(data = allspeciescolumasF, aes(size = I(log(weights_per_effort/2)*1.5), col=Species), alpha=0.2) +
#   geom_point(data=allspeciescolumasT, aes(size = I(log(weights_per_effort/2)*1.5),col=Species, x = jitter(beech_prop_ba, 90), y=jitter(rep(0.525,length(as.numeric(allspeciescolumasT$Seen))),0.65)),
#              alpha=0.2)+
#   geom_line(data = df.predict.tospecies_seenlog3b, aes(y = pred, x = beech_prop_ba),
#             col= "black", linewidth=3, alpha=0.65) +
#   labs(y="Probability of spotting\n all 3 arboreal small mammal species", x = "Beech Proportional Basal Area")+
#   xlim(-0.05,1.05)+
#   ylim(-0.01,0.54)+
#   scale_y_continuous(breaks = c(0.0,0.15, 0.3, 0.45))+
#   scale_color_manual(values = c("#619CFF","#00BA38", "#F8766D")) +
#   #geom_rug(data=allspeciescolumT, aes(col=Species, shape=Species), sides="t", position="jitter")+
#   theme_minimal()+
#   theme(
#     axis.text = element_text(size = 15),
#     axis.title = element_text(size = 13));ad.quadratic.binomial.plot.3.species.logeffortb