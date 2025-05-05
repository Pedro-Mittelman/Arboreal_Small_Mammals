# plotting Graphs from occupancy model
library (tidyverse)
library(unmarked)
library(cowplot)


#using results from single species occupancy models
occ_beech_prop_ba_fss<- rbind(occ_beech_prop_ba_sciss, occ_beech_prop_ba_gliss,
                              occ_beech_prop_ba_musss)
occ_beech_prop_ba_fss$Species <- as.factor(occ_beech_prop_ba_fss$Species)
#renaming levels
levels(occ_beech_prop_ba_fss$Species) <- c("Glis glis","Muscardinus\navellanarius", "Sciurus vulgaris" )
#reordering levels
occ_beech_prop_ba_fss$Species <- fct_relevel(occ_beech_prop_ba_fss$Species,
                                             c("Sciurus vulgaris","Muscardinus\navellanarius","Glis glis"))

single_Species_occ_plot<- ggplot(data=occ_beech_prop_ba_fss, aes(x=beech_prop_ba, y=Predicted,  linetype = Species))+
  ylim(0.0,1)+
  geom_ribbon( aes(ymin = lower, ymax = upper, fill=Species), alpha = 0.14)+
  geom_line(aes(col=Species),linewidth=2)+
  scale_linetype_manual(#labels=c("Glis glis", "Muscardinus\navellanarius", "Sciurus vulgaris"),
    values = c("solid", "solid","longdash")) +
  scale_fill_manual(values = c("#F8766D","#00BA38", "#619CFF")) +
  scale_color_manual(values = c("#F8766D","#00BA38","#619CFF" )) +
  #scale_linetype_manual(values = c("Glis" = "longdash", "Muscardinus" = "solid", "Sciurus" = "solid")) +
  labs(x="Beech proportional basal area", y="Occupancy")+
  theme_minimal()+
  theme(legend.position = c(0.55, 0.85),
    axis.text = element_text(size = 12),
    axis.title.x = element_text( size=15),
    axis.title.y = element_text( size=15),    legend.title = element_text(size = 14),  # Increase legend title size
    legend.text = element_text(size = 12),   # Increase legend text size
    legend.key.size = unit(1.75, "lines"),    # Increase legend key size
  );single_Species_occ_plot



#ggsave("single_species.occupancy.plot_loweso2b.jpg", plot=single_Species_occ_plot, width =20, height = 16, units = "cm", dpi=200)


#plotting multplied simoultaneous probability

all.species.prop <- occ_beech_prop_ba_sciss$Predicted[1:101] *  occ_beech_prop_ba_musss$Predicted[1:101]*occ_beech_prop_ba_gliss$Predicted[1:101]
all.species.propdf <- data.frame(Predicted=all.species.prop, Species="All 3 species", beech_prop_ba=seq(0, 1, by = 0.01))

# bettersavethisplot<- ggplot(data=all.species.propdf, aes(x=beech_prop_ba, y=Predicted))+
#   #ylim(0.019,0.031)+
#   geom_line(linewidth=2)+
#   labs(x="Beech proportional basal area", y="Probability of Occupancy of all 3 species")+
#   theme_minimal()

ggplot(data=all.species.propdf, aes(x=beech_prop_ba, y=Predicted))+
  ylim(0,0.3)+
  geom_line(linewidth=2)+
  labs(x="Beech proportional basal area", y="Probability of Occupancy of all 3 species")+
  theme_minimal()

dormouse.prop <-occ_beech_prop_ba_musss$Predicted[1:101]+((1-occ_beech_prop_ba_musss$Predicted[1:101])*occ_beech_prop_ba_gliss$Predicted[1:101])
sciurus.prop <- occ_beech_prop_ba_sciss$Predicted[1:101]
sciurus.dormouse.prob <- dormouse.prop *sciurus.prop
sciurus.dormouse.probdf<- data.frame(Predicted=sciurus.dormouse.prob, Species="Any Dormouse Species & Squirrel", beech_prop_ba=seq(0, 1, by = 0.01))

ggplot(data=sciurus.dormouse.probdf, aes(x=beech_prop_ba, y=Predicted))+
  ylim(0.05,0.3)+
  geom_line(linewidth=2)+
  labs(x="Beech proportional basal area", y="Probability of Occupancy\nof Squirrel and any Dormouse species")+
  theme_minimal()

#prob squirrel + hazel
hazel.prop <-occ_beech_prop_ba_musss$Predicted[1:101]
sciurus.prop <- occ_beech_prop_ba_sciss$Predicted[1:101]
sciurus.hazel.prob <- hazel.prop *sciurus.prop
sciurus.hazel.probdf<- data.frame(Predicted=sciurus.hazel.prob, Species="Hazel Dormouse & Squirrel", beech_prop_ba=seq(0, 1, by = 0.01))

ggplot(data=sciurus.hazel.probdf, aes(x=beech_prop_ba, y=Predicted))+
  ylim(0.05,0.3)+
  geom_line(linewidth=2)+
  labs(x="Beech proportional basal area", y="Probability of Occupancy\nof Squirrel and Muscardinus")+
  theme_minimal()

#prob squirrel + glis
glis.prop <-occ_beech_prop_ba_gliss$Predicted[1:101]
sciurus.prop <- occ_beech_prop_ba_sciss$Predicted[1:101]
sciurus.glis.prob <- glis.prop *sciurus.prop
sciurus.glis.probdf<- data.frame(Predicted=sciurus.glis.prob, Species="Edible Dormouse & Squirrel", beech_prop_ba=seq(0, 1, by = 0.01))

ggplot(data=sciurus.glis.probdf, aes(x=beech_prop_ba, y=Predicted))+
  ylim(0.05,0.3)+
  geom_line(linewidth=2)+
  labs(x="Beech proportional basal area", y="Probability of Occupancy\nof Squirrel and Glis")+
  theme_minimal()

##ALL TOGETHER
all.simult.prob.df<- sciurus.dormouse.probdf %>%
  rbind(sciurus.hazel.probdf) %>%
  rbind(sciurus.glis.probdf) #%>%rbind(all.species.propdf )

#transforming to factor
all.simult.prob.df$Species <- as.factor(all.simult.prob.df$Species)


simultaneous.occu.plot<- ggplot(data=all.simult.prob.df, aes(x=beech_prop_ba, y=Predicted, col=Species))+
  ylim(0.00,0.28)+
  geom_line(linewidth=2)+
  #geom_line(data=all.species.propdf, linewidth=3)+
  labs(x="Beech proportional basal area", y="Simultaneous Occupancy")+
  theme_minimal()+
  scale_color_manual(limits = c("Any Dormouse Species & Squirrel", "Edible Dormouse & Squirrel",
                                "Hazel Dormouse & Squirrel"),
values = c( "#F8766D", "#00BFC4", "#7CAE00" ))+
  theme(legend.position = c(0.80, 0.84),
        axis.text = element_text(size = 12),
        axis.title.x = element_text( size=15),
        axis.title.y = element_text( size=15),    legend.title = element_text(size = 14),  # Increase legend title size
        legend.text = element_text(size = 10),   # Increase legend text size
        legend.key.size = unit(1.35, "lines"),    # Increase legend key size
  );simultaneous.occu.plot

#ggsave("Figure4_simuloccupanciesplotsb.jpg", plot=simultaneous.occu.plot, width =20, height = 16, units = "cm", dpi=200)

simultaneous.occu.plot2<- ggplot(data=all.simult.prob.df, aes(x=beech_prop_ba, y=Predicted, col=Species))+
  ylim(0.005,0.25)+
  geom_line(linewidth=2)+
  geom_line(data=all.species.propdf, linewidth=2.2)+
  labs(x="Beech proportional basal area", y="Simultaneous Occupancy")+
  theme_minimal()+
  scale_color_manual(limits = c("Any Dormouse Species & Squirrel", "Edible Dormouse & Squirrel",
                                "Hazel Dormouse & Squirrel", "All 3 species"),
                     values = c( "#C77CFF", "#00BFC4","#7CAE00", "#F8766D"))+
  theme(legend.position = c(0.82, 0.84),
        axis.text = element_text(size = 12),
        axis.title.x = element_text( size=15),
        axis.title.y = element_text( size=15),    legend.title = element_text(size = 14),  # Increase legend title size
        legend.text = element_text(size = 10),   # Increase legend text size
        legend.key.size = unit(1.35, "lines"),    # Increase legend key size
  );simultaneous.occu.plot2

#ggsave("Figure4_simuloccupanciesplotsb.jpg", plot=simultaneous.occu.plot2, width =20, height = 16, units = "cm", dpi=200)

