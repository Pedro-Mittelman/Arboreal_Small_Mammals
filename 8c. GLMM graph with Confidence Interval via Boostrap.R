#Bootstrapping to obtain confidence intervals
#for Genralized Linear Mixed Model

#packages
library(lme4)

#this is the model to be bootsrapped
modeltospecies_seen2logeb <- glmer(Seen~(I(scale(beech_prop_ba)^2))+ (1|id_plot),
                                  data=allspeciescolum2b, family = "binomial",weights = round((log10(weights_per_effort)*10)))
summary(modeltospecies_seen2logeb)




#PLotting
general.grid<-expand.grid(beech_prop_ba = seq(0, 1, 0.005))
pred.tospecies_seenlog3<-predict(modeltospecies_seen2logeb, newdata = general.grid, type="response", re.form=NA)
df.predict.tospecies_seenlog3 <- data.frame(pred = pred.tospecies_seenlog3, general.grid)

#Plotting the graphs considering effort
set.seed(25)

#renaming levels
allspeciescolum2b$Species <- as.factor(allspeciescolum2b$Species)
levels(allspeciescolum2b$Species) <- c("Glis glis","Muscardinus\navellanarius", "Sciurus vulgaris" )
#reordering levels
allspeciescolum2b$Species <- fct_relevel(allspeciescolum2b$Species,
                                             c("Sciurus vulgaris","Muscardinus\navellanarius","Glis glis"))


#filtering for geom_rug
allspeciescolum2bT <- allspeciescolum2b %>% filter(Seen=="TRUE")
allspeciescolum2bF <- allspeciescolum2b %>% filter(Seen=="FALSE")


ad.linearquadratic.binomial.plot.3.species.logeffort <- ggplot(data = allspeciescolum2b, aes(x = jitter(beech_prop_ba, 190),
                                                                                           y = jitter(as.numeric(Seen),0.19))) +
  geom_point(data = allspeciescolum2bF, aes(size = I(log(weights_per_effort/2)*1.5), col=Species), alpha=0.2) +
  geom_point(data=allspeciescolum2bT, aes(size = I(log(weights_per_effort/2)*1.5),col=Species, x = jitter(beech_prop_ba, 90), y=jitter(rep(0.525,length(as.numeric(allspeciescolum2bT$Seen))),0.65)),
             alpha=0.2)+
  geom_line(data = df.predict.tospecies_seenlog3, aes(y = pred, x = beech_prop_ba),
            col= "black", linewidth=3, alpha=0.65) +
  labs(y="Probability of spotting\n all 3 arboreal small mammal species", x = "Beech Proportional Basal Area")+
  xlim(-0.05,1.05)+
  ylim(-0.01,0.54)+
  scale_y_continuous(breaks = c(0.0,0.15, 0.3, 0.45))+
  scale_color_manual(values = c("#619CFF","#00BA38", "#F8766D")) +
  #geom_rug(data=allspeciescolum2bT, aes(col=Species, shape=Species), sides="t", position="jitter")+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13));ad.linearquadratic.binomial.plot.3.species.logeffort


# 0 -Reducing dataset to only essential data in order to lighten the proccessing
allspeciescolum2b_light <-  allspeciescolum2b %>% select(Seen, Site, id_plot, beech_prop_ba,Species, weights_per_effort)

##MAKING CONFIDENCE INTERVAL

#1-resampling lines
listdf <- list()
for (i in 1:1000) {
  listdf[[i]] <-  sample_n(allspeciescolum2b_light, nrow(allspeciescolum2b_light), replace = T)
print(i)}
listmodels <- list()

#2-creating models  for each resample
for (i in 1:1000) {
  listmodels[[i]]  <- glmer(Seen~(I(scale(beech_prop_ba)^2))+ (1|id_plot),
        data=listdf[[i]], family = "binomial",weights = round((log10(weights_per_effort)*10)))
  print(i)
}


#2-#predicting values of Y(Sight probability) for different values of X (Beech proportional basal area)
listpredictmodels <- list()

general.grid<-expand.grid(beech_prop_ba = seq(0, 1, 0.005))

for (i in 1:1000){
  pred.species_seenlog2<-predict(listmodels[[i]], newdata = general.grid, type="response", re.form=NA)
  listpredictmodels[[i]]  <- data.frame(pred = pred.species_seenlog2, general.grid)
print(i)}

#putting all predictions for each value of X in the same column
pred.per.bba<- data.frame()

for (j in 1:length(listpredictmodels[[1]]$pred)) {
  for (i in 1:1000) {
    pred.per.bba[i,j] <- listpredictmodels[[i]][j,1]
    
  } 
  print(j)}

#deleting list of models and data frames because it uses a lot fo memory
rm(listmodels)
rm(listdf)

## sorting in ascendig order and selecting only tHe values between the 95% CONFIDENCE INTERVAL
pred.per.bba95 <- data.frame(row.names = 1:950)
for (i in 1:length(listpredictmodels[[1]]$pred)) {
  pred.per.bba95 <- cbind(pred.per.bba95, sort(pred.per.bba[,i])[26:975])
}
colnames(pred.per.bba95) <- NULL
low <- as_vector(pred.per.bba95[1,])
high <- as_vector(pred.per.bba95[950,])


##PReparing to plot

# General prediction without random effects 
general.grid<-expand.grid(beech_prop_ba = seq(0, 1, 0.005))
pred.tospecies_seenlog2<-predict(modeltospecies_seen2logeb, newdata = general.grid, type="response", re.form=NA)
df.predict.tospecies_seenlog2 <- data.frame(pred = pred.tospecies_seenlog2, general.grid)


##lines of the 95 %interval
df.predict.low <- data.frame(pred =low, general.grid)
df.predict.high <- data.frame(pred = high,general.grid)
ribonci<- cbind(df.predict.low, high)
#Plotting the graph


binomial.plot.log <- ggplot() + 
# ggplot(data = allspeciescolum2b, aes(x = jitter(beech_prop_ba, 190),
#                                    y = jitter(as.numeric(Seen),0.19))) +
  geom_point(data = allspeciescolum2bF, aes(x = jitter(beech_prop_ba, 190),
                                          y = jitter(as.numeric(Seen),0.19),
             size = I(log(weights_per_effort/2)*1.5), col=Species), alpha=0.2) +
  geom_point(data=allspeciescolum2bT, aes(size = I(log(weights_per_effort/2)*1.5),
                                        col=Species, x = jitter(beech_prop_ba, 90),
                                        y=jitter(rep(0.525,length(as.numeric(allspeciescolum2bT$Seen))),0.65)),
             alpha=0.2)+
  geom_line(data = df.predict.tospecies_seenlog2, aes(y = pred, x = beech_prop_ba),
            col= "black", linewidth=2.3, alpha=0.65) +
  labs(y="Probability of spotting\n all 3 arboreal small mammal species", x = "Beech Proportional Basal Area")+
  xlim(-0.05,1.05)+
  ylim(-0.01,0.54)+
  scale_y_continuous(breaks = c(0.0,0.15, 0.3, 0.45))+
  scale_color_manual(values = c("#619CFF","#00BA38", "#F8766D")) +
  geom_line(data = df.predict.low, aes(y = pred, x = beech_prop_ba),
            col= "black", linewidth=1, alpha=0.2) + 
  geom_line(data = df.predict.high, aes(y = pred, x = beech_prop_ba),
            col= "black", linewidth=1, alpha=0.2) + 
  geom_ribbon(data=ribonci, aes(ymin = low, ymax = high, x=beech_prop_ba), alpha = 0.1)+
  #geom_rug(data=allspeciescolum2bT, aes(col=Species, shape=Species), sides="t", position="jitter")+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 13));binomial.plot.log

binomial.plot.log<- binomial.plot.log+
  scale_color_discrete(guide = guide_legend(override.aes = list(size = 6, alpha=0.2))) +
  theme(#legend.position = "none",
    legend.position = c(0.82,0.68),
    axis.text = element_text(size = 13),
    axis.title.x = element_text( size=14),
    axis.title.y = element_text( size=14),
    legend.title = element_text(size = 14),  # Increase legend title size
    legend.text = element_text(size = 12),   # Increase legend text size
    legend.key.size = unit(1.75, "lines"),    # Increase legend key size
  );binomial.plot.log

#ggsave("Figure_4_binomial.plot.logb.jpg", plot=binomial.plot.log, width =22, height = 16, units = "cm", dpi=200)

