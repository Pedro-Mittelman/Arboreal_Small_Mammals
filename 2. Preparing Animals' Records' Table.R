# 
#Preparing Table 1
#how many total records per species and how many independent records records

#pacakges
library(tidyverse)

whichmammals <- filter(camera_data_ta, class=="Mammalia")
allmammals<- unique(whichmammals$animal_species)


for (i in 1:length(allmammals)) {
  allmammals.df$Animal_species[i] <- allmammals[i]
  allmammals.df$Total_records[i]<- length(row.names(filter(camera_data_ta, animal_species==allmammals[i])))
  allmammals.df$Independent_records[i] <- length(row.names(filter(camera_data_ta, animal_species==allmammals[i]) %>%
    group_by(id_plot,Site, Site_location, plot_type, tree_number, tree_species_c, year, id_occasion, Date_m_d) %>% 
    summarise(n())))
}

allmammals.df
