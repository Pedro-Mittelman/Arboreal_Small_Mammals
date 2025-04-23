# 
#Preparing Table 1
#how many total records per species and how many independent records records

#pacakges
library(tidyverse)

whichmammals <- filter(camera_data_ta, class=="Mammalia")
allmammals<- unique(whichmammals$animal_species)
allmammals.df <- data.frame()
allmammals.df <- data.frame("Animal_species", "Total_records", "Independent_records")
Animal_species <- vector()
Total_records <- vector()
Independent_records <- vector()


for (i in 1:length(allmammals)) {
  Animal_species[i] <- allmammals[i]
  Total_records[i]<- length(row.names(filter(camera_data_ta, animal_species==allmammals[i])))
  Independent_records[i] <- length(row.names(filter(camera_data_ta, animal_species==allmammals[i]) %>%
    group_by(id_plot,Site, Site_location, plot_type, tree_number, tree_species_c, year, id_occasion, Date_m_d) %>% 
    summarise(n())))
}
allmammals.df <- data.frame(Animal_species, Total_records, Independent_records)

allmammals.df
