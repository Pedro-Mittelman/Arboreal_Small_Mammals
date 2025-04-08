#Arboreal camera traps
#Loading data for occupancy and mixed model analyses

#packages
library(tidyverse)
library(readxl)

#reading data
camera_data_ta <- read.csv2("Data/Camera_data_23.csv")
str(camera_data_ta)


#removing unnessecary column
camera_data_ta<- camera_data_ta %>% select(-X)
#adding column related to Site number
camera_data_ta$Site <- substr(camera_data_ta$id_plot,1,1)
#adding column related to Site location
camera_data_ta$Site_location <- if_else(as.numeric(camera_data_ta$Site)>4, "Northern","Southern") 

# Columns to convert to factors
columns_to_convert_factor <- c("id_plot", "Site","Site_location", "plot_type", "camera_number", "card_number", "tree_species_c","tree_species_s", "id_tree", "platform", "year" )

# Convert selected columns to factors using lapply
camera_data_ta[columns_to_convert_factor] <- lapply(camera_data_ta[columns_to_convert_factor], factor)

# Columns to convert to numeric
columns_to_convert_numeric <- c("Height")

# Convert selected columns to numeric using lapply
camera_data_ta[columns_to_convert_numeric] <- lapply(camera_data_ta[columns_to_convert_numeric], as.numeric)

#adding data about each tree species basal area
bas <- read_excel("Data/Proportional_basal_areas.xlsx")
str(bas)
bas$id_plot <- as.factor(bas$id_plot)
camera_data_ta<-left_join(camera_data_ta,bas)

#adding data about stand age
s.age <- read_excel("Data/Stand_age_info.xlsx")
str(s.age)
s.age$id_plot <- as.factor(s.age$id_plot)
camera_data_ta<-left_join(camera_data_ta,s.age)
str(camera_data_ta)


