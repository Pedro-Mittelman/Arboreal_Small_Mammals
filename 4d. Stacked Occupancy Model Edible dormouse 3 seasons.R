
#DIVIDING IN THREE SEASONS as the other species to later run multi-species occuppancy model together
# #adding weeks of Nas because each season must have the same number of weeks
detection_data.glis.occu[,27:30] <- NA
spring_glis2 <- detection_data.glis.occu[, 1:10]
summer_glis2 <- detection_data.glis.occu[, 11:20]
autumn_glis2 <- detection_data.glis.occu[, 21:30]


# Add site names to each season
sites <- rownames(spring_glis2)

spring_glis2_df <- spring_glis2 %>%
  mutate(site = sites, season = "spring_glis2")

summer_glis2_df <- summer_glis2 %>%
  mutate(site = sites, season = "summer_glis2")

autumn_glis2_df <- autumn_glis2 %>%
  mutate(site = sites, season = "autumn_glis2")

# Stack the three seasons together
stacked_glis2 <- bind_rows(spring_glis2_df, summer_glis2_df, autumn_glis2_df)

# Create a unique ID for each site-season combination
stacked_glis2$site_season <- paste0(stacked_glis2$site, "_", stacked_glis2$season)

# Move ID columns to the front
stacked_glis2 <- stacked_glis2 %>% relocate(site_season, site, season)


# Extract id_occasion from rownames
spring_glis2_df$id_occasion <- rownames(spring_glis2)
summer_glis2_df$id_occasion <- rownames(summer_glis2)
autumn_glis2_df$id_occasion <- rownames(autumn_glis2)

# Stack and keep id_occasion
stacked_glis2 <- bind_rows(spring_glis2_df, summer_glis2_df, autumn_glis2_df) %>%
  mutate(season = as.factor(season),
         site_season = paste0(id_occasion, "_", season))


site_covariates <- camera_data_ta %>% filter(year=="2023", Site_location=="Southern") %>% group_by(id_occasion, id_plot,plot_type, platform, tree_species_c, Site, Site_location) %>%
  summarise(beech_prop_ba=mean(beech_prop_ba),spruce_prop_ba=mean(spruce_prop_ba),douglas_prop_ba=mean(douglas_prop_ba),
            mean(effort_days), Height= mean(Height),DBH=mean(DBH), mean(Max_tree_height),Crown_radius=mean(Crown_radius95),
            Mass_branches=mean(Mass_branches), mean(NDIV), beech_quad=(mean(beech_prop_ba)^2),s.age=mean(s.age_2025))
site_covariates$beech_quad <- (scale(site_covariates$beech_prop_ba)[,1])^2
site_covariates$Site <- droplevels(site_covariates$Site)
site_covariates$platform <- as.factor(as.character(site_covariates$platform))
site_covariates$id_occasion <- as.factor((site_covariates$id_occasion))


# Join site-level covariates
stacked_glis2 <- left_join(stacked_glis2, site_covariates, by = c("id_occasion" = "id_occasion"))


# Detection matrix (drop non-detection columns)
detection_matrix_glis2 <- stacked_glis2 %>%
  select(starts_with("V")) %>%  # or select those 30 weekly columns if they don’t start with "X"
  as.matrix()

# Build site covariate table
site_covs_glis2 <- stacked_glis2 %>%
  select(id_plot,site_season, season, plot_type, tree_species_c, platform, Site, Site_location,
         beech_prop_ba, spruce_prop_ba, douglas_prop_ba, Height, DBH, Crown_radius,
         Mass_branches, s.age) %>%
  distinct()

# Set rownames to match the detection matrix
rownames(site_covs_glis2) <- site_covs_glis2$site_season
