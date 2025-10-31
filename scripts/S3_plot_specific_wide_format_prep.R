################################################################################
########################## Topographic Variables ###############################
################################################################################

# Last update - Josh: 02/04/2025

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data prepped with competition indexes
load("data/comp/growth_comp.RData")

################################################################################

# Organize dataset
subalpine <- growth_comp
subalpine$id <- paste0(subalpine$Plot, "_", subalpine$Tree) # create ID column


################################################################################
# Function for plot prep

prep_plot_data <- function(data, plot_id,
                           elevation, slope, aspect,
                           soil_moisture, soil_temp, air_temp, rel_humidity,
                           moisture_class) {
  
  # Identify height class columns that actually exist
  hc_cols <- intersect(c("hc1", "hc3", "hc4"), names(data))
  
  data %>%
    filter(Plot == plot_id) %>%
    
    # ---- Plot-level covariates ----
  mutate(
    elevation_m = elevation,
    slope_deg = slope,
    aspect_deg = aspect,
    soil_moisture = soil_moisture,
    soil_temperature = soil_temp,
    air_temperature = air_temp,
    relative_humidity = rel_humidity,
    moisture_class = moisture_class
  ) %>%
    
    # ---- Height class recode (only if columns exist) ----
  {
    if (length(hc_cols) > 0) {
      mutate(., across(all_of(hc_cols),
                       ~ case_when(
                         . == "1" ~ "Dominant",
                         . == "2" ~ "Co-Dominant",
                         . == "3" ~ "Suppressed",
                         TRUE ~ as.character(.)
                       )))
    } else {
      warning("No height class columns (hc1, hc3, hc4) found in dataset for ", plot_id)
      .
    }
  } %>%
    
    # ---- Competition indices ----
  mutate(
    ci_1 = round(Hegyi_dbh1, 2),
    ci_2 = round(Hegyi_dbh3, 2),
    ci_3 = round(Hegyi_dbh4, 2)
  ) %>%
    
    # ---- Basal area & growth (metric) ----
  mutate(
    basal_area_1982 = pi * (dbh1 / 2)^2,
    basal_area_2016 = pi * (dbh3 / 2)^2,
    basal_area_2022 = pi * (dbh4 / 2)^2,
    
    ba_growth_1982_2016 = basal_area_2016 - basal_area_1982,
    ba_growth_2016_2022 = basal_area_2022 - basal_area_2016,
    
    BAI_1982_2016_cm2yr = ba_growth_1982_2016 / (2016 - 1982),
    BAI_2016_2022_cm2yr = ba_growth_2016_2022 / (2022 - 2016),
    
    BAI_1982_2016_m2yr = BAI_1982_2016_cm2yr / 10000,
    BAI_2016_2022_m2yr = BAI_2016_2022_cm2yr / 10000
  ) %>%
    
    # ---- Mortality ----
  mutate(
    dead_census = case_when(
      deadPeriod == 0 ~ "Alive",
      deadPeriod %in% 1:9 ~ "After Census 1",
      deadPeriod %in% 10:12 ~ "After Census 2",
      TRUE ~ as.character(deadPeriod)
    )
  ) %>%
    
    # ---- Count dead ----
  {count_dead(., dead_col = "dead_census")}
}


prep_plot_data_single <- function(data,
                                  plot_id,
                                  elevation,
                                  slope,
                                  aspect,
                                  soil_moisture,
                                  soil_temp,
                                  air_temp,
                                  rel_humidity,
                                  moisture_class) {
  
  data %>%
    subset(Plot == plot_id) %>%
    
    # Standardize height classes
    mutate(
      hc3 = case_when(
        hc3 == "1" ~ "Dominant",
        hc3 == "2" ~ "Co-Dominant",
        hc3 == "3" ~ "Suppressed",
        TRUE ~ as.character(hc3)
      ),
      hc4 = case_when(
        hc4 == "1" ~ "Dominant",
        hc4 == "2" ~ "Co-Dominant",
        hc4 == "3" ~ "Suppressed",
        TRUE ~ as.character(hc4)
      )
    ) %>%
    
    # Assign plot-level covariates
    mutate(
      elevation = elevation,
      slope_deg = slope,
      aspect_deg = aspect,
      soil_moisture = soil_moisture,
      soil_temp = soil_temp,
      air_temp = air_temp,
      rel_humidity = rel_humidity,
      moisture_class = moisture_class
    ) %>%
    
    # Round competition indices (only need ci_2 and ci_3 here)
    mutate(
      ci_2 = round(Hegyi_dbh3, 2),
      ci_3 = round(Hegyi_dbh4, 2)
    ) %>%
    
    # Compute basal area (cm²)
    mutate(
      basal_area_2016 = ((dbh3 / 2.54)^2 * 0.005454),
      basal_area_2022 = ((dbh4 / 2.54)^2 * 0.005454),
      ba_growth_2022 = basal_area_2022 - basal_area_2016,
      annualized_ba_2022 = ba_growth_2022 / (2022 - 2016),
      BAI_2016_2022_cm2yr = annualized_ba_2022 * 929.03
    ) %>%
    
    # Mortality category
    mutate(
      dead_census = ifelse(deadPeriod == 0, "Alive",
                           ifelse(deadPeriod %in% c(1:9), "After Census 1",
                                  ifelse(deadPeriod %in% c(10:12), "After Census 2", NA)
                           )
      )
    ) %>%
    
    # Clean up structure
    mutate(
      dCI = NA,  # No delta available
      growth_ratio = NA
    ) %>%
    select(
      Plot, Spec, hc4, slope_deg, aspect_deg, elevation,
      soil_moisture, soil_temp, air_temp, rel_humidity, moisture_class,
      ci_3, BAI_2016_2022_cm2yr, dCI, growth_ratio, dead_census
    )
}


### MRS 1 ######################################################################
mrs1 <- prep_plot_data(
  data = subalpine,
  plot_id = "MRS1",
  elevation = 2900,
  slope = 5,
  aspect = 90,
  soil_moisture = 3.25,
  soil_temp = 4.9,
  air_temp = 4.3,
  rel_humidity = 56.9,
  moisture_class = "Xeric"
) %>% 
  mutate(
    dCI = ci_3 - ci_1,
    dBAI_cm2yr = BAI_2016_2022_cm2yr - BAI_1982_2016_cm2yr,
    growth_ratio = BAI_2016_2022_cm2yr / BAI_1982_2016_cm2yr
  )

ggplot(mrs1, aes(dCI, growth_ratio, color = slope_deg)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c() +
  facet_wrap(~hc4)




### BW2 ######################################################################
bw2 <- prep_plot_data(
  data = subalpine,
  plot_id = "BW2",
  elevation = 2980,
  slope = 5,
  aspect = 90,
  soil_moisture = 3.35,
  soil_temp = 3.09,
  air_temp = 3.26,
  rel_humidity = 39.2,
  moisture_class = "Xeric"
) %>%
  mutate(
    dCI = ci_3 - ci_1,
    dBAI_cm2yr = BAI_2016_2022_cm2yr - BAI_1982_2016_cm2yr,
    growth_ratio = BAI_2016_2022_cm2yr / BAI_1982_2016_cm2yr
  )

# --- Visualization ---
ggplot(bw2, aes(dCI, growth_ratio, color = slope_deg)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c() +
  facet_wrap(~hc4)



### BW3 ########################################################################

bw3 <- prep_plot_data(
  data = subalpine,
  plot_id = "BW3",
  elevation = 2980,
  slope = 5,
  aspect = 90,
  soil_moisture = 6.19,
  soil_temp = 2.27,
  air_temp = 2.96,
  rel_humidity = 65.6,
  moisture_class = "Mesic"
) %>%
  mutate(
    dCI = ci_3 - ci_1,
    dBAI_cm2yr = BAI_2016_2022_cm2yr - BAI_1982_2016_cm2yr,
    growth_ratio = BAI_2016_2022_cm2yr / BAI_1982_2016_cm2yr
  )

# --- Visualization ---
ggplot(bw3, aes(dCI, growth_ratio, color = slope_deg)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c() +
  facet_wrap(~hc4)



### MRS4 ########################################################################

mrs4 <- prep_plot_data(
  data = subalpine,
  plot_id = "MRS4",
  elevation = 3170,
  slope = 10,
  aspect = 90,
  soil_moisture = 8.02,
  soil_temp = 2.85,
  air_temp = 2.30,
  rel_humidity = 64.3,
  moisture_class = "Mesic"
) %>%
  mutate(
    dCI = ci_3 - ci_1,
    dBAI_cm2yr = BAI_2016_2022_cm2yr - BAI_1982_2016_cm2yr,
    growth_ratio = BAI_2016_2022_cm2yr / BAI_1982_2016_cm2yr
  )

# --- Visualization ---
ggplot(mrs4, aes(dCI, growth_ratio, color = slope_deg)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c() +
  facet_wrap(~hc4)



### MRS5 ########################################################################

mrs5 <- prep_plot_data(
  data = subalpine,
  plot_id = "MRS5",
  elevation = 3280,
  slope = 5,
  aspect = 90,
  soil_moisture = 31.50,
  soil_temp = 2.19,
  air_temp = 0.56,
  rel_humidity = 66.6,
  moisture_class = "Hydric"
) %>%
  mutate(
    dCI = ci_3 - ci_1,
    dBAI_cm2yr = BAI_2016_2022_cm2yr - BAI_1982_2016_cm2yr,
    growth_ratio = BAI_2016_2022_cm2yr / BAI_1982_2016_cm2yr
  )

# --- Visualization ---
ggplot(mrs5, aes(dCI, growth_ratio, color = slope_deg)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c() +
  facet_wrap(~hc4)



### MRS7 ########################################################################

mrs7 <- prep_plot_data(
  data = subalpine,
  plot_id = "MRS7",
  elevation = 3260,
  slope = 15,
  aspect = 180,
  soil_moisture = 5.74,
  soil_temp = 2.97,
  air_temp = 1.12,
  rel_humidity = 64.6,
  moisture_class = "Xeric"
) %>%
  mutate(
    dCI = ci_3 - ci_1,
    dBAI_cm2yr = BAI_2016_2022_cm2yr - BAI_1982_2016_cm2yr,
    growth_ratio = BAI_2016_2022_cm2yr / BAI_1982_2016_cm2yr
  )

# --- Visualization ---
ggplot(mrs7, aes(dCI, growth_ratio, color = slope_deg)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c() +
  facet_wrap(~hc4)




### MRS11 ########################################################################

mrs11 <- prep_plot_data_single(
  data = subalpine,
  plot_id = "MRS11",
  elevation = 3347,
  slope = 5,
  aspect = 160,
  soil_moisture = 9.87,
  soil_temp = 2.61,
  air_temp = 0.47,
  rel_humidity = 66.6,
  moisture_class = "Mesic"
)

ggplot(mrs11, aes(ci_3, BAI_2016_2022_cm2yr, color = slope_deg)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c() +
  facet_wrap(~hc4)




### MRS12 ########################################################################
mrs12 <- prep_plot_data_single(
  data = subalpine,
  plot_id = "MRS12",
  elevation = 3380,
  slope = 8,
  aspect = 200,
  soil_moisture = 9.16,
  soil_temp = 3.65,
  air_temp = 4.35,
  rel_humidity = 66.4,
  moisture_class = "Xeric"
)

ggplot(mrs12, aes(ci_3, BAI_2016_2022_cm2yr, color = slope_deg)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c() +
  facet_wrap(~hc4)


### MRS13 ########################################################################
mrs13 <- prep_plot_data_single(
  data = subalpine,
  plot_id = "MRS13",
  elevation = 3337,
  slope = 10,
  aspect = 225,
  soil_moisture = 13.05,
  soil_temp = 3.93,
  air_temp = 1.02,
  rel_humidity = 67.7,
  moisture_class = "Xeric"
)

ggplot(mrs13, aes(ci_3, BAI_2016_2022_cm2yr, color = slope_deg)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_c() +
  facet_wrap(~hc4)



################################################################################

# Combine all 

# --- Multi-census plots (1–7) ---
multi <- bind_rows(mrs1, bw2, bw3, mrs4, mrs5, mrs7) %>%
  mutate(
    dataset_type = "multi",
    BAI_1982_2016_cm2yr = BAI_1982_2016_cm2yr,
    BAI_2016_2022_cm2yr = BAI_2016_2022_cm2yr,
    dBAI_cm2yr = BAI_2016_2022_cm2yr - BAI_1982_2016_cm2yr,
    growth_ratio = BAI_2016_2022_cm2yr / BAI_1982_2016_cm2yr
  )

# --- Single-census plots (11–13) ---
single <- bind_rows(mrs11, mrs12, mrs13) %>%
  mutate(
    dataset_type = "single",
    BAI_1982_2016_cm2yr = NA_real_,
    dBAI_cm2yr = NA_real_,
    growth_ratio = NA_real_
  )

# --- Merge all ---
all_plots <- bind_rows(multi, single) %>%
  mutate(
    Plot = as.factor(Plot),
    hc4 = factor(hc4, levels = c("Suppressed", "Co-Dominant", "Dominant")),
  )

# Quick check
glimpse(all_plots) # duplicates / get rid

all_plots_clean <- all_plots %>%
  # Drop redundant or legacy columns
  select(
    X, Y, Tree, Plot, Spec,
    dbh1, dbh3, dbh4,
    hc1, hc3, hc4,
    dead, deadPeriod, dead_census, dead_neighbors_6m,
    Hegyi_dbh1, Hegyi_dbh3, Hegyi_dbh4,
    ci_1, ci_2, ci_3, dCI,
    basal_area_1982, basal_area_2016, basal_area_2022,
    ba_growth_1982_2016, ba_growth_2016_2022,
    BAI_1982_2016_cm2yr, BAI_2016_2022_cm2yr,
    dBAI_cm2yr, growth_ratio,
    elevation_m, slope_deg, aspect_deg,
    soil_moisture, soil_temperature, air_temperature,
    relative_humidity, moisture_class,
    dataset_type,
    geometry
  ) %>%
  # Standardize column types
  mutate(
    Plot = as.factor(Plot),
    hc4 = factor(hc4, levels = c("Suppressed", "Co-Dominant", "Dominant")),
    dataset_type = factor(dataset_type, levels = c("multi", "single"))
  ) %>%
  mutate(
    growth_ratio = ifelse(is.infinite(growth_ratio), NA, growth_ratio),
    dBAI_cm2yr = ifelse(is.infinite(dBAI_cm2yr), NA, dBAI_cm2yr),
    across(c(BAI_1982_2016_cm2yr, BAI_2016_2022_cm2yr, dBAI_cm2yr, growth_ratio),
           ~ifelse(. < 0, NA, .))  # remove clearly erroneous negatives
  ) %>% 
  arrange(Plot, Tree)

# check
summary(select(all_plots_clean, growth_ratio, dBAI_cm2yr))
range(all_plots_clean$growth_ratio, na.rm = TRUE)

all_plots_clean %>%
  group_by(dataset_type) %>%
  summarize(
    n = n(),
    mean_growth = mean(growth_ratio, na.rm = TRUE),
    prop_NA_growth = mean(is.na(growth_ratio))
  )




subalpine <- all_plots_clean %>% 
  filter(Spec %in% c("PICO", "PIFL", "PIEN", "ABLA"))  # remove any non-target trees
  
subalpine_non_spatial <- sf::st_drop_geometry(subalpine) # create non spatial dataset

save(subalpine, file = "data/growth_long/subalpine.RData") # save
save(subalpine_non_spatial, file = "data/growth_long/subalpine_non_spatial.RData") # save



