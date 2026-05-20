#######################
# Matthew S. Channing #
# GEOG 590            #
# 5/17/2026           #
#######################


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(sf)
library(dplyr)
library(tidyr)
library(modelsummary)
library(broom)

# Write in Census ---------------------------------------------------------
# Get MSAs in Ohio
oh_cen_data <- get_decennial(
  geography = "tract",
  variables = "P1_001N",
  state = "OH",
  geometry = TRUE,
  year = 2020
)

us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) |>
  filter(estimate >= 750000) |>
  transmute(urban_name = str_remove(NAME,
                                    fixed(", OH Urbanized Area (2010)")))

oh_urban_data <- oh_cen_data |>
  st_join(us_urban_areas, left = FALSE) |>
  select(-NAME) |>
  st_drop_geometry()

cl_urban_data <- oh_urban_data %>%
  filter(urban_name == ("Cleveland"))

# Calculate Population Density --------------------------------------------
# calculate area of the tract
oh_cen_data <- oh_cen_data |>
  mutate(
    tract_area = st_area(geometry)
  )

# change from m^2 to mi^2
oh_cen_data <- oh_cen_data |>
  mutate(
    tract_area_sqmi  = as.numeric(st_area(geometry)) / 2.59e6
  )

# merge field
cl_urban_data <- cl_urban_data |>
  left_join(
    oh_cen_data |>
      st_drop_geometry() |>
      select(GEOID, variable, tract_area_sqmi),
    by = c("GEOID", "variable")
  )

# caculate density
cl_urban_data <- cl_urban_data |>
  mutate(
    tract_pop_density  = (value) /(tract_area_sqmi)
  )


# Create Landscape Calculation --------------------------------------------

cl_urban_data <- cl_urban_data |>
  mutate(threshold = case_when(
     tract_pop_density < 250  ~ "exurban",
     tract_pop_density < 550  ~ "suburban low",
     tract_pop_density < 800  ~ "suburban high",
     tract_pop_density < 1900  ~ "urban low",
     tract_pop_density > 1899 ~ "urban high"
  ))


# Create a Map ------------------------------------------------------------

# add geometry to the threshold
cl_urban_data <- cl_urban_data |>
  filter(variable == "P1_001N") |>
  left_join(
    oh_cen_data |> select(GEOID, geometry),
    by = c("GEOID" = "GEOID")
  ) |>
  st_as_sf()

# reorder thresholds
cl_urban_data <- cl_urban_data |>
  mutate(
    threshold = factor(
      threshold,
      levels = c(
        "exurban",
        "suburban low",
        "suburban high",
        "urban low",
        "urban high"
      )
    )
  )


# map the threshold
# set legend order

map1 <- ggplot(cl_urban_data) +
  geom_sf(aes(fill = threshold), color = NA) +
  scale_fill_viridis_d(na.value = "grey90",
                       labels = c(
                         "exurban"  = "Exurban (<250)",
                         "suburban low" = "Suburban Low (250–549)",
                         "suburban high" = "Suburban High (550–799)",
                         "urban low" = "Urban Low (800–1899)",
                         "urban high" = "Urban High (≥1900)"
                       )
  ) +
  theme_minimal()+
  labs(title = "Map of Census Tracts by Population per Square Mile",
       caption = "Source: CensusDataProfilevariable;tidycensusRpackage")

# Calculate Additional Variables -------------------------------------------

# Percent Children --------------------------------------------------------


# get data for under children
acs_data <- get_acs(
  geography = "tract",
  state = "OH",
  variables = c(
    total = "B01001_001",
    m_u5 = "B01001_003",
    m_5_9 = "B01001_004",
    m_10_14 = "B01001_005",
    m_15_17 = "B01001_006",
    f_u5 = "B01001_027",
    f_5_9 = "B01001_028",
    f_10_14 = "B01001_029",
    f_15_17 = "B01001_030"
  ),
  year = 2020,
  survey = "acs5",
  geometry= FALSE
)

# widen the data
acs_wide <- acs_data |>
  pivot_wider(
    id_cols= GEOID,
    names_from = variable,
    values_from = estimate
  )

# calculate percent children
acs_wide <- acs_wide |>
  mutate(
    under18 =
      m_u5 + m_5_9 + m_10_14 + m_15_17 +
      f_u5 + f_5_9 + f_10_14 + f_15_17,

    total_pop = total,

    pct_under18 = under18 / total_pop
  )

# join the children data to Cleveland
cl_urban_data <- cl_urban_data |>
  left_join(
    acs_wide |> select(GEOID, total_pop, under18, pct_under18),
    by = "GEOID"
  )

# map the sub variable
map2 <- ggplot(cl_urban_data) +
  geom_sf(aes(fill = pct_under18), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal()+
  labs(title = "Map of Census Tracts by Percent of Population under 18",
       caption = "Source: ACSDataProfilevariable;tidycensusRpackage")

# Create a Graph ----------------------------------------------------------
# model percent children and density
# model1 is standard
model1 <- lm(pct_under18 ~ tract_pop_density, data = cl_urban_data)
# model2 is the log of model1
model2 <- lm(pct_under18 ~ log(tract_pop_density), data = cl_urban_data)
# model3 is the log of model1 with a threshold interactive effect
model3 <- lm(  pct_under18 ~ log(tract_pop_density) * threshold,
  data = cl_urban_data)


# graph model1
graph1 <- ggplot(model1, aes(x = tract_pop_density, y = pct_under18))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal()+
  labs(title = "Population Density by Percent of the Population under
       18 in a Tract",
       subtitle = "2020 5-year ACS estimates",
       y = "Percent of Census Tract Population under 18",
       x = "Census Tract Population Density",
       caption = "Source: ACSDataProfilevariable;tidycensusRpackage")

summary(model1)

# graph model2
graph2 <- ggplot(model1, aes(x = log(tract_pop_density), y = pct_under18))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal()+
  labs(title = "Log of Population Density by Percent of the Population under
       18 in a Tract",
       subtitle = "2020 5-year ACS estimates",
       y = "Percent of Census Tract Population under 18",
       x = "Log of Census Tract Population Density",
       caption = "Source: ACSDataProfilevariable;tidycensusRpackage")

summary(model2)

# graph model3
graph3 <- ggplot(cl_urban_data,
       aes(x = log(tract_pop_density),
           y = pct_under18,
           color = threshold)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()+
  labs(title = "Log of Population Density by Percent of the Population under
  18 in a Tract by Population Tresholds",
       subtitle = "2020 5-year ACS estimates",
       y = "Percent of Census Tract Population under 18",
       x = "Log of Census Tract Population Density",
       caption = "Source: ACSDataProfilevariable;tidycensusRpackage")


summary(model3)

# compare the average percent between thresholds
cl_urban_data |>
  group_by(threshold) |>
  summarise(mean_pct_under18 = mean(pct_under18, na.rm = TRUE))


graph4 <- ggplot(cl_urban_data, aes(x = threshold, y = pct_under18)) +
  geom_boxplot() +
  theme_minimal()+
  labs(title = "Average Percent of the Population under 18 in a Tract by
       Population Tresholds",
       subtitle = "2020 5-year ACS estimates",
       y = "Percent of Census Tract Population under 18",
       x = "Census Tract Population Density Thresholds",
       caption = "Source: ACSDataProfilevariable;tidycensusRpackage")

list(
  "model1" = model1,
  "model2" = model2,
  "model3" = model3) |>
  modelsummary(
    stars = TRUE,
    fmt = 2,
    coef_map = c(
      "(Intercept)" = "Intercept",
      "tract_pop_density" = "Tract Density",
      "log(tract_pop_density)" = "Log Density",
      "thresholdsuburban low" = "Suburban (Low)",
      "thresholdsuburban high" = "Suburban (High)",
      "thresholdurban low" = "Urban (Low)",
      "thresholdurban high" = "Urban (High)",
      "log(tract_pop_density):thresholdsuburban low" = "Log Density × Suburban (Low)",
      "log(tract_pop_density):thresholdsuburban high" = "Log Density × Suburban (High)",
      "log(tract_pop_density):thresholdurban low" = "Log Density × Urban (Low)",
      "log(tract_pop_density):thresholdurban high" = "Log Density × Urban (High)"
    ),
    gof_map = c("nobs", "r.squared")
  )


# Population Pyramids -----------------------------------------------------
# code variables by sex

male_vars   <- paste0("B01001_", sprintf("%03d", 3:25))
female_vars <- paste0("B01001_", sprintf("%03d", 27:49))


# read in data by sex
acs_male <- get_acs(
  geography = "tract",
  state = "OH",
  county = "Cuyahoga",
  variables = male_vars,
  year = 2020,
  survey = "acs5",
  geometry = FALSE
)
acs_female <- get_acs(
  geography = "tract",
  state = "OH",
  county = "Cuyahoga",
  variables = female_vars,
  year = 2020,
  survey = "acs5",
  geometry = FALSE
)

# combine
acs_age <- bind_rows(acs_male, acs_female)

# read in total age
acs_total <- get_acs(
  geography = "tract",
  state = "OH",
  county = "Cuyahoga",
  variables = "B01001_001",
  year = 2020,
  survey = "acs5",
  geometry = FALSE
)

acs_age <- bind_rows(acs_age, acs_total)

# define sex from variables
acs_age <- acs_age |>
  mutate(
    sex = case_when(
      as.integer(substr(variable, 8, 10)) %in% 3:25  ~ "Male",
      as.integer(substr(variable, 8, 10)) %in% 27:49 ~ "Female",
      TRUE ~ NA_character_
    )
  )

# set age categories

acs_age <- acs_age |>
  mutate(
    var_num = as.integer(substr(variable, 8, 10)),
    age_group = case_when(
      var_num %in% c(3, 27) ~ "0–4",
      var_num %in% c(4, 28) ~ "5–9",
      var_num %in% c(5, 29) ~ "10–14",
      var_num %in% c(6, 7, 30, 31) ~ "15–19",
      var_num %in% c(8, 9, 10, 32, 33, 34) ~ "20–24",
      var_num %in% c(11, 35) ~ "25–29",
      var_num %in% c(12, 36) ~ "30–34",
      var_num %in% c(13, 37) ~ "35–39",
      var_num %in% c(14, 38) ~ "40–44",
      var_num %in% c(15, 39) ~ "45–49",
      var_num %in% c(16, 40) ~ "50–54",
      var_num %in% c(17, 41) ~ "55–59",
      var_num %in% c(18, 19, 42, 43) ~ "60–64",
      var_num %in% c(20, 44) ~ "65–69",
      var_num %in% c(21, 45) ~ "70–74",
      var_num %in% c(22, 46) ~ "75–79",
      var_num %in% c(23, 47) ~ "80–84",
      var_num %in% c(24:25, 48:49) ~ "85+",
      TRUE ~ NA_character_
    )
  )


# keep only Cleveland tracts
acs_age <- acs_age |>
  semi_join(cl_urban_data, by = "GEOID")

# merge threshold
acs_age <- acs_age |>
  left_join(
    cl_urban_data |> select(GEOID, threshold),
    by = "GEOID"
  )

# create threshold groups
pyramid_data <- acs_age |>
  mutate(
    urban_group = case_when(
      threshold %in% c("urban low", "urban high") ~ "Urban",
      threshold %in% c("suburban low", "suburban high") ~ "Suburban",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    !is.na(urban_group),
    !is.na(sex),
    !is.na(age_group)
  )|>
group_by(urban_group, sex, age_group) |>
  summarize(population =sum(estimate, na.rm = TRUE), .groups = "drop")




# build pyramid
# define age levels
age_levels <- c(
  "0–4","5–9","10–14","15–19","20–24",
  "25–29","30–34","35–39","40–44",
  "45–49","50–54","55–59","60–64",
  "65–69","70–74","75–79","80–84","85+"
)

pyramid_data <- pyramid_data |>
  mutate(
    age_group = factor(age_group, levels = age_levels)
  )

# flip male to be negative
pyramid_data <- pyramid_data |>
  mutate(population = ifelse(sex == "Male", -population, population))
# define urban pyramid
pyramid_urban <- pyramid_data |>
  filter(urban_group == "Urban")
# define suburban pyramid
pyramid_suburban <- pyramid_data |>
  filter(urban_group == "Suburban")

max_pop <- max(abs(pyramid_data$population), na.rm = TRUE)
max_pop_sub <- max(abs(pyramid_suburban$population), na.rm = TRUE)


urban_plot <- ggplot(pyramid_urban,
                     aes(x = population,
                         y = age_group,
                         fill = sex)) +
  geom_col(width = 0.95, alpha = 0.75) +
  theme_minimal(base_family = "Verdana",
                base_size = 12) +
  scale_x_continuous(
    labels = ~ scales::number_format(scale = .001, suffix = "k")(abs(.x)),
    limits = max_pop * c(-1, 1)
  ) +
  scale_fill_manual(values = c("darkred", "navy")) +
  labs(
    title = "Urban Population Pyramid",
    x = "Population",
    y = "Age Group",
    fill = "Sex"
  )


suburban_plot <- ggplot(pyramid_suburban,
                        aes(x = population,
                            y = age_group,
                            fill = sex)) +
  geom_col(width = 0.95, alpha = 0.75) +
  theme_minimal(base_family = "Verdana",
                base_size = 12) +
  scale_x_continuous(
    labels = ~ scales::number_format(scale = .001, suffix = "k")(abs(.x)),
    limits = max_pop_sub * c(-1, 1)
  ) +
  scale_fill_manual(values = c("darkred", "navy")) +
  labs(
    title = "Suburban Population Pyramid",
    x = "Population",
    y = "Age Group",
    fill = "Sex"
  )



# Export all figures ------------------------------------------------------

# Set the output directory
output_dir <- "C:/Users/mattc/OneDrive - University Of Oregon/Classes/2026 Spring/GEOG 590-Matt-Laptop/GEOG 590/Replication"
# Save the plot in PNG format
ggsave(paste0(output_dir, "_threshold_map.png"), plot = map1, width = 6, height = 4)
ggsave(paste0(output_dir, "_children_map.png"), plot = map2, width = 6, height = 4)
ggsave(paste0(output_dir, "_model1_graph.png"), plot = graph1, width = 6, height = 4)
ggsave(paste0(output_dir, "_model2_graph.png"), plot = graph2, width = 6, height = 4)
ggsave(paste0(output_dir, "_model3_graph.png"), plot = graph3, width = 6, height = 4)
ggsave(paste0(output_dir, "_avg_children_threshold.png"), plot = graph4, width = 6, height = 4)
ggsave(paste0(output_dir, "_urban_plot.png"), plot = urban_plot, width = 6, height = 4)
ggsave(paste0(output_dir, "_suburban_plot.png"), plot = suburban_plot, width = 6, height = 4)
