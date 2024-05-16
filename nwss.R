#Bring in the CDC NWSS program's SARS-CoV-2 files, combine them, and prep for visualizing in Microreact

#Load required packages
library(jsonlite)
library(tidyverse)

#Bring in the state names, coordinates, colors
states_regions <- read_csv("https://raw.githubusercontent.com/genomicepi/nwss/main/states_regions_colors.csv")

#Create regional colors
regional_colors <- tribble(
~Region, ~Region__colour, 
  "National", "#1b9e77",
  "Midwest", "#d95f02",
  "South", "#7570b3", 
  "Northeast","#e7298a",
  "West", "#66a61e")

#Bring in the lineage colors from the WADOH molecular epidemiology team
lineages <- read_csv("https://raw.githubusercontent.com/northwest-pgcoe/lineage_classifications/main/lineage_classifications.csv")%>%
  select(Variant = lineage_extracted, hex_code, variant_group = doh_variant_name) %>%
  mutate(hex_code = substr(hex_code, 1, 7))

#Bring all the NWSS datasets
#Need to update this one to make it generic so it pulls the most recent data since
forecasts <- read_csv("https://www.cdc.gov/coronavirus/2019-ncov/downloads/science/forecasting/hospitalizations/april2024/2024-04-29_forecasts_hosp_state.csv")%>%
  filter(model == "cfa-wwrenewal" | data_type == "observed data") %>%
  filter(location_name != "National")%>%
  select(-starts_with("quantile"))
  
regional_levels_long <- jsonlite::fromJSON("https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/NWSSRegionalLevel.json") %>%
            mutate_at(vars(Midwest, National, Northeast, South, West), as.numeric) %>%
            mutate(date = as.Date(date)) %>%
            filter(date_period == "All Results") %>%
            select(-date_period) %>%
            pivot_longer(cols = -date, names_to = "Region", values_to = "regional_levels")
  
state_trends <- jsonlite::fromJSON("https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/NWSSStateLevel.json") %>%
            mutate_at(vars(value, national_value,region_value), as.numeric) %>%
            filter(date_period == "All Results")%>%
            mutate(date = as.Date(date)) %>%
            select(date, State, value) %>%
            rename(Region = "State", regional_levels = "value" )

state_levels <- jsonlite::fromJSON("https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/NWSSStateMap.json")%>%
            mutate(date = max(regional_levels_long$date)) %>%
            mutate(activity_level = as.numeric(activity_level)) %>%
            rename(State = state_name)
            
map_dataset <- left_join(state_levels, states_regions, by="State")%>%
  mutate(id = row_number())

variants_long <- jsonlite::fromJSON("https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/NWSSVariantBarChart.json") %>%
  mutate_at(vars(-week_end), as.numeric) %>%
  mutate(date = as.Date(week_end)) %>%
  select(-week_end) %>%
  pivot_longer(cols = -date, names_to = "Variant", values_to = "share") %>%
  mutate(Region = "National", variant_present = "yes") %>%
  mutate(Variant = gsub("_", ".", Variant)) %>%
  left_join(lineages, by = "Variant") %>%
  mutate(hex_code = ifelse(is.na(hex_code) | hex_code == "", "#808080", hex_code)) %>%
  mutate(hex_code = substr(hex_code, 1, 7))%>%
  select(date, Variant, share, Region, Variant__colour = hex_code, variant_present)

state_level_data <- left_join(state_trends, state_levels, by = c("date","Region" = "State"))%>%
  full_join(forecasts, by = c("date" = "observation_or_target_date", "Region" = "location_name"))%>%
  left_join(states_regions, by = c("Region" = "State")) %>%
  mutate(geographic_level = "State-Territory")

national_data <- full_join(variants_long, regional_levels_long, by = c("date", "Region")) %>%
  mutate(cdc_region = Region, geographic_level = "Aggregated") %>%
  left_join(regional_colors, by = "Region")

combined_national_regional_state <- full_join(national_data, state_level_data, by = c("date", "Region", "regional_levels", "cdc_region", "geographic_level", "Region__colour")) %>%
  filter(!(Region %in% c("U.S. Virgin Islands", "Puerto Rico", "American Samoa", "Guam", "United States")))%>%
  mutate(id = row_number(), data_source = "NWSS")

#write the file with the national and state level data
write_csv(combined_national_regional_state, "nwss_combined_file.csv", na = "")

#Write the file with the current state levels for the map
write_csv(map_dataset, "current_state_levels.csv", na = "")

