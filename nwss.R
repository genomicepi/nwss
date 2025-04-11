# Load required packages
library(jsonlite)
library(tidyverse)

# Determine the script's directory
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set output file paths
output_file1 <- file.path(script_dir, "nwss_combined_file.csv")
output_file2 <- file.path(script_dir, "current_state_levels.csv")

# Bring in the state names, coordinates, colors
states_regions <- read_csv("https://raw.githubusercontent.com/genomicepi/nwss/main/states_regions_colors.csv")

# Create regional colors
regional_colors <- tribble(
  ~Region, ~Region__colour, 
  "National", "#1b9e77",
  "Midwest", "#d95f02",
  "South", "#7570b3", 
  "Northeast","#e7298a",
  "West", "#66a61e")

# Bring in the lineage colors
lineages <- read_csv("https://raw.githubusercontent.com/NW-PaGe/lineage_classifications/refs/heads/main/data/lineage_classifications.csv") %>%
  select(Variant = lineage_extracted, hex_code, variant_group = doh_variant_name) %>%
  mutate(hex_code = substr(hex_code, 1, 7))

regional_levels_long <- jsonlite::fromJSON("https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/NWSSRegionalLevel.json") %>%
  mutate_at(vars(Midwest_WVAL, National_WVAL, Northeast_WVAL, South_WVAL, West_WVAL), as.numeric) %>%
  mutate(date = as.Date(Week_Ending_Date)) %>%
  filter(Data_Collection_Period == "All Results") %>%
  select(-Data_Collection_Period, -Week_Ending_Date) %>%
  pivot_longer(cols = -date, names_to = "Region", values_to = "regional_levels") %>%
  mutate(Region = str_remove(Region, "_WVAL"))

state_trends <- jsonlite::fromJSON("https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/NWSSStateLevel.json") %>%
  mutate_at(vars("State/Territory_WVAL",National_WVAL,Regional_WVAL), as.numeric) %>%
  filter(Data_Collection_Period == "All Results")%>%
  mutate(date = as.Date(Week_Ending_Date)) %>%
  select(date, "State/Territory", "State/Territory_WVAL") %>%
  rename(Region = "State/Territory", regional_levels = "State/Territory_WVAL")

state_levels <- jsonlite::fromJSON("https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/NWSSStateMap.json")%>%
  mutate(date = max(regional_levels_long$date)) %>%
  mutate(activity_level = as.numeric(activity_level)) %>%
  rename(State = "State/Territory")

map_dataset <- left_join(state_levels, states_regions, by="State") %>%
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

state_level_data <- left_join(state_trends, state_levels, by = c("date","Region" = "State")) %>%
  left_join(states_regions, by = c("Region" = "State")) %>%
  mutate(geographic_level = "State-Territory")

national_data <- full_join(variants_long, regional_levels_long, by = c("date", "Region")) %>%
  mutate(cdc_region = Region, geographic_level = "Aggregated") %>%
  left_join(regional_colors, by = "Region")

combined_national_regional_state <- full_join(national_data, state_level_data, by = c("date", "Region", "regional_levels", "cdc_region", "geographic_level", "Region__colour" = "State__colour")) %>%
  filter(!(Region %in% c("U.S. Virgin Islands", "Puerto Rico", "American Samoa", "Guam", "United States")))%>%
  mutate(id = row_number(), data_source = "NWSS")

# Write the file with the national and state-level data
write_csv(combined_national_regional_state, output_file1, na = "")

# Write the file with the current state levels for the map
write_csv(map_dataset, output_file2, na = "")

# Print confirmation message
message("Files written to: ", script_dir)


