# this file generate geopackages for studies with flagged individuals for mannual outlier removal. 

library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(geosphere)
library(tidyverse)

files <- list.files("./data/movement/raw_data/global_barrier/L1/", pattern = "\\.csv$")

# read all study records 
L0_cleaning_overview <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1S6qI76q1k9B_s8tv6QT1SfGg7PX5tn5HOJ4SSGXie6Q/edit?gid=1409443043#gid=1409443043")
L0_cleaning_overview <- L0_cleaning_overview %>% dplyr::select(Study.name, Binomial, viz_checked_by_two) %>%
  mutate(Study.name = janitor::make_clean_names(Study.name)) %>%
  arrange(Binomial) 


# read L1 records
L1_files = tools::file_path_sans_ext(basename(list.files("./data/movement/raw_data/global_barrier/L1")))

file_progress <- L0_cleaning_overview %>% mutate(
  L1_cleaned = ifelse(Study.name %in% str_remove(L1_files, "_L1$") , TRUE, FALSE)
)

# read viz check records 
Viz_check_overview <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1S6qI76q1k9B_s8tv6QT1SfGg7PX5tn5HOJ4SSGXie6Q/edit?gid=1996138525#gid=1996138525")
# binomial.completed <- Viz_check_overview %>% 
#   filter(`visual 1 completed?` & `visual 2 completed?`) %>%
#   pull(Binomial)
# file_progress <- file_progress %>% mutate(
#   viz_checked_by_two = ifelse(Binomial %in% binomial.completed & L1_cleaned, TRUE, FALSE)
# ) # this is for R1 

# ------- R2 ----------
study.completed.R2 <- Viz_check_overview %>%
  filter(`visual 1 completed?` == "TRUE" & `visual 2 completed?` == "TRUE") 
study.completed.R2 <- study.completed.R2[64:nrow(study.completed.R2),] %>%
  select(Binomial, total_ind) %>%
  rename(Study.name = Binomial)

# find studies that pass all checks 
bad_studies <- unique(Viz_check_overview %>% 
  dplyr::select(Study.name.R2, `problematic trackID.unique.R2`, Initial.R2) %>%
    filter(!is.na(Study.name.R2)) %>% 
  pull(Study.name.R2) ) 

study.completed.R2 <- study.completed.R2 %>% mutate(
  ready_for_L2 = ifelse((!Study.name %in% bad_studies) == "TRUE", TRUE, FALSE)
) 


# file_progress <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1S6qI76q1k9B_s8tv6QT1SfGg7PX5tn5HOJ4SSGXie6Q/edit?gid=1409443043#gid=1409443043") %>%
#   dplyr::select(Study.name, Binomial, L1_checked, viz_checked_by_two,L2_checked) %>%
#   rename(ready_for_L2 = L2_checked)  %>% 
#   mutate(viz_checked_by_two = ifelse(viz_checked_by_two == "SPECIAL CASE", "FALSE", viz_checked_by_two),
#                                      viz_checked_by_two = as.logical(viz_checked_by_two),
#          ready_for_L2 = ifelse(ready_for_L2 == "SPECIAL CASE", "FALSE", ready_for_L2),
#          ready_for_L2 = as.logical(ready_for_L2))

good_studies <- study.completed.R2 %>% filter(ready_for_L2) %>% pull(Study.name)
L2_files = list.files("./data/movement/raw_data/global_barrier/L2")
for (study in good_studies[1:length(good_studies)]){
  if (study %in% tools::file_path_sans_ext(basename(L2_files))) {
    print(study)
    next
  } else {
    dat = read_csv(paste0("./data/movement/raw_data/global_barrier/L1/", study, "_L1.csv")) %>%
      dplyr::select("Study.name","Binomial","trackID.unique",
                    "TimestampUTC", "Location.long","Location.lat")
    write_csv(dat, paste0("./data/movement/raw_data/global_barrier/L2/", study, "_L2.csv"))
  }
}

manual_studies <- study.completed.R2 %>% filter(!ready_for_L2) %>% pull(Study.name) 
manual_studies <- janitor::make_clean_names(manual_studies)
already_exist <- list.files("./data/movement/raw_data/global_barrier/L2_viz/temp_shp/", pattern = "\\_pts.gpkg$") 
already_exist <- str_remove(already_exist, "_pts.gpkg")

manual_studies <- manual_studies[which(!manual_studies %in% already_exist)]
for (manual_study in  manual_studies[1:length(manual_studies)]) {
  
  dat = read_csv(paste0("./data/movement/raw_data/global_barrier/L1/", manual_study, "_L1.csv"))
  
  dat <- dat %>%
    arrange(trackID.unique, TimestampUTC) %>%
    group_by(trackID.unique) %>%
    mutate(
      time_diff_secs = as.numeric(difftime(TimestampUTC, lag(TimestampUTC), units = "secs")),
      step_length = distHaversine(cbind(Location.long, Location.lat),
                                  cbind(lag(Location.long), lag(Location.lat))),  # in meters
      speed_mps = step_length / time_diff_secs
    ) %>%
    mutate(
      Q1.speed = quantile(speed_mps, 0.25, na.rm = TRUE),
      Q3.speed = quantile(speed_mps, 0.75, na.rm = TRUE),
      IQR.speed = Q3.speed - Q1.speed,
      speed_flag = (speed_mps > (Q3.speed + 25*IQR.speed)) | (speed_mps < (Q1.speed - 25*IQR.speed)),
      speed_flag = ifelse(is.na(speed_flag), FALSE, speed_flag)
    ) %>%
    ungroup() %>%
    dplyr::select("Study.name","Binomial","trackID.unique",
                  "TimestampUTC", "Location.long","Location.lat", "speed_flag")
  
  pts.sf <- st_as_sf(dat %>% arrange(trackID.unique, TimestampUTC), 
                     coords = c("Location.long", "Location.lat"), crs = 4326) %>%
    mutate(Location.long = st_coordinates(.)[,1],
           Location.lat = st_coordinates(.)[,2])
  
  lines_list <- dat %>% 
    arrange(trackID.unique, TimestampUTC) %>%
    group_split(trackID.unique) %>%
    lapply(function(group) {
      st_as_sf(group, coords = c("Location.long", "Location.lat"), crs = 4326) %>%
        summarise(trackID.unique = unique(group$trackID.unique),
                  geometry = st_cast(st_combine(geometry), "LINESTRING"))
    })
  
  lines.sf <- do.call(rbind, lines_list)
  
  st_write(pts.sf, paste0("./data/movement/raw_data/global_barrier/L2_viz/temp_shp/", manual_study, "_pts.gpkg"), append = FALSE)
  st_write(lines.sf, paste0("./data/movement/raw_data/global_barrier/L2_viz/temp_shp/", manual_study, "_lines.gpkg"), append = FALSE)
}

# ------------ R 3 --------------
studies.R3 = (Viz_check_overview %>% 
  select(Binomial) %>% 
  filter(Binomial != "NA", Binomial != "Study.name") %>%
  rename(Study.name = Binomial) %>%
  mutate(Study.name = make_clean_names(Study.name)))[100:128,]

# find studies that pass all checks 
bad_studies <- unique(Viz_check_overview %>% 
                        dplyr::select(Study.name.R3, `problematic trackID.unique.R3`, Initial.R3) %>%
                        filter(!is.na(Study.name.R3)) %>% 
                        pull(Study.name.R3) ) 

studies.R3 <- studies.R3 %>% mutate(
  ready_for_L2 = ifelse((!Study.name %in% bad_studies) == "TRUE", TRUE, FALSE),
  ready_for_L2 = case_when(Study.name %in% c("mule_deer_wyoming_pt1",
                                            "mule_deer_wyoming_pt2",
                                            "white_tailed_deer_ny") ~ FALSE, .default = ready_for_L2 )
) 

good_studies <- studies.R3 %>% filter(ready_for_L2) %>% pull(Study.name)
L2_files = list.files("./data/movement/raw_data/global_barrier/L2")
for (study in good_studies[1:length(good_studies)]){
  # if (study %in% tools::file_path_sans_ext(basename(L2_files))) {
  #   print(study)
  #   next
  # } else {
    dat = read_csv(paste0("./data/movement/raw_data/global_barrier/L1/", study, "_L1.csv")) %>%
      dplyr::select("Study.name","Binomial","trackID.unique",
                    "TimestampUTC", "Location.long","Location.lat")
    write_csv(dat, paste0("./data/movement/raw_data/global_barrier/L2/", study, "_L2.csv"))
  # }
}

manual_studies <- studies.R3  %>% filter(!ready_for_L2) %>% pull(Study.name) 
manual_studies <- janitor::make_clean_names(manual_studies)
already_exist <- list.files("./data/movement/raw_data/global_barrier/L2_viz/temp_shp/", pattern = "\\_pts.gpkg$") 
already_exist <- str_remove(already_exist, "_pts.gpkg")

manual_studies <- manual_studies[which(!manual_studies %in% already_exist)]
for (manual_study in  manual_studies[12:length(manual_studies)]) {
  
  if(manual_study == "mule_deer_wyoming_pt1") {manual_study = "mule_deer_wyoming"}
  if(manual_study == "mule_deer_wyoming_pt2") {next}
  
  dat = read_csv(paste0("./data/movement/raw_data/global_barrier/L1/", manual_study, "_L1.csv"))
  
  dat <- dat %>%
    arrange(trackID.unique, TimestampUTC) %>%
    group_by(trackID.unique) %>%
    mutate(
      time_diff_secs = as.numeric(difftime(TimestampUTC, lag(TimestampUTC), units = "secs")),
      step_length = distHaversine(cbind(Location.long, Location.lat),
                                  cbind(lag(Location.long), lag(Location.lat))),  # in meters
      speed_mps = step_length / time_diff_secs
    ) %>%
    mutate(
      Q1.speed = quantile(speed_mps, 0.25, na.rm = TRUE),
      Q3.speed = quantile(speed_mps, 0.75, na.rm = TRUE),
      IQR.speed = Q3.speed - Q1.speed,
      speed_flag = (speed_mps > (Q3.speed + 25*IQR.speed)) | (speed_mps < (Q1.speed - 25*IQR.speed)),
      speed_flag = ifelse(is.na(speed_flag), FALSE, speed_flag)
    ) %>%
    ungroup() %>%
    dplyr::select("Study.name","Binomial","trackID.unique",
                  "TimestampUTC", "Location.long","Location.lat", "speed_flag")
  
  pts.sf <- st_as_sf(dat %>% arrange(trackID.unique, TimestampUTC), 
                     coords = c("Location.long", "Location.lat"), crs = 4326) %>%
    mutate(Location.long = st_coordinates(.)[,1],
           Location.lat = st_coordinates(.)[,2])
  
  lines_list <- dat %>% 
    arrange(trackID.unique, TimestampUTC) %>%
    group_split(trackID.unique) %>%
    lapply(function(group) {
      st_as_sf(group, coords = c("Location.long", "Location.lat"), crs = 4326) %>%
        summarise(trackID.unique = unique(group$trackID.unique),
                  geometry = st_cast(st_combine(geometry), "LINESTRING"))
    })
  
  lines.sf <- do.call(rbind, lines_list)
  
  st_write(pts.sf, paste0("./data/movement/raw_data/global_barrier/L2_viz/temp_shp/", manual_study, "_pts.gpkg"), append = FALSE)
  st_write(lines.sf, paste0("./data/movement/raw_data/global_barrier/L2_viz/temp_shp/", manual_study, "_lines.gpkg"), append = FALSE)
}

a <-Viz_check_overview %>% 
  select(`problematic trackID.unique.R3`, `Study.name.R3`) %>% 
  group_by(`Study.name.R3`, `problematic trackID.unique.R3`) %>% 
  summarise(n = n())
