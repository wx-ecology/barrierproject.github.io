
# --------- function ------------ #

plot_example <- function(ind, DOP_variable, threshold) {
  example.sf.pts = st_as_sf(dat_L1 %>% filter(trackID.unique == ind) %>% 
                              arrange(TimestampUTC), coords = c("Location.long", "Location.lat"), crs = 4326) %>%
    mutate(DOP_flag = ifelse(!!sym(DOP_variable) > threshold, TRUE, FALSE))
  example.sf.line <- dat_L1 %>% filter(trackID.unique == ind) %>% arrange(TimestampUTC) %>%
    summarise(
      geometry = st_cast(
        st_combine(
          st_sfc(lapply(seq_len(n()), function(i) st_point(c(Location.long[i], Location.lat[i]))), crs = 4326)
        ),
        "LINESTRING"
      ),
      .groups = "drop"
    ) %>%
    st_as_sf()
  mapview::mapView(example.sf.pts, zcol = "DOP_flag") + mapview::mapView(example.sf.line)
}


plot_example_no_flag <- function(ind, DOP_variable, threshold) {
  example.sf.pts = st_as_sf(dat_L1 %>% filter(trackID.unique == ind) %>% 
                              arrange(TimestampUTC), coords = c("Location.long", "Location.lat"), crs = 4326) %>%
    mutate(DOP_flag = ifelse(!!sym(DOP_variable) > threshold, TRUE, FALSE)) %>% filter(!DOP_flag)
  
  example.sf.line <- example.sf.pts %>% summarise(do_union = FALSE,
                                                  geometry = st_combine(geometry)) %>%
    mutate(geometry = st_cast(geometry, "LINESTRING")) %>%
    st_as_sf()
  mapview::mapView(example.sf.pts, zcol = "DOP_flag") + mapview::mapView(example.sf.line)
}

# ---- main code ---- #
study = "red_deer_euromammals_berchtesgaden_np" # << -------- change it to whatever dataset you want to check DOP 


dat_L0 <- read_csv(paste0("./data/movement/raw_data/global_barrier/L0/", study, ".csv" ))
dat_L1 <- read_csv(paste0("./data/movement/raw_data/global_barrier/L1/", study, "_L1.csv" ))

names(dat_L0)
DOP_variable = "DOP" # << -------- change to DOP or other DOP related names when appropriate

any(!is.na(dat_L0 %>% pull (DOP_variable))) # if FALSE, all DOP is NA. then this study don't have real DOP data.

## ---- visualize to find the DOP threshold ----- ###
# if decided to use DOP, then all study should probably filter by DOP for consistency regardless whether the individual is flagged or not
DOP.dat = dat_L0 %>% dplyr::select(trackID.original, TimestampUTC, Location.long, Location.lat, 
                                   DOP_variable) ## or change DOP to the correct name
dat_L1 <- dat_L1 %>% left_join(DOP.dat, by = c("trackID.original", "TimestampUTC", "Location.long", "Location.lat" ))

# study = "red_deer_euromammals_na2"
# dat_L1_2 <- read_csv(paste0("./data/movement/raw_data/global_barrier/L1/", study, "_L1.csv" )) %>%
#   mutate(TimestampUTC = ymd_hms(TimestampUTC))
# dat_L1 <- dat_L1_2 %>% left_join(.,dat_L1)

# check DOP distribution to help select initial threshold
hist(dat_L0 %>% pull (DOP_variable))

# plot to viz DOP threshold 
# inds_as_example <- (ind_to_check %>% filter(Study.name == study) %>% pull(trackID.unique))[3] ## << ---- change the number to try out different individuals 
# OR 
inds_as_example <- unique(dat_L1$trackID.unique)[4] ## << ---- change the number to try out different individuals 
# OR
inds_as_example <- c("Cervus elaphus_1236")


dat.ind = dat_L1 %>% filter(trackID.unique == inds_as_example) 
summary(dat.ind$DOP)
quantile(dat.ind$DOP, 0.9, na.rm = T)

DOP_threshold = 7.5 ##  << ---- change the number to try out different threshold

plot_example( inds_as_example, DOP_variable, DOP_threshold)
plot_example_no_flag ( inds_as_example, DOP_variable, DOP_threshold)

## ----- until satisfied, then filter out the points ---- #
dat_L1_updated.pts <- st_as_sf(dat_L1 %>% arrange(TimestampUTC), coords = c("Location.long", "Location.lat"), crs = 4326) %>%
  mutate(DOP_flag = ifelse(!!sym(DOP_variable) > DOP_threshold, TRUE, FALSE)) %>% filter(!DOP_flag)
dat_L1_updated.line <- dat_L1_updated.pts %>% summarise(do_union = FALSE, geometry = st_combine(geometry)) %>%
  mutate(geometry = st_cast(geometry, "LINESTRING")) %>% st_as_sf()


# mapview::mapView(dat_L1_updated.pts, zcol = "DOP_flag") + mapview::mapView(dat_L1_updated.line)

# save as geopackage in face of further manual deletion 
st_write(dat_L1_updated.pts, paste0("./data/movement/raw_data/global_barrier/L2_viz/temp_shp/", study, "_DOPchecked_pts.gpkg"), append = FALSE)
st_write(dat_L1_updated.line, paste0("./data/movement/raw_data/global_barrier/L2_viz/temp_shp/", manual_study, "_lines.gpkg"), append = FALSE)


## ----- 
inds_as_example <- c("Cervus elaphus_368", "Cervus elaphus_375", 
                     "Cervus elaphus_376", "Cervus elaphus_582", 
                     "Cervus elaphus_586", "Cervus elaphus_611", 
                     "Cervus elaphus_617")
dat_L1_1 <- dat_L1 %>% filter(trackID.unique %in% inds_as_example) %>%
  group_by(trackID.unique) %>%
  mutate(DOP_threshold = quantile(DOP, 0.95, na.rm=T)) %>%
  ungroup() %>%
  filter(DOP < DOP_threshold) %>%
  select(-DOP_threshold)

dat_L1_2 <- dat_L1 %>% filter(!trackID.unique %in% inds_as_example) 
dat_L1 = rbind(dat_L1_1, dat_L1_2) %>% select(Binomial, Study.name, trackID.unique,
                                              TimestampUTC, Location.long, Location.lat )

write_csv(dat_L1, paste0("./data/movement/raw_data/global_barrier/L2/", study, "_L2.csv" ))