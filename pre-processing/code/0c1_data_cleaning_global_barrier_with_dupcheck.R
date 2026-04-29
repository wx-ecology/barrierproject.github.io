# process per study shared through movebank
# for the global linear barrier project

setwd("C:/Users/Hoosterhoff/surfdrive/Global linear feature project")

library(tidyverse)
library(geosphere)
library(janitor)
library(sf)
library(rnaturalearth)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

###########################################################
L0_cleaning_overview <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1S6qI76q1k9B_s8tv6QT1SfGg7PX5tn5HOJ4SSGXie6Q/edit?gid=1409443043#gid=1409443043")

L0_cleaning_overview <- L0_cleaning_overview %>% 
  select(Study.name, Binomial) %>%
  arrange(Binomial, Study.name) %>%
  filter(!is.na(Binomial), Binomial != "??" )

files <- list.files("./Cleaned_data_L0", full.names = T)
file_locations <- tibble(Study.name = sub("\\.csv$", "", basename(files)),
                 directory = files) %>% 
  left_join(L0_cleaning_overview[,c("Study.name", "Binomial")], by = "Study.name") %>% 
  mutate(Study.name = make_clean_names(Study.name)) %>% 
  arrange(Binomial, Study.name) %>% 
  filter(!is.na(Binomial))

###########################################################
#### clean and check duplications   ---------
###########################################################

# table(file_locations$Binomial)
# file_locations %>% dplyr::select(-directory)

metadata.all = tibble()
duplication_check_all = tibble()

spp = unique(file_locations$Binomial)

for (i in 1: length(spp)) { ## Loop through everything, except jaguars and the NA species
  spp.i = spp[i]
  spp.df <- file_locations %>% filter(Binomial == spp.i)

  print(paste0("now processing ", spp.i, ", species ", i, " out of ", length(spp), " ..."))
  
  dat.spp = tibble()
  metadata.spp = tibble()
  for (ii in (1: nrow(spp.df))) {
      
    Study.name.valid = spp.df$Study.name[ii] ### HO: Had to rename this, otherwise if the column was present, the old (uncleaned) names would stay, which leads to issues later on when joining by study name 
    ####### ONE: generate spp level dataset, valid point check and get interval metadata #########
    dat.spp.ii = read_csv(spp.df$directory[ii]) %>% 
      # 0. some tracks has NA binomials, and some does not have Study.name fix that. 
      mutate(Study.name =  Study.name.valid, Binomial = spp.i) %>% 
      # 1. filter NA timestamp and locations 
      filter(!is.na(TimestampUTC), !is.na(Location.lat), !is.na(Location.long),
      # 2. filter out 0,0 locations 
             (!(Location.lat == 0 & Location.long == 0))) %>%
      mutate(TimestampUTC = parse_date_time(TimestampUTC, orders = c("ymd_HMS", "ymd", "mdy_HM", "ymd", "mdy"))) %>%
      # 3. filter out unrealistic time 
      filter(TimestampUTC < ymd("2025-04-01")) %>% ## I have changed the date here from March to April, because some of the data we got last week includes very recent records
      arrange(trackID.original, TimestampUTC)
    
    #dat.spp.ii$Study.name = make_clean_names(paste0("Jaguar_SouthAmerica_", unique(dat.spp.ii$Study.name)))
    
    # calculate temporal intervals 
    dat.temp = dat.spp.ii %>%
      group_by(Study.name, trackID.original) %>%
      mutate(interval_hours = as.numeric(difftime(TimestampUTC, lag(TimestampUTC), units = "hours")))
    
    # Categorize regularity within individuals
    individual_summary <- dat.temp  %>%
      group_by(trackID.original) %>%
      summarize(
        time_span_days = as.numeric(difftime(max(TimestampUTC), min(TimestampUTC), units = "days")),
        n_pts = n(),
        min_interval = quantile(interval_hours, 0.05, na.rm = TRUE),
        max_interval = quantile(interval_hours, 0.95, na.rm = TRUE),
        is_regular_within = if_else(min_interval == max_interval, TRUE, FALSE)
      )
    
    ### delete individuals with less than 60 days of data or less than 50 displacements # only kept the <50 displacements criterion
     track_keep <- individual_summary %>% filter(n_pts >= 50) %>% pull(trackID.original)
     dat.spp.ii <- dat.spp.ii %>% filter(trackID.original %in% track_keep)
    
    overall_summary <- individual_summary %>% 
      summarize(
        is_regular_across = if_else(all(is_regular_within), TRUE, FALSE),
        min_resolution_across = min(min_interval, na.rm = T),
        majority_resolution_across = as.numeric(names(sort(table(dat.temp$interval_hours), decreasing = TRUE)[1]))
      )
    
    metadata.spp.i <- tibble(
      Study.name = make_clean_names(unique(dat.spp.ii$Study.name)),
      Binomial = unique(dat.spp.ii$Binomial),
      n_ind = length(unique(dat.spp.ii$trackID.original)),
      n_valid_pts = nrow(dat.spp.ii),
      start_date = date(min(dat.spp.ii$TimestampUTC, na.rm = T)),
      end_date = date(max(dat.spp.ii$TimestampUTC, na.rm = T)),
      interval_type = ifelse( all((individual_summary$is_regular_within)&overall_summary$is_regular_across),
                                "regular", "varies"),
      min_interval_h = overall_summary$min_resolution_across,
      major_interval_h = overall_summary$majority_resolution_across
    )
    
    metadata.spp = rbind(metadata.spp, metadata.spp.i)
    dat.spp = rbind(dat.spp, dat.spp.ii %>% 
                      dplyr::select(Study.name, Binomial, trackID.original, 
                                    TimestampUTC, Location.long, Location.lat)) # this is the species dataset with no NA locs or times
   } 
  
  # make a unique ID for each individual within the spp
  dat.spp <- dat.spp %>% left_join(dat.spp %>% dplyr::select(Study.name, Binomial, trackID.original) %>% 
                                     distinct() %>% 
                                     mutate(serial = seq(1, nrow(.)), trackID.unique = paste0(Binomial, "_", serial)) %>%
                                     dplyr::select(-serial))
  
  ######################### TWO: check speed #################################
  speed.df <- dat.spp %>% 
    arrange(trackID.unique, TimestampUTC) %>%
    group_by(Study.name) %>%
    mutate(
      step_length = distHaversine(cbind(Location.long, Location.lat), 
                                       cbind(lag(Location.long), lag(Location.lat))),  # in meters
           time_diff = as.numeric(difftime(TimestampUTC, lag(TimestampUTC), units = "secs")),
           speed_mps = step_length / time_diff
    ) %>%
    mutate(
      Q1 = quantile(speed_mps, 0.25, na.rm = TRUE),
      Q3 = quantile(speed_mps, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      is_outlier = speed_mps > (Q3 + 100 * IQR)
    ) %>%
    ungroup()

  metadata.spp  <- metadata.spp %>% 
    left_join(
      speed.df %>% 
        group_by (Study.name) %>%
        summarise(n_speed_outliers = sum(is_outlier, na.rm = T))
  ) %>%
    left_join(
      speed.df %>% filter(is_outlier == F) %>%
        group_by (Study.name) %>%
        summarise(ave_speed_mps = mean(speed_mps, na.rm = T))      
    )
  
  dat.spp <- speed.df %>% filter(!is_outlier) %>%
    dplyr::select(Study.name, Binomial, trackID.unique, 
                  TimestampUTC, Location.long, Location.lat)
  
  ######################## extract country info for later duplication check ################
  set.seed(10)
  dat.spp.sample <- dat.spp %>% group_by(Study.name, trackID.unique) %>%
    sample_n(10) %>% st_as_sf(., coords = c("Location.long", "Location.lat"), crs = 4326, remove = FALSE)
  dat.spp.country <- st_join(dat.spp.sample, world[, c("name", "iso_a3")], join = st_within) %>%
    st_drop_geometry() %>%
    filter(!is.na(name)) %>%
    group_by(Study.name) %>% summarise(country = paste(sort(unique(name)), collapse = ", "))

  # update metadata 
  metadata.all <- metadata.all %>% 
    rbind(metadata.spp  %>% left_join(dat.spp.country))

  ######################## duplication check ################
  dupes.spp <- janitor::rget_dupes(dat.spp, Location.long, Location.lat, TimestampUTC) %>% 
    # Add a group_id to be able to filter by group at a later stage
    group_by(Location.long, Location.lat, TimestampUTC) %>% 
    mutate(dupe_group = cur_group_id()) %>% 
    group_by(dupe_group) %>% 
    mutate(trackIDs_in_group = paste(trackID.unique, collapse = "-")) %>% 
    select(-dupe_count)
  
  if (nrow(dupes.spp) == 0) { #if no duplication detected
    dat.spp %>% 
      group_by(Study.name) %>% 
      group_walk(.f = ~write_csv(.x, paste0("./Cleaned_data_L1/temp_code_check/", .y, "_L1.csv")), .keep = T) 
  # directly save into the L1 folder. L1 cleaning done (for the whole species).
    } else {
      dupe_group_lookup <- dupes.spp %>%
        select(dupe_group, trackIDs_in_group) %>%
        distinct()
      # Update the species data with a column indicating if it is a duplicate or not
      dat.spp <- dat.spp %>%
        left_join(dupes.spp) %>%
        mutate(duplicated = case_when(!is.na(dupe_group) ~ TRUE,
                                      is.na(dupe_group) ~ FALSE))
      dupe_sequences <- dat.spp %>%
        group_by(trackID.unique) %>%
        arrange(TimestampUTC) %>%
        mutate(run_id = rleid(duplicated)) %>%               # Identify runs
        group_by(trackID.unique, run_id) %>%
        filter(duplicated[1], n() > 1) %>%            # Keep only TRUE runs with length > 1
        ungroup()
      
      if (nrow(dupe_sequences) == 0) {
        dat.spp %>% 
          group_by(Study.name) %>% 
          group_walk(.f = ~write_csv(.x, paste0("./Cleaned_data_L1/temp_code_check/", .y, "_L1.csv")), .keep = T) 
        # directly save into the L1 folder. L1 cleaning done (for the whole species).
        
        } else {
          # Get studies without duplicates and save them in L1 directly
          good_studies = dat.spp %>% 
            filter(!Study.name %in% unique(dupe_sequences$Study.name)) %>% 
            pull(Study.name) %>% 
            unique()
          
          if(length(good_studies) > 0){
            dat.spp %>% 
              filter(Study.name %in% good_studies) %>% 
              group_by(Study.name) %>% 
              group_walk(.f = ~write_csv(.x, paste0("./Cleaned_data_L1/temp_code_check/", .y, "_L1.csv")), .keep = T) 
            # directly save into the L1 folder. L1 cleaning done (for these studies).
            }
        
        # then for problematic studies: 
        ## if duplication happen between two individuals consecutively, then this may not be an coincidence and would need mannual inspection
        # first, find out if the duplication between two individuals happen more than once
          problematic_dupe_pair <- dupe_group_lookup %>%
            group_by(trackIDs_in_group) %>%
            summarise(n = n()) %>%
            filter(n > 1) %>%
            pull(trackIDs_in_group)
          
          target_group <- dupe_group_lookup %>% 
            filter(trackIDs_in_group %in% problematic_dupe_pair) %>% 
            pull(dupe_group)
        
          # then, check whether these duplications are consecutive 
          
          duplication_check <- dupe_sequences %>% 
            filter(dupe_group %in% target_group) %>%
            dplyr::select(Study.name, trackID.unique, trackIDs_in_group, dupe_group, run_id) %>%
            arrange(trackID.unique, run_id) %>% 
            group_by(trackID.unique, run_id, trackIDs_in_group, Study.name) %>% 
            summarise(n = n(), .groups = "drop") %>% 
            filter(n > 1)
          
          if(nrow(duplication_check) == 0){ ## if none are consecutive, save all studies in L1
            dat.spp %>%
              filter(!(Study.name %in% good_studies)) %>% 
              group_by(Study.name) %>% 
              group_walk(.f = ~write_csv(.x, paste0("./Cleaned_data_L1/temp_code_check/", .y, "_L1.csv")), .keep = T) 
            # directly save into the L1 folder. L1 cleaning done (for this species).
            } else {
              duplication_check <- duplication_check %>% 
                group_by(trackIDs_in_group) %>% 
                mutate(relevant_study = paste(sort(unique(na.omit(Study.name))), collapse = ",")) %>% 
                distinct(trackIDs_in_group, relevant_study) %>% 
              # Join with the number of duplicated fixes among two pairs (cannot calculate from the above, because of the
              # filtering that happened here and in the creation of dupe_sequences)
                left_join(dat.spp %>% filter(duplicated) %>% group_by(trackID.unique, trackIDs_in_group) %>% summarise(n = n()) %>% ungroup() %>%  distinct(trackIDs_in_group, n),
                          by = "trackIDs_in_group") %>% 
              ## Add info on the individuals, so we can assess how much duplication there is
                separate_wider_delim(trackIDs_in_group, delim = "-", names = c("ind1", "ind2")) %>% 
                rowwise() %>% 
                mutate(ind1_n_fixes = filter(dat.spp, trackID.unique == ind1) %>% nrow(),
                       ind2_n_fixes = filter(dat.spp, trackID.unique == ind2) %>% nrow(),
                       ind1_perc = round(n / ind1_n_fixes, 2),
                       ind2_perc = round(n / ind2_n_fixes, 2))
              
              # then we export bad studies for visual checks 
              relevant_study_singles = unique(unlist(strsplit(duplication_check$relevant_study, ",")))
              
              dat.spp %>% 
                filter(Study.name %in% relevant_study_singles) %>% 
                group_by(Study.name) %>% 
                group_walk(.f = ~write_csv(.x, paste0("./Cleaned_data_L1/temp_code_check/duplication_to_check/", .y, ".csv")), .keep = T) 
            
              duplication_check_all = rbind(duplication_check_all, duplication_check)
           
              write_csv(duplication_check_all, "./Cleaned_data_L1/temp_code_check/Metadata/duplicated_inds.csv")
            }
        } 
      }
  # Write metadata and cleaned trackS
  write_csv(metadata.all, "./Cleaned_data_L1/temp_code_check/Metadata/metadata.csv")
  }



