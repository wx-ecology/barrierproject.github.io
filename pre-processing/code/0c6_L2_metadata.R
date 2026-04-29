## this script summarize final dataset metadata and make data summary viz

library(tidyverse)
library(janitor)
library(rnaturalearth)
library(sf)

# -------------------------------------------- #
# double check the files match with the record #
# -------------------------------------------- #
## check the files match the cleaning metadata 
L2_overview_names <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1S6qI76q1k9B_s8tv6QT1SfGg7PX5tn5HOJ4SSGXie6Q/edit?gid=833886232#gid=833886232") %>%
  pull(Study.name) %>%
  make_clean_names(.)
L2_files_names <- tools::file_path_sans_ext(basename(list.files("./data/movement/raw_data/global_barrier/L2"))) %>%
  str_sub(., end = -4) %>%
  make_clean_names(.)
L2_overview_names[which(! L2_overview_names %in%  L2_files_names)] # which study on google sheet but not in folder
                                                                   # tibetan gaezelle should only be in the 10d and 1d derived folder but not in the raw data folder.
L2_files_names[which(! L2_files_names %in% L2_overview_names)] # which study in folder but not on google sheet 

## check the files match the data provider sheet
data_provide_names <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1aNH7NGao7G2EkN6o7mqMWVHCBXUUquEMjjFkEKu_OxA/edit?gid=218611593#gid=218611593") %>%
  pull(Study_name) %>%
  make_clean_names(.)
L2_files_names[which(! L2_files_names %in% data_provide_names)]
data_provide_names[which(! data_provide_names %in% L2_files_names)] 
# guanaco_argentina and puma_concolor_argentina got merged into the same dataset in the data folder (but two records)

# -------------------------------------------- #
# extract metadata #
# -------------------------------------------- #
L2_files <- list.files("./data/movement/raw_data/global_barrier/L2", full.names = T)
cleaning_metadata <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1S6qI76q1k9B_s8tv6QT1SfGg7PX5tn5HOJ4SSGXie6Q/edit?gid=1409443043#gid=1409443043") %>%
  select(Study.name, Binomial, `original # ind`) %>%
  mutate(Study.name = make_clean_names(Study.name))
countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


L2_metadata <- tibble()
for (i in 1:length(L2_files)) {
  
  dat = read_csv(L2_files[i])
  
  # check data column structure
  # if (ncol(dat) != 6) {
  #   dat = dat %>% select("Study.name","Binomial","trackID.unique",
  #                        "TimestampUTC", "Location.long", "Location.lat")
  #   write_csv(dat, paste0("./data/movement/raw_data/global_barrier/L2/",
  #                         unique(dat$Study.name), "_L2.csv"))
  # }
 
  # check temporal strcuture
  if (!"POSIXct" %in% class(dat$TimestampUTC)) {
    dat$TimestampUTC = ymd_hms(dat$TimestampUTC)
  }
  
  print(paste("processing", i, "out of 300 files..."))
  
  Study.name.i = unique(dat$Study.name)
  Binomial.i = unique(dat$Binomial)
  original.n.ind.i = cleaning_metadata %>% filter(Study.name == Study.name.i) %>% pull(`original # ind`)
  final.n.ind.i = length(unique(dat$trackID.unique))
  total.n.pts.i = nrow(dat)
  start.date.i = min(dat$TimestampUTC)
  end.date.i = max(dat$TimestampUTC)
  fix.rate.i = dat %>%
    arrange(trackID.unique, TimestampUTC) %>%
    group_by(trackID.unique) %>%
    mutate(diff = round(as.numeric(TimestampUTC - lag(TimestampUTC), units = "mins")),1) %>%
    ungroup() %>%
    summarise(median.interval.mins = median(diff, na.rm = T),
              sd.interval.mins = round(sd(diff, na.rm = T), 1))
  ave.long.i = mean(dat$Location.long)
  ave.lat.i = mean(dat$Location.lat)
  
  loc.i <- st_as_sf(dat, coords = c("Location.long", "Location.lat"), crs = 4326) %>%
    st_join(., countries[, c("name", "iso_a3")]) %>%
    st_drop_geometry(.) %>% 
    select(name, iso_a3) %>%
    distinct(.)
  countries.i = paste(loc.i$name, collapse = ", ")
  iso_a3.i =  paste(loc.i$iso_a3, collapse = ", ")

  L2_metadata = rbind(L2_metadata, 
                      tibble(Study.name = Study.name.i,
                             Binomial = Binomial.i,
                             original.n.ind = original.n.ind.i, 
                             final.n.ind = final.n.ind.i,  
                             total.n.pts = total.n.pts.i, 
                             start.date = start.date.i, 
                             end.date = end.date.i, 
                             median.interval.mins = fix.rate.i$median.interval.mins,
                             sd.interval.mins = fix.rate.i$sd.interval.mins,
                             ave.long = ave.long.i,
                             ave.lat = ave.lat.i, 
                             countries = countries.i, 
                             iso = iso_a3.i))
  
}

## mannually add the tibetan_gazelle_chiatung info to the metadata. 
dat <- read_rds("./data/movement/displacement_data/global_barrier/1d_tibetan_gazelle_chiatung.rds")
L2_metadata = rbind(L2_metadata, 
                    tibble(Study.name = "tibetan_gazelle_chiatung",
       Binomial = "Procapra picticaudata",
       original.n.ind = 6, 
       final.n.ind = 6,  
       total.n.pts = nrow(dat)*2, 
       start.date = min(dat$first_timestamp), 
       end.date = max(dat$last_timestamp), 
       median.interval.mins = 24*60,
       sd.interval.mins = NA,
       ave.long = mean((dat$mid_point)[,1]),
       ave.lat = mean((dat$mid_point)[,2]), 
       countries = "China", 
       iso = "CHN"))


L2_metadata$start.date <- as_date(L2_metadata$start.date)
L2_metadata$end.date <- as_date(L2_metadata$end.date)
write_csv(L2_metadata, "./data/movement/raw_data/global_barrier/L2_metadata.csv")

# -------------------------------------------- #
# viz #
# -------------------------------------------- #
L2_metadata <- read_csv("./data/movement/raw_data/global_barrier/L2_metadata.csv")

sum(L2_metadata$final.n.ind)
sum(L2_metadata$total.n.pts)

L2_metadata %>%
  separate_rows(countries, sep = ", ") %>%
  filter(countries != "NA") %>%
  pull(countries) %>%
  unique()


# -------------------------------------------- #
# create individual lookup table (which unique individual in which study) #
# -------------------------------------------- #

L2_files <- list.files("./data/movement/raw_data/global_barrier/L2", full.names = T)

ind_study_lookup = tibble()
for (i in 1:length(L2_files)) {
  
  dat = read_csv(L2_files[i])
  print(paste("processing", i, "out of 300 files..."))
  
  df.i <- dat %>% select(Study.name,  Binomial, trackID.unique) %>% distinct()
  ind_study_lookup <- rbind(ind_study_lookup, df.i)
}

write_csv(ind_study_lookup, "./data/movement/raw_data/global_barrier/ind_study_lookup.csv")


# -------------------------------------------- #
# create study-contact table (which unique individual in which study) #
# -------------------------------------------- #  
contact_metadata <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1aNH7NGao7G2EkN6o7mqMWVHCBXUUquEMjjFkEKu_OxA/edit?gid=501527720#gid=501527720") %>%
  select(Study_name, Contributor1, Contributor2, Contributor3, Contributor4, Contributor5, Contributor6,
         Email1, Email2, Email3, Email4, Email5, Email6) %>%
  filter(!is.na(Study_name)) %>%
  mutate(Study.name = make_clean_names(Study_name)) 
length(unique(contact_metadata$Study.name))

contact_metadata_long <- 
  contact_metadata %>%
  pivot_longer(
    cols = starts_with("Contributor"),
    names_to = "contrib_num",
    names_pattern = "Contributor(\\d+)",  # extract the contributor number
    values_to = "Contributor"
  ) %>%
  pivot_longer(
    cols = starts_with("Email"),
    names_to = "email_num",
    names_pattern = "Email(\\d+)",  # extract the email number
    values_to = "Email"
  ) %>%
  # Keep rows where contributor number matches email number
  filter(contrib_num == email_num) %>%
  select(-contrib_num, -email_num, -Study_name) %>%
  filter(!is.na(Contributor) & Contributor != "")  # optionally remove empty contributors
length(unique(contact_metadata_long$Study.name))

#remove rows where Contributor is "?" only if the same study has other valid contributors,
# but keep "?" rows if they are the only contributors for that study.
contact_metadata_filter <- contact_metadata_long %>%
  group_by(Study.name) %>%
  # Check if there is any valid contributor (not "?")
  mutate(has_valid = any(Contributor != "?")) %>%
  # Filter out rows where Contributor is "?" only if there is at least one valid contributor
  filter(!(Contributor == "?" & has_valid)) %>%
  ungroup() %>%
  select(-has_valid)
length(unique(contact_metadata_filter$Study.name))

contact_metadata_filter <- contact_metadata_filter %>% mutate(is_data_retained_in_L2 = 
                                     ifelse(Study.name %in% ind_study_lookup$Study.name, "TRUE", "FALSE"))

write_csv(contact_metadata_filter, "./data/movement/raw_data/global_barrier/study_contact_lookup_draft.csv")


## ---- specific to Wenjing ---- ##
# a study summary table from the full set to which one is present in 10 day and/or 1 day research 
  all.studies = (read_csv("./data/movement/raw_data/global_barrier/study_contact_lookup_draft.csv") %>%
                   pull(Study.name) %>% unique(.))
  L2.studies = (read_csv(".//data/movement/raw_data/global_barrier/ind_study_lookup.csv") %>%
                  pull(Study.name) %>% unique(.))
  pd.10d.studies = list.files("./data/movement/displacement_data/global_barrier/10d") %>% str_remove(., "^10d_") %>% str_remove(., ".rds")
  pd.1d.studies = list.files("./data/movement/displacement_data/global_barrier/1d") %>% str_remove(., "^1d_") %>% str_remove(., ".rds")
  
#double check all studies contain all names
#  all_names <- unique(c(all.studies, L2.studies, pd.10d.studies, pd.1d.studies))
  
  df <- data.frame(
    Study.name = all.studies,
    L2.studies = all.studies %in% L2.studies,
    pd.10d.studies = all.studies %in% pd.10d.studies,
    pd.1d.studies = all.studies %in% pd.1d.studies
  )
  write_csv(df, "./data/movement/displacement_data/study_name_summaries.csv")
  
  # -------------------------------------------- #
  # create study-contact table email list for sharing updates #
  # -------------------------------------------- #  
  library(googlesheets4)
  gs4_auth()
  
  contacts <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1aNH7NGao7G2EkN6o7mqMWVHCBXUUquEMjjFkEKu_OxA/edit?gid=579736364#gid=579736364") %>%
    filter(include_in_update_1 == TRUE) %>%
    select(Study.name, Contributor, Email)
  
  metadata <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1S6qI76q1k9B_s8tv6QT1SfGg7PX5tn5HOJ4SSGXie6Q/edit?gid=833886232#gid=833886232") %>%
    select(Study.name, Binomial,countries) 
  
  contacts <- contacts %>% left_join(metadata)
  
  a <- contacts %>% group_by(Contributor) %>% summarise(n = length(unique(Email)))
  
  
  sheet_url <- "https://docs.google.com/spreadsheets/d/1nVs_tltPszvdcWPmb7FMWf2RkAx3yXCDqfNSvzXHyQ8/edit?usp=sharing"
  sheet_write(contacts, ss = sheet_url, sheet = "updated contact")
  
  email = tibble(email = contacts$Email, contributors = contacts$Contributor) %>% distinct(.)%>% arrange(contributors)
  
  sheet_url <- "https://docs.google.com/spreadsheets/d/1aNH7NGao7G2EkN6o7mqMWVHCBXUUquEMjjFkEKu_OxA/edit?gid=2128356962#gid=2128356962"
  sheet_write(email, ss = sheet_url, sheet = "unique contacts")
  
  paste(unique(email$contributors), collapse = ", ")
  