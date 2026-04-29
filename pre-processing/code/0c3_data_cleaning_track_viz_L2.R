# this file generate image pdfs for each individuals so that we can mark problematic individuals

library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)

files <- list.files("./data/movement/raw_data/global_barrier/L1/")

L0_cleaning_overview <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1S6qI76q1k9B_s8tv6QT1SfGg7PX5tn5HOJ4SSGXie6Q/edit?gid=1409443043#gid=1409443043")
L0_cleaning_overview <- L0_cleaning_overview %>% dplyr::select(Study.name, Binomial, L1_checked, viz_checked_by_two) %>%
  mutate(Study.name = janitor::make_clean_names(Study.name)) %>%
  arrange(Binomial) %>%
  filter(L1_checked == "TRUE" &
         viz_checked_by_two == "FALSE")


## ----- viz check round 1 by biome ---- ##
#viz_check = tibble()
for (s in 1:length(Binomial)) {
  spp = Binomial[s]
  files.spp = L0_cleaning_overview %>% filter(Binomial == spp) %>% pull (Study.name)
  
  
  pdf(paste0("./data/movement/raw_data/global_barrier/L2_viz/", spp, ".pdf"), width = 8, height = 6)
  spp.n.ind = 0
  
  for (f in 1:length(files.spp)) {
    
    file = files.spp[f]
    if (!file.exists(paste0("./data/movement/raw_data/global_barrier/L1/", file, "_L1.csv"))) {
      next
    }
    dat <- read_csv(paste0("./data/movement/raw_data/global_barrier/L1/", file, "_L1.csv"))
    
    # count n individual 
    spp.n.ind = spp.n.ind + length(unique(dat$trackID.unique))
    
    # make maps looping through individuals 
    inds = unique(dat$trackID.unique)
    
    # make individual plot
    for (i in 1:length(inds)) {
      
      ind.i = dat %>% filter(trackID.unique == inds[i]) %>% arrange(TimestampUTC) 
      
      res <- ind.i  %>% summarise(resolution = round( as.numeric(median(diff(TimestampUTC)),units = "hours") ,1))
      gap_threshold <- as.numeric(50 * res)
      start_date <- min(ind.i$TimestampUTC)
      end_date   <- max(ind.i$TimestampUTC)
      
      # Calculate time difference between consecutive points and mark burst
      ind.i <- ind.i %>%
        mutate(
          time_diff_hours = as.numeric(difftime(TimestampUTC, lag(TimestampUTC), units = "hours")),
          burst = cumsum(if_else(is.na(time_diff_hours) | time_diff_hours > gap_threshold, 1, 0))
        )
      
      # make points sf
      pts.sf <- st_as_sf(ind.i %>% arrange(TimestampUTC), coords = c("Location.long", "Location.lat"), crs = 4326) 
      first_point <- pts.sf[1, ]
      last_point  <- pts.sf[nrow(pts.sf), ]
      
      # make line sf
      lines.sf <- ind.i %>%
        arrange(TimestampUTC) %>%
        group_by(burst) %>%
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
      
      # title 
      title_str <- paste0(
        "Study name: ", unique(ind.i$Study.name), "\n",
        "Animal ID: ", unique(ind.i$trackID.unique), "\n",
        "Temporal resolution: ", res, " hours\n",
        "Monitoring: ", format(start_date, "%Y-%m-%d"), " to ", format(end_date, "%Y-%m-%d")
      )
      
      # graph
      p <- ggplot() +
        annotation_map_tile(type = "osm") +  # Adds OpenStreetMap tiles
        geom_sf(data = lines.sf, aes(color = factor(burst)), size = 1, alpha = 0.4) +
        geom_sf(data = pts.sf, aes(color = factor(burst)), size = 2, alpha = 0.6) +
        geom_sf(data = first_point, color = "green", size = 4, shape = 17) + # First point (triangle)
        geom_sf(data = last_point,  color = "green", size = 4, shape = 15) + # Last point (square)
        labs(
          title = title_str,
          color = "Burst",
          x = "Longitude", y = "Latitude"
        ) +
        #scale_color_brewer(palette = "Set1") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 10, hjust = 0.5, lineheight = 1.1),
          legend.position="none"
        )
      
      print(p) # Important: print() sends the plot to the PDF device[7][1]
      
    }
  }
  
  dev.off()  # Close the PDF device
  # rename pdf file to also include total number of individual of the spp 
  file.rename(from = paste0("./data/movement/raw_data/global_barrier/L2_viz/", spp, ".pdf"), 
              to = paste0("./data/movement/raw_data/global_barrier/L2_viz/", janitor::make_clean_names(spp), "_", spp.n.ind, "_ind.pdf"))

  viz_check = rbind (viz_check,
                     tibble(Binomial = spp,
                            total_ind = spp.n.ind))
  
}


## ----- viz check round 2 by study (for the new studies added after round 1) ---- ##
files.spp = L0_cleaning_overview$Study.name

viz_check = tibble()
  for (f in 1:length(files.spp)) {
    
    file = files.spp[f]
  

    if (!file.exists(paste0("./data/movement/raw_data/global_barrier/L1/", file, "_L1.csv"))) {
      print( paste0( "L1 level ", file, " not in directory"))
      next
    } else {
      
      pdf(paste0("./data/movement/raw_data/global_barrier/L2_viz/R2/", file, ".pdf"), width = 8, height = 6)

      
    dat <- read_csv(paste0("./data/movement/raw_data/global_barrier/L1/", file, "_L1.csv"))
    
    # count n individual 
    spp.n.ind = length(unique(dat$trackID.unique))
    
    # make maps looping through individuals 
    inds = unique(dat$trackID.unique)
    
    # make individual plot
    for (i in 1:length(inds)) {
      
      ind.i = dat %>% filter(trackID.unique == inds[i]) %>% arrange(TimestampUTC) 
      
      res <- ind.i  %>% summarise(resolution = round( as.numeric(median(diff(TimestampUTC)),units = "hours") ,1))
      gap_threshold <- as.numeric(50 * res)
      start_date <- min(ind.i$TimestampUTC)
      end_date   <- max(ind.i$TimestampUTC)
      
      # Calculate time difference between consecutive points and mark burst
      ind.i <- ind.i %>%
        mutate(
          time_diff_hours = as.numeric(difftime(TimestampUTC, lag(TimestampUTC), units = "hours")),
          burst = cumsum(if_else(is.na(time_diff_hours) | time_diff_hours > gap_threshold, 1, 0))
        )
      
      # make points sf
      pts.sf <- st_as_sf(ind.i %>% 
                           arrange(TimestampUTC), coords = c("Location.long", "Location.lat"), crs = 4326) 
      first_point <- pts.sf[1, ]
      last_point  <- pts.sf[nrow(pts.sf), ]
      
      # make line sf
      lines.sf <- ind.i %>%
        arrange(TimestampUTC) %>%
        group_by(burst) %>%
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
      
      # title 
      title_str <- paste0(
        "Study name: ", unique(ind.i$Study.name), "\n",
        "Animal ID: ", unique(ind.i$trackID.unique), "\n",
        "Temporal resolution: ", res, " hours\n",
        "Monitoring: ", format(start_date, "%Y-%m-%d"), " to ", format(end_date, "%Y-%m-%d")
      )
      
      # graph
      p <- ggplot() +
        annotation_map_tile(type = "osm") +  # Adds OpenStreetMap tiles
        geom_sf(data = lines.sf, aes(color = factor(burst)), size = 1, alpha = 0.4) +
        geom_sf(data = pts.sf, aes(color = factor(burst)), size = 2, alpha = 0.6) +
        geom_sf(data = first_point, color = "green", size = 4, shape = 17) + # First point (triangle)
        geom_sf(data = last_point,  color = "green", size = 4, shape = 15) + # Last point (square)
        labs(
          title = title_str,
          color = "Burst",
          x = "Longitude", y = "Latitude"
        ) +
        #scale_color_brewer(palette = "Set1") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 10, hjust = 0.5, lineheight = 1.1),
          legend.position="none"
        )
      
      print(p) # Important: print() sends the plot to the PDF device[7][1]
      
    }
    
    dev.off()  # Close the PDF device
    # rename pdf file to also include total number of individual of the spp 
    file.rename(from = paste0("./data/movement/raw_data/global_barrier/L2_viz/R2/", file, ".pdf"), 
                to = paste0("./data/movement/raw_data/global_barrier/L2_viz/R2/", janitor::make_clean_names(file), "_", spp.n.ind, "_ind.pdf"))
    
    viz_check = rbind (viz_check,
                       tibble(Study.name = file,
                              total_ind = spp.n.ind))
    }
  }


write_csv(viz_check, "./data/movement/raw_data/global_barrier/L2_viz/viz_check_sheet_R2.csv")


# 
# study.name = "impala_kenya"
# 
# dat = read_csv(paste0("./data/movement/raw_data/global_barrier/L1/", study.name, "_L1.csv"))
# 
# 
# dat2 = read_csv(paste0("./data/movement/raw_data/global_barrier/L2/", study.name, "_L2.csv"))
# 




write_csv(viz_check, "./data/movement/raw_data/global_barrier/L2_viz/viz_check_sheet.csv")


# 
# study.name = "impala_kenya"
# 
# dat = read_csv(paste0("./data/movement/raw_data/global_barrier/L1/", study.name, "_L1.csv"))
# 
# 
# dat2 = read_csv(paste0("./data/movement/raw_data/global_barrier/L2/", study.name, "_L2.csv"))
# 

