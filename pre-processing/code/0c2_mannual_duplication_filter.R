duplication_check_files <- list.files("./data/movement/raw_data/global_barrier/L1/duplication_to_check", 
                                      pattern = "\\.csv$",
                                full.names = T)

duplication_check_sheet <- read_csv("./data/movement/raw_data/global_barrier/duplicated_inds.csv")

check_studies = unique(duplication_check_sheet$relevant_study)

for (i in 1:length(check_studies)) {
  
#  check_study = check_studies[i]
 check_study = "jaguar_sao_bento"
   print(check_study)
  str_split(check_study, ",")
  
 dat = read_csv(paste0("./data/movement/raw_data/global_barrier/L1/duplication_to_check/",
                       check_study, ".csv"))
  
 ## remove based on mannual decisions for each study 
 length(unique(dat$trackID.unique))
 nrow(dat)
 unique(dat$trackID.unique)
 
 ## ------------- filter section --------- ##
 delete_list = c( "Vulpes vulpes_166" )
 dat <- dat %>% filter(!trackID.unique %in% delete_list)


# dat <- dat %>% filter(trackID.unique == "Sus scrofa_182")
#dat <- dat %>% filter(!(trackID.unique == "Panthera onca_218" & duplicated == T))

# dat = dat %>% filter(!(trackID.unique == "Felis silvestris_16"))
 ## ------------------------------------- ##
 
 length(unique(dat$trackID.unique))
 nrow(dat)

 write_csv(dat, paste0("./data/movement/raw_data/global_barrier/L1/",
                  check_study, "_L1.csv"))
 
 file.rename(from = paste0("./data/movement/raw_data/global_barrier/L1/duplication_to_check/",
                           check_study, ".csv"),
             to = paste0("./data/movement/raw_data/global_barrier/L1/duplication_to_check/done/",
             check_study, ".csv"))
 

 }
