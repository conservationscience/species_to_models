
#' @param indicators_project file path where all project data is kept eg 'N:/Quantitative-Ecology/Indicators-Project/'
#' @param location string that denotes the directory of the location you want to process eg 'Serengeti'
#' @param scenario string that denotes which impact directory you want to look in eg "Baseline"
#' @param simulation name of the BuildModel directory you want to process eg  "001_BuildModel/"
#' @returns saves massbin, abundance, biomass and generation lengths to adaptor
#' output folder

## TODO: Add max and min generation length columns
## TODO: Work out if we can only read in certain columns of the large .txt files, 
## and also use ff package to reduce memory use of these big matrices


get_age_structure_data <- function(indicators_project, location, scenario, simulation){
  
  require(tidyverse)
  #source( "C:\\Users\\ssteven\\Desktop\\Serengeti-analysis\\python-madingley\\Serengeti-Indicators\\ProcessOutputFunctions.R")
  
  # Get locations and file paths and simulation details
  
  dirs <- list.dirs(file.path(indicators_project, location, 
                              "Inputs_to_adaptor_code/Madingley_simulation_outputs", 
                              scenario, simulation), recursive = FALSE)
  model_results <- dirs[!str_detect(dirs, "input")]
  results_files <- list.files(model_results)
  model_inputs <- dirs[str_detect(dirs, "input")]
  simulation_number <- str_remove(simulation, '_BuildModel/')
  
  # Create or set output folder
  
  output_folder <- file.path(indicators_project, location,
                             "Outputs_from_adaptor_code/map_of_life",
                             scenario, simulation)
  
  if( !dir.exists( file.path(output_folder) ) ) {
    dir.create( file.path(output_folder), recursive = TRUE )
    
  }
  
  # function to filter lists by the element names
  
  filter_by_pattern <- function(pattern, your.list){
    
    require(stringr)
    
    names(your.list) %>% 
    str_detect(pattern) %>%
    keep(your.list, .)
    
  }

  # Get the new_cohorts data from which we can calculate age at first reproduction,
  # aka generation length
  
new_cohorts_name <- results_files[str_detect(results_files, "NewCohorts")]
new_cohorts <- read_tsv(file.path(model_results,new_cohorts_name, sep =""))


oldnames <- names(new_cohorts)
newnames <- str_replace(oldnames," ", "_")
names(new_cohorts) <- newnames
oldnames <- names(new_cohorts)
newnames <- str_replace(oldnames," ", "_")
names(new_cohorts) <- newnames
new_cohorts <- new_cohorts %>% dplyr::rename(ID = parent_cohort_IDs)

## Calculate generation lengths
## TODO: CHECK THIS IS CORRECT (I think it is better than using maturity b/c that signals when
## their current weight reaches adult size, not necessarily when they reproduce)

## Get timestep each cohort was born

born <- new_cohorts %>%
        dplyr::select(offspring_cohort_ID, functional_group, adult_mass, time_step) 

## Get timestep each cohort reaches reproductive maturity (check this is right)

reproduced <- new_cohorts %>%
              dplyr::select(ID, time_step) 

## Merge to create dataframe with timestep born, timestep reached maturity for
## each cohort

born_reproduced <- merge(born, reproduced, by.x = "offspring_cohort_ID", 
                        by.y = "ID")

## Calculate generation length for each cohort

generation_length <- born_reproduced %>%
                     group_by(offspring_cohort_ID) %>%
                     dplyr::filter(time_step.y == min(time_step.y)) %>%
                     dplyr::mutate(gen.length = (time_step.y - time_step.x)/12) # Convert from months to years

## Rename columns

names(generation_length) <- c("ID", "functionalgroup","adult_mass",
                             "birth_timestep","maturity_timestep",
                             "generation_length")

# Remove big objects

rm(born, reproduced, born_reproduced)


# Get growth file (this is massive, might be a problem when processing multiple
# replicates)

growth_name <- results_files[str_detect(results_files, "Growth")]
growth <- as.data.frame(read_tsv(file.path(model_results,growth_name, sep ="")))

# Get Maturity file (also huge)

maturity_name <- results_files[str_detect(results_files, "Maturity")]
maturity <- as.data.frame(read_tsv(file.path(model_results,maturity_name, sep ="")))

oldnames <- names(maturity)
newnames <- str_replace(oldnames," ", "_")
names(maturity) <- newnames

# Identify adult and juvenile cohorts and modify the data to contain only
# data for adults

all_ages_data <- growth[ , c("ID", "time_step", "Current_body_mass_g",
                             "functional_group","abundance")] %>%
  merge(new_cohorts[ , c("ID","adult_mass")],
        by = "ID", all = TRUE) %>%
  merge(generation_length[c("ID", "generation_length")], all = TRUE) %>%
  dplyr::mutate(adult = ifelse(Current_body_mass_g >= adult_mass, TRUE, FALSE)) %>%
  dplyr::mutate(biomass = Current_body_mass_g * abundance) %>%
  dplyr::mutate(new_functional_group = ifelse((functional_group == 14 | functional_group == 17), "14.17",
                                              ifelse((functional_group == 13 | functional_group == 16), "13.16",
                                                     ifelse((functional_group == 15 | functional_group == 18), "15.18", # combine iteroparous and semelparous ectotherms
                                                            functional_group)))) %>%
  dplyr::select(-functional_group)



rm(maturity, growth, new_cohorts)

# Turn the juvenile abundance and biomass into zeroes instead of subsetting, so
# we keep all time-steps (otherwise it drops any timesteps without adult cohorts
# and you end up with different time series for different groups)

adult_data <- all_ages_data %>%
              dplyr::mutate(adult_abundance = ifelse(adult == TRUE, abundance, 0)) %>%
              dplyr::mutate(adult_biomass = ifelse(adult == TRUE, biomass, 0)) %>%
              dplyr::select(-c(abundance, biomass)) %>%
              dplyr::rename(abundance = adult_abundance, biomass = adult_biomass)
              
# Function to sort cohorts into bodymass bins

# Get bodymass bins lower bounds

options(scipen = 999) # supress scientific notation

breaks <- read.csv(file.path(model_inputs, '/Model setup/Ecological definition files/MassBinDefinitions.csv'))

# Split into lower and upper bounds

bodymass_bins_upper <- breaks[-79,]
bodymass_bins_lower <- breaks[-1,]

# Combine to produce column called 'massbins' that we can use to merge other data later

bodymass_bins <- as.data.frame(cbind(bodymass_bins_lower, bodymass_bins_upper)) %>%
                 mutate(temp = paste(bodymass_bins_lower, bodymass_bins_upper,  
                                         sep = ",")) %>%
                 mutate (massbins = paste("(", temp, "]", sep = "")) %>%
                 mutate(bodymass_bin_index = c(78:1)) %>%
                 dplyr::select(-temp)

# Group by bodymass bins and timestep. 

## TODO: Check and fix massbin breaks to see if can get growth values to
## match massbin values better

## TODO: Check the biomass matrices match the massbin outputs

all_ages_list <- split(all_ages_data, all_ages_data$new_functional_group)
adult_list <- split(adult_data, adult_data$new_functional_group)


# Group cohorts into massbins and sum their abundance and biomass per timestep


group_by_massbin <- function(data, breaks) {
  
massbin_breaks <- c(breaks[,1])
  
fg <- max(data$new_functional_group, na.rm = TRUE)
  
massbin_data <- data %>%
  dplyr::select(time_step, Current_body_mass_g, new_functional_group, 
                abundance, biomass, adult) %>%
  dplyr::group_by(massbins = cut(Current_body_mass_g, 
                                 breaks = massbin_breaks, 
                                 na.rm = FALSE,
                                 dig.lab = 11)) %>%
  dplyr::group_by(massbins, time_step) %>%
  dplyr::summarise(abundance_sum = sum(abundance),
                   biomass_sum = sum(biomass)) %>%
  merge(bodymass_bins[ , c("massbins","bodymass_bin_index")],
        by = "massbins", all = TRUE) %>%
  dplyr::mutate(new_functional_group = fg) %>%
  dplyr::mutate(functional_group_index = paste(new_functional_group,
                                               bodymass_bin_index, sep = ".")) 


abundance_wide <- massbin_data %>%
                  dplyr::select(-biomass_sum) %>%
                  spread(time_step, abundance_sum,
                         fill = NA, convert = FALSE) %>%
                  dplyr::arrange(bodymass_bin_index) %>%
                   `rownames<-`(.[,4])  %>%
                  dplyr::select(-c(new_functional_group,massbins,
                                 massbins, bodymass_bin_index,
                                 functional_group_index))

abundance_matrix_temp <- as.matrix(abundance_wide[,-c(1,2)])

abundance_matrix_final <- ifelse(abundance_matrix_temp >= 1,
                                 log(abundance_matrix_temp),0)

rm(abundance_matrix_temp, abundance_wide)

biomass_wide <- massbin_data %>%
                dplyr::select(-abundance_sum) %>%
                spread(time_step, biomass_sum,
                           fill = NA, convert = FALSE) %>%
                dplyr::arrange(bodymass_bin_index) %>%
                `rownames<-`(.[,4])  %>%
                dplyr::select(-c(new_functional_group,massbins,
                                     massbins, bodymass_bin_index,
                                     functional_group_index))
  
biomass_matrix_temp <- as.matrix(biomass_wide[,-c(1,2)])
    
biomass_matrix_final <- ifelse(biomass_matrix_temp >= 1,
                               log(biomass_matrix_temp),0)

rm(biomass_matrix_temp, biomass_wide)

fg <- max(data$new_functional_group, na.rm = TRUE)

generation_lengths <- data %>%
  dplyr::select(ID, time_step, new_functional_group, 
                Current_body_mass_g, generation_length) %>%
  dplyr::group_by(massbins = cut(Current_body_mass_g, 
                                 breaks = massbin_breaks,  
                                 na.rm = FALSE,
                                 dig.lab = 11)) %>% #suppress scientific notation
  dplyr::group_by(massbins, new_functional_group) %>%
  dplyr::summarise(gen_length_mean = mean(generation_length, 
                                          na.rm = TRUE)) %>%
  # dplyr::summarise(gen_length_max = max(generation_length, 
  #                                         na.rm = TRUE)) %>%
  # dplyr::summarise(gen_length_min = min(generation_length, 
  #                                         na.rm = TRUE)) %>%
  merge(bodymass_bins[ , c("massbins","bodymass_bin_index")],
        by = "massbins", all = TRUE) %>%
  dplyr::mutate(new_functional_group = fg) %>%
  dplyr::mutate(functional_group_index = paste(new_functional_group, 
                                               bodymass_bin_index, sep = ".")) %>%
  dplyr::select(-c(new_functional_group,massbins,
                   massbins)) %>%
  dplyr::arrange(bodymass_bin_index)

output <- list(massbin_data, abundance_matrix_final, biomass_matrix_final, generation_lengths)

names(output) <- c("massbin_data_long", "abundance_wide", "biomass_wide", "generation_lengths")

return(output)

}

functional_group_data_all <- lapply(all_ages_list, group_by_massbin, breaks = breaks)

massbin_long_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                                    pattern = "massbin_data_long"))
massbin_long_all <- do.call(rbind,massbin_long_list)
rm(massbin_long_list)

saveRDS( massbin_long_all, file = file.path(output_folder,paste(scenario, 
                                  simulation_number, "massbin_long_all", sep = "_" )))
write.csv( massbin_long_all, file = file.path(output_folder,paste(scenario, 
                                    simulation_number, "massbin_long_all.csv", sep = "_" )))

abundance_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                                 pattern = "abundance_wide"))
abundance_all <- do.call(rbind,abundance_list)
rm(abundance_list)

saveRDS( abundance_all, file = file.path(output_folder,paste(scenario, 
                               simulation_number, "abundance", sep = "_" )))
write.csv( abundance_all, file = file.path(output_folder,paste(scenario, 
                              simulation_number, "abundance.csv", sep = "_" )))

biomass_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                               pattern = "biomass_wide"))
biomass_all <- do.call(rbind,biomass_list)
rm(biomass_list)

saveRDS( biomass_all, file = file.path(output_folder,paste(scenario, 
                                       simulation_number, "biomass", sep = "_" )))
write.csv( biomass_all, file = file.path(output_folder,paste(scenario,
                                  simulation_number, "biomass.csv", sep = "_" )))

# Get and save adult massbin, abundance and biomass data

adult_functional_group_data <- lapply(adult_list, group_by_massbin, breaks = breaks)

adult_massbin_long_list <- flatten(lapply(adult_functional_group_data, filter_by_pattern, 
                                    pattern = "massbin_data_long"))
adult_massbin_long_all <- do.call(rbind,adult_massbin_long_list)
rm(adult_massbin_long_list)

saveRDS( adult_massbin_long_all, file = file.path(output_folder,paste(scenario, 
                                                                simulation_number, "adult_massbin_long", sep = "_" )))
write.csv( adult_massbin_long_all, file = file.path(output_folder,paste(scenario, 
                                                                  simulation_number, "adult_massbin_long.csv", sep = "_" )))

adult_abundance_list <- flatten(lapply(adult_functional_group_data, filter_by_pattern, 
                                 pattern = "abundance_wide"))
adult_abundance_all <- do.call(rbind,adult_abundance_list)
rm(adult_abundance_list)

saveRDS( adult_abundance_all, file = file.path(output_folder,paste(scenario, 
                                                             simulation_number, "adult_abundance", sep = "_" )))
write.csv( adult_abundance_all, file = file.path(output_folder,paste(scenario, 
                                                               simulation_number, "adult_abundance.csv", sep = "_" )))

adult_biomass_list <- flatten(lapply(adult_functional_group_data, filter_by_pattern, 
                               pattern = "biomass_wide"))
adult_biomass_all <- do.call(rbind,adult_biomass_list)
rm(adult_biomass_list)

saveRDS( adult_biomass_all, file = file.path(output_folder,paste(scenario, 
                                                           simulation_number, "adult_biomass", sep = "_" )))
write.csv( adult_biomass_all, file = file.path(output_folder,paste(scenario,
                                                             simulation_number, "adult_biomass.csv", sep = "_" )))

## Get and save generation length data


generation_length_list <- flatten(lapply(functional_group_data_all, 
                              filter_by_pattern, pattern = "generation_lengths"))
generation_length_all <- do.call(rbind,generation_length_list)
rm(generation_length_list)

saveRDS( generation_length_all, file = file.path(output_folder,paste(scenario, 
                                      simulation_number, "generation_lengths", 
                                      sep = "_" )))
write.csv( generation_length_all, file = file.path(output_folder,paste(scenario, 
                      simulation_number, "generation_lengths.csv", sep = "_" )))

print("Warning: this function is still being tested, treat outputs with caution")

print(paste("Processing of files from simulation number", 
simulation_number, "in the", scenario, "scenario directory complete", sep = " "))

}


indicators_project <- "N:/Quantitative-Ecology/Indicators-Project"
location <- 'Serengeti'
scenario <- 'Test_runs'
simulation <- 'aa_BuildModel/'

system.time(get_age_structure_data(indicators_project, location, scenario, simulation))
