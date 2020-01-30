
#' @param indicators_project file path where all project data is kept eg 'N:/Quantitative-Ecology/Indicators-Project/'
#' @param location string that denotes the directory of the location you want to process eg 'Serengeti'
#' @param scenario string that denotes which impact directory you want to look in eg "Baseline"
#' @param simulation name of the BuildModel directory you want to process eg  "001_BuildModel/"
#' @param remove_juveniles string, can be "yes" or "no" to determine
#' which age structures are saved (adults only, or all ages)
#' @param burnin integer denoting burnin time (in months not years)
#' @returns saves massbin, abundance, biomass and generation lengths to adaptor
#' output folder

#' TODO: Make this function remove burnin timesteps (for efficiency).  This will
#' require updates to other indicator code though.
#' 
#' TODO: At the moment the generation lengths data requires group_by_massbin to be
#' used on the all ages data, even if we have specified we want to remove juveniles.
#' This means the apply needs to be run twice and adds a lot of time.  Find a way
#' to see if can just use adult data, or separate gen lengths function from the
#' group_by_massbin function
#' 

## For testing:
# 
indicators_project <- "N:/Quantitative-Ecology/Indicators-Project"
location <- "Serengeti"
scenario <- "Test_runs"
simulation <- "aa_BuildModel"
burnin <- 1 * 12


get_age_structure_data <- function(indicators_project, location, scenario, simulation, burnin){
  
  require(tidyverse)
  
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
  
  if ( !dir.exists( file.path(output_folder) ) ) {
   
  dir.create( file.path(output_folder), recursive = TRUE )
    
   # function to filter lists by the element names
  
  filter_by_pattern <- function(pattern, your.list) {
    
    require(stringr)
    
    names(your.list) %>% 
    str_detect(pattern) %>%
    keep(your.list, .)
    
  }

  # Get the new_cohorts data from which we can calculate mean age of parents,
  # aka generation length
  
new_cohorts_name <- results_files[str_detect(results_files, "NewCohorts")]
new_cohorts <- read_tsv(file.path(model_results,new_cohorts_name, sep =""), 
                        col_types = list(Latitude = col_skip(), # Skip the columns you don't need
                                         Longitude = col_skip()))

# replace spaces in column names with underscores

oldnames <- names(new_cohorts)
newnames <- str_replace_all(oldnames," ", "_")
names(new_cohorts) <- newnames
new_cohorts <- new_cohorts %>% dplyr::rename(ID = parent_cohort_IDs)


## Calculate generation lengths

## Get timestep each cohort was born

born <- new_cohorts %>%
        dplyr::select(offspring_cohort_ID, 
                      functional_group, 
                      adult_mass, time_step) 

## Get timestep for each year each cohort reproduces

reproduced <- new_cohorts %>%
              dplyr::select(ID, time_step) 

## Merge to create dataframe with timestep born, timestep of reproduction (so
# there can be only one timestep born per cohort, but multiple timesteps of reproduction)
# Not all cohorts born also reproduce, which is why this creates a df with fewer
# observations

born_reproduced <- merge(born, reproduced, by.x = "offspring_cohort_ID", 
                        by.y = "ID")

born_reproduced <- born %>%
                   merge(reproduced, by.x = "offspring_cohort_ID", 
                         by.y = "ID") %>%
                   dplyr::mutate(age_reproduced_years = (time_step.y - time_step.x)/12)

## Calculate generation length for each cohort (mean age of reproduction in years)

generation_length_df <- born_reproduced %>% 
                        group_by(offspring_cohort_ID) %>% 
                        summarize(generation_length = mean(age_reproduced_years)) %>% # Where gen_length is mean age of reproduction for that cohort in years
                        merge(.,born_reproduced[ , c("offspring_cohort_ID",
                              "functional_group","adult_mass")],
                            by = "offspring_cohort_ID", all = TRUE) %>%
                        unique()

## Rename columns

names(generation_length_df) <- c("ID", "generation_length",
                                 "functionalgroup","adult_mass")

# Remove big objects

rm(born, reproduced, born_reproduced)


# Get growth file (this is massive, might be a problem when processing multiple
# replicates)

growth_name <- results_files[str_detect(results_files, "Growth")]
growth <- as.data.frame(read_tsv(file.path(model_results,growth_name, sep =""),
                        col_types = list(Latitude = col_skip(), # Skip the columns you don't need
                                         Longitude = col_skip(),
                                         growth_g = col_skip(),
                                         metabolism_g = col_skip(),
                                         predation_g = col_skip(),
                                         herbivory_g = col_skip()
                                         )))

# Get cohort abundance in birth timestep (because it is excluded from growth file)
#' TODO: IMPORTANT get a better value for current bodymass (from maturity or growth)

birth_abundance <- new_cohorts %>%
  dplyr::select(offspring_cohort_ID, time_step, 
                functional_group, abundance) %>%
  dplyr::rename(ID = offspring_cohort_ID) %>%
  dplyr::mutate(Current_body_mass_g = 1) %>%
  dplyr::select(ID, time_step, Current_body_mass_g,
                functional_group, abundance)

# Add birth timestep abundance to growth



abundance <- rbind(growth, birth_abundance) %>%
             arrange(time_step)

# Combine abundance values in growth with 'adult mass' in new_cohorts to identify
# timesteps when cohorts are adults, and their adult abundance in that timestep

all_ages_data <- growth %>%
  merge(new_cohorts[ , c("offspring_cohort_ID","ID","adult_mass")],
        by.x = "ID", by.y = "offspring_cohort_ID", all = TRUE) %>%
  merge(generation_length_df[c("ID", "generation_length")], all = TRUE) %>%
  dplyr::mutate(tmp1 = ifelse(Current_body_mass_g >= adult_mass, TRUE, FALSE)) %>%
  dplyr::mutate(biomass = Current_body_mass_g * abundance) %>%
  dplyr::mutate(new_functional_group = ifelse((functional_group == 14 | functional_group == 17), "14.17",
                                       ifelse((functional_group == 13 | functional_group == 16), "13.16",
                                       ifelse((functional_group == 15 | functional_group == 18), "15.18", # combine iteroparous and semelparous ectotherms
                                       functional_group)))) %>%
  dplyr::select(-functional_group) %>%
  dplyr::filter(!is.na(time_step)) %>% # Removes cohorts that were born but died straight away
  dplyr::rename(parent_cohort_ID = ID.y) %>% 
  group_by(ID) %>% # Identify adults, including those whose bodymass have risen above then dropped below the adult mass threshold
  arrange(ID, time_step) %>%
  mutate(first = as.numeric(row_number() == min(row_number()[tmp1 == TRUE]))) %>% # This produces an error, can be ignored
  mutate(tmp2 = cumsum(first)) %>%
  mutate(adult = ifelse(tmp2 == 1, TRUE, FALSE)) %>%
  ungroup() %>%
  dplyr::select(-c(tmp1, tmp2, first))  



print("cohort data processed")

rm(growth, new_cohorts)

# Turn the juvenile abundance and biomass into NA instead of removing them, so
# we keep all time-steps (otherwise it drops any timesteps without adult cohorts
# and you end up with different time series for different groups)
# This df should be the same dimension as all_ages


juvenile_data <- all_ages_data %>%
              dplyr::mutate(juvenile_abundance = ifelse(adult == FALSE, abundance, NA)) %>%
              dplyr::mutate(juvenile_biomass = ifelse(adult == FALSE, biomass, NA)) %>%
              dplyr::select(-c(abundance, biomass)) %>%
              dplyr::rename(abundance = juvenile_abundance, biomass = juvenile_biomass) 

# Function to sort cohorts into bodymass bins

# Get bodymass bins lower bounds

options(scipen = 999) # suppress scientific notation

breaks <- read.csv(file.path(model_inputs, '/Model setup/Ecological definition files/MassBinDefinitions.csv'))

# Group by bodymass bins and timestep. 

## TODO: Check the biomass matrices match the massbin outputs

# Split the data by functional groups

all_ages_list <- split(all_ages_data, all_ages_data$new_functional_group)
juvenile_list <- split(juvenile_data, juvenile_data$new_functional_group)

# Get true duration of the simulation

sim_parameters <- read.csv(file.path(model_inputs, '/Model setup/SimulationControlParameters.csv'))
sim_parameters$Parameters <- as.character(sim_parameters$Parameter)
sim_parameters$Value <- as.character(sim_parameters$Value)

duration_months <- as.numeric(sim_parameters %>% 
                                dplyr:: filter(Parameter == "Length of simulation (years)") %>%
                                dplyr:: select(Value)) * 12


# Function to group cohorts into massbins and sum their abundance and biomass per timestep

#' @param data long format dataframe containing abundance and biomass of each cohort
#' over time
#' @param breaks dataframe with one column where rows are the lower bounds of the
#' massbins
#' @param duration_months number of monthly timesteps the simulation ran for
#' @returns wide format dataframe where rows are functional groups and columns
#' are timesteps, and long format dataframe of generation length per functional group

# To test function

# data <- adult_list[[2]]
# 
# data <- all_ages_list[[1]]

# data <- juvenile_list[[4]]

group_by_massbin <- function(data, breaks, duration_months) {
  
massbin_breaks <- c(breaks[,1])

# Split into lower and upper bounds

bodymass_bins_upper <- massbin_breaks[-79]
bodymass_bins_lower <- massbin_breaks[-1]

# Combine to produce column called 'massbins' that matches the format of 
# model output massbin columns, so we can use it to merge other data later

bodymass_bins <- as.data.frame(cbind(bodymass_bins_lower, bodymass_bins_upper)) %>%
  mutate(temp = paste(bodymass_bins_lower, bodymass_bins_upper,  
                      sep = ",")) %>%
  mutate(massbins = paste("(", temp, "]", sep = "")) %>%
  mutate(bodymass_bin_index = c(77:0)) %>%
  dplyr::select(-temp)

# Get functional group name

fg <- max(data$new_functional_group, na.rm = TRUE)

# Group cohorts into the massbins and aggregate their abundance and biomass
#   

extant_massbin_data <- data %>%
                dplyr::select(time_step, Current_body_mass_g, new_functional_group,
                              abundance, biomass, adult) %>%
                dplyr::group_by(massbins = cut(Current_body_mass_g,
                                               breaks = bodymass_bins_lower,
                                               include.lowest = FALSE,
                                               right = TRUE,
                                               na.rm = FALSE,
                                               dig.lab = 11)) %>%
                dplyr::group_by(massbins, time_step) %>%
                dplyr::summarise(abundance_sum = sum(abundance),
                                 biomass_sum = sum(biomass)) %>%
                merge(bodymass_bins[ , c("massbins","bodymass_bin_index")],
                      by = "massbins", all = TRUE) %>%
                dplyr::mutate(new_functional_group = fg) %>%
                dplyr::mutate(functional_group_index = paste(new_functional_group,
                                                             bodymass_bin_index, sep = ".")) %>%
                dplyr::mutate(occupancy = ifelse(is.na(abundance_sum), "FALSE", "TRUE"))

# Testing - this section checks if any previously extant functional groups have
# become extinct, and adds zero abundance for remaining model timesteps (otherwise
# the output data just stops at whatever timestep they become extinct.
# This feels super clunky, think it could be done a lot better, maybe with the
# madingley simulation output for extinctions

correct_timesteps <- c(0:(duration_months - 1)) # Make sure it is zero  indexed

existing_timesteps <- sort(unique(na.omit(extant_massbin_data$time_step)))
missing_timesteps <- sort(correct_timesteps[!(correct_timesteps %in% existing_timesteps)])
occupied_massbins <- unique(extant_massbin_data$functional_group_index[extant_massbin_data$occupancy == TRUE])

# Check if there are any timesteps missing (ie if any functional groups have
# gone extinct)

if (is_empty(missing_timesteps)) { # do nothing but rename

  massbin_data <- extant_massbin_data 

} else {# create empty time step rows so they don't get deleted
  
  # Create a copy of extant_massbin_data but make all values zero
  
  empty_df <- extant_massbin_data %>%
    dplyr::filter(occupancy == TRUE) %>%
    dplyr::mutate(abundance_sum = 0, biomass_sum = 0, time_step = NA) %>%
    dplyr::distinct(.)
  
  # Split into individual list for each functional group that has been extant at some
  # point during the simulation
  
  single_empty_df_list <- split(empty_df, empty_df$functional_group_index)
  
  # Loop over and add timesteps
  
  empty_df_list <- list()
  
  for (i in seq_along(single_empty_df_list)) {
    
    tmp1 <- do.call("rbind", replicate(length(missing_timesteps), single_empty_df_list[[i]], simplify = FALSE))
    
    tmp2 <- tmp1 %>%
      dplyr::mutate(time_step = missing_timesteps)
    
    empty_df_list[[i]] <- tmp2
    
  }
  
  # Bind back into one dataframe that now includes a value of zero for any 
  # timesteps after a functional group becomes extinct
  
  extinct_massbin_data <- do.call("rbind", empty_df_list)
  
  massbin_data <- rbind(extant_massbin_data, extinct_massbin_data)
  
}

massbin_data <- massbin_data %>%
                dplyr::select(- occupancy) 

massbin_data$time_step <- replace_na(massbin_data$time_step, 0) 
                

rm(extant_massbin_data, extinct_massbin_data)

# Convert from long to wide format so it matches the netcdf model output format
  
abundance_wide <- massbin_data %>%
                  dplyr::select(-biomass_sum) %>% # remove biomass
                  spread(time_step, abundance_sum,
                         fill = NA, convert = FALSE) %>%
                  dplyr::arrange(bodymass_bin_index) %>%
                   `rownames<-`(.[,4])  %>%
                  dplyr::select(-c(new_functional_group,massbins,
                                 massbins, bodymass_bin_index,
                                 functional_group_index))

abundance_matrix_temp <- as.matrix(abundance_wide) # convert to matrix

abundance_matrix_final <- ifelse(abundance_matrix_temp >= 1,
                                 log(abundance_matrix_temp),0) # convert log values

rm(abundance_matrix_temp, abundance_wide)

biomass_wide <- massbin_data %>%
                dplyr::select(-abundance_sum) %>% # Remove abundance
                spread(time_step, biomass_sum,
                           fill = NA, convert = FALSE) %>%
                dplyr::arrange(bodymass_bin_index) %>%
                `rownames<-`(.[,4])  %>%
                dplyr::select(-c(new_functional_group,massbins,
                                     massbins, bodymass_bin_index,
                                     functional_group_index))
  
biomass_matrix_temp <- as.matrix(biomass_wide)
    
biomass_matrix_final <- ifelse(biomass_matrix_temp >= 1,
                               log(biomass_matrix_temp),0)

rm(biomass_matrix_temp, biomass_wide)

fg <- max(data$new_functional_group, na.rm = TRUE)

# Calculate the generation length per massbin functional group (mean of all cohorts
# within the massbin)

generation_lengths <- data %>%
  dplyr::select(ID, time_step, new_functional_group, 
                Current_body_mass_g, generation_length) %>%
  dplyr::group_by(massbins = cut(Current_body_mass_g, 
                                 breaks = massbin_breaks,  
                                 na.rm = FALSE,
                                 dig.lab = 11)) %>% #suppress scientific notation
  dplyr::group_by(massbins, new_functional_group) %>%
  dplyr::summarise(gen_length_mean = mean(generation_length, 
                                          na.rm = TRUE),
                   gen_length_max = max(generation_length, na.rm = TRUE),
                   gen_length_min = min(generation_length, na.rm = TRUE)) %>%
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

print(paste("functional group", fg, "processed", sep = " "))

return(output)

rm(output)

}

# Convert data from all functional groups into the wide format using the
# group_by_massbin function

functional_group_data_all <- lapply(all_ages_list, group_by_massbin, 
                             breaks = breaks, duration_months = duration_months)

# Save the long format data

#' TODO: When testing is complete, remove long format part because it uses
#' up a lot of memory
#' 
massbin_long_list <- flatten(lapply(functional_group_data_all, filter_by_pattern,
                                    pattern = "massbin_data_long"))

massbin_long_all <- do.call(rbind,massbin_long_list)

rm(massbin_long_list)

#' TODO: Fix naming so it doesn't include 'BuildModel'

saveRDS( massbin_long_all, file = file.path(output_folder,paste(scenario,
                                  simulation_number, "massbin_long_all", sep = "_" )))
write.csv( massbin_long_all, file = file.path(output_folder,paste(scenario,
                                    simulation_number, "massbin_long_all.csv", sep = "_" )))

rm(massbin_long_all)

print("saved massbin data all ages (1/7 files)")

# Save all ages wide abundance dataframe

abundance_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                                 pattern = "abundance_wide"))
abundance_all <- do.call(rbind,abundance_list)
rm(abundance_list)

saveRDS( abundance_all, file = file.path(output_folder,paste(scenario, 
                               simulation_number, "abundance", sep = "_" )))
write.csv( abundance_all, file = file.path(output_folder,paste(scenario, 
                              simulation_number, "abundance.csv", sep = "_" )))


print("saved abundance data all ages (2/7 files)")

# Save all ages wide biomass dataframe

biomass_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                               pattern = "biomass_wide"))
biomass_all <- do.call(rbind,biomass_list)
rm(biomass_list)

saveRDS( biomass_all, file = file.path(output_folder,paste(scenario, 
                                       simulation_number, "biomass", sep = "_" )))
write.csv( biomass_all, file = file.path(output_folder,paste(scenario,
                                  simulation_number, "biomass.csv", sep = "_" )))

print("saved biomass data all ages (3/7 files)")

rm(biomass_all)

# Tidy and plot data
#' TODO: Remove the 'remove burnin' parts of the code after everything else is fixed
#' and we only read in the rows we want from the .txt files


remove_burn_in <- function(df, burnin) {
  
  df[,(burnin + 1):ncol(df)]
}

abundance_all <- as.data.frame(abundance_all)
is.na(abundance_all) <- !abundance_all

plot_data <- remove_burn_in(abundance_all, burnin) %>%
  dplyr::mutate(functional_group_index = row.names(.)) %>%
  dplyr::mutate(total_abundance = rowMeans(dplyr::select(.,-functional_group_index), na.rm = TRUE)) %>%
  dplyr::mutate(functional_group = substr(functional_group_index, 
                                          start = 1, stop = 2)) %>%
  dplyr::mutate(bodymass_bin_index = str_sub(functional_group_index,-2, -1)) %>%
  #dplyr::mutate(bodymass_bin_index = str_remove(bodymass_bin_index, ".")) %>%
  dplyr::select(functional_group, bodymass_bin_index, functional_group_index, total_abundance) %>%
  dplyr::mutate(functional_group_name = ifelse(functional_group == 10, "herbivorous endotherms",
                                        ifelse(functional_group == 11, "carnivorous  endotherms",
                                        ifelse(functional_group == 12, "omnivorous  endotherms",
                                        ifelse(functional_group == 13, "herbivorous ectotherms", # combine iteroparous and semelparous ectotherms
                                        ifelse(functional_group == 14, "carnivorous ectotherms",
                                        ifelse(functional_group == 15, "omnivorous ectotherms", "NA")))))))

plotName <- paste(scenario, "_", simulation_number, "_functional_group_bodymass_distribution",".tiff",sep="")
tiff(file = (paste(output_folder,plotName, sep = "/")), units ="in", width=10, height=5, res=100)


print(ggplot(data = plot_data, aes( x = bodymass_bin_index, 
                                    y = total_abundance, col = functional_group_name)) +
        geom_bar(stat = 'identity') +
        labs(x = "bodymass bins", y = "total_modelled_abundance") +
        facet_wrap( ~ functional_group_name, nrow = 3))

dev.off()

rm(abundance_all)
rm(plot_data)

# Get and save adult massbin, abundance and biomass data

# Save the long version with only adults

juvenile_functional_group_data <- lapply(juvenile_list, group_by_massbin, breaks = breaks, duration_months = duration_months)

juvenile_massbin_long_list <- flatten(lapply(juvenile_functional_group_data, filter_by_pattern, 
                                    pattern = "massbin_data_long"))
juvenile_massbin_long_all <- do.call(rbind,juvenile_massbin_long_list)
rm(juvenile_massbin_long_list)

saveRDS( juvenile_massbin_long_all, file = file.path(output_folder,paste(scenario, 
                                                                simulation_number, "juvenile_massbin_long", sep = "_" )))
write.csv( juvenile_massbin_long_all, file = file.path(output_folder,paste(scenario, 
                                                                  simulation_number, "juvenile_massbin_long.csv", sep = "_" )))

rm(juvenile_massbin_long_all)

print("saved juvenile massbin data (4/7 files)")

# Save adult wide abundance dataframe

juvenile_abundance_list <- flatten(lapply(juvenile_functional_group_data, filter_by_pattern, 
                                 pattern = "abundance_wide"))
juvenile_abundance_all <- do.call(rbind,juvenile_abundance_list)

rm(juvenile_abundance_list)

saveRDS( juvenile_abundance_all, file = file.path(output_folder,paste(scenario,
                                                             simulation_number, "juvenile_abundance", sep = "_" )))
write.csv( juvenile_abundance_all, file = file.path(output_folder,paste(scenario,
                                                               simulation_number, "juvenile_abundance.csv", sep = "_" )))

print("saved juvenile abundance data (5/7 files)")

# Save all juvenile wide biomass dataframe

juvenile_biomass_list <- flatten(lapply(juvenile_functional_group_data, filter_by_pattern,
                               pattern = "biomass_wide"))
juvenile_biomass_all <- do.call(rbind,juvenile_biomass_list)
rm(juvenile_biomass_list)

saveRDS( juvenile_biomass_all, file = file.path(output_folder,paste(scenario,
                                                           simulation_number, "juvenile_biomass", sep = "_" )))
write.csv( juvenile_biomass_all, file = file.path(output_folder,paste(scenario,
                                                            simulation_number, "juvenile_biomass.csv", sep = "_" )))

rm(juvenile_biomass_all)

print("saved juvenile biomass data (6/7 files)")

remove_burn_in <- function(df, burnin) {
  
  df[,(burnin + 1):ncol(df)]
}

juvenile_abundance_all <- as.data.frame(juvenile_abundance_all)
is.na(juvenile_abundance_all) <- !juvenile_abundance_all

plot_data <- remove_burn_in(juvenile_abundance_all, burnin) %>%
  dplyr::mutate(functional_group_index = row.names(.)) %>%
  dplyr::mutate(total_abundance = rowMeans(select(.,-functional_group_index), na.rm = TRUE)) %>%
  dplyr::mutate(functional_group = substr(functional_group_index, 
                                          start = 1, stop = 2)) %>%
  dplyr::mutate(bodymass_bin_index = str_sub(functional_group_index,-2, -1)) %>%
  dplyr::select(functional_group, bodymass_bin_index, functional_group_index, total_abundance) %>%
  dplyr::mutate(functional_group_name = ifelse(functional_group == 10, "herbivorous endotherms",
                                               ifelse(functional_group == 11, "carnivorous  endotherms",
                                               ifelse(functional_group == 12, "omnivorous  endotherms",
                                               ifelse(functional_group == 13, "herbivorous ectotherms", # combine iteroparous and semelparous ectotherms
                                               ifelse(functional_group == 14, "carnivorous ectotherms",
                                               ifelse(functional_group == 15, "omnivorous ectotherms", "NA")))))))

plotName <- paste(scenario, "_", simulation_number, "_functional_group_bodymass_distribution2",".tiff",sep="")
tiff(file = (paste(output_folder,plotName, sep = "/")), units ="in", width=10, height=5, res=100)


print(ggplot(data = plot_data, aes( x = bodymass_bin_index, 
                                    y = total_abundance, col = functional_group_name)) +
        geom_bar(stat = 'identity') +
        labs(x = "bodymass bins", y = "total_modelled_abundance") +
        facet_wrap( ~ functional_group_name, nrow = 3))

dev.off()

rm(plot_data)



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

rm(functional_group_data_all)

print("saved generation length data (7/7 files)")

#' TODO: Remove this warning when I've fixed and tested everything :/

print("Warning: this function is still being tested, treat outputs with caution")

print(paste("Processing of files from simulation number", 
simulation_number, "in the", scenario, "scenario directory complete", sep = " "))

  } else {
  
    print(paste("Folder", simulation, "has already been processed"), sep = " ")
    
  }

}


# Test function

get_age_structure_data(indicators_project, location, scenario, simulation, burnin)

