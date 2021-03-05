
## NEW VERSION

## Objective is to return the generation lengths of the 'virtual species' ie
## Functional group massbins

#' @param indicators_project file path where all project data is kept eg 'N:/Quantitative-Ecology/Indicators-Project/'
#' @param location string that denotes the directory of the location you want to process eg 'Serengeti'
#' @param scenario string that denotes which impact directory you want to look in eg "Baseline"
#' @param simulation name of the BuildModel directory you want to process eg  "001_BuildModel/"
#' @param remove_juveniles string, can be "yes" or "no" to determine
#' which age structures are saved (adults only, or all ages)
#' @param burnin integer denoting burnin time (in months not years)
#' @returns saves massbin, abundance, biomass and generation lengths to adaptor
#' output folder

#' TODO: Check the .rds files, don't think they're saving properly

#' TODO: Restructure so this takes multiple other functions like process_buildmodel
#' TODO: Fix plot functional groups? Or does it just look weird because there are few
#' timesteps?
#' TODO: Make this function remove burnin timesteps (for efficiency).  This will
#' require updates to other indicator code though.


## For testing:
# 

# inputs <- "N:/Quantitative-Ecology/Indicators-Project/Serengeti/Inputs_to_adaptor_code/Madingley_simulation_outputs/Test_runs/aa_BuildModel"
# outputs <- "N:/Quantitative-Ecology/Indicators-Project/Serengeti/Outputs_from_adaptor_code/map_of_life/Test_runs/aa_BuildModel"
# simulation_folder_name <- "aa_BuildModel"
# burnin <- 1 * 12
# scenario <- "Test_runs"
# 
# get_age_structure_data(inputs, outputs, scenario, simulation_folder_name, burnin)

inputs <- "N:/Quantitative-Ecology/Indicators-Project/Serengeti/Inputs_to_adaptor_code/Madingley_simulation_outputs/999_Test_runs/993_BuildModel"
outputs <- "N:/Quantitative-Ecology/Indicators-Project/Serengeti/Outputs_from_adaptor_code/map_of_life/999_Test_runs/993_BuildModel"
simulation_folder_name <- "993_BuildModel"
burnin <- 1 * 12
scenario <- "999_Test_runs"
# 
# get_age_structure_data(inputs, outputs, scenario, simulation_folder_name, burnin)

# inputs = simulation_paths
# simulation = simulation_folder_names

# FUNCTION 1 - Get the detailed cohort data ----


#' @param inputs string, path to directory where the raw simulation outputs
#' are saved
#' @param outputs string, path to directory where the processed simulation outputs
#' will be stored, including the outputs from this function
#' @returns a list of dataframes, one for each replicate of the simulation. Each
#' dataframe contains the following variables:Cohort_ID, timestep, current bodymass,
#' cohort abundance, ID of the cohort's parent cohort, the mass at which the cohort becomes
#' adult, cohort generation length, cohort biomass, the functional group to 
#' which the cohort belongs, and a logical value determining if the cohort are adults in that timestep 


get_generation_lengths <- function(inputs, outputs) {
  
  # Libraries and functions
  
  require(tidyverse)
  require(janitor)
  require(viridis)
  require(ggplot2)
  

  # Check how many replicates have already been processed
  
  files <- dir(outputs)
  
  files <- files[grep("generation",files)] 
  length(files)
  
  # Check and create output directory
  
  if ( !dir.exists( file.path(outputs) ) ) {
    
    dir.create( file.path(outputs), recursive = TRUE )
    
  } 
  
  # Get number of replicates etc in the buildmodel folder
    
    dirs <- list.dirs(inputs, recursive = FALSE)
    model_results <- dirs[!str_detect(dirs, "input")]
    results_files <- list.files(model_results)
    model_inputs <- dirs[str_detect(dirs, "input")]
    simulation_folder_name <- basename(inputs)
    simulation_number <- str_remove(simulation_folder_name, "_BuildModel")
    scenario_label <- tolower(substring(scenario, 5))
    scenario_label
    
    reps <- (length(results_files) - 9) / 14
    rep_numbers <- as.character(seq(0,(reps - 1)))
    rep_index <- paste("_", rep_numbers, "_", sep = "")
    
    # Check if all replicates already processed
    
    if ((dir.exists(file.path(outputs))) & (length(files) == length(rep_index))) {
      
      print(paste("BuildModel folder", sim_label, "has already been processed",
                  sep = " "))
    }
    
    # If not, continue processing
    
    if ((dir.exists(file.path(outputs))) & (length(files) != length(rep_index))) {
    
    # Split up the model output files by replicate number
    
    rep_files <- list()
    
    for (rep in seq_along(rep_index)) {
      
      rep_files[[rep]] <- results_files[grep(rep_index[rep],results_files)]
      
    }
    
    for (i in seq_along(rep_files)) {
      
      rep_label <- rep_index[i]
      rep_label <- str_sub(rep_label, end=-2)
      rep_label
      
      # Get the new_cohorts data from which we can calculate mean age of parents,
      # aka generation length
      
      new_cohorts_name <- rep_files[[i]][str_detect(rep_files[[i]], "NewCohorts")]
      
      new_cohorts <- read_tsv(file.path(model_results,new_cohorts_name, sep =""), 
                              col_names = TRUE,
                              col_types = list(Latitude = col_skip(), # Skip the columns you don't need
                                               Longitude = col_skip()))
      head(new_cohorts)
      
      # replace spaces in column names with underscores
      
      new_cohorts <- clean_names(new_cohorts)
      names(new_cohorts)

      new_cohorts <- new_cohorts %>% dplyr::rename(ID = parent_cohort_i_ds)
      
      ## Calculate generation lengths
      
      ## Get timestep each cohort was born
      
      born <- new_cohorts %>%
        dplyr::select(ID,offspring_cohort_id, 
                      functional_group, 
                      adult_mass, time_step) 
      head(born)
      
      ## Get timestep for each year each cohort reproduces
      
      reproduced <- new_cohorts %>%
        dplyr::select(ID, time_step) 
      
      head(reproduced)
      
      ## Merge to create dataframe with timestep born, timestep of reproduction (so
      # there can be only one timestep born per cohort, but multiple timesteps of reproduction)
      # Not all cohorts born also reproduce, which is why this creates a df with fewer
      # observations
      
      born_reproduced <- merge(born, reproduced, by.x = "offspring_cohort_id", 
                               by.y = "ID")
      
      born_reproduced <- born %>%
        merge(reproduced, by.x = "offspring_cohort_id", 
              by.y = "ID") %>%
        dplyr::mutate(age_reproduced_years = (time_step.y - time_step.x)/12)
      
      head(born_reproduced)
      dim(born_reproduced)
      
      # Assign cohorts to their virtual species
      
      groups <- readRDS(file.path(outputs, "groups.rds"))
      
      head(groups)
      
      bodymass_bins_upper <- unique(groups$mass_upper)
      
      # Add massbins
      
      born_reproduced_groups1 <- born_reproduced %>%
              dplyr::group_by(massbins = cut(adult_mass,
                                            breaks = bodymass_bins_upper,
                                            include.lowest = FALSE,
                                            right = TRUE,
                                            na.rm = FALSE,
                                            dig.lab = 11))
      
      # Re-format the massbin column(s) so they match the groups DF, so then
      # we can match up the two and add virtual species group_id to the 
      # new_cohorts DF.  This is a bit clunky, probably a more elegant way to
      # do it.
      
      # Split into two cols
      
      born_reproduced_groups2 <- do.call(rbind, 
                                         str_split(born_reproduced_groups1$massbins, ','))
      
      # Covnert to dataframe and rename columsn to match groups df
      
      born_reproduced_groups2 <- as.data.frame(born_reproduced_groups2) 
      
      colnames(born_reproduced_groups2) <- c("mass_lower", "mass_upper")
      head(born_reproduced_groups2)
      # Remove parentheses and convert to numeric
      
      born_reproduced_groups2 <- born_reproduced_groups2 %>%
               mutate(mass_lower = readr::parse_number(mass_lower),
                      mass_upper = as.numeric(str_extract(mass_upper, "-?[0-9.]+")))
      
      # Bind massbin columns to born_reproduced cohorts data
      
      born_reproduced_groups3 <- cbind(born_reproduced_groups1, born_reproduced_groups2)
      head(born_reproduced_groups3)
      
      # Add proper functional group index to match groups and merge semelparous/iteroparous
      
      born_reproduced_groups <- born_reproduced_groups3 %>%
              mutate(functional_group_index = ifelse(functional_group == 13, 13.16,
                                              ifelse(functional_group == 16, 13.16,
                                              ifelse(functional_group == 14, 14.17,
                                              ifelse(functional_group == 17, 14.17,
                                              ifelse(functional_group == 15, 15.18,
                                              ifelse(functional_group == 18, 15.18,
                                                    functional_group))))))) %>%
             mutate(functional_group_index = as.character(functional_group_index))
      
      born_reproduced_groups <- as.data.frame(born_reproduced_groups)
      head(born_reproduced_groups)
      
      # Should have same number of observations as born_reproduced
      nrow(born_reproduced_groups) == nrow(born_reproduced)
      
      # Split by functional group before matching by massbin
      
      head(born_reproduced_groups)
      unique(born_reproduced_groups$functional_group_index)
      unique(groups$functional_group_index)
      
      born_reproduced_groups_fg <- split(born_reproduced_groups, 
                                         born_reproduced_groups$functional_group_index)
      
      groups_fg <- split(groups, groups$functional_group_index)
      
      # Check we have the same no functional groups
      
      l_test <- length(born_reproduced_groups_fg) == length(groups_fg)
      
      if(l_test == FALSE) {
        
        stop("number of cohort functional groups do not match true number
             of functional groups")
      }
      
      n_test <- names(born_reproduced_groups_fg) == names(groups_fg)
      
      if(any(n_test == FALSE)) {
        
        stop("names of cohort functional groups do not match true names
             of functional groups")
      }
      
      # Merge with groups DF based on massbin columns in each
      
      br_groups_out <- list()
      
      for(i in seq_along(born_reproduced_groups_fg)) {
      
        br_groups_out[[i]] <- born_reproduced_groups_fg[[i]] %>%
               merge(groups_fg[[i]][c("mass_lower", "mass_upper","bodymass_index",
                                      "functional_group_name")],
                     by = c("mass_lower", "mass_upper")) %>%
               mutate(group_id = paste(functional_group, bodymass_index, sep = "."))
        
      }
      
      born_reproduced_groups <- do.call(rbind, br_groups_out)
      
      head(born_reproduced_groups)
      nrow(born_reproduced_groups) == nrow(born_reproduced)
      
      ## Calculate generation length for each group (mean age of reproduction in years)
      
      generation_length_df <- born_reproduced_groups %>% 
        group_by(group_id) %>% 
        summarize(generation_length = mean(age_reproduced_years)) %>%
        ungroup(.) %>% # Where gen_length is mean age of reproduction for that group (virtual species) in years
        merge(.,born_reproduced_groups[ , c("ID","group_id",
                                     "functional_group_index","functional_group_name",
                                     "adult_mass",
                                     "mass_lower", "mass_upper")],
              by = "group_id", all = TRUE) %>%
        unique() %>%
        rename(generation_length_yrs = generation_length, # add units
               adult_mass_g = adult_mass,
               mass_lower_g = mass_lower,
               mass_upper_g = mass_upper) %>%
        mutate(functional_group_index = as.factor(functional_group_index)) %>%
        mutate(massbin_g = paste(mass_lower_g, mass_upper_g, sep = " - "))
      
      head(generation_length_df)
      
      write.csv(generation_length_df, file.path(outputs,
                                                paste("GenerationLengths_",
                                                      simulation_number, 
                                                rep_label, ".csv", sep = "")))
      saveRDS(generation_length_df, file.path(outputs,
                                                paste("GenerationLengths_",
                                                      simulation_number, 
                                                      rep_label, ".rds", sep = "")))
      
      print(paste("Generation lengths for folder", simulation_number,
                  ", replicate number", rep_label, "complete"))
      

     gen_length_boxplot <- ggplot(generation_length_df) +
        geom_boxplot(aes(x = functional_group_name, y = generation_length_yrs,
                         group = functional_group_name, fill = functional_group_name)) +
        scale_fill_viridis(discrete=TRUE) +
        scale_color_viridis(discrete=TRUE) +
        ggtitle(paste("Simulation", simulation_number,
                      ", Replicate", rep_label, "Generation Lengths",
                      sep = " ")) +
       theme(axis.text.x=element_blank(),
             axis.ticks.x=element_blank()) +
       xlab("Functional group") + ylab("Generation lengths (years)")
     
     ggsave(file.path(outputs,
                      paste("BoxplotGenerationLengths_",
                            simulation_number, 
                            rep_label, ".png", sep = "")),
            gen_length_boxplot, device = "png")
     
    }
  }
}
   
get_generation_lengths(inputs, outputs)   
      
## Rename columns
      
      names(generation_length_df) <- c("ID", "generation_length",
                                       "functionalgroup","adult_mass")
      
      out[[i]] <- generation_length_df
      
      # Remove big objects
      
      rm(born, reproduced, born_reproduced)
      
    } 
      # Get growth file (this is massive, might be a problem when processing multiple
      # replicates)
      
      growth_name <- rep_files[[i]][str_detect(rep_files[[i]], "Growth")]
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
      
      # birth_abundance <- new_cohorts %>%
      #   dplyr::select(offspring_cohort_ID, time_step, 
      #                 functional_group, abundance) %>%
      #   dplyr::rename(ID = offspring_cohort_ID) %>%
      #   dplyr::mutate(Current_body_mass_g = 1) %>%
      #   dplyr::select(ID, time_step, Current_body_mass_g,
      #                 functional_group, abundance)
      
      # Add birth timestep abundance to growth
      
      # 
      # abundance <- rbind(growth, birth_abundance) %>%
      #             arrange(time_step)
      
      # Combine abundance values in growth with 'adult mass' in new_cohorts to identify
      # timesteps when cohorts are adults, and their adult abundance in that timestep
      
      cohort_data[[i]] <- growth %>%
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
      
      # Get abundance at maturity, if we define maturity as reaching adult mass size
      #' TODO: This gives abundance at birth and death not abundance at birth and
      #' maturity, fix
      
      # maturity <- cohort_data %>% 
      #   group_by(ID) %>%
      #   slice(top_n(n=1, wt=desc(time_step)))
      # slice(which.min(time_step),
      #       which.max(time_step)) %>%
      #   select(ID, time_step, abundance, adult, new_functional_group, 
      #          Current_body_mass_g)
      # 
      # 
      # saveRDS( maturity, file = file.path(outputs,paste("AgeStructure", scenario,
      #                                                         simulation_number,rep_numbers[i], "maturity",
      #                                                         sep = "_" )))
      # write.csv( maturity, file = file.path(outputs,paste("AgeStructure",scenario,
      #                                                           simulation_number, rep_numbers[i], "maturity.csv", sep = "_" )))
      # 
      # rm(maturity)
      
    }
    return(cohort_data)
  }
}


cohort_data <- get_detailed_cohort_data(inputs, outputs)

# FUNCTION 2 - Convert the cohort data into massbin data ----

# This function should also work at the simulation level (i.e. input are all
# the replicates in one simulation folder, and outputs are all the replicates in
# one simulation folder)

## TODO: Important - output is currently not in correct massbin order.  Might 
## need to go back to previous approach of splitting the data by functional
## groups then rebinding it after summarising by timestep and massbin
## TODO: Check the biomass matrices match the massbin outputs
## TODO: Add an age switch (pick whether to output adults or juveniles, juveniles
## are the default)

convert_cohorts_to_massbins <- function(inputs, cohort_data){
  
  dirs <- list.dirs(inputs, recursive = FALSE)
  model_results <- dirs[!str_detect(dirs, "input")]
  results_files <- list.files(model_results)
  model_inputs <- dirs[str_detect(dirs, "input")]
  simulation_folder_name <- basename(inputs)
  simulation_number <- str_remove(simulation_folder_name, "_BuildModel")
  
  out <- list()
  
  for (i in seq_along(cohort_data)) {
    
    
    # juvenile_data <- cohort_data[[i]] %>%
    # dplyr::mutate(juvenile_abundance = ifelse(adult == FALSE, abundance, NA)) %>%
    # dplyr::mutate(juvenile_biomass = ifelse(adult == FALSE, biomass, NA)) %>%
    # dplyr::select(-c(abundance, biomass)) %>%
    # dplyr::rename(abundance = juvenile_abundance, biomass = juvenile_biomass) 
    
    # Get bodymass bins
    
    options(scipen = 999) # suppress scientific notation
    
    # Get true duration of the simulation
    
    sim_parameters <- read.csv(file.path(model_inputs, '/Model setup/SimulationControlParameters.csv'))
    sim_parameters$Parameters <- as.character(sim_parameters$Parameter)
    sim_parameters$Value <- as.character(sim_parameters$Value)
    
    duration_months <- as.numeric(sim_parameters %>% 
                                    dplyr:: filter(Parameter == "Length of simulation (years)") %>%
                                    dplyr:: select(Value)) * 12
    
    
    # Split into lower and upper bounds
    
    breaks <- read.csv(file.path(model_inputs, '/Model setup/Ecological definition files/MassBinDefinitions.csv'))
    
    bodymass_bins_lower <- c(breaks[,1])
    bodymass_bins_upper <- c(1000000000, 
                             bodymass_bins_lower[bodymass_bins_lower != 
                                                   min(bodymass_bins_lower)]) # Add a huge upper limit
    
    # Combine to produce column called 'massbins' that matches the format of 
    # model output massbin columns, so we can use it to merge other data later
    
    bodymass_bins <- as.data.frame(cbind(bodymass_bins_lower, 
                                         bodymass_bins_upper)) %>%
      mutate(temp = paste(bodymass_bins_lower, 
                          bodymass_bins_upper,  
                          sep = ",")) %>%
      mutate(massbins = paste("(", temp, "]", sep = "")) %>%
      mutate(bodymass_bin_index = 
               c((length(bodymass_bins_lower) - 1):0)) %>%
      dplyr::select(-temp)
    
    # Group cohorts into the massbins and aggregate their abundance and biomass
    
    #' TODO: Check the juvenile values look right
    #' TODO: Massbins without any biomass are returning functional group
    #' values prefixed by NA (instead of 10, 11, 12 etc).  Fix this.
    
    extant_massbin_data <- cohort_data[[i]] %>%
      dplyr::select(time_step, Current_body_mass_g, new_functional_group,
                    abundance, biomass, adult) %>% # get important variables
      dplyr::group_by(massbins = cut(Current_body_mass_g,
                                     breaks = bodymass_bins_upper,
                                     include.lowest = FALSE,
                                     right = TRUE,
                                     na.rm = FALSE,
                                     dig.lab = 11)) %>% # reduce the resolution by grouping cohorts into bodymass bins
      dplyr::group_by(massbins, time_step, new_functional_group) %>%
      dplyr::summarise(abundance_sum = sum(abundance), # sum the abundance of all cohorts in that massbin
                       biomass_sum = sum(biomass), # ditto but with biomass
                       juvenile_abundance = sum(abundance[adult== FALSE]), # ditto but only for cohorts that haven't reached adulthood in that timestep
                       juvenile_biomass = sum(biomass[adult== FALSE])) %>% # ditto but with biomass
      # NB: If abundance and juvenile abundance are the same, it means all cohorts in that massbin at that timestep are juvenile (common in early timesteps)
      merge(bodymass_bins[ , c("massbins","bodymass_bin_index")],
            by = "massbins", all = TRUE) %>% # Add the bodymass bin index
      # dplyr::mutate(new_functional_group = fg) %>% # Add the functional group
      dplyr::mutate(functional_group_index = paste(new_functional_group,
                                                   bodymass_bin_index, sep = ".")) %>% # combine the two to create an index of functional-group-bodymass-bin
      dplyr::mutate(occupancy = ifelse(is.na(abundance_sum), "FALSE", "TRUE")) # Add a tag to identify empty vs occupied massbins (ie not all massbins will contain organisms at all timesteps)
    
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
      
    } else { # create empty time step rows so they don't get deleted
      
      # Create a copy of extant_massbin_data but make all values zero
      
      empty_df <- extant_massbin_data %>%
        dplyr::filter(occupancy == TRUE) %>%
        dplyr::mutate(abundance_sum = 0, biomass_sum = 0, 
                      time_step = NA) %>%
        dplyr::distinct(.)
      
      # Split into individual list for each functional group that has been extant at some
      # point during the simulation
      
      single_empty_df_list <- split(empty_df, empty_df$functional_group_index)
      
      # Loop over and add timesteps
      
      empty_df_list <- list()
      
      for (i in seq_along(single_empty_df_list)) {
        
        tmp1 <- do.call("rbind", replicate(length(missing_timesteps), 
                                           single_empty_df_list[[i]], 
                                           simplify = FALSE))
        
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
    
    # Convert the long format massbin_data into wide format, so it matches
    # the netcdf files with all ages
    
    abundance_wide <- massbin_data %>%
      dplyr::select(-biomass_sum, -juvenile_biomass, 
                    -juvenile_abundance) %>% # remove biomass
      spread(time_step, abundance_sum,
             fill = NA, convert = FALSE) %>%
      dplyr::arrange(bodymass_bin_index) %>%
      `rownames<-`(.[,4])  %>%
      dplyr::select(-c(new_functional_group,massbins,
                       massbins, bodymass_bin_index,
                       functional_group_index))
    
    abundance_matrix_temp <- as.matrix(abundance_wide) # convert to matrix
    
    out[[i]] <- ifelse(abundance_matrix_temp >= 1,
                       log(abundance_matrix_temp),0) # convert log values
    
    rm(extant_massbin_data, extinct_massbin_data, 
       abundance_matrix_temp, abundance_wide)
    
  }
  
  return(out)
  
}

test <- convert_cohorts_to_massbins(inputs, cohort_data)




# Convert from long to wide format so it matches the netcdf model output format



rm(abundance_matrix_temp, abundance_wide)

functional_group_biomass[[j]] <- massbin_data %>%
  dplyr::select(-abundance_sum,-juvenile_biomass, -juvenile_abundance) %>% # Remove abundance
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


# juvenile_biomass_wide <- massbin_data %>%
#   dplyr::select(-abundance_sum,-biomass_sum, -juvenile_abundance) %>% # Remove abundance
#   spread(time_step, juvenile_biomass,
#          fill = NA, convert = FALSE) %>%
#   dplyr::arrange(bodymass_bin_index) %>%
#   `rownames<-`(.[,4])  %>%
#   dplyr::select(-c(new_functional_group,massbins,
#                    massbins, bodymass_bin_index,
#                    functional_group_index))
# 
# juvenile_biomass_matrix_temp <- as.matrix(juvenile_biomass_wide)
# 
# juvenile_biomass_matrix_final <- ifelse(juvenile_biomass_matrix_temp >= 1,
#                                         log(juvenile_biomass_matrix_temp),0)
# 
# rm(juvenile_biomass_matrix_temp, juvenile_biomass_wide)
# 
# juvenile_abundance_wide <- massbin_data %>%
#   dplyr::select(-abundance_sum,-biomass_sum, -juvenile_biomass) %>% # Remove abundance
#   spread(time_step, juvenile_abundance,
#          fill = NA, convert = FALSE) %>%
#   dplyr::arrange(bodymass_bin_index) %>%
#   `rownames<-`(.[,4])  %>%
#   dplyr::select(-c(new_functional_group,massbins,
#                    massbins, bodymass_bin_index,
#                    functional_group_index))
# 
# juvenile_abundance_matrix_temp <- as.matrix(juvenile_abundance_wide)
# 
# juvenile_abundance_matrix_final <- ifelse(juvenile_abundance_matrix_temp >= 1,
#                                           log(juvenile_abundance_matrix_temp),0)
# 
# rm(juvenile_abundance_matrix_temp, juvenile_abundance_wide)
# 
# fg <- max(data$new_functional_group, na.rm = TRUE)
# 
# Calculate the generation length per massbin functional group (mean of all cohorts
# within the massbin)

#   if (generation_lengths == "yes") {
#     
#     generation_lengths <- data %>%
#       dplyr::select(ID, time_step, new_functional_group, 
#                     Current_body_mass_g, generation_length) %>%
#       dplyr::group_by(massbins = cut(Current_body_mass_g, 
#                                      breaks = massbin_breaks,  
#                                      na.rm = FALSE,
#                                      dig.lab = 11)) %>% #suppress scientific notation
#       dplyr::group_by(massbins, new_functional_group) %>%
#       dplyr::summarise(gen_length_mean = mean(generation_length, 
#                                               na.rm = TRUE),
#                        gen_length_max = max(generation_length, na.rm = TRUE),
#                        gen_length_min = min(generation_length, na.rm = TRUE)) %>%
#       merge(bodymass_bins[ , c("massbins","bodymass_bin_index")],
#             by = "massbins", all = TRUE) %>%
#       dplyr::mutate(new_functional_group = fg) %>%
#       dplyr::mutate(functional_group_index = paste(new_functional_group, 
#                                                    bodymass_bin_index, sep = ".")) %>%
#       dplyr::select(-c(new_functional_group,massbins,
#                        massbins)) %>%
#       dplyr::arrange(bodymass_bin_index)
#     
#     output <- list(massbin_data, abundance_matrix_final, biomass_matrix_final, 
#                    juvenile_biomass_matrix_final, juvenile_abundance_matrix_final,
#                    generation_lengths)
#     
#     names(output) <- c("massbin_data_long", "abundance_wide", "biomass_wide", 
#                        "juvenile_biomass_wide", "juvenile_abundance_wide", 
#                        "generation_lengths")
#     
#     print(paste("functional group", fg, "processed", sep = " "))
#     
#     return(output)
#     
#     rm(output)
#     
#   } else {
#     
#     output <- list(massbin_data, abundance_matrix_final, biomass_matrix_final, 
#                    juvenile_biomass_matrix_final, juvenile_abundance_matrix_final)
#     
#     names(output) <- c("massbin_data_long", "abundance_wide", "biomass_wide", 
#                        "juvenile_biomass_wide", "juvenile_abundance_wide")
#     
#     print(paste("functional group", fg, "processed", sep = " "))
#     
#     return(output)
#     
#     rm(output)
#     
#   }
# }

# Convert data from all functional groups into the wide format using the
# group_by_massbin function

functional_group_data_all <- lapply(all_ages_list, group_by_massbin, 
                                    breaks, duration_months, "yes")

# Save all ages wide abundance dataframe

abundance_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                                 pattern = "abundance_wide"))

abundance_all <- do.call(rbind,abundance_list)
rm(abundance_list)

saveRDS( abundance_all, file = file.path(outputs,paste("AgeStructure",scenario, 
                                                       simulation_number,rep_numbers[i], "abundance", sep = "_" )))
write.csv( abundance_all, file = file.path(outputs,paste("AgeStructure",scenario, 
                                                         simulation_number,rep_numbers[i], "abundance.csv", sep = "_" )))


message("saved all ages abundance data (1/6 files)")

# Save all ages wide biomass dataframe

biomass_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                               pattern = "biomass_wide"))

biomass_all <- do.call(rbind,biomass_list)

rm(biomass_list)

saveRDS( biomass_all, file = file.path(outputs,paste("AgeStructure", scenario, 
                                                     simulation_number,rep_numbers[i], "biomass", sep = "_" )))
write.csv( biomass_all, file = file.path(outputs,paste("AgeStructure", scenario,
                                                       simulation_number,rep_numbers[i], "biomass.csv", sep = "_" )))

message("saved all ages biomass data (2/6 files)")

rm(biomass_all)

# Save juvenile wide biomass dataframe

juvenile_biomass_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                                        pattern = "juvenile_biomass_wide"))

juvenile_biomass_all <- do.call(rbind,juvenile_biomass_list)

rm(juvenile_biomass_list)

saveRDS( juvenile_biomass_all, file = file.path(outputs,paste("AgeStructure", scenario, 
                                                              simulation_number, rep_numbers[i], "juvenile_biomass", sep = "_" )))
write.csv( juvenile_biomass_all, file = file.path(outputs,paste("AgeStructure", scenario,
                                                                simulation_number, rep_numbers[i],"juvenile_biomass.csv", sep = "_" )))

message("saved juvenile biomass data (3/6 files)")

rm(juvenile_biomass_all)

# Save juvenile wide abundance dataframe

juvenile_abundance_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                                          pattern = "juvenile_abundance_wide"))

juvenile_abundance_all <- do.call(rbind,juvenile_abundance_list)

rm(juvenile_abundance_list)

saveRDS( juvenile_abundance_all, file = file.path(outputs,paste("AgeStructure", scenario, 
                                                                simulation_number, rep_numbers[i],"juvenile_abundance", sep = "_" )))
write.csv( juvenile_abundance_all, file = file.path(outputs,paste("AgeStructure", scenario,
                                                                  simulation_number, rep_numbers[i],"juvenile_abundance.csv", sep = "_" )))

message("saved juvenile abundance data (3/6 files)")

rm(juvenile_abundance_all)

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

plotName <- paste(scenario, "_", simulation_number, rep_numbers[i], "_functional_group_bodymass_distribution",".tiff",sep="")
tiff(file = (paste(outputs,plotName, sep = "/")), units ="in", width=10, height=5, res=100)


print(ggplot(data = plot_data, aes( x = bodymass_bin_index, 
                                    y = total_abundance, col = functional_group_name)) +
        geom_bar(stat = 'identity') +
        labs(x = "bodymass bins", y = "total_modelled_abundance") +
        facet_wrap( ~ functional_group_name, nrow = 3))

dev.off()

rm(abundance_all)
rm(plot_data)

message("saved plot of functional group biomass (3/6 files)")

## Get and save generation length data


generation_length_list <- flatten(lapply(functional_group_data_all,
                                         filter_by_pattern, pattern = "generation_lengths"))

generation_length_all <- do.call(rbind,generation_length_list)
rm(generation_length_list)

saveRDS( generation_length_all, file = file.path(outputs,paste("AgeStructure", scenario,
                                                               simulation_number,rep_numbers[i], "generation_lengths",
                                                               sep = "_" )))
write.csv( generation_length_all, file = file.path(outputs,paste("AgeStructure",scenario,
                                                                 simulation_number, rep_numbers[i], "generation_lengths.csv", sep = "_" )))

rm(functional_group_data_all)

message("saved generation length data (6/6 files)")

#' TODO: Remove this warning when I've fixed and tested everything :/

print("Warning: this function is still being tested, treat outputs with caution")

print(paste("Processing of files from simulation number", 
            simulation_number,", replicate", rep_numbers[[i]], "in the", scenario, "scenario directory complete", sep = " "))

  }

} else {
  
  print(paste("Folder", simulation_number, "age structure data has already been processed"), sep = " ")
  
  }

}

# Test function

# get_age_structure_data(IndicatorsProject, location, scenario, simulation, burnin)



## OLD VERSION
#' @param indicators_project file path where all project data is kept eg 'N:/Quantitative-Ecology/Indicators-Project/'
#' @param location string that denotes the directory of the location you want to process eg 'Serengeti'
#' @param scenario string that denotes which impact directory you want to look in eg "Baseline"
#' @param simulation name of the BuildModel directory you want to process eg  "001_BuildModel/"
#' @param remove_juveniles string, can be "yes" or "no" to determine
#' which age structures are saved (adults only, or all ages)
#' @param burnin integer denoting burnin time (in months not years)
#' @returns saves massbin, abundance, biomass and generation lengths to adaptor
#' output folder

#' TODO: Check the .rds files, don't think they're saving properly
 
#' TODO: Restructure so this takes multiple other functions like process_buildmodel
#' TODO: Fix plot functional groups? Or does it just look weird because there are few
#' timesteps?
#' TODO: Make this function remove burnin timesteps (for efficiency).  This will
#' require updates to other indicator code though.


## For testing:
# 

# inputs <- "N:/Quantitative-Ecology/Indicators-Project/Serengeti/Inputs_to_adaptor_code/Madingley_simulation_outputs/Test_runs/aa_BuildModel"
# outputs <- "N:/Quantitative-Ecology/Indicators-Project/Serengeti/Outputs_from_adaptor_code/map_of_life/Test_runs/aa_BuildModel"
# simulation_folder_name <- "aa_BuildModel"
# burnin <- 1 * 12
# scenario <- "Test_runs"
# 
# get_age_structure_data(inputs, outputs, scenario, simulation_folder_name, burnin)

inputs <- "N:/Quantitative-Ecology/Indicators-Project/Serengeti/Inputs_to_adaptor_code/Madingley_simulation_outputs/Test_runs/ae_BuildModel"
outputs <- "N:/Quantitative-Ecology/Indicators-Project/Serengeti/Outputs_from_adaptor_code/map_of_life/Test_runs/ae_BuildModel"
simulation_folder_name <- "ae_BuildModel"
burnin <- 1 * 12
scenario <- "Test_runs"
# 
# get_age_structure_data(inputs, outputs, scenario, simulation_folder_name, burnin)

# inputs = simulation_paths
# simulation = simulation_folder_names

# FUNCTION 1 - Get the detailed cohort data ----


#' @param inputs string, path to directory where the raw simulation outputs
#' are saved
#' @param outputs string, path to directory where the processed simulation outputs
#' will be stored, including the outputs from this function
#' @returns a list of dataframes, one for each replicate of the simulation. Each
#' dataframe contains the following variables:Cohort_ID, timestep, current bodymass,
#' cohort abundance, ID of the cohort's parent cohort, the mass at which the cohort becomes
#' adult, cohort generation length, cohort biomass, the functional group to 
#' which the cohort belongs, and a logical value determining if the cohort are adults in that timestep 


get_detailed_cohort_data <- function(inputs, outputs) {
  
  # Libraries and functions
  
  require(tidyverse)
  
  # function to filter lists by the element names
  
  filter_by_pattern <- function(pattern, your.list) {
    
    require(stringr)
    
    names(your.list) %>% 
    str_detect(pattern) %>%
    keep(your.list, .)
    
  }
  
  # Check if the age structure outputs have already been processed before proceeding
  
  files <- dir(outputs)
  files <- files[grep("juvenile",files)] # If it has been processed there should
  # already be juvenile files in the output folder
  
  if ( !dir.exists( file.path(outputs) ) ) {
    
    dir.create( file.path(outputs), recursive = TRUE )
    
  } 
  
  if ((dir.exists(file.path(outputs))) & (is_empty(files))) {
  
  # Get locations and file paths and simulation details
  
  dirs <- list.dirs(inputs, recursive = FALSE)
  model_results <- dirs[!str_detect(dirs, "input")]
  results_files <- list.files(model_results)
  model_inputs <- dirs[str_detect(dirs, "input")]
  simulation_folder_name <- basename(inputs)
  simulation_number <- str_remove(simulation_folder_name, "_BuildModel")
  
  # Work out how many replicates there are in the simulation folder
  
  reps <- (length(results_files) - 9) / 14
  rep_numbers <- as.character(seq(0,(reps - 1)))
  rep_index <- paste("_", rep_numbers, "_", sep = "")
  
  # Split up the output files by replicate number
  
  cohort_data <- list()
  
  rep_files <- list()
  
  for (rep in seq_along(rep_index)) {
    
    rep_files[[rep]] <- results_files[grep(rep_index[rep],results_files)]
    
  }
  
      for (i in seq_along(rep_files)) {
      
      # Get the new_cohorts data from which we can calculate mean age of parents,
      # aka generation length
      
      new_cohorts_name <- rep_files[[i]][str_detect(rep_files[[i]], "NewCohorts")]
      new_cohorts <- read_tsv(file.path(model_results,new_cohorts_name, sep =""), 
                              col_names = TRUE,
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
      
      growth_name <- rep_files[[i]][str_detect(rep_files[[i]], "Growth")]
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
      
      # birth_abundance <- new_cohorts %>%
      #   dplyr::select(offspring_cohort_ID, time_step, 
      #                 functional_group, abundance) %>%
      #   dplyr::rename(ID = offspring_cohort_ID) %>%
      #   dplyr::mutate(Current_body_mass_g = 1) %>%
      #   dplyr::select(ID, time_step, Current_body_mass_g,
      #                 functional_group, abundance)
      
      # Add birth timestep abundance to growth
      
      # 
      # abundance <- rbind(growth, birth_abundance) %>%
      #             arrange(time_step)
      
      # Combine abundance values in growth with 'adult mass' in new_cohorts to identify
      # timesteps when cohorts are adults, and their adult abundance in that timestep
      
      cohort_data[[i]] <- growth %>%
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
      
      # Get abundance at maturity, if we define maturity as reaching adult mass size
      #' TODO: This gives abundance at birth and death not abundance at birth and
      #' maturity, fix
      
      # maturity <- cohort_data %>% 
      #   group_by(ID) %>%
      #   slice(top_n(n=1, wt=desc(time_step)))
      # slice(which.min(time_step),
      #       which.max(time_step)) %>%
      #   select(ID, time_step, abundance, adult, new_functional_group, 
      #          Current_body_mass_g)
      # 
      # 
      # saveRDS( maturity, file = file.path(outputs,paste("AgeStructure", scenario,
      #                                                         simulation_number,rep_numbers[i], "maturity",
      #                                                         sep = "_" )))
      # write.csv( maturity, file = file.path(outputs,paste("AgeStructure",scenario,
      #                                                           simulation_number, rep_numbers[i], "maturity.csv", sep = "_" )))
      # 
      # rm(maturity)

      }
  return(cohort_data)
  }
}


cohort_data <- get_detailed_cohort_data(inputs, outputs)

# FUNCTION 2 - Convert the cohort data into massbin data ----

# This function should also work at the simulation level (i.e. input are all
# the replicates in one simulation folder, and outputs are all the replicates in
# one simulation folder)

## TODO: Important - output is currently not in correct massbin order.  Might 
## need to go back to previous approach of splitting the data by functional
## groups then rebinding it after summarising by timestep and massbin
## TODO: Check the biomass matrices match the massbin outputs
## TODO: Add an age switch (pick whether to output adults or juveniles, juveniles
## are the default)

convert_cohorts_to_massbins <- function(inputs, cohort_data){
  
  dirs <- list.dirs(inputs, recursive = FALSE)
  model_results <- dirs[!str_detect(dirs, "input")]
  results_files <- list.files(model_results)
  model_inputs <- dirs[str_detect(dirs, "input")]
  simulation_folder_name <- basename(inputs)
  simulation_number <- str_remove(simulation_folder_name, "_BuildModel")
        
  out <- list()
  
      for (i in seq_along(cohort_data)) {
          
     
        # juvenile_data <- cohort_data[[i]] %>%
        # dplyr::mutate(juvenile_abundance = ifelse(adult == FALSE, abundance, NA)) %>%
        # dplyr::mutate(juvenile_biomass = ifelse(adult == FALSE, biomass, NA)) %>%
        # dplyr::select(-c(abundance, biomass)) %>%
        # dplyr::rename(abundance = juvenile_abundance, biomass = juvenile_biomass) 
      
      # Get bodymass bins
      
      options(scipen = 999) # suppress scientific notation
      
      # Get true duration of the simulation
      
      sim_parameters <- read.csv(file.path(model_inputs, '/Model setup/SimulationControlParameters.csv'))
      sim_parameters$Parameters <- as.character(sim_parameters$Parameter)
      sim_parameters$Value <- as.character(sim_parameters$Value)
      
      duration_months <- as.numeric(sim_parameters %>% 
                                      dplyr:: filter(Parameter == "Length of simulation (years)") %>%
                                      dplyr:: select(Value)) * 12
      
      
      # Split into lower and upper bounds
      
      breaks <- read.csv(file.path(model_inputs, '/Model setup/Ecological definition files/MassBinDefinitions.csv'))
      
      bodymass_bins_lower <- c(breaks[,1])
      bodymass_bins_upper <- c(1000000000, 
                               bodymass_bins_lower[bodymass_bins_lower != 
                                                       min(bodymass_bins_lower)]) # Add a huge upper limit

      # Combine to produce column called 'massbins' that matches the format of 
      # model output massbin columns, so we can use it to merge other data later
        
      bodymass_bins <- as.data.frame(cbind(bodymass_bins_lower, 
                                           bodymass_bins_upper)) %>%
                         mutate(temp = paste(bodymass_bins_lower, 
                                             bodymass_bins_upper,  
                                            sep = ",")) %>%
                         mutate(massbins = paste("(", temp, "]", sep = "")) %>%
                         mutate(bodymass_bin_index = 
                                c((length(bodymass_bins_lower) - 1):0)) %>%
                         dplyr::select(-temp)
        
        # Group cohorts into the massbins and aggregate their abundance and biomass
        
        #' TODO: Check the juvenile values look right
        #' TODO: Massbins without any biomass are returning functional group
        #' values prefixed by NA (instead of 10, 11, 12 etc).  Fix this.
        
        extant_massbin_data <- cohort_data[[i]] %>%
          dplyr::select(time_step, Current_body_mass_g, new_functional_group,
                        abundance, biomass, adult) %>% # get important variables
          dplyr::group_by(massbins = cut(Current_body_mass_g,
                                         breaks = bodymass_bins_upper,
                                         include.lowest = FALSE,
                                         right = TRUE,
                                         na.rm = FALSE,
                                         dig.lab = 11)) %>% # reduce the resolution by grouping cohorts into bodymass bins
          dplyr::group_by(massbins, time_step, new_functional_group) %>%
          dplyr::summarise(abundance_sum = sum(abundance), # sum the abundance of all cohorts in that massbin
                           biomass_sum = sum(biomass), # ditto but with biomass
                           juvenile_abundance = sum(abundance[adult== FALSE]), # ditto but only for cohorts that haven't reached adulthood in that timestep
                           juvenile_biomass = sum(biomass[adult== FALSE])) %>% # ditto but with biomass
          # NB: If abundance and juvenile abundance are the same, it means all cohorts in that massbin at that timestep are juvenile (common in early timesteps)
          merge(bodymass_bins[ , c("massbins","bodymass_bin_index")],
                by = "massbins", all = TRUE) %>% # Add the bodymass bin index
         # dplyr::mutate(new_functional_group = fg) %>% # Add the functional group
          dplyr::mutate(functional_group_index = paste(new_functional_group,
                                                       bodymass_bin_index, sep = ".")) %>% # combine the two to create an index of functional-group-bodymass-bin
          dplyr::mutate(occupancy = ifelse(is.na(abundance_sum), "FALSE", "TRUE")) # Add a tag to identify empty vs occupied massbins (ie not all massbins will contain organisms at all timesteps)
        
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
          
        } else { # create empty time step rows so they don't get deleted
          
          # Create a copy of extant_massbin_data but make all values zero
          
          empty_df <- extant_massbin_data %>%
                      dplyr::filter(occupancy == TRUE) %>%
                      dplyr::mutate(abundance_sum = 0, biomass_sum = 0, 
                                    time_step = NA) %>%
                      dplyr::distinct(.)
          
          # Split into individual list for each functional group that has been extant at some
          # point during the simulation
          
          single_empty_df_list <- split(empty_df, empty_df$functional_group_index)
          
          # Loop over and add timesteps
          
          empty_df_list <- list()
          
          for (i in seq_along(single_empty_df_list)) {
            
            tmp1 <- do.call("rbind", replicate(length(missing_timesteps), 
                                               single_empty_df_list[[i]], 
                                               simplify = FALSE))
            
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
        
        # Convert the long format massbin_data into wide format, so it matches
        # the netcdf files with all ages
        
        abundance_wide <- massbin_data %>%
                          dplyr::select(-biomass_sum, -juvenile_biomass, 
                                        -juvenile_abundance) %>% # remove biomass
                          spread(time_step, abundance_sum,
                                 fill = NA, convert = FALSE) %>%
                          dplyr::arrange(bodymass_bin_index) %>%
                          `rownames<-`(.[,4])  %>%
                          dplyr::select(-c(new_functional_group,massbins,
                                           massbins, bodymass_bin_index,
                                           functional_group_index))
        
        abundance_matrix_temp <- as.matrix(abundance_wide) # convert to matrix
        
        out[[i]] <- ifelse(abundance_matrix_temp >= 1,
                                         log(abundance_matrix_temp),0) # convert log values
        
        rm(extant_massbin_data, extinct_massbin_data, 
           abundance_matrix_temp, abundance_wide)
        
    }
  
  return(out)
  
}
        
test <- convert_cohorts_to_massbins(inputs, cohort_data)




# Convert from long to wide format so it matches the netcdf model output format



rm(abundance_matrix_temp, abundance_wide)

functional_group_biomass[[j]] <- massbin_data %>%
  dplyr::select(-abundance_sum,-juvenile_biomass, -juvenile_abundance) %>% # Remove abundance
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

        
        # juvenile_biomass_wide <- massbin_data %>%
        #   dplyr::select(-abundance_sum,-biomass_sum, -juvenile_abundance) %>% # Remove abundance
        #   spread(time_step, juvenile_biomass,
        #          fill = NA, convert = FALSE) %>%
        #   dplyr::arrange(bodymass_bin_index) %>%
        #   `rownames<-`(.[,4])  %>%
        #   dplyr::select(-c(new_functional_group,massbins,
        #                    massbins, bodymass_bin_index,
        #                    functional_group_index))
        # 
        # juvenile_biomass_matrix_temp <- as.matrix(juvenile_biomass_wide)
        # 
        # juvenile_biomass_matrix_final <- ifelse(juvenile_biomass_matrix_temp >= 1,
        #                                         log(juvenile_biomass_matrix_temp),0)
        # 
        # rm(juvenile_biomass_matrix_temp, juvenile_biomass_wide)
        # 
        # juvenile_abundance_wide <- massbin_data %>%
        #   dplyr::select(-abundance_sum,-biomass_sum, -juvenile_biomass) %>% # Remove abundance
        #   spread(time_step, juvenile_abundance,
        #          fill = NA, convert = FALSE) %>%
        #   dplyr::arrange(bodymass_bin_index) %>%
        #   `rownames<-`(.[,4])  %>%
        #   dplyr::select(-c(new_functional_group,massbins,
        #                    massbins, bodymass_bin_index,
        #                    functional_group_index))
        # 
        # juvenile_abundance_matrix_temp <- as.matrix(juvenile_abundance_wide)
        # 
        # juvenile_abundance_matrix_final <- ifelse(juvenile_abundance_matrix_temp >= 1,
        #                                           log(juvenile_abundance_matrix_temp),0)
        # 
        # rm(juvenile_abundance_matrix_temp, juvenile_abundance_wide)
        # 
        # fg <- max(data$new_functional_group, na.rm = TRUE)
        # 
        # Calculate the generation length per massbin functional group (mean of all cohorts
        # within the massbin)
        
      #   if (generation_lengths == "yes") {
      #     
      #     generation_lengths <- data %>%
      #       dplyr::select(ID, time_step, new_functional_group, 
      #                     Current_body_mass_g, generation_length) %>%
      #       dplyr::group_by(massbins = cut(Current_body_mass_g, 
      #                                      breaks = massbin_breaks,  
      #                                      na.rm = FALSE,
      #                                      dig.lab = 11)) %>% #suppress scientific notation
      #       dplyr::group_by(massbins, new_functional_group) %>%
      #       dplyr::summarise(gen_length_mean = mean(generation_length, 
      #                                               na.rm = TRUE),
      #                        gen_length_max = max(generation_length, na.rm = TRUE),
      #                        gen_length_min = min(generation_length, na.rm = TRUE)) %>%
      #       merge(bodymass_bins[ , c("massbins","bodymass_bin_index")],
      #             by = "massbins", all = TRUE) %>%
      #       dplyr::mutate(new_functional_group = fg) %>%
      #       dplyr::mutate(functional_group_index = paste(new_functional_group, 
      #                                                    bodymass_bin_index, sep = ".")) %>%
      #       dplyr::select(-c(new_functional_group,massbins,
      #                        massbins)) %>%
      #       dplyr::arrange(bodymass_bin_index)
      #     
      #     output <- list(massbin_data, abundance_matrix_final, biomass_matrix_final, 
      #                    juvenile_biomass_matrix_final, juvenile_abundance_matrix_final,
      #                    generation_lengths)
      #     
      #     names(output) <- c("massbin_data_long", "abundance_wide", "biomass_wide", 
      #                        "juvenile_biomass_wide", "juvenile_abundance_wide", 
      #                        "generation_lengths")
      #     
      #     print(paste("functional group", fg, "processed", sep = " "))
      #     
      #     return(output)
      #     
      #     rm(output)
      #     
      #   } else {
      #     
      #     output <- list(massbin_data, abundance_matrix_final, biomass_matrix_final, 
      #                    juvenile_biomass_matrix_final, juvenile_abundance_matrix_final)
      #     
      #     names(output) <- c("massbin_data_long", "abundance_wide", "biomass_wide", 
      #                        "juvenile_biomass_wide", "juvenile_abundance_wide")
      #     
      #     print(paste("functional group", fg, "processed", sep = " "))
      #     
      #     return(output)
      #     
      #     rm(output)
      #     
      #   }
      # }
      
      # Convert data from all functional groups into the wide format using the
      # group_by_massbin function
      
      functional_group_data_all <- lapply(all_ages_list, group_by_massbin, 
                                          breaks, duration_months, "yes")
      
      # Save all ages wide abundance dataframe
      
      abundance_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                                       pattern = "abundance_wide"))
      
      abundance_all <- do.call(rbind,abundance_list)
      rm(abundance_list)
      
      saveRDS( abundance_all, file = file.path(outputs,paste("AgeStructure",scenario, 
                                                                   simulation_number,rep_numbers[i], "abundance", sep = "_" )))
      write.csv( abundance_all, file = file.path(outputs,paste("AgeStructure",scenario, 
                                                                     simulation_number,rep_numbers[i], "abundance.csv", sep = "_" )))
      
      
      message("saved all ages abundance data (1/6 files)")
      
      # Save all ages wide biomass dataframe
      
      biomass_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                                     pattern = "biomass_wide"))
      
      biomass_all <- do.call(rbind,biomass_list)
      
      rm(biomass_list)
      
      saveRDS( biomass_all, file = file.path(outputs,paste("AgeStructure", scenario, 
                                                                 simulation_number,rep_numbers[i], "biomass", sep = "_" )))
      write.csv( biomass_all, file = file.path(outputs,paste("AgeStructure", scenario,
                                                                   simulation_number,rep_numbers[i], "biomass.csv", sep = "_" )))
      
      message("saved all ages biomass data (2/6 files)")
      
      rm(biomass_all)
      
      # Save juvenile wide biomass dataframe
      
      juvenile_biomass_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                                              pattern = "juvenile_biomass_wide"))
      
      juvenile_biomass_all <- do.call(rbind,juvenile_biomass_list)
      
      rm(juvenile_biomass_list)
      
      saveRDS( juvenile_biomass_all, file = file.path(outputs,paste("AgeStructure", scenario, 
                                                                          simulation_number, rep_numbers[i], "juvenile_biomass", sep = "_" )))
      write.csv( juvenile_biomass_all, file = file.path(outputs,paste("AgeStructure", scenario,
                                                                            simulation_number, rep_numbers[i],"juvenile_biomass.csv", sep = "_" )))
      
      message("saved juvenile biomass data (3/6 files)")
      
      rm(juvenile_biomass_all)
      
      # Save juvenile wide abundance dataframe
      
      juvenile_abundance_list <- flatten(lapply(functional_group_data_all, filter_by_pattern, 
                                                pattern = "juvenile_abundance_wide"))
      
      juvenile_abundance_all <- do.call(rbind,juvenile_abundance_list)
      
      rm(juvenile_abundance_list)
      
      saveRDS( juvenile_abundance_all, file = file.path(outputs,paste("AgeStructure", scenario, 
                                                                            simulation_number, rep_numbers[i],"juvenile_abundance", sep = "_" )))
      write.csv( juvenile_abundance_all, file = file.path(outputs,paste("AgeStructure", scenario,
                                                                              simulation_number, rep_numbers[i],"juvenile_abundance.csv", sep = "_" )))
      
      message("saved juvenile abundance data (3/6 files)")
      
      rm(juvenile_abundance_all)
      
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
      
      plotName <- paste(scenario, "_", simulation_number, rep_numbers[i], "_functional_group_bodymass_distribution",".tiff",sep="")
      tiff(file = (paste(outputs,plotName, sep = "/")), units ="in", width=10, height=5, res=100)
      
      
      print(ggplot(data = plot_data, aes( x = bodymass_bin_index, 
                                          y = total_abundance, col = functional_group_name)) +
              geom_bar(stat = 'identity') +
              labs(x = "bodymass bins", y = "total_modelled_abundance") +
              facet_wrap( ~ functional_group_name, nrow = 3))
      
      dev.off()
      
      rm(abundance_all)
      rm(plot_data)
      
      message("saved plot of functional group biomass (3/6 files)")
      
      ## Get and save generation length data
      
      
      generation_length_list <- flatten(lapply(functional_group_data_all,
                                               filter_by_pattern, pattern = "generation_lengths"))
      
      generation_length_all <- do.call(rbind,generation_length_list)
      rm(generation_length_list)
      
      saveRDS( generation_length_all, file = file.path(outputs,paste("AgeStructure", scenario,
                                                                           simulation_number,rep_numbers[i], "generation_lengths",
                                                                           sep = "_" )))
      write.csv( generation_length_all, file = file.path(outputs,paste("AgeStructure",scenario,
                                                                             simulation_number, rep_numbers[i], "generation_lengths.csv", sep = "_" )))
      
      rm(functional_group_data_all)
      
      message("saved generation length data (6/6 files)")
      
      #' TODO: Remove this warning when I've fixed and tested everything :/
      
      print("Warning: this function is still being tested, treat outputs with caution")
      
      print(paste("Processing of files from simulation number", 
                  simulation_number,", replicate", rep_numbers[[i]], "in the", scenario, "scenario directory complete", sep = " "))
      
    }
    
  } else {
    
    print(paste("Folder", simulation_number, "age structure data has already been processed"), sep = " ")
    
  }
  
}

# Test function

# get_age_structure_data(IndicatorsProject, location, scenario, simulation, burnin)

