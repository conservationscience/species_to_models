
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
#' @param outputs string, path to directory where the processed generation length outputs
#' will be stored, including the outputs from this function
#' @returns a list of dataframes, one for each replicate of the simulation. Each
#' dataframe contains the following variables: Virtual species group id,
#' generation length in years, parent cohort_ID, functional group index,
#' functional group name, adult mass of the cohort, lower and upper mass
#' bounds that the cohort falls within according to our massbins, and massbin label
#' @returns A boxplot for each replicate showing distribution of generation lengths
#' split by functional group 


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
      
      # Wrangle cohort data ----
      
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
      
      # Group cohorts ----
      
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
      
      # Calculate gen lengths ----
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
      
      # Save data ----
      
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
      
      # Make boxplot ----      

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
   
# get_generation_lengths(inputs, outputs)   
      
