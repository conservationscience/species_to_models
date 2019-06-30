

# TODO: why does this function return fg.names with trailing spaces??

#' Get a dataframe containing the definition of each "group".
#' 
#' A "group" is a functional group from the Madingley model outputs, divided into the
#' number of massbins that were used to run the Madingley simulation. So if there were 6
#' functional groups, and 100 massbins, there would be 6 x 100 = 600 different "groups".
#' Each "group" has a unique ID and this dataframe defines what each group actually represents
#' in the Madingley model.
#' 
#' The ID for the "group" is a reflection of the original ID in the CohortFunctionalGroupDefinitions.csv file 
#' from the Madingley model. Where two of the original ID's have been merged, they are both included.
#' Eg. the "group" ID 10.12.30 would represent functional groups 10 and 12, in the bodymass category of 30.
#' 
#' 
#' @param CohortFunctionalGroupDefinitions 
#' @param MassBinDefinitions 
#' @param SimulationControlParameters
#' @return A datafrane with the definition of each "group".

get_groups <- function( CohortFunctionalGroupDefinitions, MassBinDefinitions, SimulationControlParameters ) {
  
  # Calculate upper massbins and join with lower bins output by model
  
  Mass.bin.upper.bound <- c(max(MassBinDefinitions) + 100000, 
                            MassBinDefinitions$Mass.bin.lower.bound[1:77])
  
  massbins <- cbind(MassBinDefinitions, Mass.bin.upper.bound) %>%
    mutate(bodymass_index = c((nrow(MassBinDefinitions) - 1):0))
  
  
  # Retrieve the burnin period from your simulation outputs
  
  
  ## TODO
  ## NOTE: baseline sets burn-in as zero so need to an add if-statement so it
  ## adds 1000 if calculating for baseline scenario
  
  # burn_in <- 1000
  
  burn_in <- as.numeric(as.character(SimulationControlParameters[3,2])) * 12 
  
  # Set your realm (marine or terrestrial)
  
  realm <- "Terrestrial"
  #realm <- "Marine"
  
  # Develop unique key for each model functional group 
  
  ## Add the functional group index and subset functional groups by realm
  
  subset_functionalgroups <-  CohortFunctionalGroupDefinitions %>%
    mutate(FG_id = c(0:18)) %>% # Add zero based index
    filter(DEFINITION_Realm == realm) %>%
    select(DEFINITION_Heterotroph.Autotroph,
           DEFINITION_Nutrition.source,
           DEFINITION_Endo.Ectotherm,
           DEFINITION_Reproductive.strategy,
           FG_id)
  
  
  ## Process model functional groups so we collapse groups differentiated by
  ## reproductive strategy, because the abundance/biomass model output doesn't 
  ## actually differentiate between these groups. (NOTE - this code has not been 
  ## tested on marine groups)
  
  ## Create groups to loop through the df 
  
  trophic <- unique(subset_functionalgroups$DEFINITION_Heterotroph.Autotroph)
  nutrition <- unique(subset_functionalgroups$DEFINITION_Nutrition.source)
  metabol <- unique(subset_functionalgroups$DEFINITION_Endo.Ectotherm)
  
  ## Break down each level of functional group until you get to reproductive strategy.
  ## Where trophic level, nutrition and metabolic pathways are the same but reproductive
  ## strategy differs, collapse the functional group id
  
  merged_functionalgroups_temp <- data.frame()
  
  for(troph in trophic){
    temp1 <- subset_functionalgroups %>% 
      filter(DEFINITION_Heterotroph.Autotroph == troph)
    for(diet in nutrition){
      temp2 <- temp1 %>% filter(DEFINITION_Nutrition.source == diet)
      for(therm in metabol){
        temp3 <- temp2 %>% filter(DEFINITION_Endo.Ectotherm == therm)
        temp3$functionalgroup_index <- temp3$FG_id
        if(nrow(temp3) > 1){
          functionalgroup_index <- paste(temp3$FG_id, collapse = '.')
          temp3$functionalgroup_index <- functionalgroup_index
        }
        
        merged_functionalgroups_temp <- rbind(merged_functionalgroups_temp, temp3)
        
      }
    }
  }
  
  rm(temp1, temp2, temp3)
  
  ## Subset the merged functional group dataframe to include only one row for 
  ## merged groups
  
  merged_functionalgroups <- merged_functionalgroups_temp %>% 
    select(-FG_id, 
           -DEFINITION_Reproductive.strategy) %>%
    distinct() %>%
    mutate(fg.name = tolower(paste(
      DEFINITION_Nutrition.source,
      DEFINITION_Endo.Ectotherm, sep = " ")))
  
  # Repeat the massbins by number of merged functional groups
  
  replicated_massbins <- replicate(nrow(merged_functionalgroups), massbins, 
                                   simplify = FALSE)
  replicated_massbins <- do.call(rbind, replicated_massbins)
  
  # Likewise, create a mirrored repetition of each functional group by number
  # of bodymass bins. Final replicated_massbins and replicated_functional groups
  # should have the same number of rows
  
  
  
  
  
  replicated_functionalgroups <- merged_functionalgroups %>% 
    slice(rep(1:n(), 
              each = nrow(massbins)))
  
  
  ## Merge replicated bodymass and functional group dataframes to create a 
  ## species definition dataframe, list and index
  
  species_definitions <-  data.frame(replicated_functionalgroups, 
                                     replicated_massbins) %>%
    mutate(species_index = paste(functionalgroup_index, 
                                 bodymass_index, sep = ".")) 
  
  # TODO
  # @ Simone, not sure where you were going with this
  # sorry
  
  #species_definitions_list <- split(species_definitions, 
  #                                  species_definitions$fg.name)
  return( species_definitions )
}

  
