

#' Get biomass of each "group" (see madingley_get_groups.R) across each time step
#' 
#' 
#' @param Autotroph_file_location The output from the Madingley model in the file MassBinsOutputs_(scenario name)_(replicate number)_(cell number).nc
#' @return A dataframe where the rows are all autotrophs, evergreens and deciduous, and the columns are timesteps
#'         

#Autotroph_file_location <- "N:\\Quantitative-Ecology\\Indicators-Project\\Serengeti\\Inputs_to_adaptor_code\\Madingley_simulation_outputs\\Test_runs\\aa_BuildModel\\baseline_0012019-11-1_12.12.50\\BasicOutputs_baseline_001_0_Cell0.nc"

madingley_get_autotroph_biomass <- function( Autotroph_file_location  ) {
  

    file.paths <- Autotroph_file_location
    # 
    extract.nc.data <- function(file.paths){
      
      #Use the file paths to extract the sds keys you need to get the data
      data <- lapply(Autotroph_file_location, nc_open)
      
      #Get a list of data content
      #schema <- lapply(data, schema.sds)
      
      files <- "random"
      
      #Names list so we know which cell and simulation the output belongs to
      names(data) <- files
      
      #Get a list of variables these outputs contain
      schema1 <- data[[1]]
      vars <- names(schema1$var)
      
      #Create empty parent.list for each cell+simulation, each containing a variable.list
      #containing the output data for 29 variables
      parent.list <- list()
      variable.list <- list()
      
      for (j in seq_along(data)) {#Loop over each cell for each simulation
        cell.sim <- data[[j]]
        #names(cell.sim) <- files[[1]]####
        for (i in seq_along(vars)) {
          variable.list[[i]] <- ncvar_get(cell.sim,vars[[i]]) #Loop over each variable
        }
        names(variable.list) <- vars #make sure variables are names
        parent.list[[j]] <- variable.list
      }
      
      names(parent.list) <- files #### #make sure each cell simulation is named
      return(parent.list)
      
      nc_close(data) #Check this doesn't stop the function from working
    }

    results <- flatten(extract.nc.data(Autotroph_file_location))

    evergreens <- results$`evergreen biomass density`
    deciduous <- results$`deciduous biomass density`
    autotrophs <- results$`autotroph biomass density`
    
    plant_biomass <- as.data.frame(rbind(autotrophs, evergreens, deciduous))
 
    return( plant_biomass )
}

  
  
  