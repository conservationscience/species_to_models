# This file is all of the code needed to match species to the Madingley outputs
# You only need to change your working directory to the directory that you 
# downloaded the GitHub repository to.


############################################################################
### Setup
############################################################################
library( readxl )
library("dplyr")
library( functionaltraits )


# Choose the species list you want to load
species_list <- read.table( 
  "species_lists/mapoflife_serengeti_headerremoved.csv", 
  sep=",", header=TRUE, stringsAsFactors=FALSE, quote="" )
species_list <- species_list$Scientific.Name

# select a location to store the databases files on your computer
databases <- functionaltraits::Databases$new( "functionaltraits_data" )

# check databases are ready
databases$ready()

# you have to download the databases the first time
databases$initialise()

# check that they downloaded properly
databases$ready()

############################################################################
### Get the trait data, and save it
############################################################################

# get ALL the traits for those species; don't check synonyms.
# this code runs a lot faster but we can't calculate energy_source or metabolism_pathway without 
# taxonomic information
results <- databases$search( unique(serengeti_species) )

# use this if you want to check synonyms and also get taxonomic information
results <- find_species_traits( databases, serengeti_species )

# use this if you want to skip synonyms (it runs a lot faster, but you can't calculate some of the colu)
#results <- databases$search( unique(serengeti_species) )


output <- results$results


#####################
# calculate the additional columns
# see ?compute_column
#####################
output$energy_source <- compute_column(
  input = output,
  calculator = function( species ) {
    if( !is.na( species$kingdom ) ) {
      
      # if the phylum is not Chordata, then it's not endothermic
      if( species$kingdom == "Animalia" ) {
        return( "heterotroph" )
        
      }
      else if( species$kingdom == "Plantae" ) {
        return( "autotroph" )
      }
      else return( NA )
    }
    else return( NA )
  }
)

output$thermoregulation = compute_column(
  input = output,
  calculator = function( species ) {
    # the rule for this code is
    # if it's in Aves or if it's in Mammalia, then it's an endotherm
    # if it's not, then it's an ectotherm
    # there are some edge cases eg. in some fish which they shiver or use muscles to
    # produce some warmth, but primarily I think the models and most people
    # would still consider these ectotherms rather than strictly regulated endotherms
    # like mammals and birds
    
    # check if phylum information is available, else return NA
    if( !is.na( species$phylum ) ) {
      
      # if the phylum is not Chordata, then it's not endothermic
      if( species$phylum == "Chordata" ) {
        
        if( !is.na( species$class) ) {
          # have to check the classes
          if( species$class == "Mammalia" ) {
            return( "endotherm" )
          }
          else if( species$class == "Aves" ) {
            return( "endotherm" )
          }
          else return( "ectotherm" )
        }
        else return( NA )
        
      }
      else {
        return( "ectotherm" )
      }
    }
    else return( NA )
  }
)


##### add diet information
output$n_jones <-  rep( NA, nrow( output ) )
output$n_kissling<-rep( NA, nrow( output ) )
output$n_wilman_bird<-rep( NA, nrow( output ) )
output$n_olivira<-rep( NA, nrow( output ) )

output$nutrition_source<-rep( NA, nrow( output ) )
output$n_ambigous <- rep( NA, nrow( output ) )


# these functions take in a row of the dataframe, and
# return either "herbivore", "omnivore", "carnivore" or NA depending on
# what database c they take information from
get_jones_trophiclevel <- function( species ) {
  if( !is.na( species[[ "jones_X6.2_TrophicLevel"]] ) ) {
    # need to convert the numbering system in pantheria (1=herbivore,2=omnivore,3=carnivore)
    return( case_when(
      species[["jones_X6.2_TrophicLevel"]] == 1 ~ "herbivore",
      species[["jones_X6.2_TrophicLevel"]] == 2 ~ "omnivore",
      species[["jones_X6.2_TrophicLevel"]] == 3 ~ "carnivore",
      species[["jones_X6.2_TrophicLevel"]] == NA ~ "NA"
    ) )
  }
  else return(NA)
}

get_kissling_trophiclevel <- function( species ) {
  if( !is.na( species[["kissling_TrophicLevel"]] ) ) {
    # need to make the text lowercase
    return( tolower( species[["kissling_TrophicLevel"]] ) )
  }
  else return(NA)
}

get_wilman_bird_trophiclevel <- function( species ) {
  if( !is.na( species[["wilman_bird_Diet.5Cat"]] ) ) {
    # need to convert the system in eltontraits
    # PlantSeed or FruiNect = herbivore; Omnivore = omnivore; Invertebrate or VertFishScav = carnivore;
    return( case_when(
      species[["wilman_bird_Diet.5Cat"]] == "PlantSeed" ~ "herbivore",
      species[["wilman_bird_Diet.5Cat"]] == "FruiNect" ~ "herbivore",
      species[["wilman_bird_Diet.5Cat"]] == "Omnivore" ~ "omnivore",
      species[["wilman_bird_Diet.5Cat"]] == "Invertebrate" ~ "carnivore",
      species[["wilman_bird_Diet.5Cat"]] == "VertFishScav" ~ "carnivore",
      species[["wilman_bird_Diet.5Cat"]] == NA ~ "NA"
    ) )
  }
  else return( NA )
}

get_olivira_trophiclevel <- function( species ) {
  # if any one of the oliveira columns isn't a NA, then we can use it.
  
  # the logic is; if it only eats (leaves, flowers, seeds or fruits) then it's a herbivore
  # if it only eats (athro or verts) then it's a carnivore
  # if it eats from either of the above, it's an omnivore
  if( 
    !is.na( species[[ "oliveira_Leaves" ]] ) 
    || !is.na( species[[ "oliveira_Flowers"]] )
    || !is.na( species[[ "oliveira_Seeds"]] )
    || !is.na( species[[ "oliveira_Fruits"]] )
    || !is.na( species[[ "oliveira_Arthro"]] )
    || !is.na( species[[ "oliveira_Vert"]] )
  ) {
    # first lets group the variables into two categories
    herbivore <- case_when(
      species[[ "oliveira_Leaves"]] == 1 ~ TRUE,
      species[[ "oliveira_Flowers"]] == 1 ~ TRUE,
      species[[ "oliveira_Seeds"]] == 1 ~ TRUE,
      species[[ "oliveira_Fruits"]] == 1 ~ TRUE,
      # default value is FALSE
      1 == 1 ~ FALSE
    )
    carnivore <- case_when(
      species[[ "oliveira_Arthro"]] == 1 ~ TRUE,
      species[[ "oliveira_Vert"]] == 1 ~ TRUE,
      # default value is FALSE
      1 == 1 ~ FALSE
    )
    # if it's a herbivore and a carnivore, then it's an omnivore
    if( herbivore == TRUE && carnivore == TRUE ) {
      return( "omnivore" )
    }
    # and logically the rest follows
    else if( herbivore == FALSE && carnivore == TRUE ) {
      return( "carnivore" )
    }
    else if( herbivore == TRUE && carnivore == FALSE ) {
      return( "herbivore" )
    }
  }
  else return( NA )
}

#### loop through the dataframe and calculate the columns
# this code is very inefficient and slow but it gets the job done
for( i in 1:nrow( output ) ) {
  
  # get the information from each database column, so it's easy to work with
  jones <- get_jones_trophiclevel( as.list( output[i, ] ) )
  kissling <- get_jones_trophiclevel( as.list( output[i, ] ) )
  wilman_bird <- get_wilman_bird_trophiclevel( as.list( output[i, ] ) )
  olivira <- get_olivira_trophiclevel( as.list( output[i, ] ) )
  
  # add it to the output
  output[[ i, "n_jones"]] <- jones
  output[[ i, "n_kissling"]] <- kissling
  output[[ i, "n_wilman_bird"]] <- wilman_bird
  output[[ i, "n_olivira"]] <- olivira
  
  # calculate nutrition_source column and also whether that value is caluclated from
  # multiple databases (n_ambigious)
  all_values <- c( jones, kissling, wilman_bird, olivira )
  all_values <- na.omit( unique( all_values ) )
  
  # If there's only one value then we take that value
  if( length(all_values) == 1 ) {
    output[[ i, "nutrition_source"]] <- all_values
    output[[ i, "n_ambigous"]] <- FALSE
  }
  
  # if there's more than one value, then we take the most inclusive category ()
  else if( length( all_values > 1 ) ) {
    ## if omnivore is present by any of the databases, then we assume that it's an omnivore
    # regardless of whether another database thinks it's a carnivore or herbivore
    if( any("omnivore" %in% all_values) ) {
      output[[ i, "nutrition_source" ]] <- "omnivore"
      output[[ i, "n_ambigous"]] <- TRUE
    }
    else if( any("carnivore" %in% all_values) && any("herbivore" %in% all_values) ) {
      output[[ i, "nutrition_source" ]] <- "omnivore"
      output[[ i, "n_ambigous"]] <- TRUE
    }
    else if( any("carnivore" %in% all_values) ) {
      output[[ i, "nutrition_source" ]] <- "carnivore"
      output[[ i, "n_ambigous"]] <- TRUE
    }
    else if( any("herbivore" %in% all_values) ) {
      output[[ i, "nutrition_source" ]] <- "herbivore"
      output[[ i, "n_ambigous"]] <- TRUE
    }
  }
  
}


### old function
# output$nutrition_source = compute_column(
#   input = output,
#   calculator = function( species ) {
#     if( !is.na( species[["jones_X6.2_TrophicLevel"]] ) ) {
#       # need to convert the numbering system in pantheria (1=herbivore,2=omnivore,3=carnivore)
#       return( case_when(
#         species[["jones_X6.2_TrophicLevel"]] == 1 ~ "herbivore",
#         species[["jones_X6.2_TrophicLevel"]] == 2 ~ "omnivore",
#         species[["jones_X6.2_TrophicLevel"]] == 3 ~ "carnivore",
#         species[["jones_X6.2_TrophicLevel"]] == NA ~ "NA"
#       ) )
#     }
#     
#     else if( !is.na( species[["kissling_TrophicLevel"]] ) ) {
#       # need to make the text lowercase
#       return( tolower( species[["kissling_TrophicLevel"]] ) )
#     }
#     
#     else if( !is.na( species[["wilman_bird_Diet.5Cat"]] ) ) {
#       # need to convert the system in eltontraits
#       # PlantSeed or FruiNect = herbivore; Omnivore = omnivore; Invertebrate or VertFishScav = carnivore;
#       return( case_when(
#         species[["wilman_bird_Diet.5Cat"]] == "PlantSeed" ~ "herbivore",
#         species[["wilman_bird_Diet.5Cat"]] == "FruiNect" ~ "herbivore",
#         species[["wilman_bird_Diet.5Cat"]] == "Omnivore" ~ "omnivore",
#         species[["wilman_bird_Diet.5Cat"]] == "Invertebrate" ~ "carnivore",
#         species[["wilman_bird_Diet.5Cat"]] == "VertFishScav" ~ "carnivore",
#         species[["wilman_bird_Diet.5Cat"]] == NA ~ "NA"
#       ) )
#     }
#     
#     # oliveira last, because we have to do some data massaging to get the final classification
#     # if any one of the oliveira columns isn't a NA, then we can use it.
#     
#     # the logic is; if it only eats (leaves, flowers, seeds or fruits) then it's a herbivore
#     # if it only eats (athro or verts) then it's a carnivore
#     # if it eats from either of the above, it's an omnivore
#     else if( 
#       !is.na( species[[ "oliveira_Leaves" ]] ) 
#       || !is.na( species[[ "oliveira_Flowers"]] )
#       || !is.na( species[[ "oliveira_Seeds"]] )
#       || !is.na( species[[ "oliveira_Fruits"]] )
#       || !is.na( species[[ "oliveira_Arthro"]] )
#       || !is.na( species[[ "oliveira_Vert"]] )
#     ) {
#       # first lets group the variables into two categories
#       herbivore <- case_when(
#         species[[ "oliveira_Leaves"]] == 1 ~ TRUE,
#         species[[ "oliveira_Flowers"]] == 1 ~ TRUE,
#         species[[ "oliveira_Seeds"]] == 1 ~ TRUE,
#         species[[ "oliveira_Fruits"]] == 1 ~ TRUE,
#         # default value is FALSE
#         1 == 1 ~ FALSE
#       )
#       carnivore <- case_when(
#         species[[ "oliveira_Arthro"]] == 1 ~ TRUE,
#         species[[ "oliveira_Vert"]] == 1 ~ TRUE,
#         # default value is FALSE
#         1 == 1 ~ FALSE
#       )
#       # if it's a herbivore and a carnivore, then it's an omnivore
#       if( herbivore == TRUE && carnivore == TRUE ) {
#         return( "omnivore" )
#       }
#       # and logically the rest follows
#       else if( herbivore == FALSE && carnivore == TRUE ) {
#         return( "carnivore" )
#       }
#       else if( herbivore == TRUE && carnivore == FALSE ) {
#         return( "herbivore" )
#       }
#     }
#     return( NA ) # this case should never be run
#   }
# )

bodymass_columns <- c(
  "jones_X5.1_AdultBodyMass_g",
  "tacutu_Adult.weight..g.",
  "slavenko_Maximum_mass_g",
  "fishbase_Weight",
  "oliveira_Body_mass_g",
  "myhrvold_adult_body_mass_g",
  "earnst_mass.g.",
  "lislevand_M_mass",
  "lislevand_F_mass",
  "wilman_bird_BodyMass.Value",
  "wilman_mam_BodyMass.Value",
  "pacifici_AdultBodyMass_g"
)

output$bodymass = compute_column(
  input = output,
  calculator = function( species ) {
    species <- species[bodymass_columns]
    # get all the bodymass values in the columns for this row; remove duplicates; and remove NA values
    bodymass_values <- na.omit( 
      unique(
        round( as.numeric( species ), digits = 0 )
      )
    )
    if( length( bodymass_values > 0 ) ) {
      ##### calculate bodymass as a median value ########
      ##### changes this if you want to calculate it in some other way #####
      ### note that bodymass_values is rounded & has all of the unique values removed though ####
      return( median( bodymass_values ) )
    }
    else return( NA )
  }
)

output$bodymass_range = compute_column(
  input = output,
  calculator = function( species ) {
    species <- species[bodymass_columns]
    # get all the bodymass values in the columns for this row; remove duplicates; and remove NA values
    bodymass_values <- na.omit( 
      unique(
        round( as.numeric( species ), digits = 0 )
      )
    )
    if( length(bodymass_values == 0 ) ) return( NA )
    if( length( bodymass_values > 1 ) ) {
      return( diff( range( bodymass_values ) ) )
    }
    else return( 0 )
  }
)

