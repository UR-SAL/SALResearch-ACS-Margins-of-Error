################################################################################
# Author: Alex Broening
# Date: 07.04.2024 - 8.4.2024
# Purpose: This .R file contains all the functions used in the ErrorIndex_Notebook tool, 
#   which allows the user to select a number of variables from the ACS, and construct an index from
#   from those variables for where the clusters of high and low margins of error are. The tool allows the user
#   to select a number of options for how that index and maps are created, as well as for the geographies to be
#   analyzed.
################################################################################

################################################################################
#################### SETUP #####################################################
################################################################################

### Install Packages
# install.packages("sfdep")
# install.packages("spdep")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("tidycensus")
# 
# ### Load Libraries
# library(tidycensus)
# library(sf)
# library(tidyverse)
# library(dplyr)
# library(sfdep)
# library(spdep)

## Replace Key with individual key. Available at https://api.census.gov/data/key_signup.html
census_api_key("753915d84691b3657721d57a715b97feafbbf81a")

### Options Settings
## Prevent Scientific Notation in View()
options(scipen = 999)
## Cache Geometries for faster repeated ACS API Queries 
options(tigris_use_cache = TRUE)

################################################################################
#################### CALL Functions ############################################
################################################################################

#' NAME
#' 
#' Desc
#'
#' @param yr desc
#' @param varlist desc
#' @param states desc
#' @param level desc
#' @param surv desc
#' @param dur desc
#' 
#' @return Wide Dataframe
acs_call <- function(yr = 2022, varlist = var_list, states = state_list, level = "tract", surv = "acs5", dur = FALSE) {
  start <- Sys.time()
  output <- data.frame()
  for (state in states) {
    temp <- get_acs(
      geography = level,
      variables = varlist,
      survey = surv,
      year = yr,
      state = state,
      output = "wide",
      geometry = TRUE
    )
    output <- rbind(output, temp)
  }
  if (dur == TRUE) {print(Sys.time()-start)}
  return(output)
}

#' NAME
#' 
#' Desc
#'
#' @param states desc
#' 
#' @return desc
get_state_list <- function(states = "contig") {
  #Loads all state fips codes
  all <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53","54","55","56","60","66","69","72","74","78")
  if (states == "contig") {
    #removes non states, and hawaii and alaska
    return(all[-which(all %in% c(alaska = "02", 
                                 hawaii = "15",
                                 dc = "11",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all") {
    #removes nothing
    return(all)  
  } else if (states == "all_states") {
    #removes nonstates
    return(all[-which(all %in% c(dc = "11",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "states_pr") {
    #removes non states, leaving Puerto Rico
    return(all[-which(all %in% c(dc = "11",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "states_dc") {
    #removes non states, leaving District of Columbia
    return(all[-which(all %in% c(puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "states_dc_pr") {
    #removes non states, leaving Puerto Rico and District of Columbia
    return(all[-which(all %in% c(guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "contig_dc") {
    #removes non states, removes Hawaii and Alaska, leaving District of Columbia
    return(all[-which(all %in% c(alaska = "02", 
                                 hawaii = "15",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  }
}

################################################################################
#################### CLEANING Functions ########################################
################################################################################

#' Calculate Coefficient of Variations for ACS Data
#' 
#' Desc
#'
#' @param df_wide a wide dataframe produced with the tidycensus get_acs() function
#' 
#' @return Wide Dataframe with additional columsn for each Margin of Error Variable
mutate_cv <- function(df_wide) {
  #   Pulls names of Columns
  heads <- names(df_wide)
  #   For each column displaying a Margin of Error:
  for (x in heads) {
    if (endsWith(x, "M")) {
      #       Creates new column name- variable name, ending with "CV"
      newhead <- paste0(substr(x, 1, (nchar(x)-1)),"CV")
      #       Sets estimatehead equal column name of estimates for variable selected in if statement
      estimatehead <- as.character(paste0(substr(x, 1, nchar(x)-1), "E"))
      #       Mutates new column with calculated CV, under column name from newhead
      df_wide <- df_wide %>% 
        mutate({{newhead}} := ((get(!!x)/1.645)/get(!!estimatehead)*100))
    }
  } 
  #   Returns the new dataframe
  return(df_wide)
}

#' Remove Structural Zero-Estimate Geographies
#' 
#' Desc
#'
#' @param sfdataframe a wide sf class dataframe produced with the tidycensus get_acs() function
#' 
#' @return Wide Dataframe without the rows populated with only zero-esimtates
drop_zeroes <- function(sfdataframe) {
  temp <- as.data.frame(sfdataframe, drop = TRUE)
  og_length <- nrow(temp)
  temp <- st_as_sf(subset(temp, rowSums(temp[, endsWith(names(temp), "E") & names(temp) != "NAME"] != 0, na.rm = TRUE) != 0))
  new_length <- nrow(temp)
  print(paste0(as.character(og_length - new_length), " rows were dropped from the dataframe \'", deparse(substitute(sfdataframe)), "\'" ))
  return(temp)
}

#' Infinite Value Replacement
#' 
#' Desc
#'
#' @param sfdataframe a wide sf class dataframe produced with the tidycensus get_acs() function
#' 
#' @return Wide Dataframe without the rows populated with only zero-esimtates
inf_replace <- function(sfdataframe) {
  #Args:
  #sfdataframe - accepts a dataframe or SF dataframe, in which to replace all Inf values with NA values.
  
  temp <- as.data.frame(sfdataframe)
  temp[, -which(names(temp) == "geometry")] <- lapply(temp[, -which(names(temp) == "geometry")], function(x) replace (x, is.infinite(x), as.numeric(NA)))
  return(st_as_sf(temp))
}

################################################################################
#################### ANALYSIS Functions ########################################
################################################################################

#' Calculating Neighbors and NeighborWeight Values
#' 
#' Calculates the 
#'
#' @param sfdataframe a wide sf class dataframe produced with the tidycensus get_acs() function
#' @param cents_dataframe desc 
#' @param nb_def desc 
#' @param nb_attrib desc 
#' @param varhead the name of a column indicating the variable to be selected for neighborhood creation - the estimate, MOE, or CV column
#' @param dur desc 
#' 
#' @return DESC
mutate_nbs_wts <- function (sfdataframe, cents_dataframe = FALSE, nb_def = "knear", nb_attrib = FALSE, dur = FALSE, varhead) {
  start <- Sys.time()
  if (varhead == FALSE) {
    varhead <- parent.frame()$x
  }
  if (endsWith(varhead, "CV")) {
    nbhead <- gsub("CV", "NB", varhead)
    wthead <- gsub("CV", "WT", varhead)
  } else if (endsWith(varhead, "E") | endsWith(varhead, "M")) {
    nbhead <- paste0(substr(varhead, 1, nchar(vahread)-1), "NB")
    wthead <- paste0(substr(varhead, 1, nchar(vahread)-1), "WT")
  } else {
    print("Choose a valid varhead value.")
  }
  
  ## KNEAREST
  #Set Number of Neighbors if not given
  if (nb_def == "knear") {
    if (nb_attrib == FALSE) {
      nb_attrib = 30
    }
    if (!is.data.frame(cents_dataframe)) {
      cents_dataframe <- suppressWarnings(st_centroid(sfdataframe)) 
    }
    ##mutate in Neighbors and Weights
    tempdf <- cents_dataframe %>% 
      #filter out any observations with missing values
      filter(!is.na(cents_dataframe[[varhead]])) %>% 
      mutate(
        {{nbhead}} := st_knn(., k = nb_attrib),
        {{wthead}} := st_weights(get(!!nbhead))
      )
    
  } 
  ## QUEENS  
  else if (nb_def == "queens") {
    #filter for NA
    tempdf <- sfdataframe %>% 
      filter(!is.na(sfdataframe[[varhead]]))
    
    #Remove no Neighbor geographies
    no_neighbors <- which(card(poly2nb(tempdf)) == 0)
    if (length(no_neighbors) > 0) {
      print(paste0("Geographies Dropped Because No Neighbors: ", length(no_neighbors)))
      print(as.data.frame(tempdf, drop = TRUE)[no_neighbors, "NAME"])
    }
    tempdf <- tempdf[-no_neighbors,] 
    
    #Mutate Commands
    tempdf <- tempdf %>%
      mutate(
        {{nbhead}} := st_contiguity(geometry),
        {{wthead}} := st_weights(get(!!nbhead))
      )
  } 
  ## ROOKS
  else if (nb_def == "rooks") {
    #filter for NA
    tempdf <- sfdataframe %>% 
      filter(!is.na(sfdataframe[[varhead]]))
    
    #Remove no Neighbor geographies
    no_neighbors <- which(card(poly2nb(tempdf, queen = FALSE)) == 0)
    if (length(no_neighbors) > 0) {
      print(paste0("Geographies Dropped Because No Neighbors: ", length(no_neighbors)))
      print(as.data.frame(tempdf, drop = TRUE)[no_neighbors, "NAME"])
    }
    tempdf <- tempdf[-no_neighbors,] 
    
    #Mutate Commands
    tempdf <- tempdf %>%
      mutate(
        {{nbhead}} := st_contiguity(geometry, queen = FALSE),
        {{wthead}} := st_weights(get(!!nbhead))
      )
  } 
  ## BAND  
  else if (nb_def == "band") {
    #Calculate Centroids if Not Given
    if (!is.data.frame(cents_dataframe)) {
      cents_dataframe <- suppressWarnings(st_centroid(sfdataframe)) 
    }
    #Filter For NA
    
    tempdf <- cents_dataframe %>% 
      filter(!is.na(cents_dataframe[[varhead]]))
    
    #Set Distance Band if Not Given (All geographies get at least one neighbor)
    if (nb_attrib == FALSE) {
      nb_attrib = critical_threshold(cents_dataframe)
      print(paste0("The minimum distance for all geographies to have at least 1 neighbor (calculated from centroids) was found and used: ", as.character(nb_attrib), " KM"))
    } else {
      #Otherwise Remove no-neighbor Geographies
      no_neighbors <- which(card(st_dist_band(tempdf$geometry, lower = 0, upper = nb_attrib)) == 0)
      if (length(no_neighbors) > 0) {
        print(paste0("Geographies Dropped Because No Neighbors: ", length(no_neighbors)))
        print(as.data.frame(tempdf, drop = TRUE)[no_neighbors, "NAME"])
      }
      
      tempdf <- tempdf[-no_neighbors,] 
      
      if (length(tempdf) == 0) {
        stop("No geographies with neighbors. Try a valid band-distance, or allow automatic definition of band-distance.")
      }
      
    }
    #Mutate Commands
    tempdf <- tempdf %>%
      mutate(
        {{nbhead}} := st_dist_band(.$geometry, lower = 0, upper = nb_attrib),
        {{wthead}} := st_weights(get(!!nbhead))
      )
  } 
  ##INVERSE - NOT WORKING
  else if (nb_def == "inverse") {
    #     THIS DOESNT WORK YET
    # if (nb_attrib == FALSE) {
    #   nb_attrib = critical_threshold(cents_dataframe)
    # }
    # tempdf <- sfdataframe %>%
    # filter(!is.na(cents_dataframe[[varhead]])) %>%   
    # mutate(
    #     {{nbhead}} := st_dist_band(geometry, lower = 0, upper = nb_attrib),
    #     {{wthead}} := st_inverse_distance(get(!!nbhead), geometry)
    #   )
    #
  }
  if (dur == TRUE) {print(Sys.time()-start)}
  return(tempdf)
}

#' Calculating Getis-Ord Gi* Values
#' 
#' DESC 
#'
#' @param sfdataframe a wide sf class dataframe produced with the tidycensus get_acs() function, and with valid neighbor and weight columns
#' @param sim_ct desc 
#' @param sig_lev desc 
#' @param varhead the name of a column indicating the variable to be selected for neighborhood creation - the estimate, MOE, or CV column
#' @param dur desc 
#' 
#' @return DESC
mutate_gi <- function(sfdataframe, sim_ct = 999, sig_lev = 0.1, dur = FALSE, varhead) {
  start <- Sys.time()
  if (varhead == FALSE) {
    varhead <- parent.frame()$x
  }
  nbhead <- gsub("CV", "NB", varhead)
  wthead <- gsub("CV", "WT", varhead)
  cathead <- gsub("CV", "CAT", varhead)
  
  gidf <- as.data.frame(sfdataframe, drop = TRUE) %>% 
    # Bring only necessary columns
    select(all_of(c("GEOID", varhead, nbhead, wthead))) %>% 
    # Perform Gi Calculation
    mutate(
      gi = local_g_perm(get(!!varhead), get(!!nbhead), get(!!wthead), nsim = sim_ct)
    ) %>% 
    # Create columns out of nested dataframes
    unnest(gi) %>% 
    # Reclass observations: 0 if not significant, -1 if significant cold spot, 1 if significant hotspot. auto Sig level set to 0.1
    mutate(
      {{cathead}} := ifelse(p_folded_sim > sig_lev, 0, ifelse(gi > 0, 1, -1))
    )
  if (dur == TRUE) {print(Sys.time()-start)}
  return(gidf)
}

#' Re-Merging Gi* Values Calculated with mutate_gi()
#' 
#' DESC 
#'
#' @param left desc
#' @param right desc 
#' @param keep_gi desc 
#' @param keep_psim the name of a column indicating the variable to be selected for neighborhood creation - the estimate, MOE, or CV column
#' @param keep_nb desc 
#' @param keep_wt desc 
#' @param varhead desc 
#' 
#' @return DESC
join_gi <- function(left, right, keep_gi= FALSE, keep_psim = FALSE, keep_nb = FALSE, keep_wt = FALSE, varhead) {
  if (varhead == FALSE) {
    varhead <- parent.frame()$x
  }
  gihead <- gsub("CV", "GI", varhead)
  psimhead <- gsub("CV", "PSIM", varhead)
  nbhead <- gsub("CV", "NB", varhead)
  wthead <- gsub("CV", "WT", varhead)
  cathead <- gsub("CV", "CAT", varhead)
  
  ##RENAME COLS
  names(right)[names(right) == "gi"] <- gihead
  names(right)[names(right) == "p_folded_sim"] <- psimhead
  names(right)[names(right) == "p_folded_sim"] <- nbhead
  names(right)[names(right) == "p_folded_sim"] <- wthead
  ###
  
  # Columns to be rejoined
  joincols <- c("GEOID", cathead)
  
  ## Add cols if Kept by args
  if (keep_gi == TRUE) {
    joincols <- append(joincols, gihead)
  } 
  if (keep_psim == TRUE) {
    joincols <- append(joincols, psimhead)
  }
  if (keep_nb == TRUE) {
    joincols <- append(joincols, nbhead)
  }
  if (keep_wt == TRUE) {
    joincols <- append(joincols, wthead)
  }
  
  # Rejoin Columns to main Dataframe
  output <- left_join(
    left, #DF to join to
    right[, which(names(right) %in% joincols)], #Select only desired columns
    by = join_by(GEOID == GEOID), #column to join by
  )
  return(output)
}

#' Wrapper Function to Run Gi* Analysis
#' 
#' DESC 
#'
#' @param sf_dataframe desc
#' @param nb_def. desc 
#' @param nb_attrib. desc 
#' @param sim_ct. the name of a column indicating the variable to be selected for neighborhood creation - the estimate, MOE, or CV column
#' @param sig_lev. desc 
#' @param keep_nb desc 
#' @param keep_wt desc 
#' @param varhead desc 
#' @param dur desc
#' 
#' @return DESC
calc_gi <- function (sf_dataframe. = df, nb_def. = "knear", nb_attrib. = FALSE, sim_ct. = 999, sig_lev. = 0.1, keep_gi.= FALSE, keep_psim. = FALSE, keep_nb. = FALSE, keep_wt. = FALSE, dur = FALSE)  {
  start <- Sys.time()
  #Setup Dataframes for Regular Geographies and Centroid Geographies (Used depending on neighbor definition)
  sfdf <- sf_dataframe.
  cent_df <- suppressWarnings(st_centroid(sf_dataframe.)) 
  #Create list of columns to iterate through
  cvheads <- names(sfdf)[endsWith(names(sfdf), "CV")]
  
  ##Contstruct list of arguments to pass to neighbor+weight function
  nbwt_arglist <- list(sfdataframe = sfdf, cents_dataframe = cent_df, nb_def = nb_def., nb_attrib = nb_attrib., varhead = FALSE)
  
  ##Note: "Varhead" is used to grab the correct variable, and is defaulted to FALSE in the arglists, so that the individual functions grab the currently iterating column from CVHEADS
  
  #MAIN LOOP
  for (x in cvheads) {
    # CALL NEIGHBOR AND WEIGHTS FUNCTION
    nbwt_df <- do.call(mutate_nbs_wts, nbwt_arglist)
    
    #Construct Argument List for Gi* Function
    gi_arglist <- list(sfdataframe = nbwt_df, sim_ct = sim_ct., sig_lev = sig_lev., varhead = FALSE)
    
    # CALL GI* CALCULATIONS FUNCTION
    gi_df <- do.call(mutate_gi, gi_arglist)
    
    #Construct Argument List for Re-Joining Function
    join_arglist <- list(left = sfdf, right = gi_df, keep_gi = keep_gi., keep_psim = keep_psim., keep_nb = keep_nb., keep_wt = keep_wt., varhead = FALSE)
    
    # CALL REJOINING FUNCTION
    sfdf <- do.call(join_gi, join_arglist)
  }
  # ADD COLUMN FOR TOTAL INDEX VALUE
  index <- as.data.frame(sfdf, drop = TRUE) %>% 
    mutate(
      index_val = rowSums(.[, which(endsWith(names(.), "CAT"))], na.rm = TRUE)
    )
  if (dur == TRUE) {print(Sys.time()-start)}
  #OUTPUT SF DATAFRAME OF INDEX
  return(st_as_sf(index))
}

################################################################################
#################### PLOTTING Functions ########################################
################################################################################
get_us_outline <- function (sfdataframe) {
  outline <- st_union(get_acs(geography = "state", state = state_list, variables = "B19013_001", year = 2022, geometry = TRUE))
}

index_plot <- function (gi_dataframe = index) {
  ggplot(gi_dataframe) +
    geom_sf(aes(fill = index_val), lwd = 0) +
    scale_fill_gradient2(name = "IndexValue",low = "deepskyblue4", mid = "white", high = "darkred") +
    theme_void() +
    coord_sf(crs = st_crs(5070))
}

variable_plot <- function (gi_dataframe = index, outline = border, dur = FALSE) {
  start <- Sys.time()
  gi_df_long <- pivot_longer(gi_dataframe, cols = ends_with("CAT"), names_to = "variable", values_to = "var_values")
  gi_df_long <- gi_df_long %>% 
    filter(., !is.na(var_values))
  varmap <- ggplot() +
    geom_sf(data = outline) +
    geom_sf(data = gi_df_long, aes(fill = var_values), lwd = 0) +
    scale_fill_gradient2(name = "IndexValue",low = "deepskyblue4", mid = "white", high = "darkred") +
    theme_void() +
    coord_sf(crs = st_crs(5070))+
    facet_wrap(vars(variable))
  if (dur == TRUE) {print(Sys.time()-start)}
  return(varmap)
}
