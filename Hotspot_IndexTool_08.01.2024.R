################################################################################
# Author: Alex Broening
# Date: 07.04.2024 - 8.1.2024
# Purpose: This tool allows the user to select a number of variables from the ACS, and construct an index from
#   from those variables for where the clusters of high and low margins of error are. The tool allows the user
#   to select a number of options for how that index and maps are created, as well as for the geographies to be
#   analyzed.
################################################################################

################################################################################
#################### SETUP #####################################################
################################################################################

### Install Packages
install.packages("sfdep")
install.packages("spdep")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("sf")
install.packages("tidycensus")

### Load Libraries
library(tidycensus)
library(sf)
library(tidyverse)
library(dplyr)
library(sfdep)
library(spdep)

### Load Census API Key
## Replace Key with individual key. Available at https://api.census.gov/data/key_signup.html
census_api_key("753915d84691b3657721d57a715b97feafbbf81a")

### Options Settings

## Prevent Scientific Notation in View()
options(scipen = 999)
## Cache Geometries for faster repeated ACS API Queries 
options(tigris_use_cache = TRUE)

################################################################################
#################### PREP ######################################################
################################################################################
### Load possible ACS Variables from which to select
## Replace year with desired end-year, replace dataset with survey to look at
## All dataset options viewable here: https://walker-data.com/tidycensus/reference/load_variables.html
## Suggested ACS datasets: "acs1", "acs5", "acs1/profile", "acs5/profile"

acs_vars <- load_variables(year = 2022, dataset = "acs5")

### View the Table
view(acs_vars)     
                      
### Create Variable List
## Create named vector of strings, from the "name" column of the available variable list
## Name all variables with a reference name of the user's choosing.
## Example given below, to be deleted.

var_list <- c(
  mhi = "B19013_001",
  mhv = "B25077_001"
#  ump = "B27011_008",
#  pov = "B17001_002",
#  bch = "B16010_041",
#  pop = "B01003_001"
  
# Repeat as desired:
#  NAME = "variable name"
)

### Define Statelist Function
## User can filter by desired states to be used in index. Defaults to contiguous US, excluding DC. 
## Other options are: "all", "all_states", "all_pr", "all_dc", "all_dc_pr", "contig_dc"
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
} else if (states == "all_pr") {
  #removes non states, leaving Puerto Rico
  return(all[-which(all %in% c(dc = "11",
                               guam = "66",
                               virginislands = "78",
                               outlying = "74",
                               amsamoa = "60",
                               marianaislands = "69"))])
} else if (states == "all_dc") {
  #removes non states, leaving District of Columbia
  return(all[-which(all %in% c(puertorico = "72",
                               guam = "66",
                               virginislands = "78",
                               outlying = "74",
                               amsamoa = "60",
                               marianaislands = "69"))])
} else if (states == "all_dc_pr") {
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

### Create the State List
## Arg options: "contig" (default), all", "all_states", "all_pr", "all_dc", "all_dc_pr"
state_list <- get_state_list(state = "contig")

################################################################################
#################### Define Functions ##########################################
################################################################################

### API ACS CALL FUNCTION
## Calls the US Census American Community Survey API, with a number of user defined variables, to output a dataframe in wide format
acs_call <- function(yr = 2022, varlist = var_list, states = state_list, level = "tract", surv = "acs5") {
#   Arguments:
#     yr - accepts integers 2010-2022 (updated 07.04.2024), defines the end-year of multi-year surveys
#     varlist - named vector of strings, recalling variables from the ACS
#     states - accepts list of states or states fips codes. Defaults to list defined above with get_state_list()
#     level - accepts string, defining level of geography the ACS estimates should evaluate -
#         Options (small to large): "block group", "tract" (default), "county", "state", "us", "cbsa"
#     surv - accepts string. Defines which dataset to pull from. 
#         Options: "acs1", "acs3", "acs5" (default), for 1, 3, and 5 year estimates

#   Loop through ACS query for each state given - allows for single pull of block group and tract estimates across states
  start <- Sys.time()
  
  df <- map_dfr(states, ~{
    get_acs(
      geography = level,
      variables = varlist,
      survey = surv,
      year = yr,
      state = .x,
      output = "wide",
      geometry = TRUE
    )
  }, .id = "state"
  )

#   Remove "state" placeholder column
  df$state = NULL
  end <- Sys.time()
  print(end - start)
  return(df)
}

### SECONDARY ACS CALL FUNCTION WIHTOUT MAPDFR (UNUSED)
acs_call2 <- function(yr = 2022, varlist = var_list, states = state_list, level = "tract", surv = "acs5") {
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
return(output)
}
}

### CALCULATE COEFFICIENT OF VARIATION FUNCTION
## Takes a wide dataframe created from an ACS API Query (through get_acs() in TidyCensus), 
##   and calculates Coefficient of Variation for each queried variable.
mutate_cv <- function(df_wide) {
#   Arguments:
#     df_wide - Accepts wide dataframe created from ACS API query through TidyCensus. 
#       Estimates columns must end with "E' and Margin of Error Columns must end with "M", as output in wide format by get_acs()

#   Pulls names of Columns
  heads <- names(df_wide)
#   For each MoE column:
  for (x in heads) {
    if (endsWith(x, "M")) {
#     Creates new column name- variable name, ending with "CV"
      newhead <- paste0(substr(x, 1, (nchar(x)-1)),"CV")
#     sets estimatehead equal column name of estimates for variable selected in if statement
      estimatehead <- as.character(paste0(substr(x, 1, nchar(x)-1), "E"))
#     mutates new column with calculated CV, under column name from newhead
      df_wide <- df_wide %>% 
        mutate({{newhead}} := ((get(!!x)/1.645)/get(!!estimatehead)*100))
    }
  } 
# Returns the new dataframe
  return(df_wide)
}

###INITIAL NEIGHBORS WEIGHTS FUNCTION (UNUSED)
mutate_neighbors_weights <- function(sfdataframe = df, nb_def = "knear", nb_attrib = FALSE) {
#   Mutates in two new variable to the input sfdataframe, adding a column for neighbors list and a column for spatial weights. 

# Args
    # sfdataframe - accepts an SF Dataframe, containing the geometries of the Census geographies that form the neighborlist
    # nd_def - accepts a string, determing the neighborhood definition. 
    #   Options: "knear" - K-nearest neighbors (takes nb_attribute to define k)
    #             "queens" - Queens Contiguity neighbors
    #             "rooks" - Rooks contiguity neighbors
    #             "band" - All neighbors within a given distance (takes nb_attribute to define given distance)
    #             "inverse" - directly creates weights value relative to distance from geography (takes nb_attribute to define cutoff distance)
    #              ^^ DOES NOT WORK AT MOMENT 07.11.2022
    # nb_attrib - accepts a numeric value, defining an additional attribute used in creating neighbors and weights - see nd_def options
  
  if (nb_def == "knear") {
#   Set Default number of nearest neighbors: 30 
    if (nb_attrib ==  FALSE) {
      nb_attrib = 30
    }
#   Mutate neighbors list    
    sfdataframe <- sfdataframe %>% 
      mutate(
        nb = st_knn(st_centroid(sfdataframe), k = nb_attrib),
        wt = st_weights(nb))
    return(sfdataframe)
  } else if (nb_def == "queens") {
    sfdataframe <- sfdataframe %>% 
      mutate(
        nb = st_contiguity(geometry),
        wt = st_weights(nb)
      )
    return(sfdataframe)
    
  } else if (nb_def == "rooks") {
    sfdataframe <- sfdataframe %>% 
      mutate(
        nb = st_contiguity(geometry, queen = FALSE),
        wt = st_weights(nb)
      )   
    return(sfdataframe)
    
  } else if (nb_def == "band") {
    if (nb_attrib == FALSE) {
      #pulls minimum threshold, where all observations have at least one neighbor
      nb_attrib = critical_threshold(st_centroid(sfdataframe))
      print(paste0("The minimum distance for all geographies to have at least 1 neighbor (calculated from centroids) was found and used: ", as.character(nb_attrib), " KM"))
    }
    sfdataframe <- sfdataframe %>% 
      mutate(
        nb = st_dist_band(geometry, lower = 0, upper = nb_attrib),
        wt = st_weights(nb)
      )
    return(sfdataframe)
    
  } else if (nb_def == "inverse") {
#     THIS DOESNT WORK YET
    # centroids <- st_centroid(sfdataframe)
    # if (nb_attrib == FALSE) {
    #   nb_attrib = critical_threshold(centroids)
    # }
    # sfdataframe <- sfdataframe %>% 
    #   mutate(
    #     nb = st_dist_band(geometry, lower = 0, upper = nb_attrib),
    #     wt = st_inverse_distance(nb, centroids$geometry)
    #   )
    # 
    return(NULL)
  }
}

### FUNCTION TO REMOVE PROBLEMATIC OBSERVATIONS
# Removes all rows of a dataframe or SF dataframe resulting from a TidyCensus query, in which all estimate values are 0, indicating a structural problem with the geography
# Additionally, prints the number of cases dropped.
drop_zeroes <- function(sfdataframe) {
#ARGS:
  #sfdataframe - accepts a dataframe or SF dataframe, from which to drop the 0-estimate rows.
  
  temp <- as.data.frame(sfdataframe, drop = TRUE)
  og_length <- nrow(temp)
  temp <- st_as_sf(subset(temp, rowSums(temp[, endsWith(names(temp), "E") & names(temp) != "NAME"] != 0, na.rm = TRUE) != 0))
  new_length <- nrow(temp)
  print(paste0(as.character(og_length - new_length), " rows were dropped from the dataframe ", deparse(substitute(dataframe)) ))
  return(temp)
}

### FUNCTION TO REPLACE INF Vals WITH NAs
## Allows for consistent NA handling by the later calculations
## Replaces all Inf Values in an SF dataframe with NA values instead.
inf_replace <- function(sfdataframe) {
#Args:
  #sfdataframe - accepts a dataframe or SF dataframe, in which to replace all Inf values with NA values.
  
  temp <- as.data.frame(sfdataframe)
  temp[, -which(names(temp) == "geometry")] <- lapply(temp[, -which(names(temp) == "geometry")], function(x) replace (x, is.infinite(x), as.numeric(NA)))
  return(st_as_sf(temp))
}

### FUNCTION TO CALCULATE SPATIAL LAGS (UNUSED)
mutate_lags <- function(sfdataframe = df) {
  temp <- sfdataframe
  heads <- names(temp)
  newhead <- FALSE
  for (x in heads) {
    if (endsWith(x, "CV")) {
      print(x)  
      cvcol <- x
      newhead <- gsub("CV", "LAG", as.character(x))
      print(temp[[x]])
      # temp %>%
      #   print(temp[[x]])
#        mutate({{newhead}} := st_lag(temp[[x]], nb, wt))
    }
  }
  return(temp)
}


### FUNCTION TO CALCULATE NEIGHBORHOOD LISTS, NEIGHBOR WEIGHTS, AND GI STATS
## Calculates and merges in gi values and classes for each valid observation-acs_var combination
## See comments below to change how many intermediate calculations are returned in the output SF Dataframe
mutate_nbs_wts_gi <- function(sfdataframe = df, nb_def = "knear", nb_attrib = FALSE, sim_ct = 999, sig_lev = 0.1) {
  #Setup
  sfdf <- sfdataframe
  heads <- names(sfdf)
  cvheads <- heads[endsWith(heads, "CV")]

### MAIN LOOP FOR EACH ACS VAR
  for (x in cvheads) {
    ## DEFINE NEW VARIABLE HEADS
    cvhead <- x
    mhead <- gsub("CV", "M", x)
    ehead <- gsub("CV", "E", x)
    nbhead <- gsub("CV", "NB", x)
    wthead <- gsub("CV", "WT", x)
    gihead <- gsub("CV", "GI", x)
    psimhead <- gsub("CV", "PSIM", x)
    cathead <- gsub("CV", "CAT", x)
    #COPY DF TO RUN NB AND WT CALCS
    tempdf <- select(sfdf, all_of(c("GEOID", cvhead)))
    ##TEST what came over
    print(names(tempdf))
    
    ## for now just Knearest
    if (nb_def == "knear") {
      if (nb_attrib == FALSE) {
        nb_attrib = 30
      }
      ##mutate in Neighbors and Weights
      tempdf <- tempdf %>% 
        #filter out any observations with missing values
        filter(!is.na(tempdf[[x]])) %>% 
        mutate(
          ##am i able to optimize centroids to avoid recalc? prob - create two dfs, one with centroids one with full shapes at top of function
          {{nbhead}} := st_knn(st_centroid(.), k = nb_attrib),
          {{wthead}} := st_weights(get(!!nbhead))
        )
    }
    
    ##NEW DF FOR GI OUTPUT
    gidf <- as.data.frame(tempdf, drop = TRUE) %>% 
      # Bring only necessary columns
      select(all_of(c("GEOID", cvhead, nbhead, wthead))) %>% 
      # Perform Gi Calculation
      mutate(
        gi = local_g_perm(get(!!cvhead), get(!!nbhead), get(!!wthead), nsim = sim_ct)
      ) %>% 
      # Create columns out of nested dataframes
      unnest(gi) %>% 
      # Reclass observations: 0 if not significant, -1 if significant cold spot, 1 if significant hotspot. auto Sig level set to 0.1
      mutate(
        {{cathead}} := ifelse(p_folded_sim > sig_lev, 0, ifelse(gi > 0, 1, -1))
      )

    # COMMENT if the Gi and P vals are not desired in output table
    ###
    names(gidf)[names(gidf) == "gi"] <- gihead
    names(gidf)[names(gidf) == "p_folded_sim"] <- psimhead
    ###
    
    # Choose Columns to be rejoined
    joincols <- c("GEOID", gihead, psimhead, cathead)
    
    # Rejoin Columns to main Dataframe
    sfdf <- left_join(
      sfdf, #DF to join to
      gidf[, which(names(gidf) %in% joincols)], #Select only desired columns
      by = join_by(GEOID == GEOID), #column to join by
    )
  }
  return(sfdf)
}


### FUNCTION TO CALCULATE FINAL INDEX VALUE FROM GI CLASSES
## Takes Dataframe produced by mutate_nbs_wts_gi()
index_calc <- function(sfdataframe = df) {
  index <- as.data.frame(sfdataframe, drop = TRUE) %>% 
    mutate(
      index_val = rowSums(.[, which(endsWith(names(.), "CAT"))], na.rm = TRUE)
    )
  return(st_as_sf(index))
}

##TEMPORARY CALL FUCNTION
calc_index <- function () {
  start <- Sys.time()
  df <- acs_call()
  df <- drop_zeroes(df)
  df <- mutate_cv(df)
  df <- inf_replace(df)
  df <- mutate_nbs_wts_gi(sfdataframe = df)
  df <- index_calc()
  end <- Sys.time()
  print(paste0("calc_index() took a runtime of: ", start-end))
  return(df)
}

################################################################################
#################### CALL FUNCTIONS ############################################
################################################################################
state_list <- c("51", "01")
state_list <- get_state_list()
df <- calc_index()

ggplot(df) +
  geom_sf(aes(fill = index_val), color = "black", lwd = 0.05) +
  scale_fill_gradient2(name = "IndexValue",low = "deepskyblue4", mid = "white", high = "darkred") +
  theme_void()
# 
#     ###SET NB ATTRIBUTE
#     if (nb_def == "knear") {
#       ###KNEAREST NEIGHBORS DEFAULT TO 30 - CAN OPTIMIZE AND REMOVE FROM FOR LOOP
#       if (nb_attrib == FALSE) {
#         nb_attrib = 30
#       }
#     } else if (nb_def == "band") {
#       ###DISTANCE BAND DEFAULT TO CRITICAL THRESHOLD CENTROIDS
#       if (nb_attrib == FALSE) {
#         nb_attrib = critical_threshold(temp_cent)
#         print(paste0("The minimum distance for all geographies to have at least 1 neighbor (calculated from centroids) was found and used: ", as.character(nb_attrib), " KM"))
#       }
#     } else if (nb_def == "inverse") {
#       ### CUTOFF INVERSE BAND DEFAULT TO CRITICAL THRESHOLD CENTROIDS - SHOULD CHANGE
#       ### UNFINISHED ###
#       if (nb_attrib == FALSE) {
#         nb_attrib = critical_threshold(temp_cent)
#         print(paste0("The minimum distance for all geographies to have at least 1 neighbor (calculated from centroids) was found and used: ", as.character(nb_attrib), " KM"))
#       }
#     }
#     print(nb_attrib)
    
    
    
    

### MUTATE    
# 
#    if (nb_def == "knear") {
#       #   Mutate neighbors list
#       temp <- temp %>%
#         mutate(
#           {{nbhead}} := st_knn(temp_cent, k = nb_attrib),
#           {{wthead}} := st_weights(get(!!nbhead))
#           )
#       return(temp)
# 
#     } else if (nb_def == "queens") {
#       sfdataframe <- sfdataframe %>%
#         mutate(
#           nb = st_contiguity(geometry),
#           wt = st_weights(nb)
#         )
#       return(sfdataframe)
# 
#     } else if (nb_def == "rooks") {
#       sfdataframe <- sfdataframe %>%
#         mutate(
#           nb = st_contiguity(geometry, queen = FALSE),
#           wt = st_weights(nb)
#         )
#       return(sfdataframe)
# 
#     } else if (nb_def == "band") {
#       sfdataframe <- sfdataframe %>%
#         mutate(
#           nb = st_dist_band(geometry, lower = 0, upper = nb_attrib),
#           wt = st_weights(nb)
#         )
#       return(sfdataframe)
# 
#     } else if (nb_def == "inverse") {
#       #     THIS DOESNT WORK YET
#       # centroids <- st_centroid(sfdataframe)
#       # if (nb_attrib == FALSE) {
#       #   nb_attrib = critical_threshold(centroids)
#       # }
#       # sfdataframe <- sfdataframe %>%
#       #   mutate(
#       #     nb = st_dist_band(geometry, lower = 0, upper = nb_attrib),
#       #     wt = st_inverse_distance(nb, centroids$geometry)
#       #   )
#       #
#       return(NULL)
#     }
  }
}
###FUNCTION PSEUDOCODE
  
  #select CV columns
  #for item in CV Column
    #select all valid rows
      #if queens
        #mutate queens nb/wt
      #if rooks
        #mutate rooks nb/wt
      #if knear
        #mutate knear nb/wt
      #if band
        #mutate band nb/wt
      #if inverse
        #mutate inverse nb/wt

      #Mutate LAG
      #Mutate Gi*

      #add to col list
                      


################################################################################
### DEFINE LIST OF VARIABLES TO PULL
################################################################################
varlist <- c(
  name = "variable"
)
# acs pull, at user defined level, user defined area, user defined variables, user defined year

#remove state column - meaningless

# Remove all rows with a zero estimate

###DONT NEED TO CALCULATE LOW AND HIGH

# Calculate CV for each pulled variable


# calculate spatial weights
  #User defined rule? user defined k val?
  # big function with ifelse for the different neighborhood def functions

#global g

#local g function 
  args:
  
    number of sims
    variable list <- from acs pull, need to adjust names with patterns to match CV names
    neighborhood defintion - takes a string to plug into if statement - "queens" "knear" "rooks" "inverse"
    extra neighbor val - defaults to FALSE, used for k nearest, distance band, etc
    dataframe
    



#local g

  #add neighbors to index
  #add weights to index
  #add spatial lag to index

#FOR each variable:

  #add GI column to index
    #user defined n of sims
  #unnest gi

#reclassify function - user defined thresholds for reliability - takes list of limits and list of names.
    automatically adds insig. 
    
    reclassifies, factors, labels with list of names
    
#New columns per variable to reclass hot spot...
    assigns each tract 1, 0, -1 for hot/cold/no spot
  
#creates index col
    rowsums of new columns\
    