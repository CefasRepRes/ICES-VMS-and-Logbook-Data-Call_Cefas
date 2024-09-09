library(vmstools)
library(lubridate)
library(doBy)
library(data.table)
library(icesVocab)

# source("C:\\Users\\MD09\\Documents\\git\\ICES-VMS-and-Logbook-Data-Call_Cefas\\0_global.R")
source("global-subset.R")

#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 1: Pre-processing and cleaning TACSAT and EFLALO data                     ----
#
#'------------------------------------------------------------------------------

#'------------------------------------------------------------------------------
# 1.0 Preparations                                                          ----
#'------------------------------------------------------------------------------
# Load the data underlying VMStools    
# data(euharbours); if(substr(R.Version()$os,1,3)== "lin")
# data(harbours)
# data(ICESareas)
# 
# harbours_alt <- 
#   harbours |> 
#   # Convert spelling to ISO
#   dplyr::mutate(harbour = iconv(harbour, from = "latin1", to = "UTF-8")) |> 
#   as_tibble() |> 
#   sf::st_as_sf(coords = c("lon", "lat"),
#                crs = 4326) |> 
#   sf::st_transform(crs = 3857) |> 
#   # the range in harbour is always 3 km
#   sf::st_buffer(dist = 3000) |> 
#   sf::st_transform(crs = 4326) |> 
#   dplyr::select(harbour)




#### MIKE: Try initially only with 2023 data 

year = 2023

# Looping through the years to submit
for(year in yearsToSubmit){
  print(paste0("Start loop for year ",year))
  
  
  #'----------------------------------------------------------------------------
  # 1  load TACSAT and EFLALO data from file                               ----
  #'----------------------------------------------------------------------------
  
  load(
        file.path(
          dataPath,
          paste0("tacsat-eflalo-data-processed.RData" )
        ));
  
  # tacsat_name <-
  #   load(
  #     file.path(
  #       dataPath,
  #       paste0("tacsat_", year, ".RData")
  #     )); #- data is saved as tacsat_2009, tacsat_2010 etc
  # eflalo_name <-
  #   load(
  #     file.path(
  #       dataPath,
  #       paste0("eflalo_", year, ".RData")
  #     )); #- data is saved as eflalo_2009, eflalo_2010 etc
 
  
  ## MIKE: Load the data form GeoFISH source. Do not LOAD as SF , only as plain text 
  ## ONLY VESSELS >= 12 meters
  
 
  
  ## MIKE: Load the data form GeoFISH source. Do not LOAD as SF, only as plain text 
  ## ONLY VESSELS >= 12 meters
  # tacsat <- data.frame(get(tacsat_name)) # rename to tacsat
  # eflalo <- data.frame(get(eflalo_name)) # rename to eflalo
  
  #- Make sure data is in right format
  # tacsat <- formatTacsat(tacsat)
  # eflalo <- formatEflalo(eflalo)
  
  ### IF TOO MUCH ISSUES , MOVE ON.
  
  ### IF TOO MUCH ISSUES , MOVE ON.
  
  
  
  
  
  #'----------------------------------------------------------------------------
  # 2. Clean the EFLALO data --------------------------------------------------
  #'----------------------------------------------------------------------------
  # 1.3.1 Keep track of removed points -----------------------------------------
  remrecsEflalo <-
    matrix(
      NA,
      nrow = 8, ncol = 4,
      dimnames =
        list(
          c("total", "duplicated", "impossible time", "before 1st Jan", "departArrival", "overlappingTrips", "MetierL4_LE_GEAR", "MetierL6_LE_MET") ,
          c("rows", "percentage_rows", "tot_kg", "percentage_tot_kg"))
    )
  
  remrecsEflalo["total",c("rows", "percentage_rows") ] <- c(nrow(eflalo), "100%")
  remrecsEflalo["total",c("tot_kg", "percentage_tot_kg") ] <- c(sum(eflalo$LE_KG_TOT), "100%")
  
  
  # 1.3.2 Warn for outlying catch records --------------------------------------
  # This code is retained to provide an example for how to check for and replace
  # outlying values in catch data (i.e. trips which record catches of individual 
  # species well above the range expected. It is better to thoroughly examine your
  # logbook data and satisfy yourself that you have looked for issues such as this
  # and resolved them, than using an automated function to overwrite data
  #
  # Main script - remove change of outliers - should be checked in the logbook instead.
  # idxkg <- get_indices("", "KG", eflalo)
  # idxeur <- get_indices("", "EURO", eflalo)
  # idxoth <- setdiff(1:ncol(eflalo), c(idxkg, idxeur))
  # eflalo <- eflalo[, c(idxoth, idxkg, idxeur)]
  #
  # specs <- sort(get_species(eflalo))
  # specBounds <- get_bounds(specs, eflalo)
  # specBounds <- cbind(specs, specBounds)
  # specBounds[is.na(specBounds[, 2]), 2] <- "0"
  #
  # idx <- unlist(lapply(specs, function(x) get_indices(x, "KG", eflalo)))
  #
  # eflalo2 <- replace_outliers(eflalo, specBounds, idx)
  # 
  # if(!identical(eflalo, eflalo2)){
  # warning(paste("There are unrealistic landings in the eflalo data, please check f4"))
  # f4 <- generics::setdiff(eflalo, eflalo2)
  # View(f4)
  # }  
  # 
  # 
  # 1.3.3 Remove non-unique trip numbers --------------------------------------
  eflalo_bk <- eflalo
  # Apply the trip ID function to the eflalo data frame
  trip_id <- create_trip_id(eflalo)
  
  # Remove records with non-unique trip identifiers
  eflalo <- eflalo[!duplicated(trip_id), ]
  
  #Check number of TRUE
  duplicated(trip_id) [   duplicated(trip_id)  ==  T ]
  
  # Calculate the number of remaining records and the percentage of records removed
  
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["duplicated", ] <- c(num_records, 100+ percent_removed)
  
  # 1.3.4 Remove impossible time stamp records ---------------------------------
  
  # Apply the convert to date-time function to the FT_DDAT and FT_DTIME columns
  # eflalo$FT_DDATIM <- convert_to_datetime(eflalo$FT_DDAT, eflalo$FT_DTIME)
  # eflalo$FT_DDATIM = ymd_hms(paste(eflalo$FT_DDAT, eflalo$FT_DTIME), tz = "GMT")
  # Apply the function to the FT_LDAT and FT_LTIME columns
  # eflalo$FT_LDATIM <- convert_to_datetime(eflalo$FT_LDAT, eflalo$FT_LTIME)
  # eflalo$FT_LDATIM = ymd_hms(paste(eflalo$FT_LDAT, eflalo$FT_LTIME), tz = "GMT") 
  
  # Remove records with NA in either FT_DDATIM or FT_LDATIM
  eflalo <- eflalo[!is.na(eflalo$FT_DDATIM) & !is.na(eflalo$FT_LDATIM), ]
  
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["impossible time", ] <- c(num_records, 100 + percent_removed)
  
  # 1.3.5 Remove trip starting before 1st Jan ----------------------------------
  
  eflalo_bk <- eflalo
  # eflalo <- eflalo_bk
  # Call the remove before january function with the appropriate arguments
  eflalo <- remove_before_jan(eflalo, year)
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["before 1st Jan", ] <- c(num_records, 100 + percent_removed)
  
  
  # 1.3.6 Remove records with arrival date before departure date  --------------
  
  eflalo_bk <- eflalo ; #  eflalo_bk -> eflalo
  
  
  # Find the indices of rows where 'FT_LDATIM' is greater than or equal to 'FT_DDATIM'
  idx <- which(eflalo$FT_LDATIM >= eflalo$FT_DDATIM)
  
  # Keep only the rows in 'eflalo' where 'FT_LDATIM' is greater than or equal to 'FT_DDATIM'
  eflalo <- eflalo[idx,]
  
  # Calculate the number of rows and the percentage change in the number of rows
  # Store these values in the 'departArrival' row of 'remrecsEflalo'
  
  remrecsEflalo["departArrival", ] <- c(
    nrow(eflalo), # Number of rows in the updated 'eflalo'
    100 + round(
      (nrow(eflalo) - as.numeric(remrecsEflalo["total", 1])) / # Change in number of rows
        as.numeric(remrecsEflalo["total", 1]) * 100, # Relative to the original number of rows
      2) # Rounded to 2 decimal places
  )
  
  
  
  
  # 1.3.7 Remove trip with overlap with another trip ---------------------------
  
  
  # Order 'eflalo' by 'VE_COU', 'VE_REF', 'FT_DDATIM', and 'FT_LDATIM'
  eflalo <- orderBy(~ VE_COU + VE_REF + FT_DDATIM + FT_LDATIM, data = eflalo)
  
  
  # 1.3.7.1 Remove duplicated trips ( same Departure and  Landing dates ) with different FT_REF's
  
  ## This function assigns the first FT_REF to those duplicated
  
  
  # If a trip (same depart and return times) has more than one FT_REF, make them all into the same (first) FT_REF. 
  
  dt1 <- data.table(eflalo)[,.(VE_REF, FT_REF, FT_DDATIM, FT_LDATIM)]
  dt1 <- unique(dt1, by = c("VE_REF", "FT_REF"))
  
  setkey(dt1, VE_REF, FT_DDATIM, FT_LDATIM)
  
  dt2 <- dt1[, ref := .N > 1, by = key(dt1)][ref == T]
  dt3 <- dt2[,.(FT_REF_NEW = FT_REF[1]), by = .(VE_REF, FT_DDATIM, FT_LDATIM)]
  dt4 <- merge(dt2, dt3)
  
  eflalo2 <- merge(data.table(eflalo), dt4, all.x = T) ## by default JOIN both tables based in common columns . In this case FT_REF
  
  ##data table modify the original table with no-assignation symbol. Below statements change teh content directly
  
  eflalo2 = eflalo2[!is.na(FT_REF_NEW), FT_REF := FT_REF_NEW] ## change the columns content based on row conditions
  eflalo2 = eflalo2[, `:=` (FT_REF_NEW = NULL, ref = NULL )  ] ## remove the auxiliary columns
  
  
  ## Assign results to EFLALO as a data.frame
  
  eflalo <- data.frame(eflalo2)
  
  eflalo_bk <- eflalo ; #  eflalo_bk -> eflalo
  
  
  
  # 1.3.7.2 Remove overlapping trips.    Landing date previous of Departure date next trip  
  
  
  
  # 1.3.7.3 Remove overlapping trips.    Landing date previous of Departure date next trip  
  
  
  # 1.3.7.4 Remove overlapping trips.    Landing date previous of Departure date next trip  
  
  eflalo_bk <- eflalo ; #  eflalo_bk -> eflalo
  
  
  
  
  
  ## Use the function for detect and correct overlapping trips 
  ## it return a list with the corrected eflalo [1] and the eflalo trips not resolved [2]
  
  eflalo_list =  overlap_trips ( eflalo )
  eflalo = eflalo_list[[1]] 
  eflalo_overlap = eflalo_list[[2]] 
  eflalo_overlap |> dim()
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["overlappingTrips",] <- c(num_records, (100 + percent_removed))
  
  
   
  
  
  #'----------------------------------------------------------------------------
  # 2.1 METIERS ICES Vocabulary Quality Control ----------------------------------------
  #'----------------------------------------------------------------------------
  #'
  #' Check the fields with related Metier ICES Vocabularies prior the analysis block (2_eflalo_tacsat_analysis.R)
  #' Some functions in this analysis will rise errors if there are  values with  not valid controlled vocabulary 
  #' 
  
  
  ### 1.4.1 Check Metier L4 (Gear) categories are accepted -----------------------
  
  m4_ices         <-  getCodeList("GearType")
  table ( eflalo$LE_GEAR %in%m4_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't
  
  # Get summary  of   DATSU valid/not valid records
  eflalo [ ! eflalo$LE_GEAR %in%m4_ices$Key,]%>%group_by(LE_GEAR)%>%dplyr::select(LE_GEAR)%>%tally()
  
  
  # Correct or dismiss not valid records (if any) and filter only valid ones
  
  
  ## Following previous year approaches. Replace specific UK gear types to valid DATSU gear types:  
  
  ## TBN to OTB: 
  
  eflalo = eflalo %>% mutate(LE_GEAR = ifelse(LE_GEAR == 'TBN', 'OTB', LE_GEAR) )
  
  ## LL to LLS: 
  
  eflalo = eflalo %>% mutate(LE_GEAR = ifelse(LE_GEAR == 'LL', 'LLS', LE_GEAR) )
  
  ## TB to TBB: 
  
  eflalo = eflalo %>% mutate(LE_GEAR = ifelse(LE_GEAR == 'TB', 'TBB', LE_GEAR) )
  
  ## FIX to FPO: 
  
  eflalo = eflalo %>% mutate(LE_GEAR = ifelse(LE_GEAR == 'FIX', 'FPO', LE_GEAR) )
  
  ## PS1 to PS: 
  
  eflalo = eflalo %>% mutate(LE_GEAR = ifelse(LE_GEAR == 'PS1', 'PS', LE_GEAR) )
  
  ##Check again after modifications
  
  table ( eflalo$LE_GEAR %in%m4_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't
  
  # Get summary  of   DATSU valid/not valid records
  eflalo [ ! eflalo$LE_GEAR %in%m4_ices$Key,]%>%group_by(LE_GEAR)%>%dplyr::select(LE_GEAR)%>%tally()
  
  
  eflalo |>  distinct(LE_GEAR  ) |>  arrange(LE_GEAR)
  
  
  eflalo  <-  eflalo%>%filter(LE_GEAR %in% m4_ices$Key)
  
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  # Update the remrecsEflalo data frame with these values
  remrecsEflalo["MetierL4_LE_GEAR", ] <- c(num_records, 100 + percent_removed)
  
  
  
  ### 3.5.5 Check Metier L6 (Fishing Activity) categories are accepted -----------
  
  m6_ices         <-  getCodeList("Metier6_FishingActivity")
  
  
  table ( eflalo$LE_MET %in%m6_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't
  
  # Get summary  of   DATSU valid/not valid records
  eflalo [ ! eflalo$LE_MET  %in%m6_ices$Key,]%>%group_by(LE_MET)%>%dplyr::select(LE_MET)%>%tally()
  
  
  
  # Correct them if any not valid and filter only valid ones
  eflalo = eflalo |>  mutate (LE_MET =  ifelse(is.na ( LE_MET),  'MIS_MIS_0_0_0', LE_MET))
  
  ## Check again all is correct 
  
  eflalo [ ! eflalo$LE_MET  %in%m6_ices$Key,]%>%group_by(LE_MET)%>%dplyr::select(LE_MET)%>%tally()
  
  
  
  eflalo <-  eflalo%>%filter(LE_MET %in% m6_ices$Key)
  
  # Calculate the number of remaining records and the percentage of records removed
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) / as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  
  
  # Add to remrecsEflalo
  remrecsEflalo["MetierL6_LE_MET",] <- c(num_records, 100 + percent_removed)
  
  
  #'----------------------------------------------------------------------------
  # 2.4 SAVE THE EFLALO AND THE QC REMRECSEFLALO FILE --------------------------
  #'----------------------------------------------------------------------------
  
  
  # Save the remrecsEflalo file 
  save(
    remrecsEflalo,
    file = file.path(outPath, paste0("remrecsEflalo", year, ".RData"))
  )
  
  #   Save the cleaned eflalo file 
  save(
    eflalo,
    file = file.path(outPath,paste0("cleanEflalo",year,".RData"))
  )
  message("Cleaning eflalo completed for year ", year)
  print(remrecsEflalo)




  
  
  
  
  
  #'----------------------------------------------------------------------------
  # 3  Clean the TACSAT data                                               ----
  #'----------------------------------------------------------------------------
  # 1.2.0 Keep track of removed points -----------------------------------------
  remrecsTacsat <-
    matrix(
      NA,
      nrow = 8, ncol = 2,
      dimnames =
        list(
          c("total", "outsideICESarea", "duplicates", "notPossible", "pseudoDuplicates", "harbour", "Speed_ISNULL_unreal", "VMSwithEFLALO_records"),
          c("rows", "percentage"))
    )
  
  remrecsTacsat["total", ] <- c(nrow(tacsat), "100%")
  
  # 1.2.1 Remove VMS pings outside the ICES areas ------------------------------
  # # Transform ICESareas and tacsat to sf objects
  # ia <- transform_to_sf(ICESareas, coords = c("SI_LONG", "SI_LATI"))
  # 
  # # Transform tacsat to an sf object
  # tacsat <- transform_to_sf(tacsat, coords = c("SI_LONG", "SI_LATI"))
  # 
  # # Make ia valid and transform it
  # ia <- ia %>%
  #   sf::st_make_valid() %>%
  #   sf::st_transform(4326) |> 
  #   sf::st_zm()
  # 
  # # Find intersections
  # overs <- sf::st_intersects(tacsat, ia)
  # 
  # # See what points fall out the ICES area
  # tacsatx <- tacsat[!(lengths(overs) > 0),]
  # 
  # Filter tacsat
  # tacsat <- tacsat[lengths(overs) > 0,]
  # 
  # # Calculate the percentage of remaining records 
  # percentage_remaining <- round(nrow(tacsat)/as.numeric(remrecsTacsat["total",1])*100,2)
  # 
  # # Update remrecsTacsat
  # remrecsTacsat["outsideICESarea",] <- c(nrow(tacsat), percentage_remaining)
  
  # 1.2.2 Remove duplicate records ---------------------------------------------
  
  tacsat_bk <- tacsat
  
  # Convert SI_DATE and SI_TIME to POSIXct
  # tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  # tacsat$SI_DATIM = ymd_hms(paste(tacsat$SI_DATE ,tacsat$SI_TIME), tz = "GMT")
  
  # Create a unique identifier for each row
  tacsat$unique_id <- paste(tacsat$VE_REF, tacsat$SI_LATI, tacsat$SI_LONG, tacsat$SI_DATIM)
  
  # Remove duplicates based on the unique identifier
  tacsat <- tacsat[!duplicated(tacsat$unique_id), ]
  
  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  remrecsTacsat["duplicates",] <- c(nrow(tacsat), percentage_remaining)
  
  
  # 1.2.3 Remove points that have impossible coordinates -----------------------
  
  # Extract coordinates from tacsat
 
  coords <- tacsat |> dplyr::select ( SI_LONG, SI_LATI )
 
  
  # Check for impossible positions
  
  invalid_positions <- which(coords[,2] > 90 | coords[,2] < -90 | coords[,1] > 180 | coords[,1] < -180)
  
  
  if (length(invalid_positions) > 0) {
    # Print the invalid positions
    print(tacsat[invalid_positions,])
    
    # Remove points with impossible positions
    tacsat <- tacsat[-invalid_positions,]
  }
  
  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  remrecsTacsat["notPossible",] <- c(nrow(tacsat), percentage_remaining)
  
  
  
  
  # 1.2.4 Remove points which are pseudo duplicates as they have an interval rate < x minutes ------------------
  
  # Sort tacsat and calculate intervals
  
  ##======= CEFAS calculate INTERVALS in GEOFISH DATABASE 
  
  #tacsat <- sfsortTacsat(tacsat)
  #tacsat$INTV <- intervalTacsat(as.data.frame(tacsat), level = "vessel", fill.na = TRUE)$INTV
  
  # Remove rows with small intervals
  
  tacsat$intv_mins = tacsat$INTV  * 60 ##convert the INTERVAL in minutes 
  tacsat <- tacsat[tacsat$intv_mins >= intThres, ] ## Get only records larger than thershold defined in GLOBAL0.R
  

  
  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  remrecsTacsat["pseudoDuplicates",] <- c(nrow(tacsat), percentage_remaining)
  
  # Remove intv_mins column from tacsat
  tacsat$intv_mins  = NULL ##delete the temporary field created
  
  # 1.2.4 Remove points with not valid/realistic speed values  ------------------
  
  
  tacsat_si_sp_na = tacsat |>  filter (  is.na(SI_SP))
  
  
  tacsat_si_sp_na |> distinct(IVMS_TRIP)
  tacsat_si_sp_na |>  group_by(IVMS_TRIP) |>  summarise( n= n())
  
  
  tacsat  =  tacsat |>  filter ( ! is.na(SI_SP))
  
  tacsat  =   tacsat |>  filter (  SI_SP <= 30 )
  
  
  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  
  remrecsTacsat["Speed_ISNULL_unreal",] <- c(nrow(tacsat), percentage_remaining)
  
  

  
  
  #'----------------------------------------------------------------------------
  # 3.1  FILTER TACSAT FOR TRIPS AVAILABLE IN CLEAN EFLALO --------------------------
  #'----------------------------------------------------------------------------
  #'
  #' Some trips were removed from the cleaning of eflalo dataset. The TACSAT data cannot be used with their related eflalo records
  #' So the VMS data with not related EFLALO records are removed to follow up the analysis
  
  eflalo_ft_refs =  eflalo |> distinct(FT_REF)
  
  tacsat =  tacsat  %>% filter (FT_REF %in%  eflalo_ft_refs$FT_REF )  
  
  
  
  # Calculate the percentage of remaining records
  percentage_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  
  # Update remrecsTacsat
  remrecsTacsat["VMSwithEFLALO_records",] <- c(nrow(tacsat), percentage_remaining)
  
 
  
  
  
  # 1.2.5 Remove points in harbour ---------------------------------------------
  
  # # Find intersections
  # overs <- sf::st_intersects(tacsat, harbours_alt)
  # 
  # # See what points fall out the ICES area
  # tacsatx <- tacsat[lengths(overs) > 0,]
  # 
  # # Filter tacsat
  # tacsat <- tacsat[!(lengths(overs) > 0),]
  # 
  # # Calculate the percentage of remaining records 
  # percentage_remaining <- round(nrow(tacsat)/as.numeric(remrecsTacsat["total",1])*100,2)
  # 
  # # Update remrecsTacsat
  # remrecsTacsat["harbour",] <- c(nrow(tacsat), percentage_remaining)
  
  #  Save the remrecsTacsat file
  save(
    remrecsTacsat,
    file = file.path(outPath, paste0("remrecsTacsat", year, ".RData"))
  )
  tacsat <- as.data.frame(tacsat)
  # tacsat <- tacsat %>% dplyr::select(-geometry)
  tacsat <- tacsat %>% dplyr::select(-unique_id)
  #  Save the cleaned tacsat file
  
  save(
    tacsat,
    file = file.path(outPath, paste("cleanTacsat", year, ".RData", sep = ""))
  )
  
  message("Cleaning tacsat completed for year ", year)
  print(remrecsTacsat)
  
  


} 
 



# Housekeeping
rm(harbours, harbours_alt, ICESareas, tacsat_name, eflalo_name, tacsat, eflalo, remrecsTacsat, remrecsEflalo,
   ia, overs, tacsatx, coords, invalid_positions, 
   trip_id, percent_removed, num_records, idx, dt1, result, overlapping.trips)
rm(list = ls(pattern = "_20"))


#'------------------------------------------------------------------------------
# End of script                                                             
#'------------------------------------------------------------------------------
