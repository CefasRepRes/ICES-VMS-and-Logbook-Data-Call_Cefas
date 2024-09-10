#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 2: Linking TACSAT and EFLALO data                                       ----
#
#'------------------------------------------------------------------------------

setwd("C:/Users/MD09/OneDrive - CEFAS/projects/datacalls/ices/2024")

source("global-subset.R")



year = 2023

# Looping through the years to submit
for(year in yearsToSubmit){
  print(paste0("Start loop for year ", year))
  
  #'----------------------------------------------------------------------------
  # 2.1.0 load TACSAT and EFLALO data from file                             ----
  #'----------------------------------------------------------------------------
  load(file = paste0(outPath, paste0("/cleanEflalo",year,".RData")) )
  load(file = paste0(outPath, paste0("/cleanTacsat", year,".RData")) )
  
  
  # Assign geometry column to tacsat for later operations
  tacsat$geometry <- NULL
  
  #'----------------------------------------------------------------------------
  # 2.1.1 Merge TACSAT and EFLALO    - ASSSIGN EFLALO details to TACSAT/VMS records     ----
  #'----------------------------------------------------------------------------
 
         #  tacsatp <- mergeEflalo2Tacsat(eflalo,tacsat)
        
  
  ## CEFAS version update
  ## TACSAT data formated previously by CEfas script version provide the information given by the resutl fo function above 
  ## The result of funciton above is TACSAT data with the link to FT_REF in eflalo records. 
  
        tacsatp = tacsat
  

 
  
  ## END of CEFAS UPDATE 
  
  #'----------------------------------------------------------------------------
  # 2.1.2 Assign gear and length                                              ----
  #'----------------------------------------------------------------------------
  # Define the columns to be added
  cols <- c("LE_GEAR", "LE_MSZ", "VE_LEN", "VE_KW", "LE_RECT", "LE_MET", "LE_WIDTH", "VE_FLT", "VE_COU")
  
   
  
  # Use a loop to add each column
  for (col in cols) {
    
  #  col = 'LE_GEAR'
    # Match 'FT_REF' values in 'tacsatp' and 'eflalo' and use these to add the column from 'eflalo' to 'tacsatp'
    tacsatp[[col]] <- eflalo[[col]][match(as.numeric(tacsatp$FT_REF), as.numeric(eflalo$FT_REF))]
  
    
  }
  
  
  tacsatp <- as.data.frame(tacsatp)

  
  # Save not merged tacsat data
  # Subset 'tacsatp' where 'FT_REF' equals 0 (not merged)
  # tacsatp$FT_REF <- tacsatp$SI_FT
  tacsatpmin <- subset(tacsatp, FT_REF == 0)
  
  # Feedback on tacsatpmin
  cat(sprintf("%.2f%% of of the tacsat data did not merge\n", (nrow(tacsatpmin) / (nrow(tacsatpmin) + nrow(tacsatp))) * 100))
  
  # Save 'tacsatpmin' to a file named "tacsatNotMerged<year>.RData" in the 'outPath' directory
  save(
    tacsatpmin,
    file = file.path(outPath, paste0("tacsatNotMerged", year, ".RData"))
  )
  
  # Subset 'tacsatp' where 'FT_REF' does not equal 0 (merged)
  tacsatp <- subset(tacsatp, FT_REF != 0)
  

  
 
  

  #'----------------------------------------------------------------------------
  # 2.1.3 For multi gear/metier etc trips, divide the pings to the right gear/metier etc. ----
  #'----------------------------------------------------------------------------
 
   tacsatp_bk <- tacsatp
  # tacsatp <- tacsatp_bk
  
  
  tacsatpa_LE_GEAR <- trip_assign(tacsatp, eflalo, col = "LE_GEAR", trust_logbook = T)
  

  
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_GEAR$FT_REF,], tacsatpa_LE_GEAR), fill = T)


  tacsatpa_LE_MSZ <- trip_assign(tacsatp, eflalo, col = "LE_MSZ", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MSZ$FT_REF,], tacsatpa_LE_MSZ), fill = T)

  tacsatpa_LE_RECT <- trip_assign(tacsatp, eflalo, col = "LE_RECT", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_RECT$FT_REF,], tacsatpa_LE_RECT), fill = T)

  ## Get teh statistics of VMS position derived ICES RECT vs reported RECTANGLE in LOGBOOKS
  
  tacsatprects  = tacsatp
  
  tacsatprects$LE_RECT_VMS =  ICESrectangle(dF = tacsatp |>  as.data.frame())
  
  tacsatprects |>  filter ( LE_RECT == LE_RECT_VMS) |>  dim ( )
  
  #VMS RECT == LOGBOOK RECT : 1046574             
  
  tacsatprects |>  filter ( LE_RECT != LE_RECT_VMS) |>  dim ( )
  #VMS RECT != LOGBOOK RECT : 645448                
  
  
  645448       / dim(tacsatp)[1]
  #################################################
  
  
  tacsatpa_LE_MET <- trip_assign(tacsatp, eflalo, col = "LE_MET", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MET$FT_REF,], tacsatpa_LE_MET), fill = T)

  dim(tacsatp)


  ###  CEFAS DO NOT HAVE WIDTH OF GEAR INFORMATION
# 
#   if("LE_WIDTH" %in% names(eflalo)){
#     tacsatpa_LE_WIDTH <- trip_assign(tacsatp, eflalo, col = "LE_WIDTH", trust_logbook = T)
#     tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_WIDTH$FT_REF,], tacsatpa_LE_WIDTH), fill = T)
#   }
  

  
  # Save 'tacsatp' to a file named "tacsatMerged<year>.RData" in the 'outPath' directory
  save(
    tacsatp,
    file = file.path(outPath, paste0("tacsatMerged", year, ".RData"))
  )
  
  
  #####################################################################
  ### Identify Fishing Activity  - TACSAT/VMS records as fishing  #####
  #####################################################################
  
  load(file = file.path(outPath, paste0("tacsatMerged", year, ".RData")  ) ) 
  
  ## Load the Fishing Speed Arrays from the AD-HOC speed profile analysis
  
  load(file =   file.path(outPath, paste0("fishing_speed_met5_array_2024.RData")) )
  
  # start by correctly formatting the level 5 metier
  
  tacsatp$LE_L5MET <-  sapply(strsplit(tacsatp$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  
  
  
   
  join_q = join_by(LE_L5MET, between ( SI_SP, min, max) )
  
  tacsatp = tacsatp |>  left_join( fishing_speed_met5_array  , by = join_q )
  
  tacsatp = tacsatp |>  mutate ( SI_STATE = ifelse ( is.na (min) & is.na (max) , 's', 'f')) |>  select( -colnames (speedarr_met5 ))
  
  
  
  ##Get statistics on the number of VMS records fishing /no fishing
  
  tacsatp |>  group_by(SI_STATE )   |>  summarise ( n =  n())
  
  
  
  
  
  
    
  # 2.2 Dispatch landings of merged eflalo at the ping scale
  # ----------------------------------------------------------------------------
    
 
  
  # 2.2.2 Dispatch landings of merged eflalo at the ping scale
  # -------------------------------------------------
  
    
    
  ## CEFAS get teh total kilograms and value landed when data is imported from GEOFISH
  # Get the indices of columns in eflalo that contain "LE_KG_" or "LE_EURO_"
  #   
  # idx_kg <- grep("LE_KG_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_KG_TOT")])
  # idx_euro <- grep("LE_EURO_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_EURO_TOT")])
  # 
  # # Calculate the total KG and EURO for each row
  # if("LE_KG_TOT" %!in% names(eflalo)){
  #   eflalo$LE_KG_TOT <- rowSums(eflalo[, idx_kg], na.rm = TRUE)
  # }
  # if("LE_EURO_TOT" %!in% names(eflalo)){
  #   eflalo$LE_EURO_TOT <- rowSums(eflalo[, idx_euro], na.rm = TRUE)
  # }
  # 
  # eflalo_bk <- eflalo
  # eflalo <- eflalo_bk
  # 
  # # Remove the columns used for the total calculation
  # eflalo <- eflalo[, -c(idx_kg, idx_euro)]
  
  # Split eflalo into two data frames based on the presence of FT_REF in tacsatp
    
  eflaloNM <- subset(eflalo, !FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))

  message(sprintf("%.2f%% of the eflalo data not in tacsat\n", (nrow(eflaloNM) / (nrow(eflaloNM) ))))
  
  
  # Convert SI_STATE to binary (0/1) format
  tacsatp$SI_STATE <- ifelse(tacsatp$SI_STATE == "f", 1, 0)
  
  # Filter rows where SI_STATE is 1
  # tacsatEflalo <- tacsatp[tacsatp$SI_STATE == 1,]
  
  tacsatp <- tacsatp[tacsatp$SI_STATE == 1,]
  tacsatp <- tacsatp[!is.na(tacsatp$INTV),]
  
 
  
     
  
  #Set catch date to be equal to SI_DATE 
  ## THIS IS ONLY A REQUIREMENT FOR RUN SPLITAMONGPINGS2 
  tacsatp$LE_CDAT <- tacsatp$SI_DATE
  tacsatp <- as.data.frame(tacsatp)
  
  # Distribute landings among pings, first by day, metier and trip; then by metier and trip; then by trip
  tacsatEflalo <- splitAmongPings2(tacsatp, eflaloM) # was originally splitAmongPings2(tacsatp, eflalo)
  
 
  
  ## Get the summary of dispatched landing values to VMS records 
  
  
  tot_kg_eflalo = eflalo |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
  tot_kg_eflaloM = eflaloM |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
  tot_kg_eflaloTacsat = tacsatEflalo |> summarise(total = sum ( LE_KG_TOT)) |> pull() 
  summary_landings_tacsatEflalo = data.frame ( ref = c(  "total_landings_kg_eflalo ","total_landings_kg_eflaloM", "total_landings_kg_tacsat_eflalo"  ) , 
               total = c(  tot_kg_eflalo,  tot_kg_eflaloM   ,  tot_kg_eflaloTacsat )   ) |> 
    add_row (  ref = "eflaloM - tacsatEflalo total kg", total =  tot_kg_eflaloM - tot_kg_eflaloTacsat  )
    
    save(
      summary_landings_tacsatEflalo,
      file = file.path(outPath, paste0("summary_landings_tacsatEflalo", year, ".RData"))
    )
    
    
    save(
      tacsatEflalo,
      file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
    )
    
    
    ### IDENTIFY THE EFLALO RECORD WITH NOT RELATED VMS /TACSAT DATA 
  
  
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatEflalo$FT_REF, "Y", "N")


  save(
    eflalo,
    file = file.path(outPath, paste0("/cleanEflalo2", year, ".RData"))
  )
  
  ## SAVE FINAL VERSION OF TACSATP 
  
  save(
    tacsatp,
    file = file.path(outPath, paste0("/tacsatp", year, ".RData"))
  )
  
  
  print("Dispatching landings completed")

  
}






#'------------------------------------------------------------------------------
# 2.3 Add information to tacsatEflalo                                     ----
#'------------------------------------------------------------------------------
# If you already have cleaned tacsatEflalo files elsewhere, 
# change file location below, and make sure data is called tacsatEflalo
# Loop trough years to submit
for(year in yearsToSubmit){
  print(paste0("Start loop for year ",year))
  load(file = paste0(outPath,"tacsatEflalo",year,".RData"))
  
  # 2.3.1 Assign c-square, year, month, quarter, area and create table 1
  # ------------------------------------------------------------------
  # Add habitat and bathymetry to the tacsatEflalo file
  tacsatEflalo <- tacsatEflalo |> 
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI"), remove = F) |> 
    sf::st_set_crs(4326) |> 
    st_join(eusm, join = st_intersects) |> 
    st_join(bathy, join = st_intersects) |> 
    mutate(geometry = NULL) |> 
    data.frame()
  
  # Calculate the c-square based on longitude and latitude
  tacsatEflalo$Csquare <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  
  # Extract the year and month from the date-time
  tacsatEflalo$Year <- year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month <- month(tacsatEflalo$SI_DATIM)
  
  # Calculate the kilowatt-hour and convert interval to hours
  ### CEFAS INTV is already in hours so not need to transform 
  
  tacsatEflalo$kwHour <- tacsatEflalo$VE_KW * tacsatEflalo$INTV 
  tacsatEflalo$INTV <- tacsatEflalo$INTV 
  
  # Add the calculated gear width to each fishing point
  tacsatEflalo$GEARWIDTH <- add_gearwidth(tacsatEflalo)
  
  # Add surface swept area(m2) for each point in the tacsateflalo
  tacsatEflalo$SA_M2 <- add_swept_area(tacsatEflalo)
  
  # Add subsurface swept area(m2) for each point in the tacsateflalo
  tacsatEflalo$SA_M2_subsurface <- add_subsurface_swept_area(tacsatEflalo)
  
  # Check if logical
  tacsatEflalo[,.(min = min(GEARWIDTH), max = max(GEARWIDTH)), by = .(LE_MET)]
  
    
  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo_hab_depth", year, ".RData"))
  )
  

}


# Housekeeping
rm(speedarr, tacsatp, tacsatEflalo,
    eflalo, eflaloM, eflaloNM)


#----------------
# End of file
#----------------
