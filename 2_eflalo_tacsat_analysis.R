#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 2: Linking TACSAT and EFLALO data                                       ----
#
#'------------------------------------------------------------------------------

setwd("C:/Users/MD09/OneDrive - CEFAS/projects/datacalls/ices/2024")

source("C:\\Users\\MD09\\Documents\\git\\ICES-VMS-and-Logbook-Data-Call_Cefas\\global-subset.R")



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
  # 2.1.1 Merge TACSAT and EFLALO                                             ----
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

  dim(tacsatpa_LE_GEAR)
  dim(tacsatp)



  tacsatp %>% filter(is.na(LE_GEAR)) %>% tally()

  tacsatp %>%  filter( FT_REF == '610917780')

  eflalo %>% filter ( FT_REF == 'c')

  tacsatp$FT_REF <- as.numeric(tacsatp$FT_REF)
  eflalo$FT_REF <- as.numeric(eflalo$FT_REF)


  tacsatpa_LE_MSZ <- trip_assign(tacsatp, eflalo, col = "LE_MSZ", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MSZ$FT_REF,], tacsatpa_LE_MSZ), fill = T)

  tacsatpa_LE_RECT <- trip_assign(tacsatp, eflalo, col = "LE_RECT", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_RECT$FT_REF,], tacsatpa_LE_RECT), fill = T)

  tacsatpa_LE_MET <- trip_assign(tacsatp, eflalo, col = "LE_MET", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MET$FT_REF,], tacsatpa_LE_MET), fill = T)


  tacsatp %>% filter (is.na( LE_GEAR ) )   %>% left_join(eflalo %>% rename(eLE_GEAR = LE_GEAR), by = "FT_REF" ) %>%
    mutate (   LE_GEAR  = eLE_GEAR )


  [tacsatp$FT_REF %!in% tacsatpa_LE_GEAR$FT_REF, LE_GEAR := ]


  tacsatp$LE_GEAR     = eflalo$LE_GEAR    [ match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_MSZ      = eflalo$LE_MSZ     [ match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_RECT     = eflalo$LE_RECT    [ match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_MET      = eflalo$LE_MET     [ match(tacsatp$FT_REF, eflalo$FT_REF)]

  if("LE_WIDTH" %in% names(eflalo)){
    tacsatpa_LE_WIDTH <- trip_assign(tacsatp, eflalo, col = "LE_WIDTH", trust_logbook = T)
    tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_WIDTH$FT_REF,], tacsatpa_LE_WIDTH), fill = T)
  }
  
  #Set catch date to be equal to SI_DATE 
  ## THIS IS ONLY A REQUIREMENT FOR RUN SPLITAMONGPINGS2 
  tacsatp$LE_CDAT <- tacsatp$SI_DATE
  
  tacsatp <- as.data.frame(tacsatp)
  
  # Save 'tacsatp' to a file named "tacsatMerged<year>.RData" in the 'outPath' directory
  save(
    tacsatp,
    file = file.path(outPath, paste0("tacsatMerged", year, ".RData"))
  )
  
  
  
  
  
  
  
    
  # 2.2 Dispatch landings of merged eflalo at the ping scale
  # -------------------------------------------------
    
  #'----------------------------------------------------------------------------
  # 2.2.1 continued, filter out invalid metier level 6 codes                           
  #'----------------------------------------------------------------------------
    kept <- nrow(tacsatp)
    removed <- nrow(tacsatp %>% filter(LE_MET %!in% valid_metiers))
    tacsatp <- tacsatp %>% filter(LE_MET %in% valid_metiers)
    cat(sprintf("%.2f%% of of the tacsatp removed due to invalid metier l6 \n", (removed / (removed + kept) * 100)))
  
  # 2.2.2 Dispatch landings of merged eflalo at the ping scale
  # -------------------------------------------------
  
  # Get the indices of columns in eflalo that contain "LE_KG_" or "LE_EURO_"
  idx_kg <- grep("LE_KG_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_KG_TOT")])
  idx_euro <- grep("LE_EURO_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_EURO_TOT")])
  
  # Calculate the total KG and EURO for each row
  if("LE_KG_TOT" %!in% names(eflalo)){
    eflalo$LE_KG_TOT <- rowSums(eflalo[, idx_kg], na.rm = TRUE)
  }
  if("LE_EURO_TOT" %!in% names(eflalo)){
    eflalo$LE_EURO_TOT <- rowSums(eflalo[, idx_euro], na.rm = TRUE)
  }
  
  eflalo_bk <- eflalo
  eflalo <- eflalo_bk
  
  # Remove the columns used for the total calculation
  eflalo <- eflalo[, -c(idx_kg, idx_euro)]
  
  # Split eflalo into two data frames based on the presence of FT_REF in tacsatp
  eflaloNM <- subset(eflalo, !FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))

  message(sprintf("%.2f%% of the eflalo data not in tacsat\n", (nrow(eflaloNM) / (nrow(eflaloNM) ))))
  
  
  tacsatp$SI_STATE <- ifelse(tacsatp$SI_SP >1 & tacsatp$SI_SP <=6, 'f', 's')
  
  tacsatp %>% filter(SI_STATE == 'f') %>% tally()
    
  # Convert SI_STATE to binary (0/1) format
  tacsatp$SI_STATE <- ifelse(tacsatp$SI_STATE == "f", 1, 0)
  
  # Filter rows where SI_STATE is 1
  # tacsatEflalo <- tacsatp[tacsatp$SI_STATE == 1,]
  tacsatp <- tacsatp[tacsatp$SI_STATE == 1,]
  
  tacsatp <- tacsatp[!is.na(tacsatp$INTV),]
  
  typeof(tacsatp$SI_DATE)
  typeof(eflalo$LE_CDAT)
  
  eflalo$LE_CDAT <- as.character(eflalo$LE_CDAT)
  
  # Distribute landings among pings, first by day, metier and trip; then by metier and trip; then by trip
  tacsatEflalo <- splitAmongPings2(tacsatp, eflalo) # was originally splitAmongPings2(tacsatp, eflalo)
  
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatEflalo$FT_REF, "Y", "N")

  
  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )

  save(
    eflalo,
    file = file.path(outPath, paste0("/cleanEflalo2", year, ".RData"))
  )
  
  save(
    tacsatp,
    file = file.path(outPath, paste0("/tacsatp", year, ".RData"))
  )
  
  
  print("Dispatching landings completed")
  
  
  
  print("")
  
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
  tacsatEflalo$kwHour <- tacsatEflalo$VE_KW * tacsatEflalo$INTV / 60
  tacsatEflalo$INTV <- tacsatEflalo$INTV / 60
  
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
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )
  

}


# Housekeeping
rm(speedarr, tacsatp, tacsatEflalo,
    eflalo, eflaloM, eflaloNM)


#----------------
# End of file
#----------------
