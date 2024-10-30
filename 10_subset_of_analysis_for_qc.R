#'------------------------------------------------------------------------------
#
# Script to extract and process VMS and logbook data for ICES VMS data call
# 2: Linking TACSAT and EFLALO data                                       ----
#
#'------------------------------------------------------------------------------
library(dplyr)
library(vmstools)

setwd("C:/Users/MD09/OneDrive - CEFAS/projects/datacalls/ices/2024")

source("global-subset.R")

yearsToSubmit = 2009:2023

year = 2023

# Looping through the years to submit
for(year in yearsToSubmit){
  
  
  tacsat_raw <-
    load(
      file.path(
        dataPath,
        paste0("tacsat-", year, ".RData")
      )); #- data is saved as tacsat_2009, tacsat_2010 etc
  
  eflalo_raw <-
    load(
      file.path(
        dataPath,
        paste0("eflalo-", year, ".RData") )  ) 
  
  
  print(paste0("Start loop for year ", year))
  
  #'----------------------------------------------------------------------------
  # 2.1.0 load TACSAT and EFLALO data from file                             ----
  #'----------------------------------------------------------------------------
 # load(file = paste0(outPath, paste0("/cleanEflalo",year,".RData")) )
 
   
 
  
  # Save 'tacsatp' to a file named "tacsatMerged<year>.RData" in the 'outPath' directory
  load( 
    file = file.path(outPath, paste0("tacsatMerged", year, ".RData"))
  )
  
  
  
  tacsatprects  = tacsatp
  
  tacsatprects$LE_RECT_VMS =  ICESrectangle(dF = tacsatp |>  as.data.frame())
  
  tacsat_ICES_rect_coincidence = tacsatprects |>  filter ( LE_RECT == LE_RECT_VMS) |>  dim ( ) 
  
  #VMS RECT == LOGBOOK RECT : 1046574             
  
  tacsat_ICES_rect_not_coincidence = tacsatprects |>  filter ( LE_RECT != LE_RECT_VMS) |>  dim ( )
  #VMS RECT != LOGBOOK RECT : 645448      
  

  
  print(paste0("finish QC1 for", year))
  
  
  #####################################################################
  ### Identify Fishing Activity  - TACSAT/VMS records as fishing  #####
  #####################################################################
  
 
  # start by correctly formatting the level 5 metier
  
  tacsatp$LE_L5MET <-  sapply(strsplit(tacsatp$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  
  
  
  ## Join the TACSAT data with the speed array ranges to identify fishing VMS records ('f') and not fishing records ( steaming, 's')
  
  
  join_q = join_by(LE_L5MET, between ( SI_SP, min, max) )
  tacsatp = tacsatp |>  left_join( fishing_speed_met5_array  , by = join_q )
  
  tacsatp = tacsatp |>  mutate ( SI_STATE = ifelse ( is.na (min) & is.na (max) , 's', 'f')) |>  dplyr::select( -colnames (fishing_speed_met5_array ))
  
  
  
  
  
  #-------------------------------------------------------------------------------------------------
  ## QC 2:   Get statistics on the number of VMS records fishing /no fishing
  #-------------------------------------------------------------------------------------------------
  
  tacsatp_fishing_steaming =  tacsatp |>  group_by(SI_STATE )   |>  summarise ( n =  n())
  
  
  save(
    tacsatp_fishing_steaming,
    file = file.path(outPath, paste0("qc2_tacsatp_fishing_steaming_", year, ".RData"))
  )
  
  
  print(paste0("finish QC2 for", year))
  
  
  
  ##############################################################################
  #### QC 1.1  Get teh ICES rectangles coincidence after select only fishing ##############
  
  tacsatp_fishing  = tacsatp |>  filter( SI_STATE == 'f' )
  tacsatp_fishing$LE_RECT_VMS =  ICESrectangle(dF = tacsatp_fishing |>  as.data.frame())
  
  tacsat_f_ICES_rect_coincidence = tacsatp_fishing |>  filter ( LE_RECT == LE_RECT_VMS) |>  dim ( ) 
  
  #VMS RECT == LOGBOOK RECT : 1046574             
  
  tacsat_f_ICES_rect_not_coincidence = tacsatp_fishing |>  filter ( LE_RECT != LE_RECT_VMS) |>  dim ( )
  
  tacsat_eflalo_ICES_R_report =  data.frame(ref = c ("VMS records with LB coincidence ICES R.",
                                                     "VMS records with LB not coincidence ICES R.",
                                                     "VMS Fishing records with LB coincidence ICES R.",
                                                     "VMS Fishing records with LB not coincidence ICES R."), 
                                            records = c( tacsat_ICES_rect_coincidence[1], 
                                                         tacsat_ICES_rect_not_coincidence[1],
                                                         tacsat_f_ICES_rect_coincidence[1], 
                                                         tacsat_f_ICES_rect_not_coincidence[1])) |> 
    add_row(ref = "propotion of coincidence / total" ,  
            records =  round (( tacsat_ICES_rect_coincidence[1] / ( tacsat_ICES_rect_not_coincidence[1] + tacsat_ICES_rect_coincidence[1])  ), 2)   * 100    ) |> 
    add_row(ref = "propotion of fishing coincidence / total" ,  
            records =  round (( tacsat_f_ICES_rect_coincidence[1] / ( tacsat_f_ICES_rect_not_coincidence[1] + tacsat_f_ICES_rect_coincidence[1])  ), 2)   * 100    )
  
  
  
  save(
    tacsat_eflalo_ICES_R_report,
    file = file.path(outPath, paste0("qc1_tacsat_eflalo_ICES_R_report", year, ".RData"))
  )
  
  
  
  
}







  
  #QC2 END -------------------------------------------------------------------------------------------------------------
  
  # Looping through the years to submit
  for(year in yearsToSubmit){
    
    
    print(paste0("Start loop for year ", year))
    
    # Save 'tacsatp' to a file named "tacsatMerged<year>.RData" in the 'outPath' directory
    load( 
      file = file.path(outPath, paste0("tacsatMerged", year, ".RData"))
    )
    
  
  load( 
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )
  
  load(file = paste0(outPath, paste0("/cleanEflalo",year,".RData")) )
  
  
  eflaloM <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
  
   
  
  #-------------------------------------------------------------------------------------------------
  ## QC 3:  Get the summary of dispatched landing values to VMS records 
  #-------------------------------------------------------------------------------------------------
  
  
  tot_kg_eflalo = eflalo |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
  tot_kg_eflaloM = eflaloM |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
  tot_kg_eflaloTacsat = tacsatEflalo |> summarise(total = sum ( LE_KG_TOT)) |> pull() 
  summary_landings_tacsatEflalo = data.frame ( ref = c(  "total_landings_kg_eflalo","total_landings_kg_eflaloM", "total_landings_kg_tacsat_eflalo"  ) , 
                                               total = c(  tot_kg_eflalo,  tot_kg_eflaloM   ,  tot_kg_eflaloTacsat )   ) |> 
    add_row (  ref = "eflaloM - tacsatEflalo total kg", total =  tot_kg_eflaloM - tot_kg_eflaloTacsat  )
  
  save(
    summary_landings_tacsatEflalo,
    file = file.path(outPath, paste0("qc3_summary_landings_tacsatEflalo", year, ".RData"))
  )
  
  print(paste0("finish QC3 for", year))
  
  #QC3 END -------------------------------------------------------------------------------------------------------------
  
 
  
  }
  
  ##########################################################
  ###Analysis of the EFLALO/TACSAT with SplitAmongPings versions
  ##############################################################
  

##########################################################
###  SplitAmongPings2 ICES WG 2024   #####################
##############################################################

yearsToSubmit = 2009:2023



## Load the Fishing Speed Arrays from the AD-HOC speed profile analysis
fish_array = "C:\\Users\\RM12\\OneDrive - CEFAS\\Roi\\projects\\datacalls\\ices\\march_2024\\ICES-VMS-and-Logbook-Data-Call_Cefas\\Results"
load(file =   file.path(fish_array, paste0("fishing_speed_met5_array_2024.RData")) )


# Looping through the years to submit
for(year in yearsToSubmit){
  

  print(year)
  #####################################################################
  ### Identify Fishing Activity  - TACSAT/VMS records as fishing  #####
  #####################################################################
  
  
  load(file = paste0(outPath, paste0("/cleanEflalo",year,".RData")) )
  load(file = file.path(outPath, paste0("tacsatMerged", year, ".RData")  ) ) 
  
  
  # start by correctly formatting the level 5 metier
  
  tacsatp$LE_L5MET <-  sapply(strsplit(tacsatp$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  
  
  
  ## Join the TACSAT data with the speed array ranges to identify fishing VMS records ('f') and not fishing records ( steaming, 's')
  
  
  join_q = join_by(LE_L5MET, between ( SI_SP, min, max) )
  tacsatp = tacsatp |>  left_join( fishing_speed_met5_array  , by = join_q )

  tacsatp = tacsatp |>  mutate ( SI_STATE = ifelse ( is.na (min) & is.na (max) , 's', 'f')) |>  select( -colnames (fishing_speed_met5_array ))
  
  
 
  
  
  
  
  # 2.2 Dispatch landings of merged eflalo at the ping scale
  # ----------------------------------------------------------------------------
  
  
  
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
  
  ## Remove the LE_RECT column in tacsatp that was assigned form logbook 
  
  tacsatp$LE_RECT = NULL
  
  
  
  
  
  #Set catch date to be equal to SI_DATE 
  ## THIS IS ONLY A REQUIREMENT FOR RUN SPLITAMONGPINGS2 
  
  tacsatp <- as.data.frame(tacsatp)
  
 
  
  # Distribute landings among pings, first by day, metier and trip; then by metier and trip; then by trip
  tacsatEflalo <- splitAmongPings2(tacsatp, eflaloM) # was originally splitAmongPings2(tacsatp, eflalo)
 
  
  tacsatEflalo_res = tacsatEflalo[[1]]
  save(
    tacsatEflalo_res,
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )
  
  
  if ( year == yearsToSubmit[1]) { 
    
    stats_all = tacsatEflalo[[2]]
    stats_all = stats_all |> mutate ( year_a = year )
    
  } else { 
    
    stats_year = tacsatEflalo[[2]] |> mutate ( year_a = year )
    print( stats_year)
    stats_all =  rbind(  stats_all , stats_year )
    }
  
  #-------------------------------------------------------------------------------------------------
  ## QC 3:  Get the summary of dispatched landing values to VMS records 
  #-------------------------------------------------------------------------------------------------
  
  
  tot_kg_eflalo = eflalo |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
  tot_kg_eflaloM = eflaloM |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
  tot_kg_eflaloTacsat = tacsatEflalo_res |> summarise(total = sum ( LE_KG_TOT)) |> pull() 
  summary_landings_tacsatEflalo = data.frame ( ref = c(  "total_landings_kg_eflalo","total_landings_kg_eflaloM", "total_landings_kg_tacsat_eflalo"  ) , 
                                               total = c(  tot_kg_eflalo,  tot_kg_eflaloM   ,  tot_kg_eflaloTacsat )   ) |> 
    add_row (  ref = "eflaloM - tacsatEflalo total kg", total =  tot_kg_eflaloM - tot_kg_eflaloTacsat  )
  
  save(
    summary_landings_tacsatEflalo,
    file = file.path(outPath, paste0("qc3_summary_landings_tacsatEflalo", year, ".RData"))
  )
  
  
} 

save(
  stats_all,
  file = file.path(outPath, paste0("tacsatEflalo_stats_sap2_all_years.RData"))
)
  
  
  
  
  
  
  
  
  
  
  
  ##########################################################
  ###  SplitAmongPings 0.77   ##############################
  ##############################################################
  
  yearsToSubmit = 2009:2023
  
 
  
  ## Load the Fishing Speed Arrays from the AD-HOC speed profile analysis
  fish_array = "C:\\Users\\RM12\\OneDrive - CEFAS\\Roi\\projects\\datacalls\\ices\\march_2024\\ICES-VMS-and-Logbook-Data-Call_Cefas\\Results"
  load(file =   file.path(fish_array, paste0("fishing_speed_met5_array_2024.RData")) )
  
  
  # Looping through the years to submit
  for(year in yearsToSubmit){
    
  #year = 2023
  print(year)
  #####################################################################
  ### Identify Fishing Activity  - TACSAT/VMS records as fishing  #####
  #####################################################################
 
  
  load(file = paste0(outPath, paste0("/cleanEflalo",year,".RData")) )
  load(file = file.path(outPath, paste0("tacsatMerged", year, ".RData")  ) ) 
  

  # start by correctly formatting the level 5 metier
  
  tacsatp$LE_L5MET <-  sapply(strsplit(tacsatp$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  
  
  
  ## Join the TACSAT data with the speed array ranges to identify fishing VMS records ('f') and not fishing records ( steaming, 's')
 
  join_q = join_by(LE_L5MET, between ( SI_SP, min, max) )
  tacsatp = tacsatp |>  left_join( fishing_speed_met5_array  , by = join_q )
  
  tacsatp = tacsatp |>  mutate ( SI_STATE = ifelse ( is.na (min) & is.na (max) , 's', 'f')) |>  dplyr::select( -colnames (fishing_speed_met5_array ))
  
  
  
  
  
  
  # 2.2 Dispatch landings of merged eflalo at the ping scale
  # ----------------------------------------------------------------------------
  
   
  
  # Split eflalo into two data frames based on the presence of FT_REF in tacsatp
  
  eflaloNM <- subset(eflalo, !FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
  
  message(sprintf("%.2f%% of the eflalo data not in tacsat\n", (nrow(eflaloNM) / (nrow(eflaloNM) ))))
  
  
 
  
  # Filter rows where SI_STATE is 1
  # tacsatEflalo <- tacsatp[tacsatp$SI_STATE == 1,]
  
  # Convert SI_STATE to binary (0/1) format
  tacsatp$SI_STATE <- ifelse(tacsatp$SI_STATE == "f", 1, 0)
  
  tacsatp <- tacsatp[tacsatp$SI_STATE == 1,]
  tacsatp <- tacsatp[!is.na(tacsatp$INTV),]
  
  ## Remove the LE_RECT column in tacsatp that was assigned form logbook 
  
  tacsatp$LE_RECT = NULL
  
  
  
  
  
  #Set catch date to be equal to SI_DATE 
  ## THIS IS ONLY A REQUIREMENT FOR RUN SPLITAMONGPINGS2 
 
  tacsatp <- as.data.frame(tacsatp)
  
  
 
 
  tacsatEflalo_0p77 <-
    splitAmongPings_0p77(
      tacsat = tacsatp,
      eflalo = eflalo,
      variable = "all",
      level = c("day","ICESrectangle",  "trip"),
      conserve = TRUE, 
      conserve_ML2 = TRUE, 
      by = "INTV"
    )
   
  
  tacsatEflalo = tacsatEflalo_0p77[[1]] 
  stats_splitamongpings = tacsatEflalo_0p77[[2]] |> mutate(year = year )
  stats_vms_splitamongpings = tacsatEflalo_0p77[[3]] |> mutate(year = year )
  
  
  save(
    tacsatEflalo ,
    file = file.path(outPath, paste0("tacsatEflalo_0p77_", year, ".RData"))
  )
  
  print( paste0("Saved the result of SplitAmongPings_", year))
  
  save(
    stats_splitamongpings ,
    file = file.path(outPath, paste0("tacsatEflalo_0p77_", year, "_summary_split_kg.RData"))
  )
  
  
  save(
    stats_vms_splitamongpings ,
    file = file.path(outPath, paste0("tacsatEflalo_0p77_", year, "_summary_split_vms.RData"))
  )
  
  
  print( paste0("Saved the stats of SplitAmongPings_", year))
  
 
  
  
  #-------------------------------------------------------------------------------------------------
  ## QC 3:  Get the summary of dispatched landing values to VMS records 
  #-------------------------------------------------------------------------------------------------
  
  
  tot_kg_eflalo = eflalo |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
  tot_kg_eflaloM = eflaloM |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
  tot_kg_eflaloNM = eflaloNM |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
  tot_kg_eflaloTacsat = tacsatEflalo |> summarise(total = sum ( LE_KG_TOT, na.rm = T)) |> pull() 
  summary_landings_tacsatEflalo = data.frame ( ref = c(  "total_landings_kg_eflalo","total_landings_kg_eflaloM","total_landings_kg_eflaloNM",  "total_landings_kg_tacsat_eflalo"  ) , 
                                               total = c(  tot_kg_eflalo,  tot_kg_eflaloM   , tot_kg_eflaloNM   , tot_kg_eflaloTacsat )   ) |> 
    add_row (  ref = "eflaloM - tacsatEflalo total kg", total  =  round(tot_kg_eflaloM - tot_kg_eflaloTacsat, 2 )   ) |> 
    add_row (  ref = "eflalo  - tacsatEflalo total kg", total  =  round(tot_kg_eflalo  - tot_kg_eflaloTacsat, 2)   )
  
########## ADD THE 
  
  save(
    summary_landings_tacsatEflalo,
    file = file.path(outPath, paste0("splitAmongPings_comparison/qc3_summary_landings_tacsatEflalo_0p77_", year, ".RData"))
  )
  
  print( paste0("Saved the summary stats of tacsatEflalo_", year))
  
  
  }
  
  
  

  
  
  
  
  
  
  ##########################################################
  ###  SplitAmongPings 0.76     ############################
  ##############################################################
  
  yearsToSubmit = 2009:2023
  
  
  
  ## Load the Fishing Speed Arrays from the AD-HOC speed profile analysis
  fish_array = "Y:\\FISHERIES M MoU\\Working_Area\\spatial_fisheries_data\\ices_datacalls\\ices_vms_lb_datacall_2024\\data/"
  load(file =   file.path(fish_array, paste0("fishing_speed_met5_array_2024.RData")) )
  
  temp_res_local = 'C:\\Users\\RM12\\OneDrive - CEFAS\\Roi\\projects\\datacalls\\ices\\march_2024\\ICES-VMS-and-Logbook-Data-Call_Cefas\\Results\\'
  
  # Looping through the years to submit
  for(year in yearsToSubmit){
    
    print(paste ("year analysis: " ,year) ) 
    
    
    #####################################################################
    ### Identify Fishing Activity  - TACSAT/VMS records as fishing  #####
    #####################################################################
    
    
    load(file = paste0(temp_res_local, paste0("cleanEflalo",year,".RData")) )
    load(file = file.path(temp_res_local, paste0("tacsatMerged", year, ".RData")  ) ) 
    
    
    # start by correctly formatting the level 5 metier
    
    tacsatp$LE_L5MET <-  sapply(strsplit(tacsatp$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  
    
    
    ## Join the TACSAT data with the speed array ranges to identify fishing VMS records ('f') and not fishing records ( steaming, 's')
    
    
    join_q = join_by(LE_L5MET, between ( SI_SP, min, max) )
    tacsatp = tacsatp |>  left_join( fishing_speed_met5_array  , by = join_q )
    
    tacsatp = tacsatp |>  mutate ( SI_STATE = ifelse ( is.na (min) & is.na (max) , 's', 'f')) |>  select( -colnames (fishing_speed_met5_array ))
    
    
    
    
    # 2.2 Dispatch landings of merged eflalo at the ping scale
    # ----------------------------------------------------------------------------
    
    
    
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
    
    
    ##### splitamongpings 0p76
    
    
    tacsatEflalo_0p76 <-
      splitAmongPings_0p76(
        tacsat = tacsatp,
        eflalo = eflalo,
        variable = "all",
        level = "day",
        conserve = TRUE, 
        by = "INTV"
      )
    
    
    save(
      tacsatEflalo_0p76,
      file = file.path(outPath, paste0("tacsatEflalo_0p76_", year, ".RData"))
    )
    
    
    #-------------------------------------------------------------------------------------------------
    ## QC 3:  Get the summary of dispatched landing values to VMS records using splitamongpings 0p76
    #-------------------------------------------------------------------------------------------------
    
    
    length(which(!level %in% c("day","ICESrectangle","trip")))>0
    
    tot_kg_eflalo = eflalo |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
    tot_kg_eflaloM = eflaloM |> summarise(total = sum ( LE_KG_TOT)  ) |> pull() 
    tot_kg_eflaloTacsat = tacsatEflalo_0p76 |> summarise(total = sum ( LE_KG_TOT, na.rm = T)) |> pull() 
    summary_landings_tacsatEflalo = data.frame ( ref = c(  "total_landings_kg_eflalo","total_landings_kg_eflaloM", "total_landings_kg_tacsat_eflalo"  ) , 
                                                 total = c(  tot_kg_eflalo,  tot_kg_eflaloM   ,  tot_kg_eflaloTacsat )   ) |> 
      add_row (  ref = "eflaloM - tacsatEflalo total kg", total =  tot_kg_eflaloM - tot_kg_eflaloTacsat  )
    
    ########## ADD THE 
    
    save(
      summary_landings_tacsatEflalo,
      file = file.path(outPath, paste0("splitAmongPings_comparison/qc3_summary_landings_tacsatEflalo_0p76_", year, ".RData"))
    )
    
    
    
    
    
  } 
  
  
  
  
  
  
  
  ##############################################################################################
  
  
  
  
  
  ##########################################################
  ###  Grid to C-Square SAP077  #################################
  ##############################################################
  
  yearsToSubmit = 2009:2023
  
  
  year = 2023
  
  # Looping through the years to submit
  for(year in yearsToSubmit){
  
   
  load(file = paste0(outPath,paste0("/tacsatEflalo_0p77_", year, ".RData"))) 
  
    
  # Calculate the c-square based on longitude and latitude
  tacsatEflalo$Csquare <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  
  table1_sap0p77 = tacsatEflalo 
  
  table1_sap0p77 |>  colnames()
  
  
  ## Run this line ONLY to analyse the location with 0 landings aportioned by SAP0p77
 ###  table1_sap0p77  = table1_sap0p77 |> filter ( LE_KG_TOT == 0 )
  
  table1_sap0p77  = table1_sap0p77 |>  mutate( kg_0 = ifelse ( LE_KG_TOT == 0, '0_kg_vms' , 'no0_kg_vms' ) )
  
  table1_agg_sap0p77 <- table1_sap0p77 %>%
    # Separate LE_MET into met4 and met5, dropping extra pieces
    separate(col = LE_MET, c("MetierL4", "MetierL5"), sep = '_', extra = "drop", remove = FALSE) %>%
    # Group by several variables
    group_by(SI_YEAR,  Csquare , MetierL4, MetierL5, kg_0) %>%
    # Summarise the grouped data
    summarise(
      No_Records = n(),
      AverageFishingSpeed = mean(SI_SP),
      FishingHour = sum(INTV, na.rm = TRUE),
      AverageInterval = mean(INTV, na.rm = TRUE),
      AverageVesselLength = mean(VE_LEN, na.rm = TRUE),
      AveragekW = mean(VE_KW, na.rm = TRUE),
      TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
      TotValue = sum(LE_EURO_TOT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.frame()
  
  
  
  table1_agg_sap0p77_coord  = table1_agg_sap0p77  |> 
    cbind ( CSquare2LonLat(table1_agg_sap0p77$Csquare , 0.05) ) |> 
    st_as_sf(coords = c( "SI_LONG", "SI_LATI"), remove = F) |> 
    st_set_crs(value = 4326) |> 
    filter( SI_LATI |> between ( 40, 70), SI_LONG |>  between ( -15, 20))  
  
  
 
  
  
  
  load(file = paste0(outPath,paste0("/tacsatEflalo_SAP2_", year, ".RData")))   
  
  
  # Calculate the c-square based on longitude and latitude
  tacsatEflalo$Csquare <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  
  table1_sap2  = tacsatEflalo 
  
  table1_sap2 |>  colnames()
  
  table1_agg_sap2 <- table1_sap2 %>%
    # Separate LE_MET into met4 and met5, dropping extra pieces
    separate(col = LE_MET, c("MetierL4", "MetierL5"), sep = '_', extra = "drop", remove = FALSE) %>%
    # Group by several variables
    group_by(SI_YEAR,  Csquare , MetierL4, MetierL5) %>%
    # Summarise the grouped data
    summarise(
      No_Records = n(),
      AverageFishingSpeed = mean(SI_SP),
      FishingHour = sum(INTV, na.rm = TRUE),
      AverageInterval = mean(INTV, na.rm = TRUE),
      AverageVesselLength = mean(VE_LEN, na.rm = TRUE),
      AveragekW = mean(VE_KW, na.rm = TRUE),
      TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
      TotValue = sum(LE_EURO_TOT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.frame()
  
  
  }
  
  

  
  
  
  
  
  
  ### Join both tables to compare the reuslts of SAP functions 
  
  
 table1_comparisons =  table1_agg_sap2 |> left_join(table1_agg_sap0p77, 
                                                    by = join_by(SI_YEAR,  Csquare , MetierL4, MetierL5)  ) 
  
  
 table1_comparisons |> colnames()
 
 tabtable1_comparisons_diff   = table1_comparisons |> 
   mutate( n_records_diff  = No_Records.x- No_Records.y , 
           effort_h = FishingHour.x - FishingHour.y , 
           le_kg_diff = round(TotWeight.x - TotWeight.y, 2)   )
 
 tabtable1_comparisons_diff |>  
   group_by(MetierL4, MetierL5) |>  
   summarise(hours =  mean( effort_h ) ,kg = mean  ( abs ( le_kg_diff)), kg_max = max(le_kg_diff ) , kg_min = min(le_kg_diff )   )  |> 
   arrange(desc( kg)) |> 
   as.data.frame()  
 
 
 
 
 
 tabtable1_comparisons_diff_coord  = tabtable1_comparisons_diff  |> 
   cbind ( CSquare2LonLat(tabtable1_comparisons_diff$Csquare , 0.05) ) |> 
   st_as_sf(coords = c( "SI_LONG", "SI_LATI"), remove = F) |> st_set_crs(value = 4326) |> 
   filter( SI_LATI |> between ( 40, 70), SI_LONG |>  between ( -15, 20)) |> 
   mutate ( le_kg_diff_class = cut(le_kg_diff, breaks =  seq ( from = -2000000, to = 1250000, by = 250000) ) ) 
 
 tabtable1_comparisons_diff_coord |>  filter( le_kg_diff_class |>  is.na())
 
 tabtable1_comparisons_diff_coord |> st_drop_geometry( ) |> 
   ggplot() + geom_histogram(aes(log( le_kg_diff)) ) 
 
 tabtable1_comparisons_diff_coord |> st_drop_geometry( ) |> 
   ggplot() + geom_histogram(aes(n_records_diff) ) 
 
 ggplot(tabtable1_comparisons_diff_coord  ) + 
   geom_sf(aes( color = le_kg_diff_class), size = 0.5) + 
   scale_color_discrete( ) + facet_wrap(~ le_kg_diff_class)
 
 ?scale_color_discrete
 
 tabtable1_comparisons_diff_coord |> distinct(SI_YEAR)
 

 
 getwd() 
 
 ### Get the data with no SAR for non mobile bottom contact gears
 
 con = dbConnect(odbc::odbc(),      "geofish_dev" )
 
 csquares = st_read( dsn =  con, 
                     layer =  DBI::Id  ( schema = 'marine_statistical_grids' ,
                                         table  = 'c_squares_all_pol_marine_fao_div_eez_ices_ospar_uk_regions') )  #,
                     #query =  "select * from marine_statistical_grids.c_squares_all_pol_marine_fao_div limit 10 ")
 
 
 ## Comparison between result of SAP2 and SAP0p77
 
 csquares_an = csquares |>  filter( c_square  %in% tabtable1_comparisons_diff_coord$Csquare )
 
 tabtable1_comparisons_diff_coord_cs = csquares_an |>  inner_join(tabtable1_comparisons_diff_coord |>  st_drop_geometry() , by = join_by(c_square == Csquare))
 
 st_write(tabtable1_comparisons_diff_coord_cs, dsn = ".\\Results\\splitAmongPings_comparison\\maps_csquares\\splitamongpings_0p77_2_comparisons.gpkg", layer =  "sap2_vs_sap0p77_csquares_diff", append = FALSE)
 
 
 ### SAP0p77 results to CSquares 
 
 
 
 
 csquares_sap0p77 = csquares |>  filter( c_square  %in% table1_agg_sap0p77$Csquare )
 
 table1_agg_sap0p77_diff_coord_cs = csquares_sap0p77 |>  inner_join(table1_agg_sap0p77_coord |>  st_drop_geometry() , by = join_by(c_square == Csquare))
 
 
 table1_agg_sap0p77_diff_coord_cs |> st_drop_geometry() |>  distinct(uk_marine_zone )
 table1_agg_sap0p77_diff_coord_cs |>  filter  (uk_marine_zone == 'England'  & TotWeight    )
 
 st_write(table1_agg_sap0p77_diff_coord_cs, dsn = ".\\Results\\splitAmongPings_comparison\\maps_csquares\\splitamongpings_0p77_2_comparisons.gpkg", layer =  "table1_agg_sap0p77_diff_coord_cs", append = FALSE)
 
 
 
 
 
 ### SAP0p77 results to CSquares - Difference between cels with VMS with effort by 0 kg
 
 
 
 table1_agg_sap0p77pivot  =  table1_agg_sap0p77  |> 
   pivot_wider(names_from = kg_0, values_from = - c ( SI_YEAR ,Csquare ,       MetierL4, MetierL5, kg_0  ) ,values_fill =  0    )
 
 table1_agg_sap0p77pivot_diff_kg_0 = 
   table1_agg_sap0p77pivot |> 
   mutate ( vms_kg_vs_0_kg  = No_Records_no0_kg_vms   - No_Records_0_kg_vms  ,
            hours_kg_vs_0_kg  = FishingHour_no0_kg_vms - FishingHour_0_kg_vms  ) |> 
   mutate ( vms_kg_and_0_kg  = No_Records_no0_kg_vms  +  No_Records_0_kg_vms ,
            hours_kg_and_0_kg  = FishingHour_no0_kg_vms +  FishingHour_0_kg_vms  ) |> 
   filter (!is.na ( vms_kg_vs_0_kg)  ) 
 
 table1_agg_sap0p77pivot_diff_kg_0_tot = 
   table1_agg_sap0p77pivot_diff_kg_0|> 
   group_by(SI_YEAR , Csquare , MetierL4  ) |> 
   mutate(vms_no0_kg_vms = sum ( No_Records_no0_kg_vms, na.rm = T) ,
          vms_0_kg_vms = sum ( No_Records_0_kg_vms, na.rm = T) ,
          FishingHour_no0_kg_vms = sum ( FishingHour_no0_kg_vms, na.rm = T) , 
          FishingHour_0_kg_vms = sum ( FishingHour_0_kg_vms, na.rm = T) ) |> 
   reframe(vms_kg_vs_0_kg_tot = sum ( vms_kg_vs_0_kg, na.rm = T),
             hours_kg_vs_0_kg_tot = sum(hours_kg_vs_0_kg, na.rm = T), 
             vms_kg_and_0_kg_tot = sum ( vms_kg_and_0_kg, na.rm = T),
             hours_kg_and_0_kg_tot = sum(hours_kg_and_0_kg, na.rm = T), 
             vms_no0_kg_vms_prop = round ( ( vms_no0_kg_vms/vms_kg_and_0_kg_tot ) , 2)  ,
             FishingHour_no0_kg_vms_prop = round ( ( FishingHour_no0_kg_vms/hours_kg_and_0_kg_tot ) , 2) 
             )  |> 
   as.data.frame()
 
 summary(table1_agg_sap0p77pivot_diff_kg_0_tot)
 table1_agg_sap0p77pivot_diff_kg_0_tot |>  filter ( vms_no0_kg_vms_prop  > 1)
 
 ### save the general aggregated dataset product. No comaprison stats, etc
 
 csquares_sap0p77 = csquares |> filter( c_square  %in% table1_agg_sap0p77pivot_diff_kg_0_tot$Csquare )
 
 table1_agg_sap0p77_diff_coord_cs = csquares_sap0p77 |>  inner_join(table1_agg_sap0p77_coord |>  st_drop_geometry() , by = join_by(c_square == Csquare))
 
 
 table1_agg_sap0p77_diff_coord_cs |> st_drop_geometry() |>  distinct(uk_marine_zone )
 table1_agg_sap0p77_diff_coord_cs |>  filter  (uk_marine_zone == 'England'  & TotWeight    )
 
 st_write(table1_agg_sap0p77_diff_coord_cs, dsn = ".\\Results\\splitAmongPings_comparison\\maps_csquares\\splitamongpings_0p77_2_comparisons.gpkg", layer =  "table1_agg_sap0p77_diff_coord_cs", append = FALSE)
 
 
 ### save the general aggregated dataset product. No comparison stats, etc
 
 
 csquares_sap0p77 = csquares |>  filter( c_square  %in% table1_agg_sap0p77$Csquare )
 
 csquares_sap0p77 = csquares_sap0p77 |> distinct(c_square, uk_marine_zone, icesname , geom )
 
 
 table1_agg_sap0p77_diff_coord_cs = csquares_sap0p77 |>  inner_join(table1_agg_sap0p77_coord |>  st_drop_geometry() , by = join_by(c_square == Csquare))
 
 
 table1_agg_sap0p77_diff_coord_cs |> st_drop_geometry() |>  distinct(uk_marine_zone )
 table1_agg_sap0p77_diff_coord_cs |>  filter  (uk_marine_zone == 'England'  & TotWeight    )
 
 st_write(table1_agg_sap0p77_diff_coord_cs, dsn = ".\\Results\\splitAmongPings_comparison\\maps_csquares\\splitamongpings_0p77_2_comparisons.gpkg", layer =  "table1_agg_sap0p77_diff_coord_cs", append = FALSE)
 
 
 ### save the general aggregated dataset product. comparison of effort and records with 0 kg allocated
 
 csquares_sap0p77 = csquares |>  filter( c_square  %in% table1_agg_sap0p77$Csquare )
 
 csquares_sap0p77 = csquares_sap0p77 |> distinct(c_square, uk_marine_zone, icesname , geom )
 
 
 table1_agg_sap0p77_diff_kg_0_coord_cs = csquares_sap0p77 |> inner_join(table1_agg_sap0p77pivot_diff_kg_0_tot |>  st_drop_geometry() , by = join_by(c_square == Csquare))
 
 summary ( table1_agg_sap0p77_diff_kg_0_coord_cs)
 table1_agg_sap0p77_diff_kg_0_coord_cs |> st_drop_geometry() |>  distinct(uk_marine_zone )
 table1_agg_sap0p77_diff_kg_0_coord_cs |>  filter  (uk_marine_zone == 'England'  & TotWeight    )
 
 st_write(table1_agg_sap0p77_diff_kg_0_coord_cs, dsn = ".\\Results\\splitAmongPings_comparison\\maps_csquares\\splitamongpings_0p77_2_comparisons.gpkg", layer =  "table1_table1_agg_sap0p77_diff_kg_0_coord_cs", append = FALSE)
 
 
 
 
 
 
##############################################################################################################################################
  
  
 


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




#----------------
# DATA SUBMISSION QC 
#----------------


library(dplyr)

table1 = read.csv( here("Results\\ices_vms_lb_datacall_2024_outputs\\table1Save.csv") , header = T ) 

colnames(table2)

table1_15_23 = table1 |> filter ( SI_YEAR |> between(2015, 2023))

 write.csv( x = table1_15_23 ,file =  here("Results\\ices_vms_lb_datacall_2024_outputs\\table1Save_2015_2023.csv") , na = "", row.names=FALSE, col.names=TRUE, sep=",", quote=FALSE)
 
 save(table1_15_23, file =  here("Results\\ices_vms_lb_datacall_2024_outputs\\table1Save_2015_2023.RData") )
 
 table2 = read.csv( here("Results\\ices_vms_lb_datacall_2024_outputs\\table2Save.csv") , header = T ) 
 table2_15_23 = table2 |> filter (  YEAR |> between(2015, 2023))
 write.csv( x= table2_15_23, file =  here("Results\\ices_vms_lb_datacall_2024_outputs\\table2Save_2015_2023.csv") , na = "", row.names=FALSE, col.names=TRUE, sep=",", quote=FALSE)
 
 save(table2_15_23, file =  here("Results\\ices_vms_lb_datacall_2024_outputs\\table2Save_2015_2023.RData") )
 
 