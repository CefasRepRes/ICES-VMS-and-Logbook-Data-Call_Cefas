library(vmstools)
library(lubridate)
library(doBy)
library(data.table)
library(icesVocab)
library(icesSharePoint)
library(RPostgres)
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(here)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)
library(tcltk)

#setwd("C:/Users/MD09/OneDrive - CEFAS/projects/datacalls/ices/2024")

year <- 2023
yearsToSubmit <- 2023


# Set paths
path <- paste0(getwd(), "/") # Working directory
datapath <- paste0('Y:\\FISHERIES M MoU\\Working_Area\\spatial_fisheries_data\\ices_datacalls\\ices_vms_lb_datacall_2024\\data') # Data  directory


codePath  <- paste0(path, "Scripts/")   # Location to store R scripts
dataPath  <- paste0(path, "Data/")      # Location to store tacsat (VMS) and eflalo (logbook) data

dataPath <- paste0('Y:\\FISHERIES M MoU\\Working_Area\\spatial_fisheries_data\\ices_datacalls\\ices_vms_lb_datacall_2024\\data\\input_data') # Data  directory
intoutPath  <- paste0('Y:\\FISHERIES M MoU\\Working_Area\\spatial_fisheries_data\\ices_datacalls\\ices_vms_lb_datacall_2024\\data\\output_data') # Data  directory
outPath <- paste0('Y:\\FISHERIES M MoU\\Working_Area\\spatial_fisheries_data\\ices_datacalls\\ices_vms_lb_datacall_2024\\data\\results') # Data  directory

outPath   <- paste0(path, "Results/")   # Location to store the results
plotPath  <- paste0(path, "Plots/") 

# Create directories if they don't exist
dir.create(codePath, showWarnings = T)
dir.create(dataPath, showWarnings = T)
dir.create(outPath, showWarnings = T)
dir.create(plotPath, showWarnings = T)



 

if(!file.exists(paste0(dataPath, "hab_and_bathy_layers.zip"))){
   
  # Download the zip file
  ## icesSharePoint::spgetfile(file = "SEAwise Documents/hab_and_bathy_layers.zip", site = "/ExpertGroups/DataExpports/VMS_Data_call", destdir = dataPath)
  ## The first time you run this code you will be prompted to enter your ICES Sharepoint password. Type it in the pop-up window to proceed with the download


# Extract the zip archive
 unzip(paste0(dataPath, "hab_and_bathy_layers.zip"), exdir = dataPath, overwrite = TRUE, junkpaths = TRUE)
}

eusm <- readRDS(paste0(dataPath, "eusm.rds"))
eusm <- eusm %>% st_transform(4326)
bathy <- readRDS(paste0(dataPath, "ICES_GEBCO.rds"))
bathy <- bathy %>% st_set_crs(4326)

#'------------------------------------------------------------------------------
# 0.2 Settings for analysis                                                 ----
#'------------------------------------------------------------------------------

# Setting thresholds
spThres       <- 20   # Maximum speed threshold in analyses in nm
intThres      <- 3    # Minimum difference in time interval in minutes to prevent pseudo duplicates
intvThres     <- 240  # Maximum difference in time interval in minutes to prevent unrealistic intervals
lanThres      <- 1.5  # Maximum difference in log10-transformed sorted weights

# Set the years to submit
yearsToSubmit <- c(2009:2023)

# Set the gear names for which automatic fishing activity is wanted
autoDetectionGears <- c("TBB","OTB","OTT", "OTM","SSC","SDN","DRB","PTB","HMD", "MIS")

# Decide if you want to visually analyze speed-histograms to identify fishing activity peaks
visualInspection <- TRUE

# Specify how landings should be distributed over the VMS pings
linkEflaloTacsat <- c("trip")

# Define function for NOT in data 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Define a function to create a unique trip identifier
create_trip_id <- function(eflalo) {
  paste(eflalo$LE_ID, eflalo$LE_CDAT, sep="-")
}

valid_metiers <- fread("https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv")$Metier_level6

# Define a function to remove records starting before the 1st of January
remove_before_jan <- function(eflalo, year) {
  # Convert the start of the year to a POSIXct datetime object
  start_of_year <- as.POSIXct(paste(year, "-01-01 00:00:00", sep = ""), format = "%Y-%m-%d %H:%M")
  
  # Ensure FT_DDATIM is in the correct datetime format
  eflalo$FT_DDATIM <- as.POSIXct(eflalo$FT_DDATIM, format = "%Y-%m-%d %H:%M")
  
  # Remove records with FT_DDATIM before the start of the year
  eflalo <- eflalo[eflalo$FT_DDATIM >= start_of_year,]
  
  return(eflalo)
}

# Define a function to calculate intervals in the TACSAT data
intvTacsat <- function (tacsat, level = "trip", weight = c(1, 0), fill.na = FALSE) {
  # Check if 'weight' is a length 2 numeric vector
  if (length(weight) != 2) 
    stop("weight must be specified as a length 2 numeric vector")
  
  # Normalize 'weight' to sum to 1
  weight <- weight/sum(weight, na.rm = TRUE)
  
  # Sort 'tacsat' (assuming 'sortTacsat' is a function that sorts 'tacsat')
  tacsat <- sortTacsat(tacsat)
  
  # Convert 'SI_DATE' and 'SI_TIME' to POSIXct if 'SI_DATIM' is not already present
  if (!"SI_DATIM" %in% colnames(tacsat)) 
    tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  
  # If level is 'trip', calculate intervals for each trip
  if (level == "trip") {
    # Check if 'FT_REF' is present
    if (is.null(tacsat$FT_REF)) 
      stop("no trip number available to merge on trip level")
    
    # Split 'tacsat' by 'VE_REF'
    sptacsat <- split(tacsat, tacsat$VE_REF)
    
    # Calculate intervals for each trip
    tacsat$INTV <- unlist(lapply(sptacsat, function(x) {
      # Convert 'FT_REF' to factor
      FT_REF <- as.factor(x$FT_REF)
      
      # Calculate intervals for each 'FT_REF'
      res <- by(x, FT_REF, function(y) {
        # If there is more than one row, calculate intervals
        if (nrow(y) > 1) {
          # Calculate differences in 'SI_DATIM' for each row
          difftime_xmin1 <- c(NA, difftime(y$SI_DATIM[2:nrow(y)], y$SI_DATIM[1:(nrow(y) - 1)], units = "mins"))
          difftime_xplus1 <- c(difftime_xmin1[-1], NA)
          
          # Calculate intervals based on 'weight'
          if (any(weight == 0)) {
            if (weight[1] == 0) 
              INTV <- difftime_xplus1
            if (weight[2] == 0) 
              INTV <- difftime_xmin1
          } else {
            INTV <- rowSums(cbind(difftime_xmin1 * weight[1], difftime_xplus1 * weight[2]))
          }
          
          # If 'fill.na' is TRUE, fill NA values in 'INTV'
          if (fill.na) {
            idx <- which(is.na(INTV))
            INTV[idx] <- rowSums(cbind(difftime_xmin1[idx], difftime_xplus1[idx]), na.rm = TRUE)
            INTV[idx][which(INTV[idx] == 0)] <- NA
          }
          
          return(INTV)
        } else {
          return(NA)
        }
      })
      
      return(unsplit(res, FT_REF))
    }))
    
    # Set 'INTV' to NA where 'FT_REF' equals 0
    tacsat$INTV[which(tacsat$FT_REF == "0")] <- NA
  }
  
  # If level is 'vessel', calculate intervals for each vessel
  if (level == "vessel") {
    # Calculate differences in 'SI_DATIM' for each row
    difftime_xmin1 <- c(NA, difftime(tacsat$SI_DATIM[2:nrow(tacsat)], tacsat$SI_DATIM[1:(nrow(tacsat) - 1)], units = "mins"))
    difftime_xplus1 <- c(difftime_xmin1[-1], NA)
    
    # Calculate intervals based on 'weight'
    if (any(weight == 0)) {
      if (weight[1] == 0) 
        INTV <- difftime_xplus1
      if (weight[2] == 0) 
        INTV <- difftime_xmin1
    } else {
      INTV <- rowSums(cbind(difftime_xmin1 * weight[1], difftime_xplus1 * weight[2]))
    }
    
    # If 'fill.na' is TRUE, fill NA values in 'INTV'
    if (fill.na) {
      idx <- which(is.na(INTV))
      INTV[idx] <- rowSums(cbind(difftime_xmin1[idx], difftime_xplus1[idx]), na.rm = TRUE)
      INTV[idx][which(INTV[idx] == 0)] <- NA
    }
    
    # Assign 'INTV' to 'tacsat'
    tacsat$INTV <- INTV
    
    # Get unique vessels
    vessels <- unique(tacsat$VE_REF)
    
    # Get first and last rows for each vessel
    first.vessels <- unlist(lapply(as.list(vessels), function(x) which(tacsat$VE_REF == x)[1]))
    last.vessels <- unlist(lapply(as.list(vessels), function(x) rev(which(tacsat$VE_REF == x))[1]))
    
    # Set 'INTV' to NA for first and last rows of each vessel based on 'weight'
    if (weight[1] != 0) 
      tacsat$INTV[first.vessels] <- NA
    if (weight[2] != 0) 
      tacsat$INTV[last.vessels] <- NA
    
    # If 'fill.na' is TRUE, fill NA values in 'INTV' for first and last rows of each vessel
    if (fill.na) {
      tacsat$INTV[first.vessels] <- difftime_xplus1[first.vessels]
      tacsat$INTV[last.vessels] <- difftime_xmin1[last.vessels]
    }
  }
  
  return(tacsat)
}


valid_metiers <- fread("https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv")$Metier_level6



# Define a function to assign tripnumber to Tacsat data


trip_assign <- function(tacsatp, eflalo, col = "LE_GEAR", trust_logbook = T){
  
  
  if(col == "LE_MET"){
    tst <- data.table(eflalo)[get(col) %in% valid_metiers & !is.na(get(col)) ,.(uniqueN(get(col))), by=.(FT_REF)]
  }else{
    tst <- data.table(eflalo)[!is.na(get(col)),.(uniqueN(get(col))), by=.(FT_REF)]
  }
  
  
  if(nrow(tst[V1>1])==0){
    warning(paste("No duplicate", col, "in tacsatp"))
    return(data.frame())
  }
  
  e <- data.table(eflalo)[FT_REF %in% tst[V1>1]$FT_REF]
  
  tz <- data.table(tacsatp)[FT_REF  %in% tst[V1>1]$FT_REF]
  
  tz[, (col) := NULL]
  
  dim(tacsatp)
  
  #unique(tacsatp) %>%  as.data.frame()
  
   
  
  # eflalo_u_col = eflalo[eflalo$FT_REF %!in% e$FT_REF,] %>%  distinct (FT_REF, LE_GEAR )
  # tacsatp_u_col =  tacsatp |> 
  #   inner_join(eflalo_u_col, by = c("FT_REF" = "FT_REF"), relationship = "many-to-many")
  
  
  if(trust_logbook){
    
    ## First bind by landing date
    
    e2 <- e[,.(get(col)[length(unique(get(col))) == 1]), by = .(FT_REF, LE_CDAT)]
    
    names(e2) <- c("FT_REF", "LE_CDAT", col)
    
    e2 = unique ( e2)
    
    tz1 <- tz |> 
      left_join(e2, by = c("FT_REF" = "FT_REF", "SI_DATE" = "LE_CDAT"))
    
    tz2 <- unique(tz1) %>%  as.data.frame()
    
    
    
    #If some are still missing, use haul information  ( LE_SDATIM , LE_EDATIM) to get the closest time
    #   if(nrow(tz[is.na(get(col))]) > 0){ 
    #    
    #      if  ("LE_SDATTIM" %in% names(e)) {
    #     
    #         str( e)
    #         
    #        if ( ! class(e$FT_DDATIM )[1] == "POSIXct" )  { 
    #         #set formats right
    #         e$FT_DDATIM <- as.POSIXct(paste(e$FT_DDAT, e$FT_DTIME,
    #                                         sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
    #         e$FT_LDATIM <- as.POSIXct(paste(e$FT_LDAT, e$FT_LTIME,
    #                                         sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
    #   
    #         e$LE_SDATTIM <- as.POSIXct(paste(e$LE_SDAT, e$LE_STIME,
    #                                          sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
    #         e$LE_EDATTIM <- as.POSIXct(paste(e$LE_EDAT, e$LE_ETIME,
    #                                          sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
    #        } 
    #         
    #         tx <- tz[is.na(get(col))]
    #         tx[, (col) := NULL]
    #         
    #         
    #        
    #          q1 = e[,.(meantime = LE_SDATTIM), by = .(get(col), FT_REF)]
    #          q2 = e[,.(meantime = LE_EDATTIM), by = .(get(col), FT_REF)]
    #           mx <- rbind(q1 ,q2 )
    #           names(mx) <-  c(col, "FT_REF", "meantime")
    #           setkey(mx, FT_REF, meantime)
    #           tx <- mx[tx, roll="nearest"]
    #           tx$meantime <- NULL
    #           
    #        
    #         
    #           
    #           tx[, time := SI_DATIM]
    #           
    #           setkey(tx, FT_REF, time)
    #           
    #          
    #           tz <- rbindlist(list(tz[!is.na(get(col))], tx), fill = T)
    #     
    #      } else {
    #        print("dataframe EFLALO  has no LE_SDATTIM column")
    #        
    #      }
    #    
    # }else{
    #   
    #   tz[, (col) := NA]
    # tz_all
    #   }
    
    # Bind to the category with most value
    
    if( nrow(tz2 %>%  filter ( is.na ( get(col)  ))) > 0  ){
      
      ft_ref_isna = tz2 %>%  filter ( is.na ( get(col) )) %>%  distinct(FT_REF) %>% pull()
      tz3 = tz2 %>%  filter ( FT_REF %in% ft_ref_isna ) %>%  as.data.frame()
      tz4 = tz3 |> filter ( is.na ( get(col) ) )
      e2 = e %>%  filter ( FT_REF %in% ft_ref_isna )
      
      
      if(!"LE_KG_TOT" %in% names(e2)){
        idxkgeur <- colnames(e2)[grepl("LE_KG_|LE_EURO_", colnames(e2))]
        # Calculate the total KG and EURO for each row
        e2$LE_KG_TOT <- rowSums(e2[,..idxkgeur], na.rm = TRUE)
      }
      
      highvalue <- e2[,.(LE_KG_TOT = sum(LE_KG_TOT, na.rm = T)), by = .(FT_REF, get(col))]
      highvalue <- highvalue[,.(get[which.max(LE_KG_TOT)]), by = .(FT_REF)]
      names(highvalue) <-  c("FT_REF", col)
      
      tx4 <- tz4
      tx5 = tx4 %>%  select ( - any_of( col )  )
      tz5 <- tx5 %>%  inner_join ( highvalue, by =  "FT_REF")
      
    }
    
    
    tz_col_na =   tz2 %>%  filter(  !is.na ( get( col) ))   
    tz_all =  rbind (tz_col_na , tz5 )  
    
    
    #tacsatp_fill <- rbindlist(  list( tacsatp_u_col , tz_all  )    , fill = T)
    
    
    return(tz_all)
    
  }
  
  
}


# splitAmongPings2 <- function(tacsatp, eflalo, conserve = c("Day", "Metier") {
splitAmongPings2 <- function(tacsatp, eflalo) {
  require(data.table)
  
  t <- data.table(tacsatp)[ SI_STATE == 1]
  e <- data.table(eflalo)
  
  if(any(is.na(t$INTV)))
    stop("NA values in intervals (INTV) in tacsatp, please add an interval to all pings")
  
  e[, SI_DATE := LE_CDAT]
  #find all column names with KG or EURO in them
  kg_euro <- grep("KG|EURO", colnames(e), value = T)
  
  ### sum by FT_REF, LE_MET, SI_DATE
  
  n1 <- e[FT_REF %in% t$FT_REF,lapply(.SD,sum, na.rm = T),by=.(FT_REF, LE_MET, SI_DATE),
          .SDcols=kg_euro][, ide1 := 1:.N]
  
  setkey(t, FT_REF, LE_MET, SI_DATE)
  setkey(n1, FT_REF, LE_MET, SI_DATE)
  
  ts1 <- merge(t, n1)
  
  setkey(ts1, FT_REF, LE_MET, SI_DATE)
  ts1[,Weight:=INTV/sum(INTV, na.rm = T), by=.(FT_REF, LE_MET, SI_DATE)]
  ts1[,(kg_euro):= lapply(.SD, function(x) x * Weight), .SDcols=kg_euro]
  
  ### sum by FT_REF, LE_MET
  n2 <- n1[ide1 %!in% ts1$ide1, lapply(.SD,sum, na.rm = T),by=.(FT_REF, LE_MET),
           .SDcols=kg_euro][, ide2 := 1:.N]
  
  setkey(t, FT_REF, LE_MET)
  setkey(n2, FT_REF, LE_MET)
  
  ts2 <- merge(t, n2)
  
  setkey(ts2, FT_REF, LE_MET)
  ts2[,Weight:=INTV/sum(INTV, na.rm = T), by=.(FT_REF, LE_MET)]
  ts2[,(kg_euro):= lapply(.SD, function(x) x * Weight), .SDcols=kg_euro]
  
  
  ### sum by FT_REF
  n3 <- n2[ide2 %!in% ts2$ide2, lapply(.SD,sum, na.rm = T),by=.(FT_REF),
           .SDcols=kg_euro][, ide3 := 1:.N]
  
  setkey(t, FT_REF)
  setkey(n3, FT_REF)
  
  ts3 <- merge(t, n3)
  
  setkey(ts3, FT_REF)
  ts3[,Weight:=INTV/sum(INTV, na.rm = T), by=.(FT_REF)]
  ts3[,(kg_euro):= lapply(.SD, function(x) x * Weight), .SDcols=kg_euro]
  
  
  #Combine all aggregations
  ts <- rbindlist(list(t, ts1, ts2, ts3), fill = T)
  ts[ ,`:=`(Weight = NULL, ide1 = NULL, ide2 = NULL, ide3 = NULL)]
  diffs = setdiff(names(ts), kg_euro)
  
  out <- ts[,lapply(.SD,sum, na.rm = T),by=diffs,
            .SDcols=kg_euro]
  
  return(data.frame(out))
  
}


## Define a function to identify peaks in speed distributions
callNumberPeak <- function(){
  tt <- tktoplevel()
  peaks <- tclVar(5)
  
  f1 <- tkframe(tt)
  tkpack(f1, side='top')
  tkpack(tklabel(f1, text='peaks: '), side='left')
  tkpack(tkentry(f1, textvariable=peaks), side='left')
  
  tkpack(tkbutton(tt, text='Next', command=function() tkdestroy(tt)),
         side='right', anchor='s')
  
  tkwait.window(tt)
  return(as.numeric(tclvalue(peaks)))}




act.tac <- function (tacsat, units = "year", analyse.by = "LE_L5MET", storeScheme = NULL, 
                     plot = FALSE, level = "all"){
  require("mixtools")
  if (!"SI_DATIM" %in% colnames(tacsat)) 
    tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME, # id datim doesn't exist in eflalo, creates in
                                        sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  if (!analyse.by %in% c("LE_L5MET", "VE_REF")) 
    stop("Analysing only by level 5 metier or vessel")                            ## would it be useful to examine speeds at L5 or L6 metier?
  tacsat$ID <- 1:nrow(tacsat)
  tacsat$SI_STATE <- NA                                                   ## creates ID and blank column to receive state
  tacsatOrig <- tacsat  
  idx <- which(is.na(tacsat$SI_SP) == FALSE)
  tacsat <- tacsat[idx, ]                                                 ## drops rows which do not have a valid speed value
  storeScheme$sigma0[which(storeScheme$sigma0 == 0)] <- "d"
  if (units == "all") {
    yrs <- 0
    mths <- 0
    wks <- 0
  }
  if (units == "year") {
    yrs <- sort(unique(format(tacsat$SI_DATIM, "%Y")))
    mths <- 0
    wks <- 0
  }
  if (units == "month") {
    yrs <- sort(unique(format(tacsat$SI_DATIM, "%Y")))
    mths <- an(sort(unique(format(tacsat$SI_DATIM, "%m"))))
    wks <- 0
  }
  if (units == "week") {
    yrs <- sort(unique(format(tacsat$SI_DATIM, "%Y")))
    wks <- an(sort(unique(format(tacsat$SI_DATIM, "%W")))) + 
      1
    mths <- 0
  }
  runScheme <- expand.grid(years = yrs, months = mths, weeks = wks)
  for (iRun in 1:nrow(runScheme)) {
    yr <- runScheme[iRun, "years"]
    mth <- runScheme[iRun, "months"]
    wk <- runScheme[iRun, "weeks"]
    if (nrow(runScheme) == 1) {
      sTacsat <- tacsat
    }
    else {
      if (mth == 0 & wk == 0) 
        sTacsat <- subset(tacsat, format(tacsat$SI_DATIM, 
                                         "%Y") == yr)
      if (mth == 0 & wk != 0) 
        sTacsat <- subset(tacsat, format(tacsat$SI_DATIM, 
                                         "%Y") == yr & (an(format(tacsat$SI_DATIM, "%W")) + 
                                                          1) == wk)
      if (mth != 0 & wk == 0) 
        sTacsat <- subset(tacsat, format(tacsat$SI_DATIM, 
                                         "%Y") == yr & format(tacsat$SI_DATIM, "%m") == 
                            mth)
    }
    if (plot == TRUE) 
      x11()
    if ("LE_L5MET" %in% colnames(tacsat) & analyse.by == "LE_L5MET") {
      gearList <- names(which((rowSums(table(sTacsat$LE_L5MET, 
                                             sTacsat$SI_SP)) - table(sTacsat$LE_L5MET, sTacsat$SI_SP)[, 
                                                                                                      "0"]) > 40))
      tyg <- subset(sTacsat, is.na(LE_L5MET) == FALSE & 
                      LE_L5MET %in% gearList)
      tygmr <- tyg
      tygmr$SI_SP <- -1 * tygmr$SI_SP
      tygmr <- rbind(tyg, tygmr)
      tng <- subset(sTacsat, is.na(LE_L5MET) == TRUE | !LE_L5MET %in% 
                      gearList)
      tngmr <- tng
      tngmr$SI_SP <- -1 * tngmr$SI_SP
      tngmr <- rbind(tng, tngmr)
      res <- list()
      for (iGr in unique(tyg$LE_L5MET)) {
        tbl <- table(subset(tygmr, LE_L5MET == iGr)$SI_SP)
        spd <- an(names(rev(sort(tbl))[1]))
        idx <- which(subset(tygmr, LE_L5MET == iGr)$SI_SP == 
                       spd)
        nxt <- ifelse(names(rev(sort(tbl))[1]) == ac(spd), 
                      ifelse(abs(an(names(rev(sort(tbl))[2]))) == 
                               abs(spd), names(rev(sort(tbl))[3]), names(rev(sort(tbl))[2])), 
                      names(rev(sort(tbl))[1]))
        if (tbl[ac(spd)]/tbl[nxt] > 5) {
          idx <- sample(idx, tbl[ac(spd)] - tbl[nxt] * 
                          2, replace = FALSE)
          if (length(which(abs(an(names(tbl))) %in% spd)) > 
              1) 
            idx <- c(idx, sample(which(subset(tygmr, 
                                              LE_L5MET == iGr)$SI_SP == (-1 * spd)), tbl[ac(-1 * 
                                                                                              spd)] - tbl[nxt] * 2, replace = FALSE))
        }
        else {
          idx <- -1:-nrow(subset(tygmr, LE_L5MET == iGr))
        }
        if (is.null(storeScheme) == TRUE) {
          res[[iGr]] <- try(normalmixEM(subset(tygmr, 
                                               LE_L5MET == iGr)$SI_SP[-idx], maxit = 1000, 
                                        k = 5, maxrestarts = 20, mean.constr = c("-b", 
                                                                                 "-a", 0, "a", "b"), sd.constr = c("b", 
                                                                                                                   "a", 0.911, "a", "b"), sigma = rep(1, 5)))
        }
        else {
          if ("means" %in% colnames(storeScheme)) {
            ss <- storeScheme[which(storeScheme$years == 
                                      yr & storeScheme$months == mth & storeScheme$weeks == 
                                      wk & storeScheme$analyse.by == iGr), "means"]
            sigma <- anf(storeScheme[which(storeScheme$years == 
                                             yr & storeScheme$months == mth & storeScheme$weeks == 
                                             wk & storeScheme$analyse.by == iGr), "sigma0"])
            fixPeaks <- ac(storeScheme[which(storeScheme$years == 
                                               yr & storeScheme$months == mth & storeScheme$weeks == 
                                               wk & storeScheme$analyse.by == iGr), "fixPeaks"])
            if (length(c(na.omit(as.numeric(strsplit(ss, 
                                                     " ")[[1]])))) == 3) {
              constraintmn <- c("-a", 0, "a")
            }
            else {
              constraintmn <- c("-b", "-a", 0, "a", "b")
            }
            if (length(c(na.omit(as.numeric(strsplit(ss, 
                                                     " ")[[1]])))) == 3) {
              constraintsd <- c("a", "b", "a")
            }
            else {
              constraintsd <- c("b", "a", sigma, "a", 
                                "b")
            }
            if (fixPeaks) 
              constraintmn <- c(na.omit(anf(unlist(strsplit(ss, 
                                                            " ")))))
            res[[iGr]] <- try(normalmixEM(subset(tygmr, 
                                                 LE_L5MET == iGr)$SI_SP[-idx], maxit = 1000, 
                                          mu = c(na.omit(as.numeric(strsplit(ss, 
                                                                             " ")[[1]]))), sigma = rep(1, length(constraintsd)), 
                                          maxrestarts = 20, mean.constr = constraintmn, 
                                          sd.constr = constraintsd))
          }
          else {
            ss <- storeScheme[which(storeScheme$years == 
                                      yr & storeScheme$months == mth & storeScheme$weeks == 
                                      wk & storeScheme$analyse.by == iGr), "peaks"]
            sigma <- anf(storeScheme[which(storeScheme$years == 
                                             yr & storeScheme$months == mth & storeScheme$weeks == 
                                             wk & storeScheme$analyse.by == iGr), "sigma0"])
            fixPeaks <- ac(storeScheme[which(storeScheme$years == 
                                               yr & storeScheme$months == mth & storeScheme$weeks == 
                                               wk & storeScheme$analyse.by == iGr), "fixPeaks"])
            if (ss == 3) {
              constraintmn <- c("-a", 0, "a")
            }
            else {
              constraintmn <- c("-b", "-a", 0, "a", "b")
            }
            if (ss == 3) {
              constraintsd <- c("a", "b", "a")
            }
            else {
              constraintsd <- c("b", "a", sigma, "a", 
                                "b")
            }
            if (length(ss) > 0) {
              if (is.na(ss) == TRUE) 
                res[[iGr]] <- try(normalmixEM(subset(tygmr, 
                                                     LE_L5MET == iGr)$SI_SP[-idx], maxit = 1000, 
                                              k = 5, maxrestarts = 20, mean.constr = c("-b", 
                                                                                       "-a", 0, "a", "b"), sd.constr = c("b", 
                                                                                                                         "a", sigma, "a", "b"), sigma = rep(1, 
                                                                                                                                                            5)))
              if (is.na(ss) == FALSE) 
                res[[iGr]] <- try(normalmixEM(subset(tygmr, 
                                                     LE_L5MET == iGr)$SI_SP[-idx], maxit = 1000, 
                                              k = ss, maxrestarts = 20, mean.constr = constraintmn, 
                                              sd.constr = constraintsd, sigma = rep(1, 
                                                                                    length(constraintsd))))
            }
            else {
              res[[iGr]] <- try(normalmixEM(subset(tygmr, 
                                                   LE_L5MET == iGr)$SI_SP[-idx], maxit = 1000, 
                                            k = 5, maxrestarts = 20, mean.constr = c("-b", 
                                                                                     "-a", 0, "a", "b"), sd.constr = c("b", 
                                                                                                                       "a", sigma, "a", "b"), sigma = rep(1, 
                                                                                                                                                          5)))
            }
          }
        }
        if (plot == TRUE) 
          plot(res[[iGr]], 2, breaks = 100, xlim = c(-20, 
                                                     20))
      }
      if (level == "vessel") {
        for (iGr in unique(tyg$LE_L5MET)) if (!class(res[[iGr]]) == 
                                              "try-error") {
          res[[iGr]] <- res[[iGr]]$mu
        }
        else {
          res[[iGr]] <- rep(NA, 5)
        }
        res <- lapply(res, function(x) {
          if (class(x) == "try-error") {
            x <- rep(NA, 5)
          }
          else {
            x
          }
        })
        res <- lapply(res, sort)
      }
      if (level == "vessel") {
        if (nrow(tygmr) > 40) 
          shipList <- names(which((rowSums(table(tygmr$VE_REF, 
                                                 tygmr$SI_SP)) - table(tygmr$VE_REF, tygmr$SI_SP)[, 
                                                                                                  "0"]) > 20))
        shipFit <- list()
        if (exists("shipList")) {
          for (iShip in shipList) {
            tbl <- table(subset(tygmr, VE_REF == iShip)$SI_SP)
            spd <- an(names(rev(sort(tbl))[1]))
            idx <- which(subset(tygmr, VE_REF == iShip)$SI_SP == 
                           spd)
            nxt <- ifelse(names(rev(sort(tbl))[1]) == 
                            ac(spd), ifelse(abs(an(names(rev(sort(tbl))[2]))) == 
                                              abs(spd), names(rev(sort(tbl))[3]), names(rev(sort(tbl))[2])), 
                          names(rev(sort(tbl))[1]))
            if (tbl[ac(spd)]/tbl[nxt] > 5) {
              idx <- sample(idx, tbl[ac(spd)] - tbl[nxt] * 
                              2, replace = FALSE)
              if (length(which(abs(an(names(tbl))) %in% 
                               spd)) > 1) 
                idx <- c(idx, sample(which(subset(tygmr, 
                                                  VE_REF == iShip)$SI_SP == (-1 * spd)), 
                                     tbl[ac(-1 * spd)] - tbl[nxt] * 2, replace = FALSE))
            }
            else {
              idx <- -1:-nrow(subset(tygmr, VE_REF == 
                                       iShip))
            }
            shipTacsat <- subset(tygmr, VE_REF == iShip)
            if (length(res[[names(which.max(table(shipTacsat$LE_L5MET)))]]) == 
                3) {
              constraintmn <- c("-a", 0, "a")
            }
            else {
              constraintmn <- c("-b", "-a", 0, "a", "b")
            }
            if (length(res[[names(which.max(table(shipTacsat$LE_L5MET)))]]) == 
                3) {
              constraintsd <- c("a", "b", "a")
            }
            else {
              constraintsd <- c("b", "a", 0.911, "a", 
                                "b")
            }
            shipFit[[iShip]] <- try(normalmixEM(shipTacsat$SI_SP[-idx], 
                                                mu = res[[names(which.max(table(shipTacsat$LE_L5MET)))]], 
                                                maxit = 2000, sigma = rep(1, length(constraintsd)), 
                                                mean.constr = constraintmn, sd.constr = constraintsd))
            if (class(shipFit[[iShip]]) != "try-error") {
              mu <- shipFit[[iShip]]$mu
              sds <- shipFit[[iShip]]$sigma
              probs <- dnorm(x = shipTacsat$SI_SP, mean = mu[ceiling(length(mu)/2)], 
                             sd = sds[ceiling(length(mu)/2)])
              for (i in (ceiling(length(mu)/2) + 1):length(mu)) probs <- cbind(probs, 
                                                                               dnorm(x = shipTacsat$SI_SP, mean = mu[i], 
                                                                                     sd = sds[i]))
              SI_STATE <- apply(probs, 1, which.max)
              if (length(mu) == 3) {
                SI_STATE <- af(SI_STATE)
                levels(SI_STATE) <- c("f", "s")
                SI_STATE <- ac(SI_STATE)
              }
              if (length(mu) == 5) {
                SI_STATE <- af(SI_STATE)
                levels(SI_STATE) <- c("h", "f", "s")
                SI_STATE <- ac(SI_STATE)
              }
              tacsat$SI_STATE[which(tacsat$ID %in% shipTacsat$ID)] <- SI_STATE[1:(length(SI_STATE)/2)]
            }
            else {
              tacsat$SI_STATE[which(tacsat$ID %in% shipTacsat$ID)] <- NA
            }
          }
        }
      }
      else {
        for (iGr in unique(tyg$LE_L5MET)) {
          if (!class(res[[iGr]]) == "try-error") {
            mu <- res[[iGr]]$mu
            sds <- res[[iGr]]$sigma
            probs <- dnorm(x = subset(tyg, LE_L5MET == 
                                        iGr)$SI_SP, mean = mu[ceiling(length(mu)/2)], 
                           sd = sds[ceiling(length(mu)/2)])
            for (i in (ceiling(length(mu)/2) + 1):length(mu)) probs <- cbind(probs, 
                                                                             dnorm(x = subset(tyg, LE_L5MET == iGr)$SI_SP, 
                                                                                   mean = mu[i], sd = sds[i]))
            SI_STATE <- apply(probs, 1, which.max)
            if (length(mu) == 3) {
              SI_STATE <- af(SI_STATE)
              levels(SI_STATE) <- c("f", "s")
              SI_STATE <- ac(SI_STATE)
            }
            if (length(mu) == 5) {
              SI_STATE <- af(SI_STATE)
              levels(SI_STATE) <- c("h", "f", "s")
              SI_STATE <- ac(SI_STATE)
            }
            tacsat$SI_STATE[which(tacsat$ID %in% subset(tyg, 
                                                        LE_L5MET == iGr)$ID)] <- SI_STATE
          }
        }
      }
      if (nrow(tngmr) > 40) 
        nonshipList <- names(which((rowSums(table(tngmr$VE_REF, 
                                                  tngmr$SI_SP)) - table(tngmr$VE_REF, tngmr$SI_SP)[, 
                                                                                                   "0"]) > 20))
      nonshipFit <- list()
      if (exists("nonshipList")) {
        for (iShip in nonshipList) {
          tbl <- table(subset(tngmr, VE_REF == iShip)$SI_SP)
          spd <- an(names(rev(sort(tbl))[1]))
          idx <- which(subset(tngmr, VE_REF == iShip)$SI_SP == 
                         spd)
          nxt <- ifelse(names(rev(sort(tbl))[1]) == ac(spd), 
                        ifelse(abs(an(names(rev(sort(tbl))[2]))) == 
                                 abs(spd), names(rev(sort(tbl))[3]), names(rev(sort(tbl))[2])), 
                        names(rev(sort(tbl))[1]))
          if (tbl[ac(spd)]/tbl[nxt] > 5) {
            idx <- sample(idx, tbl[ac(spd)] - tbl[nxt] * 
                            2, replace = FALSE)
            if (length(which(abs(an(names(tbl))) %in% 
                             spd)) > 1) 
              idx <- c(idx, sample(which(subset(tngmr, 
                                                VE_REF == iShip)$SI_SP == (-1 * spd)), 
                                   tbl[ac(-1 * spd)] - tbl[nxt] * 2, replace = FALSE))
          }
          else {
            idx <- -1:-nrow(subset(tngmr, VE_REF == iShip))
          }
          shipTacsat <- subset(tngmr, VE_REF == iShip)
          if (exists("shipFit")) {
            if (iShip %in% names(shipFit)) {
              if (length(shipFit[[iShip]]$mu) == 3) {
                constraintmn <- c("-a", 0, "a")
              }
              else {
                constraintmn <- c("-b", "-a", 0, "a", 
                                  "b")
              }
              if (length(shipFit[[iShip]]$mu) == 3) {
                constraintsd <- c("a", "b", "a")
              }
              else {
                constraintsd <- c("b", "a", 0.911, "a", 
                                  "b")
              }
              nonshipFit[[iShip]] <- try(normalmixEM(shipTacsat$SI_SP[-idx], 
                                                     k = length(shipFit[[iShip]]$mu), maxit = 2000, 
                                                     sigma = rep(1, length(constraintsd)), 
                                                     mean.constr = constraintmn, sd.constr = constraintsd))
            }
            else {
              nonshipFit[[iShip]] <- try(normalmixEM(shipTacsat$SI_SP[-idx], 
                                                     k = 5, maxit = 2000, mean.constr = c("-b", 
                                                                                          "-a", 0, "a", "b"), sd.constr = c("b", 
                                                                                                                            "a", 0.911, "a", "b"), sigma = rep(1, 
                                                                                                                                                               5)))
            }
            if (!class(nonshipFit[[iShip]]) == "try-error") {
              mu <- nonshipFit[[iShip]]$mu
              sds <- nonshipFit[[iShip]]$sds
              probs <- dnorm(x = shipTacsat$SI_SP, mean = mu[ceiling(length(mu)/2)], 
                             sd = sds[ceiling(length(mu)/2)])
              for (i in (ceiling(length(mu)/2) + 1):length(mu)) probs <- cbind(probs, 
                                                                               dnorm(x = shipTacsat$SI_SP, mean = mu[i], 
                                                                                     sd = sds[i]))
              SI_STATE <- apply(probs, 1, which.max)
              if (length(mu) == 3) {
                SI_STATE <- af(SI_STATE)
                levels(SI_STATE) <- c("f", "s")
                SI_STATE <- ac(SI_STATE)
              }
              if (length(mu) == 5) {
                SI_STATE <- af(SI_STATE)
                levels(SI_STATE) <- c("h", "f", "s")
                SI_STATE <- ac(SI_STATE)
              }
              tacsat$SI_STATE[which(tacsat$ID %in% shipTacsat$ID)] <- SI_STATE[1:(length(SI_STATE)/2)]
            }
          }
        }
      }
    }
    if ("VE_REF" %in% colnames(tacsat) & analyse.by == "VE_REF") {
      vesselList <- names(which((rowSums(table(sTacsat$VE_REF, 
                                               sTacsat$SI_SP)) - table(sTacsat$VE_REF, sTacsat$SI_SP)[, 
                                                                                                      "0"]) > 40))
      tyv <- subset(sTacsat, is.na(VE_REF) == FALSE & VE_REF %in% 
                      vesselList)
      tyvmr <- tyv
      tyvmr$SI_SP <- -1 * tyvmr$SI_SP
      tyvmr <- rbind(tyv, tyvmr)
      tnv <- subset(sTacsat, is.na(VE_REF) == TRUE | !VE_REF %in% 
                      vesselList)
      tnvmr <- tnv
      tnvmr$SI_SP <- -1 * tnvmr$SI_SP
      tnvmr <- rbind(tnv, tnvmr)
      if (nrow(tyv) > 40) 
        shipList <- names(which((rowSums(table(tyvmr$VE_REF, 
                                               tyvmr$SI_SP)) - table(tyvmr$VE_REF, tyvmr$SI_SP)[, 
                                                                                                "0"]) > 20))
      shipFit <- list()
      if (exists("shipList")) {
        for (iShip in shipList) {
          tbl <- table(subset(tyvmr, VE_REF == iShip)$SI_SP)
          spd <- an(names(rev(sort(tbl))[1]))
          idx <- which(subset(tyvmr, VE_REF == iShip)$SI_SP == 
                         spd)
          nxt <- ifelse(names(rev(sort(tbl))[1]) == ac(spd), 
                        ifelse(abs(an(names(rev(sort(tbl))[2]))) == 
                                 abs(spd), names(rev(sort(tbl))[3]), names(rev(sort(tbl))[2])), 
                        names(rev(sort(tbl))[1]))
          if (tbl[ac(spd)]/tbl[nxt] > 5) {
            idx <- sample(idx, tbl[ac(spd)] - tbl[nxt] * 
                            2, replace = FALSE)
            if (length(which(abs(an(names(tbl))) %in% 
                             spd)) > 1) 
              idx <- c(idx, sample(which(subset(tyvmr, 
                                                VE_REF == iShip)$SI_SP == (-1 * spd)), 
                                   tbl[ac(-1 * spd)] - tbl[nxt] * 2, replace = FALSE))
          }
          else {
            idx <- -1:-nrow(subset(tyvmr, VE_REF == iShip))
          }
          shipTacsat <- subset(tyvmr, VE_REF == iShip)
          if (is.null(storeScheme) == TRUE) {
            shipFit[[iShip]] <- try(normalmixEM(subset(tyvmr, 
                                                       VE_REF == iShip)$SI_SP[-idx], maxit = 2000, 
                                                k = 5, mean.constr = c("-b", "-a", 0, "a", 
                                                                       "b"), sd.constr = c("b", "a", 0.911, 
                                                                                           "a", "b"), sigma = rep(1, 5)))
          }
          else {
            if ("means" %in% colnames(storeScheme)) {
              ss <- storeScheme[which(storeScheme$years == 
                                        yr & storeScheme$months == mth & storeScheme$weeks == 
                                        wk & storeScheme$analyse.by == iShip), 
                                "means"]
              sigma <- anf(storeScheme[which(storeScheme$years == 
                                               yr & storeScheme$months == mth & storeScheme$weeks == 
                                               wk & storeScheme$analyse.by == iShip), 
                                       "sigma0"])
              fixPeaks <- ac(storeScheme[which(storeScheme$years == 
                                                 yr & storeScheme$months == mth & storeScheme$weeks == 
                                                 wk & storeScheme$analyse.by == iShip), 
                                         "fixPeaks"])
              if (length(c(na.omit(as.numeric(strsplit(ss, 
                                                       " ")[[1]])))) == 3) {
                constraintmn <- c("-a", 0, "a")
              }
              else {
                constraintmn <- c("-b", "-a", 0, "a", 
                                  "b")
              }
              if (length(c(na.omit(as.numeric(strsplit(ss, 
                                                       " ")[[1]])))) == 3) {
                constraintsd <- c("a", "b", "a")
              }
              else {
                constraintsd <- c("b", "a", sigma, "a", 
                                  "b")
              }
              if (fixPeaks) 
                constraintmn <- c(na.omit(anf(unlist(strsplit(ss, 
                                                              " ")))))
              shipFit[[iShip]] <- try(normalmixEM(subset(tyvmr, 
                                                         VE_REF == iShip)$SI_SP[-idx], mu = c(na.omit(as.numeric(strsplit(ss, 
                                                                                                                          " ")[[1]]))), maxit = 2000, mean.constr = constraintmn, 
                                                  sd.constr = constraintsd, sigma = rep(1, 
                                                                                        length(constraintsd))))
            }
            else {
              ss <- storeScheme[which(storeScheme$years == 
                                        yr & storeScheme$months == mth & storeScheme$weeks == 
                                        wk & storeScheme$analyse.by == iShip), 
                                "peaks"]
              sigma <- anf(storeScheme[which(storeScheme$years == 
                                               yr & storeScheme$months == mth & storeScheme$weeks == 
                                               wk & storeScheme$analyse.by == iShip), 
                                       "sigma0"])
              fixPeaks <- ac(storeScheme[which(storeScheme$years == 
                                                 yr & storeScheme$months == mth & storeScheme$weeks == 
                                                 wk & storeScheme$analyse.by == iShip), 
                                         "fixPeaks"])
              if (ss == 3) {
                constraintmn <- c("-a", 0, "a")
              }
              else {
                constraintmn <- c("-b", "-a", 0, "a", 
                                  "b")
              }
              if (ss == 3) {
                constraintsd <- c("a", "b", "a")
              }
              else {
                constraintsd <- c("b", "a", sigma, "a", 
                                  "b")
              }
              if (length(ss) > 0) {
                if (is.na(ss) == TRUE) 
                  shipFit[[iShip]] <- try(normalmixEM(subset(tyvmr, 
                                                             VE_REF == iShip)$SI_SP[-idx], maxit = 2000, 
                                                      k = 5, mean.constr = c("-b", "-a", 
                                                                             0, "a", "b"), sd.constr = c("b", 
                                                                                                         "a", sigma, "a", "b"), sigma = rep(1, 
                                                                                                                                            5)))
                if (is.na(ss) == FALSE) 
                  shipFit[[iShip]] <- try(normalmixEM(subset(tyvmr, 
                                                             VE_REF == iShip)$SI_SP[-idx], maxit = 2000, 
                                                      k = ss, mean.constr = constraintmn, 
                                                      sd.constr = constraintsd, sigma = rep(1, 
                                                                                            length(constraintsd))))
              }
              else {
                shipFit[[iShip]] <- try(normalmixEM(subset(tyvmr, 
                                                           VE_REF == iShip)$SI_SP[-idx], maxit = 2000, 
                                                    k = 5, mean.constr = c("-b", "-a", 
                                                                           0, "a", "b"), sd.constr = c("b", 
                                                                                                       "a", sigma, "a", "b"), sigma = rep(1, 
                                                                                                                                          5)))
              }
            }
          }
          if (plot == TRUE) 
            plot(shipFit[[iShip]], 2, breaks = 100, xlim = c(-20, 
                                                             20))
          if (!class(shipFit[[iShip]]) == "try-error") {
            mu <- shipFit[[iShip]]$mu
            sds <- shipFit[[iShip]]$sigma
            probs <- dnorm(x = shipTacsat$SI_SP, mean = mu[ceiling(length(mu)/2)], 
                           sd = sds[ceiling(length(mu)/2)])
            for (i in (ceiling(length(mu)/2) + 1):length(mu)) probs <- cbind(probs, 
                                                                             dnorm(x = shipTacsat$SI_SP, mean = mu[i], 
                                                                                   sd = sds[i]))
            SI_STATE <- apply(probs, 1, which.max)
            if (length(mu) == 3) {
              SI_STATE <- af(SI_STATE)
              levels(SI_STATE) <- c("f", "s")
              SI_STATE <- ac(SI_STATE)
            }
            if (length(mu) == 5) {
              SI_STATE <- af(SI_STATE)
              levels(SI_STATE) <- c("h", "f", "s")
              SI_STATE <- ac(SI_STATE)
            }
            tacsat$SI_STATE[which(tacsat$ID %in% shipTacsat$ID)] <- SI_STATE[1:(length(SI_STATE)/2)]
          }
        }
      }
      if (nrow(tnvmr) > 40) 
        nonshipList <- names(which((rowSums(table(tnvmr$VE_REF, 
                                                  tnvmr$SI_SP)) - table(tnvmr$VE_REF, tnvmr$SI_SP)[, 
                                                                                                   "0"]) > 20))
      nonshipFit <- list()
      if (exists("nonshipList")) {
        for (iShip in nonshipList) {
          tbl <- table(subset(tnvmr, VE_REF == iShip)$SI_SP)
          spd <- an(names(rev(sort(tbl))[1]))
          idx <- which(subset(tnvmr, VE_REF == iShip)$SI_SP == 
                         spd)
          nxt <- ifelse(names(rev(sort(tbl))[1]) == ac(spd), 
                        ifelse(abs(an(names(rev(sort(tbl))[2]))) == 
                                 abs(spd), names(rev(sort(tbl))[3]), names(rev(sort(tbl))[2])), 
                        names(rev(sort(tbl))[1]))
          if (tbl[ac(spd)]/tbl[nxt] > 5) {
            idx <- sample(idx, tbl[ac(spd)] - tbl[nxt] * 
                            2, replace = FALSE)
            if (length(which(abs(an(names(tbl))) %in% 
                             spd)) > 1) 
              idx <- c(idx, sample(which(subset(tnvmr, 
                                                VE_REF == iShip)$SI_SP == (-1 * spd)), 
                                   tbl[ac(-1 * spd)] - tbl[nxt] * 2, replace = FALSE))
          }
          else {
            idx <- -1:-nrow(subset(tnvmr, VE_REF == iShip))
          }
          shipTacsat <- subset(tnvmr, VE_REF == iShip)
          if (length(shipFit[[iShip]]$mu) == 3) {
            constraintmn <- c("-a", 0, "a")
          }
          else {
            constraintmn <- c("-b", "-a", 0, "a", "b")
          }
          if (length(shipFit[[iShip]]$mu) == 3) {
            constraintsd <- c("a", "b", "a")
          }
          else {
            constraintsd <- c("b", "a", 0.911, "a", "b")
          }
          nonshipFit[[iShip]] <- try(normalmixEM(shipTacsat$SI_SP[-idx], 
                                                 k = length(shipFit[[iShip]]$mu), maxit = 2000, 
                                                 mean.constr = constraintmn, sd.constr = constraintsd, 
                                                 sigma = rep(1, length(constraintsd))))
          if (!class(nonshipFit[[iShip]]) == "try-error") {
            mu <- nonshipFit[[iShip]]$mu
            sds <- nonshipFit[[iShip]]$sigma
            probs <- dnorm(x = shipTacsat$SI_SP, mean = mu[ceiling(length(mu)/2)], 
                           sd = sds[ceiling(length(mu)/2)])
            for (i in (ceiling(length(mu)/2) + 1):length(mu)) probs <- cbind(probs, 
                                                                             dnorm(x = shipTacsat$SI_SP, mean = mu[i], 
                                                                                   sd = sds[i]))
            SI_STATE <- apply(probs, 1, which.max)
            if (length(mu) == 3) 
              SI_STATE <- af(SI_STATE)
            levels(SI_STATE) <- c("f", "s")
            SI_STATE <- ac(SI_STATE)
            if (length(mu) == 5) 
              SI_STATE <- af(SI_STATE)
            levels(SI_STATE) <- c("h", "f", "s")
            SI_STATE <- ac(SI_STATE)
            tacsat$SI_STATE[which(tacsat$ID %in% shipTacsat$ID)] <- SI_STATE[1:(length(SI_STATE)/2)]
          }
        }
      }
    }
  }
  leftOverTacsat <- tacsatOrig[which(!tacsatOrig$ID %in% tacsat$ID), 
  ]
  tacsat <- rbind(tacsat, leftOverTacsat)
  tacsat <- orderBy(~ID, tacsat)
  cat("Note that in case of 5 peaks: no fishing = h, fishing = f, steaming / no fishing = s\n")
  cat("Note that in case of 3 peaks: fishing = f, steaming / no fishing = s\n")
  return(tacsat$SI_STATE)
}




## The  overlap_trips function created by Jeppe ICES , remove all trips that were overlapping. 
## This modified version correct the end and start date of overlapping trips  


overlap_trips =   function  ( eflalo   ) { 
      
      eflalo <- orderBy(~ VE_COU + VE_REF + FT_DDATIM + FT_LDATIM, data = eflalo)
      
      # If a trip (same depart and return times) has more than one FT_REF, make them all into the same (first) FT_REF. 
      dt1 <- data.table(eflalo)[,.(VE_REF, FT_REF, FT_DDATIM, FT_LDATIM)]
      
      dt1 <- unique(dt1, by = c("VE_REF", "FT_REF"))
      
      setkey(dt1, VE_REF, FT_DDATIM, FT_LDATIM)
      dt2 <- dt1[, ref := .N > 1, by = key(dt1)][ref == T]
      
      dt3 <- dt2[,.(FT_REF_NEW = FT_REF[1]), by = .(VE_REF, FT_DDATIM, FT_LDATIM)]
      
      dt4 <- merge(dt2, dt3)
      
      eflalo2 <- merge(data.table(eflalo), dt4, all.x = T)
      eflalo2[!is.na(FT_REF_NEW), FT_REF := FT_REF_NEW]
      eflalo2[, FT_REF_NEW := NULL]
      
      eflalo <- data.frame(eflalo2)
      
      eflalo <- eflalo %>% select(-ref)
      
      # Create a data table 'dt1' with the necessary columns from 'eflalo'
      dt1 <- data.table(ID = eflalo$VE_REF, FT = eflalo$FT_REF,
                        startdate = eflalo$FT_DDATIM,
                        enddate = eflalo$FT_LDATIM)
      
      # Remove duplicate rows from 'dt1'
      dt1 <- dt1[!duplicated(paste(dt1$ID, dt1$FT)), ]
      
      # Set keys for 'dt1' for efficient joining and overlapping
      setkey(dt1, ID, startdate, enddate)
      
      # Find overlapping trips in 'dt1'
      result <- foverlaps(dt1, dt1, by.x = c("ID", "startdate", "enddate"),
                          by.y = c("ID", "startdate", "enddate"))
      
      # Filter 'result' to get only the rows where trips overlap
      overlapping.trips <- subset(result, startdate < i.enddate & enddate > i.startdate & FT != i.FT)
      
      # If there are overlapping trips, remove them from 'eflalo' and save them to a file
      if (nrow(overlapping.trips) > 0) {
        eflalo <- eflalo[!eflalo$FT_REF %in% overlapping.trips$FT, ]
        
        print("THERE ARE OVERLAPPING TRIPS IN THE DATASET -> SEE THE FILE overlappingTrips SAVED IN THE RESULTS FOLDER")
      }



return(list (eflalo ,overlapping.trips )  )
} 


### WORK ON PROGRESS: FUNTION TO CORECT THE START AND END DATES OF THE OVERLAPPING FISHING TRIPS
# 
# overlap_trips =   function  ( eflalo_df  ) { 
#   
#   # Create a data table 'dt1' with the necessary columns from 'eflalo'
#   
#   dt1 <- data.table(ID = eflalo$VE_REF, FT = eflalo$FT_REF,
#                     startdate = eflalo$FT_DDATIM,
#                     enddate = eflalo$FT_LDATIM)
#   
#   # Get only unique  the VE_REF and FT_REF . Remove  duplicate rows from 'dt1'. Due to EFLALO format many EFLALO-P1 rows are duplicated . 
#   
#   dt1 <- dt1[!duplicated(paste(dt1$ID, dt1$FT)), ]
#   
#   
#   # Set keys for 'dt1' for efficient joining and overlapping
#   
#   setkey(dt1, ID, startdate, enddate)
#   
#   # Find overlapping trips in 'dt1' using FOVERLAPS function. The last two columns in both by.x and by.y should each 
#   # correspond to the start and end interval columns in x and y respectively.
#   
#   result <- data.table::foverlaps(dt1, dt1, 
#                                   by.x = c("ID", "startdate", "enddate"),
#                                   by.y = c("ID", "startdate", "enddate")   ) 
#   
#   # Filter 'result' to get only the rows where trips overlap
#   overlapping.trips = subset(result, startdate < i.enddate & enddate > i.startdate & FT != i.FT)
#   
#   overlapping.trips = orderBy(~ ID +  startdate + enddate, data = overlapping.trips)
#   
#   ## ifelse has a problem  with the date formats , so it is used REPLACE function instead
#   
#   ## Get the NEW STARTING DATES as THE END DATE OF THE PREVIOUS TRIP and ADD 30 seconds
#   
#   overlapping.trips  = overlapping.trips  |>  
#     mutate  ( tot_FT = FT + i.FT ) |> 
#     group_by(ID, tot_FT ) |> 
#     mutate (i.startdate = dplyr::lag ( enddate  )  ) |> 
#     mutate (  startdate = replace(  i.startdate + dseconds( x= 30)   , is.na ( i.startdate) , startdate  ) )  |> 
#     ungroup() |> 
#     select ( ID, FT, startdate, enddate) |> 
#     ungroup()  |> group_by(ID, FT ) |> 
#     mutate( u_ft = row_number() ) |> 
#     filter ( u_ft == 1 ) |>  select ( -u_ft ) |> 
#     as.data.frame() # |>  filter (ID ==  'A10795'  ) |> 
#   
#   ## Discard the trips with recalculated  landing dates before departure date trips 
#   
#   idx <- which(overlapping.trips$enddate > overlapping.trips$startdate)
#   overlapping.trips_corrected =  overlapping.trips[idx,]
#   
#   ## Select and update the trips new calculated departure and return dates
#   
#   eflalo_corrected = eflalo_df |>
#     left_join(overlapping.trips_corrected , by =  join_by(FT_REF  == FT , VE_REF == ID)) |> 
#     mutate ( FT_DDATIM =   lubridate::as_datetime  ( ifelse ( is.na(startdate),
#                                                               as.character( FT_DDATIM ), 
#                                                               as.character(startdate)) )  , 
#              FT_LDATIM =   lubridate::as_datetime  ( ifelse ( is.na(enddate),
#                                                               as.character( FT_LDATIM ), 
#                                                               as.character(enddate)) )     )  |> 
#     select( - startdate,  -enddate)  
#   
#   
#   ## Select the  trips with recalculated  landing dates before departure date trips 
#   
#   idx <- which(overlapping.trips$enddate <= overlapping.trips$startdate)
#   overlapping.trips_erroneus =  overlapping.trips[idx,]
#   
#   ## Select the erroneus tips landings reported. There were overlapping trips and when recalcualted 
#   ## the landing dats is equal or earlier than departure dates
#   
#   eflalo_erroneus = eflalo_df  |>
#     inner_join(overlapping.trips_erroneus , by =  join_by(FT_REF  == FT , VE_REF == ID))   |> 
#     select( - startdate,  -enddate)  
#   
#   return(list (eflalo_corrected ,eflalo_erroneus )  )
# }



# Define a function to add gear width to metier
add_gearwidth <- function(x, met_name = "LE_MET", oal_name = "VE_LEN", kw_name = "VE_KW"){
  
  require(data.table)
  require(dplyr)
  require(sfdSAR)
  require(icesVMS)
  
  setDT(x)
  ID <- c(oal_name, kw_name)
  x[,(ID):= lapply(.SD, as.numeric), .SDcols = ID]
  x[, Metier_level6 := get(met_name)]
  
  
  #Updated metiers
  metier_lookup <- fread("https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv")
  
  if(any(x[, get(met_name)] %!in% metier_lookup$Metier_level6))
    stop(paste("Non valid metiers in tacsatEflalo:", paste(x[x[, get(met_name)] %!in% metier_lookup$Metier_level6][, get(met_name)], collapse = ", ")))
  
  gear_widths <- get_benthis_parameters()
  
  aux_lookup <- data.table(merge(gear_widths, metier_lookup, by.x = "benthisMet", by.y = "Benthis_metiers", all.y = T))
  aux_lookup <- aux_lookup[,.(Metier_level6, benthisMet, avKw, avLoa, avFspeed, subsurfaceProp, gearWidth, firstFactor, secondFactor, gearModel, 
                              gearCoefficient, contactModel)]
  
  aux_lookup <<- unique(aux_lookup)
  
  
  aux_lookup <- data.table(merge(gear_widths, metier_lookup, by.x = "benthisMet", by.y = "Benthis_metiers", all.y = T))
  aux_lookup <- aux_lookup[,.(Metier_level6, benthisMet, avKw, avLoa, avFspeed, subsurfaceProp, gearWidth, firstFactor, secondFactor, gearModel, 
                              gearCoefficient, contactModel)]
  
  aux_lookup[gearCoefficient == "avg_kw", gearCoefficient := kw_name]
  aux_lookup[gearCoefficient == "avg_oal", gearCoefficient := oal_name]
  
  aux_lookup <- unique(aux_lookup)
  
  vms <- x |> 
    left_join(aux_lookup, by = "Metier_level6")
  
  vms$gearWidth_model <-
    predict_gear_width(vms$gearModel, vms$gearCoefficient, vms)
  
  if("avg_gearWidth" %!in% names(vms))
    vms[, avg_gearWidth := NA]
  
  
  gearWidth_filled <-
    with(vms,
         ifelse(!is.na(avg_gearWidth), avg_gearWidth,
                ifelse(!is.na(gearWidth_model), gearWidth_model,
                       gearWidth)
         ))
  
  return(gearWidth_filled)
}



add_swept_area <- function(x, met_name = "LE_MET", oal_name = "VE_LEN", kw_name = "VE_KW", width_name = "GEARWIDTH"){
  
  require(data.table)
  require(dplyr)
  require(sfdSAR)
  require(icesVMS)
  
  setDT(x)
  ID <- c(oal_name, kw_name)
  x[,(ID):= lapply(.SD, as.numeric), .SDcols = ID]
  x[, Metier_level6 := get(met_name)]
  
  
  #Updated metiers
  metier_lookup <- fread("https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv")
  
  if(any(x[, get(met_name)] %!in% metier_lookup$Metier_level6))
    stop(paste("Non valid metiers in tacsatEflalo:", paste(x[x[, get(met_name)] %!in% metier_lookup$Metier_level6][, get(met_name)], collapse = ", ")))
  
  gear_widths <- get_benthis_parameters()
  
  aux_lookup <- data.table(merge(gear_widths, metier_lookup, by.x = "benthisMet", by.y = "Benthis_metiers", all.y = T))
  aux_lookup <- aux_lookup[,.(Metier_level6, benthisMet, avKw, avLoa, avFspeed, subsurfaceProp, gearWidth, firstFactor, secondFactor, gearModel, 
                              gearCoefficient, contactModel)]
  
  aux_lookup <<- unique(aux_lookup)
  
  
  aux_lookup <- data.table(merge(gear_widths, metier_lookup, by.x = "benthisMet", by.y = "Benthis_metiers", all.y = T))
  aux_lookup <- aux_lookup[,.(Metier_level6, benthisMet, avKw, avLoa, avFspeed, subsurfaceProp, gearWidth, firstFactor, secondFactor, gearModel, 
                              gearCoefficient, contactModel)]
  
  aux_lookup[gearCoefficient == "avg_kw", gearCoefficient := kw_name]
  aux_lookup[gearCoefficient == "avg_oal", gearCoefficient := oal_name]
  
  aux_lookup <- unique(aux_lookup)
  
  vms <- x |> 
    left_join(aux_lookup, by = "Metier_level6")
  
  vms$surface <-
    predict_surface_contact(vms$contactModel,
                            vms$INTV,
                            vms$GEARWIDTH,
                            vms$SI_SP)
  
  vms[contactModel == "trawl_contact", surface := surface * 1000]
  
  return(vms$surface)
}




add_subsurface_swept_area <- function(x, met_name = "LE_MET", oal_name = "VE_LEN", kw_name = "VE_KW", width_name = "GEARWIDTH"){
  
  require(data.table)
  require(dplyr)
  require(sfdSAR)
  require(icesVMS)
  
  setDT(x)
  ID <- c(oal_name, kw_name)
  x[,(ID):= lapply(.SD, as.numeric), .SDcols = ID]
  x[, Metier_level6 := get(met_name)]
  
  
  #Updated metiers
  metier_lookup <- fread("https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv")
  
  if(any(x[, get(met_name)] %!in% metier_lookup$Metier_level6))
    stop(paste("Non valid metiers in tacsatEflalo:", paste(x[x[, get(met_name)] %!in% metier_lookup$Metier_level6][, get(met_name)], collapse = ", ")))
  
  gear_widths <- get_benthis_parameters()
  
  aux_lookup <- data.table(merge(gear_widths, metier_lookup, by.x = "benthisMet", by.y = "Benthis_metiers", all.y = T))
  aux_lookup <- aux_lookup[,.(Metier_level6, benthisMet, avKw, avLoa, avFspeed, subsurfaceProp, gearWidth, firstFactor, secondFactor, gearModel, 
                              gearCoefficient, contactModel)]
  
  aux_lookup <<- unique(aux_lookup)
  
  
  aux_lookup <- data.table(merge(gear_widths, metier_lookup, by.x = "benthisMet", by.y = "Benthis_metiers", all.y = T))
  aux_lookup <- aux_lookup[,.(Metier_level6, benthisMet, avKw, avLoa, avFspeed, subsurfaceProp, gearWidth, firstFactor, secondFactor, gearModel, 
                              gearCoefficient, contactModel)]
  
  aux_lookup[gearCoefficient == "avg_kw", gearCoefficient := kw_name]
  aux_lookup[gearCoefficient == "avg_oal", gearCoefficient := oal_name]
  
  aux_lookup <- unique(aux_lookup)
  
  vms <- x |> 
    left_join(aux_lookup, by = "Metier_level6")
  
  vms$surface <-
    predict_surface_contact(vms$contactModel,
                            vms$INTV,
                            vms$GEARWIDTH,
                            vms$SI_SP)
  
  vms[contactModel == "trawl_contact", surface := surface * 1000]
  
  # calculate subsurface contact
  vms$subsurface <- vms$surface * as.numeric(vms$subsurfaceProp) * .01
  
  
  return(vms$subsurface)
}
