library(vmstools)
library(lubridate)
library(doBy)
library(data.table)
library(icesVocab)
library(RPostgres)
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(here)
library(tidyr)
library(lubridate)

setwd("C:/Users/MD09/OneDrive - CEFAS/projects/datacalls/ices/2024")


# Set paths
path <- paste0(getwd(), "/") # Working directory
codePath  <- paste0(path, "Scripts/")   # Location to store R scripts
dataPath  <- paste0(path, "Data/")      # Location to store tacsat (VMS) and eflalo (logbook) data
outPath   <- paste0(path, "Results/")   # Location to store the results
plotPath  <- paste0(path, "Plots/") 

# Create directories if they don't exist
dir.create(codePath, showWarnings = T)
dir.create(dataPath, showWarnings = T)
dir.create(outPath, showWarnings = T)
dir.create(plotPath, showWarnings = T)

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
  suppressWarnings(tz[, (col) := NULL])
  
  eflalo_u_col = eflalo[eflalo$FT_REF %!in% e$FT_REF,] %>%  distinct (FT_REF, LE_GEAR )
  tacsatp_u_col =  tacsatp |> 
    left_join(eflalo_u_col, by = c("FT_REF" = "FT_REF"), relationship = "many-to-many")
  
  
  if(trust_logbook){
    
    ## First bind by landing date
    
    e2 <- e[,.(get(col)[length(unique(get(col))) == 1]), by = .(FT_REF, LE_CDAT)]
    names(e2) <- c("FT_REF", "LE_CDAT", col)
    
    tz <- tz |> 
      left_join(e2, by = c("FT_REF" = "FT_REF", "SI_DATE" = "LE_CDAT"), relationship = "many-to-many")
    
    tz <- unique(tz) %>%  as.data.frame()
    
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
    
    if(nrow(tz %>%  filter ( is.na ( LE_GEAR))) > 0){
      
      ft_ref_isna = tz %>%  filter ( is.na ( LE_GEAR)) %>%  distinct(FT_REF) %>% pull()
      tz2 = tz %>%  filter ( FT_REF %in% ft_ref_isna ) %>%  as.data.frame()
      e2 = e %>%  filter ( FT_REF %in% ft_ref_isna )
      
      
      if(!"LE_KG_TOT" %in% names(e2)){
        idxkgeur <- colnames(e2)[grepl("LE_KG_|LE_EURO_", colnames(e2))]
        # Calculate the total KG and EURO for each row
        e2$LE_KG_TOT <- rowSums(e2[,..idxkgeur], na.rm = TRUE)
      }
      
      highvalue <- e2[,.(LE_KG_TOT = sum(LE_KG_TOT, na.rm = T)), by = .(FT_REF, get(col))]
      highvalue <- highvalue[,.(get[which.max(LE_KG_TOT)]), by = .(FT_REF)]
      names(highvalue) <-  c("FT_REF", col)
      
      tx2 <- tz2 
      tx2 = tx2 %>%  select ( - any_of( col )  )
      tz2 <- tx2 %>%  inner_join ( highvalue, by =  "FT_REF")
      
    }
    
    
    tz =   tz %>%  filter(  !is.na ( LE_GEAR))   
    tz_all =  rbind (tz , tz2 )  
    
    
    tacsatp_fill <- rbindlist(  list( tacsatp_u_col , tz_all  )    , fill = T)
    
    
    return(tacsatp_fill)
    
  }
  
  
}