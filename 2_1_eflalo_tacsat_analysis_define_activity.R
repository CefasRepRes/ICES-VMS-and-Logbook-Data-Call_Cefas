

#'----------------------------------------------------------------------------
# 2.1.4 Define VMS Records Fishing Activity                               ----
#'----------------------------------------------------------------------------
#'
#' This is an ad-hoc analysis to be run when the spped profiles methods are reviewed.
#' The speed profiles resulting of this analysis requires to be checked against ground-truth data such observers data 
#
 

setwd("C:/Users/MD09/OneDrive - CEFAS/projects/datacalls/ices/2024")

source("C:\\Users\\MD09\\Documents\\git\\ICES-VMS-and-Logbook-Data-Call_Cefas\\global-subset.R")

year = 2023

## this script is an ad_hoc analysis to be run under request to review given fishing speed values
## YEAR_ANALYSIS of the ad_hoc analysis

year_analysis = 2024

load(file = paste0(outPath, paste0("/cleanEflalo", year,".RData")))
load(file = paste0(outPath, paste0("/cleanTacsat", year,".RData")))
load(file = paste0(outPath, paste0("/tacsatMerged", year,".RData")))

## Calculate SI_YEAR field with teh year in SI_DATIM

tacsatp = tacsatp  |>  mutate ( SI_YEAR = lubridate::year( SI_DATIM )  ) 


# Calculate time interval between points (NA :: CEFAS DOES IN GEOGFISH GDB )
 ## tacsatp <- intvTacsat(tacsatp, level = "trip", fill.na = TRUE)

# Reset values that are simply too high to 2x the regular interval rate   
# ( CEFAS DOES HAVE THE INTV in HOURS so transform intvthres in hours ) 

   tacsatp$INTV[tacsatp$INTV > intvThres/60   ] = 1.5 * ( intvThres/60 ) 
   
    tacsatp |> filter ( INTV > 6 ) |>  arrange(desc(INTV)) |> pull(INTV)

# Assume that pings with NA in INTV has the normal interval value

    tacsatp$INTV[is.na(tacsatp$INTV)] <- intvThres/60

# Remove points with NA's in them in critical places
    
dim(tacsatp)
    
    
idx <-
  which(
    is.na(tacsatp$VE_REF) == TRUE |
      is.na(tacsatp$SI_LONG) == TRUE |
      is.na(tacsatp$SI_LATI) == TRUE |
      is.na(tacsatp$SI_DATIM) == TRUE |
      is.na(tacsatp$SI_SP) == TRUE
  )
if (length(idx) > 0) {
  tacsatp <- tacsatp[-idx, ]
}


# Define speed thresholds associated with fishing for gears =====================


# Investigate speed pattern through visual inspection of histograms # 

# Create a histogram of speeds for different gears
# Start a new PNG device
# Create a histogram of speeds for different gears

diag.plot <- ggplot(data = tacsatp, aes(SI_SP)) +
  geom_histogram(aes(fill = LE_GEAR), breaks = seq(0, 20, by = 1), color = "white") +
  facet_wrap(~ LE_GEAR, ncol = 4, scales = "free_y") +
  labs(x = "Speed (knots)", y = "Frequency", title = "Histogram of Speeds by Gear") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(colour = "black"),
    axis.text.x = element_text(colour = "black"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 20),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey60", colour = "black", linewidth = 1),
    panel.background = element_blank()
  ) +
  scale_fill_manual(values = c("#000000", "#FCCF3F", "#FF0000", "#00FF00", "#0000FF",
                               "#FF00FF", "#808080", "#800000", "#808000",
                               "#008000", "#800080", "#008080", "#000080", "#666699", "#808080",
                               "#003366", "#CCA099", "#333300", "#993300", "#993366", "#333399",
                               "#333333"))

ggsave(diag.plot, filename = file.path(outPath, paste0("SpeedHistogram_", year, ".jpg")))
diag.plot

# start by correctly formatting the level 5 metier

tacsatp$LE_L5MET <-  sapply(strsplit(tacsatp$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  

# Create a data frame with minimum and maximum speed thresholds for each gear
speedarr <- as.data.frame(
  cbind(
    LE_L5MET = sort(unique(tacsatp$LE_L5MET)),
    min = NA,
    max = NA
  ),
  stringsAsFactors = FALSE
)

# Fill out the minimum and maximum speed thresholds
speedarr$min <- rep(1, nrow(speedarr)) # It is important to fill out the personally inspected thresholds here!
speedarr$max <- rep(6, nrow(speedarr))


# Analyse activity automated for common gears only. Use the speedarr for the other gears =============== 

subTacsat <- subset(tacsatp, LE_GEAR %in% autoDetectionGears)
nonsubTacsat <- subset(tacsatp, !LE_GEAR %in% autoDetectionGears)


 
if (visualInspection == TRUE){
  storeScheme <-
    ac.tac.anal(
      subTacsat,
      units = "year",
      analyse.by = "LE_L5MET",
      identify = "means")
 
}else  {
 
  storeScheme <-
    expand.grid(
      years = year,
      months = 0,
      weeks = 0,
      analyse.by = unique(subTacsat[,"LE_L5MET"])
    )
 
  storeScheme$peaks <- NA
  storeScheme$means <- NA
  storeScheme$fixPeaks <- FALSE
  storeScheme$sigma0 <- 0.911
  
  
  # Fill the storeScheme values based on analyses of the pictures = 
  
  storeScheme$LE_GEAR <- sapply(strsplit(as.character(storeScheme$analyse.by), "_"), `[`, 1)
  
  # Define mean values of the peaks and the number of peaks when they are different from 5 # 
  
  storeScheme$means[which(storeScheme$LE_GEAR == "TBB")] <- c("-11.5 -6 0 6 11.5")
  storeScheme$means[which(storeScheme$LE_GEAR == "OTB")] <- c("-9 -3 0 3 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "OTT")] <- c("-9 -3 0 3 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "OTM")] <- c("-9 -3 0 3 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "MIS")] <- c("-9 -3 0 3 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "SSC")] <- c("-9 0 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "LLD")] <- c("-9 0 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "LLS")] <- c("-9 0 9")
  storeScheme$means[which(storeScheme$LE_GEAR == "PTB")] <- c("-10 -3 0 3 10")
  storeScheme$means[which(storeScheme$LE_GEAR == "DRB")] <- c("-10 0 10")
  storeScheme$means[which(storeScheme$LE_GEAR == "HMD")] <- c("-9 0 9")
  storeScheme$peaks[which(storeScheme$LE_GEAR == "SSC")] <- 3
  storeScheme$peaks[which(storeScheme$LE_GEAR == "LLD")] <- 3
  storeScheme$peaks[which(storeScheme$LE_GEAR == "LLS")] <- 3
  storeScheme$peaks[which(storeScheme$LE_GEAR == "DRB")] <- 3
  storeScheme$peaks[which(storeScheme$LE_GEAR == "HMD")] <- 3
  storeScheme$peaks[which(is.na(storeScheme$peaks) == TRUE)] <- 5
  storeScheme <- storeScheme[,-(dim(storeScheme)[2])]
}

#  acTa <- ac.tac.anal(subTacsat, units = "year", storeScheme = storeScheme, analyse.by = "LE_L5MET", identify = "peaks")


save(
  storeScheme,
  file = file.path(outPath, paste0("storeschema_2023", year, ".RData")))

load (file = file.path(outPath, paste0("storeschema_2023", year, ".RData") ) ) 

 
## Filter the storeScheme for the given year 

storeScheme_year = storeScheme |> filter ( years == year)
 
sub_subTacsat = subTacsat  |>
                filter (LE_L5MET %in% unique(storeScheme_year   |> select(analyse.by) |> pull() ) )  |> 
                filter(SI_YEAR == year )


nonsub_subTacsat = subTacsat  |> 
                   filter (! LE_L5MET %in% unique(storeScheme_2023  |> select(analyse.by) |> pull() ) ) 


acTa <-
  act.tac(
    sub_subTacsat |> filter(SI_YEAR == 2023) ,
    units = "year",
    analyse.by = "LE_L5MET",
    storeScheme = storeScheme_2023 |>  filter (analyse.by %in% unique(subTacsat$LE_L5MET) ) ,
    plot = FALSE,
    level = "all")

tt =  ( sub_subTacsat |> filter(SI_YEAR == 2023) ) |>  as.data.frame()
 tt[lubridate::year( tt$SI_DATIM )  == 2024,  ] 


 
 if ( dim (nonsub_subTacsat)[1]  > 0  )  nonsubTacsat =  rbind(  nonsub_subTacsat , nonsubTacsat ) 
 
 subTacsat = sub_subTacsat  
 subTacsat$SI_STATE <- acTa
 
 
  

#' Obtain the range speeds result by gear type. 
#' This will obtain the model results ranges in a data frame and can be used to identify fishing VMS records 
#' in the rest of the TACSAT dataset

fishing_speed_matrix_autodetect <- subTacsat %>%
  filter(SI_STATE == "f") %>%
  group_by(LE_L5MET) %>%
  dplyr::summarise(
    min  = min(SI_SP),
    max  = max(SI_SP)
)



#### Apply the rules to round up/down the speed tresholds.
#' The rational behind the round up the maximum threshodl and  down the minimun is the 
#' limited number of VMS positions avialble due to the 2 hours  low-rate   positions reported by vessels
#' There are many fishing operations that last less than 2 hours , therefore may are missed due to the low-frequency transmition
#' It is preferable to retain VMS positions that are slightly above the ranges obtained with the model to get better definition
#' of the fishing grounds and a better estimate of the hours of fishing in these grounds.  

fishing_speed_matrix_autodetect_round = fishing_speed_matrix |>  mutate ( max = case_when( max <=4  ~ 4.5, 
                                            between ( max, 4, 4.5 ) ~ 5   , 
                                            between ( max, 4.5, 5 ) ~ 5.5   , 
                                            between ( max, 5, 5.5 ) ~ 6 , 
                                            between ( max, 5.5, 6 ) ~ 6.5, 
                                            between ( max, 6, 6.5 ) ~ 6.75,
                                            max >=6.5 ~7 ,
                                            .default = max  )  )  |>
  mutate ( min = case_when( min <= 1  ~ 1, 
                            between ( min, 1, 1.5 ) ~ 1.25   , 
                            between ( min, 1.5, 4 ) ~ 1.5    ,
                            .default = min  )  )


print(fishing_speed_matrix_autodetect_round |> as.data.frame())

message(paste("These are your maximum and minimum fishing speeds (in knots), as defined by the autodetection algorithm, for ", year, ". Check they look realistic!", sep  =""))


# Write the fishing_speed_matrix_round  table to a text file and R File 

save(fishing_speed_matrix_autodetect_round, file = file.path(outPath, "fishing_speeds_by_metier_and_year_autodetect.RData"))

 
write.table(fishing_speed_matrix_round, file = file.path(outPath, "fishing_speeds_by_metier_and_year_autodetect.txt"), 
            append = TRUE, sep = "\t", row.names = FALSE, col.names = !file.exists(file.path(outPath, "fishing_speeds_by_metier_and_year_autodetect.txt")))
write.table( fishing_speed_matrix_round |> as.data.frame() ,"clipboard",sep="\t",row.names= FALSE  )    

 




summary_table <- subTacsat %>%
  filter(SI_STATE == "h") %>%
  group_by(LE_L5MET) %>%
  dplyr::summarise(
    min_SI_SP = min(SI_SP),
    max_SI_SP = max(SI_SP)
  )


# subTacsat <-
#   subTacsat[,
#             -rev(grep("ID", colnames(subTacsat)))[1]
#   ]
 
# unique(subTacsat$LE_GEAR)
# 
# 
# 
# for (iGear in autoDetectionGears) {
#   
#   subDat <- subset(subTacsat, LE_GEAR == iGear)
#   
#   unique(subDat$SI_STATE)
#   
#   # Check if there are non-missing values for "s" state
#   if (any(!is.na(subDat$SI_SP[which(subDat$SI_STATE == "0")]))) {
#     minS <- min(subDat$SI_SP[which(subDat$SI_STATE == "0")], na.rm = TRUE)
#  
#   } else {
#     minS <- Inf  # or assign a default value or handle the case accordingly
#   }
#   
#   # Check if there are non-missing values for "f"/ 1 state
#  
#   if (any(!is.na(subDat$SI_SP[which(subDat$SI_STATE == "1")]))) {
#     
#     minF <- min(subDat$SI_SP[which(subDat$SI_STATE == "1")], na.rm = TRUE)
#  
#   } else {
#     minF <- Inf  # or assign a default value or handle the case accordingly
#   }
#   
#   if (minS < minF) {
#     storeScheme$fixPeaks[which(storeScheme$analyse.by == iGear)] <- TRUE
#     
#     subacTa <- activityTacsat(
#       subDat,
#       units = "year",
#       analyse.by = "LE_GEAR",
#       storeScheme,
#       plot = FALSE,
#       level = "all"
#     )
#     subTacsat$SI_STATE[subDat$ID] <- subacTa
#   }
# }  
# 
# 
# 
# subTacsat <-
#   subTacsat[,
#             -rev(grep("ID", colnames(subTacsat)))[1]
#   ]

# Assign for visually inspected gears a simple speed rule classification =============== 

### Apply 
 
metiers <- unique(nonsubTacsat$LE_L5MET)
speedarr_nonsubtacsat = speedarr |> filter (LE_L5MET %in% metiers )
speedarr_nonsubtacsat$min = 0
speedarr_nonsubtacsat$max = 1.5
speedarr_nonsubtacsat[grep("FPO",speedarr_nonsubtacsat$LE_L5MET ), 'min'] = 0.5
speedarr_nonsubtacsat[grep("FPO",speedarr_nonsubtacsat$LE_L5MET ), 'max'] = 3
nonsubTacsat$SI_STATE <- NA

for (mm in metiers) {
  nonsubTacsat$SI_STATE[
    nonsubTacsat$LE_L5MET == mm &
      nonsubTacsat$SI_SP >= speedarr_nonsubtacsat[speedarr_nonsubtacsat$LE_L5MET == mm, "min"] &
      nonsubTacsat$SI_SP <= speedarr_nonsubtacsat[speedarr_nonsubtacsat$LE_L5MET == mm, "max"]
  ] <- "f";
}


nonsubTacsat$SI_STATE[
  nonsubTacsat$LE_GEAR == "NA" &
    nonsubTacsat$SI_SP >= 1 &
    nonsubTacsat$SI_SP <= 6
] <- "f"
nonsubTacsat$SI_STATE[ is.na(nonsubTacsat$SI_STATE) ] <- "s"

nonsubTacsat |>  filter ( SI_STATE == 's' ) 

##remove the MIS_MIS since it exists in the autodetection- mobile  gears  
speedarr_nonsubtacsat = speedarr_nonsubtacsat |>  filter(LE_L5MET  != 'MIS_MIS' )

# Combine the two dataset together again =============== 

 

fishing_speed_met5_array = bind_rows ( fishing_speed_matrix_autodetect_round , speedarr_nonsubtacsat )  |> as.data.frame()



save(fishing_speed_met5_array, file = file.path(outPath, paste0("fishing_speed_met5_array_", year_analysis, ".RData")))

write.table( fishing_speed_met5_array |> as.data.frame() ,"clipboard",sep="\t",row.names= FALSE  )    
write.table(fishing_speed_met5_array, file = file.path(outPath, paste0("fishing_speed_met5_array_", year_analysis, ".RData")), 
            append = TRUE, sep = "\t", row.names = FALSE )


# This next step is retained from previous code. The new function to assign
# fishing activity states does not use "h" (harbour), but if you are using your
# own workflow code, you may wish to look for this. We do not recommend it.
#
# Set fishing sequences with hauling in the middle to "f" ##################
#
# idx <-
# which(
#   tacsatp$SI_STATE[2:(nrow(tacsatp) - 1)] == "h" &
#     tacsatp$SI_STATE[1:(nrow(tacsatp) - 2)] == "f" &
#     tacsatp$SI_STATE[3:(nrow(tacsatp))    ] == "f" &
#     tacsatp$VE_REF[2:(nrow(tacsatp) - 1)] == tacsatp$VE_REF[1:(nrow(tacsatp) - 2)] &
#     tacsatp$VE_REF[2:(nrow(tacsatp) - 1)] == tacsatp$VE_REF[3:(nrow(tacsatp))]
#  ) + 1
# tacsatp$SI_STATE[idx] <- "f"



###########################################################################
##
##'   Quality Control of the Fishing Speed Arrays Analysis 2024 
##'   Validation of the results with Observers Trips data
##'
##########################################################################

##'
##' The Observers data record the start and end of the hauls. This dates 
##' can be used to delimited the start and end of a fishing trip in the VMS data 
##' 
##' 


#### Load the observers data in GeoFISH
path_observers_data = "C:/Users/RM12/OneDrive - CEFAS/Roi/projects/datacalls/ices/march_2024/ICES-VMS-and-Logbook-Data-Call_Cefas/fishing_speeds_profiles_analysis_2024/observers_data/"
trip_metier   = read.csv(file.path(paste0(path_observers_data,"trip_metier_2019_23_ARS.csv") )  ) 
sfs_haul_positions   = read.csv(file.path(paste0(path_observers_data,"SFS_Haul_positions 2023.csv") )  ) 
sfs_haul_positions = sfs_haul_positions |> select (TripIdentifier, HaulNo,   shotdate , shottime, hauldate , haultime  ) 

obs_haul_positions   = read.csv(file.path(paste0(path_observers_data,"OBS_Haul_positions 20192023.csv") )  ) 

obs_haul_positions = obs_haul_positions |> select (tripid , haulno ,   shotdate , shottime, hauldate , haultime  ) 


colnames ( obs_haul_positions ) = colnames(sfs_haul_positions)
 
haul_positions = bind_rows(sfs_haul_positions, obs_haul_positions)

trip_metier_haul = trip_metier |>  
  inner_join( haul_positions  |> 
                mutate ( shot_datim = lubridate::dmy_hms (paste ( shotdate, shottime, ' ') ) , haul_datim   = lubridate::dmy_hms (paste ( hauldate, haultime, ' ') )) ,
              by = join_by(TRIP.CODE == TripIdentifier, HAUL.CODE == HaulNo )) |> 
  arrange(shot_datim)


trip_metier_haul |> mutate( y=  lubridate::year(shot_datim )) |>  distinct(y)
 
