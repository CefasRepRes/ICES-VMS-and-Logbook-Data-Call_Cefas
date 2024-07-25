library(RPostgres)
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(here)
library(tidyr)
library(lubridate)

setwd(here("datacalls/ices/2024"))

# connect to the geofish dev database
gdev = dbConnect(
  Postgres(),
  host = "",
  dbname = "",
  port = 5432,
  user = "",
  password = ""
)

# retrieve country data for all vessels in the necessary year
cou <- dbGetQuery(
  gdev,
  "select ft_ref, ve_cou
    from eflalo.eflalo_ft
    where ft_ref in (
      select distinct(ft_ref)
      from eflalo.eflalo_ft
      where ft_year = 2023
    )"
)

# extract eflalo data (eflalo.sql)
eflalo <- dbGetQuery(gdev,
                     'select *
                     from vms_lb_extraction.ices_2024_eflalo')


# extract tacsat data (tacsat.sql)
tacsat <- dbGetQuery(gdev, 
                     'select *
                     from vms_lb_extraction.ices_2024_tacsat')

# save the data as it appears from the queries 
rm(gdev)
# save.image(here("tacsat-eflalo-data.RData"))
load(here("tacsat-eflalo-data.RData"))


## process eflalo data
# eflalo <- eflalo %>% filter(ve_len >= 12)
eflalo <- left_join(eflalo, cou, by = "ft_ref")
# colnames(eflalo)[colnames(eflalo) == "eflalo_le_le_id"] <- "le_id"

names(eflalo) <- toupper(names(eflalo))

eflalo$FT_DDAT =   ymd(eflalo$FT_DDAT  , tz = "GMT" )  
eflalo$FT_LDAT =   ymd(eflalo$FT_LDAT  , tz = "GMT" ) 
eflalo$LE_CDAT =   ymd(eflalo$LE_CDAT  , tz = "GMT" ) 
eflalo$FT_DDATIM = ymd_hms( paste ( eflalo$FT_DDAT , eflalo$FT_DTIME ) , tz = "GMT"  ) 
eflalo$FT_LDATIM = ymd_hms( paste ( eflalo$FT_LDAT , eflalo$FT_LTIME ) , tz = "GMT"  ) 
eflalo$YEAR = lubridate::year(eflalo$FT_DDATIM )
eflalo$MONTH = lubridate::month(eflalo$FT_LDATIM)
eflalo$QUARTER = lubridate::quarter(eflalo$FT_LDATIM)

# specify columns for use in pivot_wider
pivot_names <- c("LE_SPE", "LE_KG", "LE_EURO")
eflalo_cols <- colnames(eflalo)
pivot_id_cols <- setdiff(eflalo_cols, pivot_names)

eflalo_long <- eflalo

eflalo = eflalo %>%
  pivot_wider(id_cols = pivot_id_cols,
              names_from = c(LE_SPE),
              values_from = c(LE_KG, LE_EURO))



## process tacsat data
drop_cols <- c("si_longlati_error", "si_he_error", "si_sp_error", "duplicate")
tacsat <- tacsat %>% select(-one_of(drop_cols))

names(tacsat) = toupper(names(tacsat))

tacsat$SI_DATE = ymd(tacsat$SI_DATE, tz = "GMT")  
tacsat$SI_DATIM = ymd_hms(paste(tacsat$SI_DATE, tacsat$SI_TIME), tz = "GMT")

tacsat['FT_REF'] = tacsat$SI_FT

# tacsat$SI_STATE[which(tacsat$SI_STATE != "f")] = 0
# tacsat$SI_STATE[which(tacsat$SI_STATE == "f")] = 1
# 
# tacsat = tacsat %>% filter(SI_STATE == 1)



# identify non-data-frame objects
non_data_frames <- sapply(ls(), function(x) !is.data.frame(get(x)))

# remove non-data-frame objects
rm(list = names(non_data_frames[non_data_frames]))
rm(non_data_frames)

# save.image(here("tacsat-eflalo-data-processed.RData"))
load(here("tacsat-eflalo-data-processed.RData"))

eflalo %>% filter(FT_REF == 610917780)


tacsat %>% distinct(si_ft) %>% tally()
eflalo %>% distinct(ft_ref) %>% tally()
