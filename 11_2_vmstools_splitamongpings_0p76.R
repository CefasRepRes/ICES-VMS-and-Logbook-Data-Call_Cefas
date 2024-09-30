



### splitAmongPings_0p76  ########################



splitAmongPings_0p76 <-  function(tacsat,eflalo,variable="all",level="day",conserve=TRUE,by=NULL,returnAll=T){
  
  #level: day,ICESrectangle,trip
  #conserve: T,F
  #variable: kgs,value,effort
  require(data.table)
  #- Create extra columns with time stamps
  if(!"FT_REF" %in% colnames(tacsat)) stop("tacsat file needs FT_REF detailing trip number")
  if(!"SI_STATE" %in% colnames(tacsat)) stop("tacsat file needs SI_STATE detailing activity of vessel")
  if(level == "trip" & conserve == TRUE) stop("conserve catches only at level = ICESrectangle or day")
  
  if(!"SI_DATIM" %in%   colnames(tacsat)) tacsat$SI_DATIM     <- as.POSIXct(paste(tacsat$SI_DATE,   tacsat$SI_TIME,     sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
  if(!"LE_CDATIM" %in%  colnames(eflalo)) eflalo$LE_CDATIM    <- as.POSIXct(eflalo$LE_CDAT,                                     tz="GMT", format="%d/%m/%Y")
  
  if(is.null(by)==FALSE){
    if(any(is.na(tacsat[,by])) | any(tacsat[,by] == 0)) stop("'by' column in tacsat contains NA or zero's. Cannot execute with NA's or zeros")
  }
  
  #- Levels have hierachy, and need to be suplemented with lower levels
  if(level == "day"){               level <- c("day","ICESrectangle","trip")
  } else {
    if(level == "ICESrectangle"){ level <- c("ICESrectangle","trip")
    } else {
      if(level == "trip"){
        level <- c("trip")
      }
    }
  }
  
  #- Add ID to keep track of merged and non-merged sets
  tacsat$ID               <- 1:nrow(tacsat)
  
  #- identifyers of eflalo colnames
  eflaloCol               <- colnames(eflalo)
  kgs                     <- grep("LE_KG",colnames(eflalo))
  eur                     <- grep("LE_EURO",colnames(eflalo))
  
  #- Subset tacsat file
  remtacsat               <- subset(tacsat,SI_STATE==0)
  tacsat                  <- subset(tacsat,SI_STATE != 0) #only attribute variable to fishing pings
  tacsatTrip              <- subset(tacsat,FT_REF != 0)
  remainTacsat            <- sort(unique(tacsatTrip$ID))
  
  #- Subset eflalo file
  eflalo$ID               <- 1:nrow(eflalo)
  eflaloTrip              <- subset(eflalo,       FT_REF %in% sort(unique(tacsatTrip$FT_REF)) & VE_REF %in% sort(unique(tacsatTrip$VE_REF)))
  #eflaloNoTrip            <- subset(eflalo,      !FT_REF %in% sort(unique(tacsatTrip$FT_REF)))
  eflaloNoTrip            <- eflalo[which(!eflalo$ID %in% eflaloTrip$ID),-match("ID",colnames(eflalo))]
  #eflaloVessel            <- subset(eflaloNoTrip, VE_REF %in% sort(unique(tacsatTrip$VE_REF)))
  #eflaloNoVessel          <- subset(eflaloNoTrip,!VE_REF %in% sort(unique(tacsatTrip$VE_REF)))
  eflaloVessel            <- eflaloNoTrip[which(paste(eflaloNoTrip$VE_REF,format(eflaloNoTrip$LE_CDATIM,"%Y")) %in% unique(paste(tacsatTrip$VE_REF,format(tacsatTrip$SI_DATIM,"%Y")))),]
  eflaloNoVessel          <- eflaloNoTrip[which(!paste(eflaloNoTrip$VE_REF,format(eflaloNoTrip$LE_CDATIM,"%Y")) %in% unique(paste(tacsatTrip$VE_REF,format(tacsatTrip$SI_DATIM,"%Y")))),]
  
  #-------------------------------------------------------------------------------
  # 1a) Merge eflalo to tacsat with matching FT_REF
  #-------------------------------------------------------------------------------
  
  if(dim(tacsatTrip)[1]>0 & dim(eflaloTrip)[1] >0){
    
    if("day" %in% level){
      
      print("level: day")
      if(!"SI_YEAR" %in% colnames(tacsatTrip))  tacsatTrip$SI_YEAR    <- an(format(tacsatTrip$SI_DATIM,format="%Y"))
      if(!"SI_DAY" %in%  colnames(tacsatTrip))  tacsatTrip$SI_DAY     <- an(format(tacsatTrip$SI_DATIM,format="%j"))
      if(!"LE_RECT" %in% colnames(tacsatTrip))  tacsatTrip$LE_RECT    <- ICESrectangle(tacsatTrip)
      
      if(!"SI_YEAR" %in% colnames(eflaloTrip))  eflaloTrip$SI_YEAR    <- an(format(eflaloTrip$LE_CDATIM,format="%Y"))
      if(!"SI_DAY" %in%  colnames(eflaloTrip))  eflaloTrip$SI_DAY     <- an(format(eflaloTrip$LE_CDATIM,format="%j"))
      
      #- Count pings in tacsat set
      nPings                <- countPings(~year+VE_REF+FT_REF+icesrectangle+day,tacsatTrip, by=by)
      
      #- Do the merging of eflalo to tacsat
      
      res           <- eflalo2Pings(eflaloTrip,tacsatTrip,nPings,c("SI_YEAR","VE_REF","FT_REF","LE_RECT","SI_DAY"),eflaloCol[c(kgs,eur)],remainTacsat,by=by)
      eflaloTrip    <- res[["eflalo"]]
      byDayTacsat   <- res[["tacsat"]]
      remainTacsat  <- res[["remainTacsat"]]
    }
    if("ICESrectangle" %in% level){
      print("level: rectangle")
      if(!"SI_YEAR" %in% colnames(tacsatTrip))  tacsatTrip$SI_YEAR    <- an(format(tacsatTrip$SI_DATIM,format="%Y"))
      if(!"LE_RECT" %in% colnames(tacsatTrip))  tacsatTrip$LE_RECT    <- ICESrectangle(tacsatTrip)
      
      if(!"SI_YEAR" %in% colnames(eflaloTrip))  eflaloTrip$SI_YEAR    <- an(format(eflaloTrip$LE_CDATIM,format="%Y"))
      
      #- Count pings in tacsat set
      nPings                <- countPings(~year+VE_REF+FT_REF+icesrectangle,tacsatTrip,by=by)
      
      #- Do the merging of eflalo to tacsat
      res           <- eflalo2Pings(eflaloTrip,tacsatTrip,nPings,c("SI_YEAR","VE_REF","FT_REF","LE_RECT"),        eflaloCol[c(kgs,eur)],remainTacsat,by=by)
      eflaloTrip    <- res[["eflalo"]]
      byRectTacsat  <- res[["tacsat"]]
      remainTacsat  <- res[["remainTacsat"]]
    }
    
    if("trip" %in% level){
      print("level: trip")
      if(!"SI_YEAR" %in% colnames(tacsatTrip))  tacsatTrip$SI_YEAR    <- an(format(tacsatTrip$SI_DATIM,format="%Y"))
      if(!"SI_YEAR" %in% colnames(eflaloTrip))  eflaloTrip$SI_YEAR    <- an(format(eflaloTrip$LE_CDATIM,format="%Y"))
      
      #- Count pings in tacsat set
      nPings                <- countPings(~year+VE_REF+FT_REF,tacsatTrip,by=by)
      
      #- Do the merging of eflalo to tacsat
      res           <- eflalo2Pings(eflaloTrip,tacsatTrip,nPings,c("SI_YEAR","VE_REF","FT_REF"),                  eflaloCol[c(kgs,eur)],remainTacsat,by=by)
      eflaloTrip    <- res[["eflalo"]]
      byTripTacsat  <- res[["tacsat"]]
      remainTacsat  <- res[["remainTacsat"]]
    }
    
    #-------------------------------------------------------------------------------
    # 1b) Bind all tacsat files with matching FT_REF
    #-------------------------------------------------------------------------------
    
    if(length(remainTacsat) > 0) warning("Not all tacsat records with tripnumber have been merged!!")
    if(nrow(eflaloTrip) > 0) warning("Not all eflalo records with matching VMS tripnumber have been merged!!")
    if("day" %in% level){ tacsatFTREF <- rbind(byDayTacsat,byRectTacsat,byTripTacsat)
    } else {
      if("ICESrectangle" %in% level){ tacsatFTREF <- rbind(byRectTacsat,byTripTacsat) }
      else { tacsatFTREF <- byTripTacsat}
    }
    
    tacsatFTREF[,kgeur(colnames(tacsatFTREF))]    <- sweep(tacsatFTREF[,kgeur(colnames(tacsatFTREF))],1,tacsatFTREF$pings,"/")
    tacsatFTREF$ID                                <- af(ac(tacsatFTREF$ID.x))
    DT                                            <- data.table(tacsatFTREF)
    eq1                                           <- c.listquote(paste("sum(",colnames(tacsatFTREF[,kgeur(colnames(tacsatFTREF))]),",na.rm=TRUE)",sep=""))
    tacsatFTREF                                   <- DT[,eval(eq1),by=ID.x]; tacsatFTREF <- data.frame(tacsatFTREF); setnames(tacsatFTREF,colnames(tacsatFTREF),c("ID",colnames(eflaloTrip[,kgeur(colnames(eflaloTrip))])))
  }
  
  #-------------------------------------------------------------------------------
  # 2a) Merge eflalo to tacsat with no matching FT_REF
  #-------------------------------------------------------------------------------
  
  #- If you don't want to loose catch or value data, conserve the non-merged
  #   eflalo catches and distribute these over the tacsat records
  
  
  if(conserve == TRUE){
    if(dim(tacsat)[1]>0 & dim(eflaloVessel)[1] > 0){
      
      #-------------------------------------------------------------------------------
      # 2a-1) Merge eflalo to tacsat with matching VE_REF
      #-------------------------------------------------------------------------------
      
      if("day" %in% level){
        print("level: day & conserve = T, by vessel")
        if(!"SI_YEAR" %in% colnames(tacsat))  tacsat$SI_YEAR                <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"SI_DAY" %in%  colnames(tacsat))  tacsat$SI_DAY                 <- an(format(tacsat$SI_DATIM,format="%j"))
        if(!"LE_RECT" %in% colnames(tacsat))  tacsat$LE_RECT                <- ICESrectangle(tacsat)
        
        if(!"SI_YEAR" %in% colnames(eflaloVessel))  eflaloVessel$SI_YEAR    <- an(format(eflaloVessel$LE_CDATIM,format="%Y"))
        if(!"SI_DAY" %in%  colnames(eflaloVessel))  eflaloVessel$SI_DAY     <- an(format(eflaloVessel$LE_CDATIM,format="%j"))
        
        #- Count pings in tacsat set
        nPings                <- countPings(~year+VE_REF+icesrectangle+day,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res           <- eflalo2Pings(eflaloVessel,tacsat,nPings,c("SI_YEAR","VE_REF","LE_RECT","SI_DAY"),      eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloVessel  <- res[["eflalo"]]
        byDayTacsat   <- res[["tacsat"]]
      }
      
      if("ICESrectangle" %in% level){
        print("level: rectangle & conserve = T, by vessel")
        if(!"SI_YEAR" %in% colnames(tacsat))  tacsat$SI_YEAR                <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"LE_RECT" %in% colnames(tacsat))  tacsat$LE_RECT                <- ICESrectangle(tacsat)
        
        if(!"SI_YEAR" %in% colnames(eflaloVessel))  eflaloVessel$SI_YEAR    <- an(format(eflaloVessel$LE_CDATIM,format="%Y"))
        
        #- Count pings in tacsat set
        nPings                <- countPings(~year+VE_REF+icesrectangle,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res           <- eflalo2Pings(eflaloVessel,tacsat,nPings,c("SI_YEAR","VE_REF","LE_RECT"),               eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloVessel  <- res[["eflalo"]]
        byRectTacsat  <- res[["tacsat"]]
      }
      if(TRUE){ #-For remainder of vessel merging not at ICESrectangle level
        print("level: year & conserve = T, by vessel")
        if(!"SI_YEAR" %in% colnames(tacsat))              tacsat$SI_YEAR    <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"SI_YEAR" %in% colnames(eflaloVessel))  eflaloVessel$SI_YEAR    <- an(format(eflaloVessel$LE_CDATIM,format="%Y"))
        
        #- Count pings in tacsat set
        nPings                <- countPings(~year+VE_REF,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res           <- eflalo2Pings(eflaloVessel,tacsat,nPings,c("SI_YEAR","VE_REF" ),               eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloVessel  <- res[["eflalo"]]
        byVessTacsat  <- res[["tacsat"]]
      }
      
      #-------------------------------------------------------------------------------
      # 2b-1) Bind all tacsat files with matching VE_REF
      #-------------------------------------------------------------------------------
      
      if("day" %in% level){ tacsatVEREF <- rbind(byDayTacsat,byRectTacsat,byVessTacsat)
      } else {
        if("ICESrectangle" %in% level){
          tacsatVEREF <- rbind(byRectTacsat,byVessTacsat)
        } else { tacsatVEREF <- byVessTacsat}}
      tacsatVEREF[,kgeur(colnames(tacsatVEREF))]    <- sweep(tacsatVEREF[,kgeur(colnames(tacsatVEREF))],1,tacsatVEREF$pings,"/")
      tacsatVEREF$ID                                <- af(ac(tacsatVEREF$ID.x))
      DT                                            <- data.table(tacsatVEREF)
      eq1                                           <- c.listquote(paste("sum(",colnames(tacsatVEREF[,kgeur(colnames(tacsatVEREF))]),",na.rm=TRUE)",sep=""))
      tacsatVEREF                                   <- DT[,eval(eq1),by=ID.x]; tacsatVEREF <- data.frame(tacsatVEREF); setnames(tacsatVEREF,colnames(tacsatVEREF),c("ID",colnames(eflaloVessel[,kgeur(colnames(eflaloVessel))])))
      
    }
    
    if(dim(tacsat)[1] > 0 & dim(eflaloNoVessel)[1] > 0){
      #-------------------------------------------------------------------------------
      # 2a-2) Merge eflalo to tacsat with no matching FT_REF or VE_REF
      #-------------------------------------------------------------------------------
      if("day" %in% level){
        print("level: day & conserve = T, no vessel match")
        if(!"SI_YEAR" %in% colnames(tacsat))  tacsat$SI_YEAR                    <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"SI_DAY" %in%  colnames(tacsat))  tacsat$SI_DAY                     <- an(format(tacsat$SI_DATIM,format="%j"))
        if(!"LE_RECT" %in% colnames(tacsat))  tacsat$LE_RECT                    <- ICESrectangle(tacsat)
        
        if(!"SI_YEAR" %in% colnames(eflaloNoVessel))  eflaloNoVessel$SI_YEAR    <- an(format(eflaloNoVessel$LE_CDATIM,format="%Y"))
        if(!"SI_DAY" %in%  colnames(eflaloNoVessel))  eflaloNoVessel$SI_DAY     <- an(format(eflaloNoVessel$LE_CDATIM,format="%j"))
        
        #- Count pings in tacsat set
        nPings                <- countPings(~year+icesrectangle+day,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        
        res               <- eflalo2Pings(eflaloNoVessel,tacsat,nPings,c("SI_YEAR","LE_RECT","SI_DAY"),               eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloNoVessel    <- res[["eflalo"]]
        byDayTacsat       <- res[["tacsat"]]
      }
      
      if("ICESrectangle" %in% level){
        print("level: rectangle & conserve = T, no vessel match")
        if(!"SI_YEAR" %in% colnames(tacsat))  tacsat$SI_YEAR    <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"LE_RECT" %in% colnames(tacsat))  tacsat$LE_RECT    <- ICESrectangle(tacsat)
        
        if(!"SI_YEAR" %in% colnames(eflaloNoVessel))  eflaloNoVessel$SI_YEAR    <- an(format(eflaloNoVessel$LE_CDATIM,format="%Y"))
        
        #- Count pings in tacsat set
        nPings            <- countPings(~year+icesrectangle,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res               <- eflalo2Pings(eflaloNoVessel,tacsat,nPings,c("SI_YEAR","LE_RECT"),                        eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloNoVessel    <- res[["eflalo"]]
        byRectTacsat      <- res[["tacsat"]]
      }
      if(TRUE){ #-For remainder of merging not at ICESrectangle level
        print("level: year & conserve = T, no vessel match")
        if(!"SI_YEAR" %in% colnames(tacsat))          tacsat$SI_YEAR            <- an(format(tacsat$SI_DATIM,format="%Y"))
        if(!"SI_YEAR" %in% colnames(eflaloNoVessel))  eflaloNoVessel$SI_YEAR    <- an(format(eflaloNoVessel$LE_CDATIM,format="%Y"))
        
        #- Count pings in tacsat set
        nPings            <- countPings(~year,tacsat,by=by)
        
        #- Do the merging of eflalo to tacsat
        res               <- eflalo2Pings(eflaloNoVessel,tacsat,nPings,c("SI_YEAR"),                        eflaloCol[c(kgs,eur)],NULL,by=by)
        eflaloNoVessel    <- res[["eflalo"]]
        byVessTacsat      <- res[["tacsat"]]
      }
      #-------------------------------------------------------------------------------
      # 2b-2) Bind all tacsat files with no matching FT_REF or VE_REF
      #-------------------------------------------------------------------------------
      
      if("day" %in% level){ tacsatREF <- rbind(byDayTacsat,byRectTacsat,byVessTacsat)
      } else {
        if("ICESrectangle" %in% level){
          tacsatREF <- rbind(byRectTacsat,byVessTacsat) } 
        else { tacsatREF <- byVessTacsat }
      }
      
      
      tacsatREF[,kgeur(colnames(tacsatREF))]      <- sweep(tacsatREF[,kgeur(colnames(tacsatREF))],1,tacsatREF$pings,"/")
      tacsatREF$ID                                <- af(ac(tacsatREF$ID.x))
      DT                                          <- data.table(tacsatREF)
      eq1                                         <- c.listquote(paste("sum(",colnames(tacsatREF[,kgeur(colnames(tacsatREF))]),",na.rm=TRUE)",sep=""))
      tacsatREF                                   <- DT[,eval(eq1),by=ID.x]; tacsatREF <- data.frame(tacsatREF); setnames(tacsatREF,colnames(tacsatREF),c("ID",colnames(eflaloVessel[,kgeur(colnames(eflaloVessel))])))
    }
  }#End conserve
  
  #-------------------------------------------------------------------------------
  # 3) Merge all tacsat files together and return
  #-------------------------------------------------------------------------------
  
  if(conserve==TRUE){
    if(exists("tacsatFTREF")){one   <- tacsatFTREF} else{ one   <- numeric()}
    if(exists("tacsatVEREF")){two   <- tacsatVEREF} else{ two   <- numeric()}
    if(exists("tacsatREF"))  {three <- tacsatREF}   else{ three <- numeric()}
    tacsatTot       <- rbind(one,two,three)
    DT              <- data.table(tacsatTot)
    eq1             <- c.listquote(paste("sum(",colnames(tacsatTot[,kgeur(colnames(tacsatTot))]),",na.rm=TRUE)",sep=""))
    tacsatTot       <- DT[,eval(eq1),by=ID]; tacsatTot <- data.frame(tacsatTot); setnames(tacsatTot,colnames(tacsatTot),c("ID",colnames(eflalo[,kgeur(colnames(eflalo))])))
    tacsatReturn    <- merge(tacsat,tacsatTot,by="ID",all.x=TRUE)
    if(variable == "value") tacsatReturn <- tacsatReturn[,c(1:dim(tacsat)[2],grep("EURO",colnames(tacsatReturn)))]
    if(variable == "kgs")   tacsatReturn <- tacsatReturn[,c(1:dim(tacsat)[2],grep("KG",colnames(tacsatReturn)))]
    if(variable == "all")   tacsatReturn <- tacsatReturn
  } else {
    if(exists("tacsatFTREF")==FALSE){stop("You have selected not to conserve catches, but there is no trip identifier in the tacsat file")}
    tacsatReturn  <- merge(tacsat,tacsatFTREF,by="ID",all.x=TRUE)
    if(variable == "value") tacsatReturn <- tacsatReturn[,c(1:dim(tacsat)[2],grep("EURO",colnames(tacsatReturn)))]
    if(variable == "kgs")   tacsatReturn <- tacsatReturn[,c(1:dim(tacsat)[2],grep("KG",colnames(tacsatReturn)))]
    if(variable == "all")   tacsatReturn <- tacsatReturn
  }
  if(returnAll & nrow(remtacsat)>0)
    tacsatReturn <- orderBy(~ID,data=rbindTacsat(tacsatReturn,remtacsat))
  
  return(orderBy(~ID,data=tacsatReturn)[,-match("ID",colnames(tacsatReturn))])
  
}




######################### COUNTPINGS FUNCTION #####################################




countPings <- function(formula,tacsat,grid=NULL,by=NULL){
  
  #Turn selected variables into element list
  form <- formula
  if (form[[1]] != "~")
    stop("Error: Formula must be one-sided.")
  formc <- as.character(form[2])
  formc <- gsub(" ", "", formc)
  if (!is.element(substring(formc, 1, 1), c("+", "-")))
    formc <- paste("+", formc, sep = "")
  vars <- unlist(strsplit(formc, "[\\+\\-]"))
  vars <- vars[vars != ""]
  signs <- formc
  for (i in 1:length(vars)) {
    signs <- gsub(vars[i], "", signs)
  }
  signs <- unlist(strsplit(signs, "")) #Currently we do not use signs
  
  #Define which variables selected are column names, time variables or spatial variables
  Vars      <- vars[which(!vars %in% c("day","week","month","quarter","year","gridcell","icesrectangle","icesarea"))]
  timeVars  <- vars[which(vars %in% c("day","week","month","quarter","year"))]
  spatVars  <- vars[which(vars %in% c("gridcell","icesrectangle","icesarea"))]
  
  #Add time notation if you want this as output
  if(length(timeVars)>0){
    if(!length(grep("SI_DATIM",colnames(tacsat)))>0) tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE,  tacsat$SI_TIME,   sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
    if("day" %in% timeVars & !"SI_DAY" %in% colnames(tacsat)){       tacsat$SI_DAY   <- an(format(tacsat$SI_DATIM,format="%j"))};      if("day" %in% timeVars){   ; timeVars[which(timeVars=="day")]      <- "SI_DAY"}
    if("week" %in% timeVars & !"SI_WEEK" %in% colnames(tacsat)){      tacsat$SI_WEEK  <- an(format(tacsat$SI_DATIM,format="%W"))};     if("week" %in% timeVars){   ; timeVars[which(timeVars=="week")]     <- "SI_WEEK" }
    if("month" %in% timeVars & !"SI_MONTH" %in% colnames(tacsat)){     tacsat$SI_MONTH <- an(format(tacsat$SI_DATIM,format="%m"))};    if("month" %in% timeVars){   ; timeVars[which(timeVars=="month")]    <- "SI_MONTH"}
    if("quarter" %in% timeVars & !"SI_QUART" %in% colnames(tacsat)){   tacsat$SI_QUART <- an(substr(quarters(tacsat$SI_DATIM),2,2))};  if("quarter" %in% timeVars){ ; timeVars[which(timeVars=="quarter")]  <- "SI_QUART"}
    if("year" %in% timeVars & !"SI_YEAR" %in% colnames(tacsat)){      tacsat$SI_YEAR  <- an(format(tacsat$SI_DATIM,format="%Y"))};     if("year" %in% timeVars){   ; timeVars[which(timeVars=="year")]     <- "SI_YEAR" }
  }
  #Add spatial notation if you want this as output
  if(length(spatVars)>0){
    if("gridcell" %in% spatVars & is.null(grid) == TRUE) stop("Grid needs to be specified to use the 'gridcell' option")
    if("gridcell" %in% spatVars & is.null(grid) == FALSE){
      #Create coordinates of tacsat data
      coords                    <- cbind(x=tacsat$SI_LONG,y=tacsat$SI_LATI)
      sPDF                      <- SpatialPointsDataFrame(coords,data=tacsat)
      #Turn grid into a spatial pixel dataframe
      grid                      <- as(grid,"SpatialPixels");
      grid                      <- as(grid,"SpatialPixelsDataFrame")
      #Overlay the two spatial frameworks to see to which gridcell each tacsat coordinate belongs
      gridCellIndex             <- over(as(sPDF,"SpatialPoints"),as(grid,"SpatialPixels"))
      newCoords                 <- sPDF@coords[gridCellIndex,]
      
      tacsat$GR_LONG            <- newCoords[,1]
      tacsat$GR_LATI            <- newCoords[,2]
      spatVars[which(spatVars=="gridcell")] <- "GR_LONG"; spatVars <- c(spatVars,"GR_LATI")
    }
    if("icesrectangle" %in% spatVars){
      if(!"LE_RECT" %in% colnames(tacsat))
        tacsat$LE_RECT <- ICESrectangle(tacsat)
      spatVars[which(spatVars=="icesrectangle")] <- "LE_RECT"
    }
    if("icesarea" %in% spatVars){
      if(!"LE_AREA" %in% colnames(tacsat))
        tacsat$LE_AREA <- ICESarea(tacsat)
      spatVars[which(spatVars=="icesarea")] <- "LE_AREA"
    }
  }
  
  if(is.null(by)){
    tacsat$SUM      <- 1
  } else {
    tacsat$SUM      <- tacsat[,by]
  }
  totVars         <- c(Vars,timeVars,spatVars)
  
  #Do the counting of pings
  for(iVars in 1:length(totVars)) tacsat[,totVars[iVars]] <- af(ac(tacsat[,totVars[iVars]]))
  DT              <- data.table(tacsat)
  eq              <- c.listquote(totVars)
  
  res             <- DT[,sum(SUM),by=eval(eq)]
  setnames(res,colnames(res),c(totVars,"pings"))
  #colnames(res)   <- c(totVars,"pings")
  
  return(data.frame(res))}






################# EFLALO2PINGS FUNCTION ######################################


eflalo2Pings <- function(eflalo,tacsat,pings,vars,eflaloCol,remainTacsat,by=NULL){
  #- Merge landings and values to get unique eflalo set given 'totVars'
  for(iVars in 1:length(vars)){
    eflalo[,vars[iVars]] <- af(ac(eflalo[,vars[iVars]]))
    tacsat[,vars[iVars]] <- af(ac(tacsat[,vars[iVars]]))
  }
  
  DT                    <- data.table(eflalo)
  eq1                   <- c.listquote(paste("sum(",colnames(eflalo[,kgeur(colnames(eflalo))]),",na.rm=TRUE)",sep=""))
  eq2                   <- c.listquote(vars)
  
  eflalo            <- data.frame(DT[,eval(eq1),by=eval(eq2)]); colnames(eflalo) <- c(vars,eflaloCol)
  eflalo$ID         <- 1:nrow(eflalo)
  
  #- Merge eflalo to pings to get number of pings per eflalo record
  byPing             <- merge(eflalo,data.frame(pings),by=vars,all=FALSE)
  byTacsat           <- merge(tacsat,byPing,by=vars,all=FALSE)
  if(is.null(by)==FALSE)
    byTacsat$pings   <- byTacsat$pings / byTacsat[,by]
  
  try(print(paste("kg in eflalo",round(sum(byPing [,kgeur(colnames(byPing))])))))
  try(print(paste("kg in merged tacsat",round(sum(sweep(byTacsat[,kgeur(colnames(byTacsat))],1,byTacsat$pings,"/"))))))
  
  #- Bookkeeping which tacsat ID's have been merged and which have not yet been merged
  remainTacsat          <- remainTacsat[which(!remainTacsat %in% byTacsat$ID.x)]
  
  #- Bookkeeping which eflalo catches have been merged and which have not yet been merged
  idx                   <- sort(unique(byPing$ID))
  try(print(paste("kg removed from eflalo",round(sum(eflalo[idx,kgeur(colnames(eflalo))])))))
  eflalo[idx,kgeur(colnames(eflalo))]   <- 0
  
  return(list(eflalo=eflalo,tacsat=byTacsat,remainTacsat=remainTacsat))}
