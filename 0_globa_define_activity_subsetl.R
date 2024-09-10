

ac.tac.anal <- function(tacsat, units = "year", storeScheme = storeScheme, analyse.by = "LE_L5MET", identify = "peaks") {
  
 
  
  if (!"LE_L5MET" %in% colnames(tacsat))
    stop("Provide gear type (as column 'LE_L5MET' and if unknown, provide it as 'MIS'")
  if (!analyse.by %in% c("LE_L5MET", "VE_REF"))
    warning("Analysing by unknown column variable, please check!")
  if (analyse.by %in% colnames(tacsat)) {
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
      mths <- sort(unique(month(tacsat$SI_DATIM)))
      wks <- 0
    }
    if (units == "week") {
      yrs <- sort(unique(format(tacsat$SI_DATIM, "%Y")))
      wks <- sort(unique(week(tacsat$SI_DATIM)))
      mths <- 0
    }
    runScheme <- expand.grid(years = yrs, months = mths,
                             weeks = wks, stringsAsFactors = FALSE)
    storeScheme <- expand.grid(years = yrs, months = mths,
                               weeks = wks, analyse.by = unique(tacsat[, get (analyse.by) ]),
                               stringsAsFactors = FALSE)
    storeScheme$peaks <- NA
    storeScheme$fixPeaks <- FALSE
    storeScheme$sigma0 <- 0.911
    if (identify == "means")
      storeScheme$means <- NA
    for (iRun in 1:nrow(runScheme)) {
      yr <- runScheme[iRun, "years"]
      mth <- runScheme[iRun, "months"]
      wk <- runScheme[iRun, "weeks"]
      if (nrow(runScheme) == 1) {
        sTacsat <- tacsat
      } else {
        if (mth == 0 & wk == 0)
          sTacsat <- subset(tacsat, format(tacsat$SI_DATIM, "%Y") == yr)
        if (mth == 0 & wk != 0)
          sTacsat <- subset(tacsat, format(tacsat$SI_DATIM, "%Y") == yr & week(tacsat$SI_DATIM) == wk)
        if (mth != 0 & wk == 0)
          sTacsat <- subset(tacsat, format(tacsat$SI_DATIM, "%Y") == yr & month(tacsat$SI_DATIM) == mth)
      }
      for (iBy in na.omit(unique(sTacsat[, get (analyse.by)]))) {
        
        dat <- subset(sTacsat, sTacsat[, get(analyse.by) ] == iBy)
        datmr <- dat
        datmr$SI_SP <- -1 * dat$SI_SP
        datmr <- rbind(dat, datmr)
        xrange <- pmin(20, range(datmr$SI_SP), na.rm = TRUE)
        datmr$SI_SP[which(abs(datmr$SI_SP) > 20)] <- NA
        hist(datmr$SI_SP, breaks = seq(-20, 20, 0.5),
             main = paste(iBy, ifelse(yr > 0, yr, ""), 
                          ifelse(mth > 0, mth, ""),
                          ifelse(wk > 0, wk, "")), xaxt = "n")
        axis(1, at = seq(-20, 20, 1))
        
        # Introduce a delay before calling the callNumberPeak() function
        Sys.sleep(1)
        
        # Call the callNumberPeak() function to get user input
        pks <- callNumberPeak()
        
        storeScheme[which(storeScheme$years == yr & storeScheme$months == mth & storeScheme$weeks == wk & storeScheme$analyse.by == iBy), "peaks"] <- pks
        
        if (identify == "means") {
          valPeaks <- callPeakValue(pks)
          if (substr(valPeaks, 1, 1) == " ")
            valPeaks <- substr(valPeaks, 2, nchar(valPeaks))
          storeScheme[which(storeScheme$years == yr & storeScheme$months == mth & storeScheme$weeks == wk & storeScheme$analyse.by == iBy), "means"] <- valPeaks
        }
      }
    }
  } else {
    stop("analyse.by statement not found as a column in the specified tacsat dataset")
  }
  return(storeScheme)
}









act.tac <- function (tacsat, units = "year", analyse.by = "LE_L5MET", storeScheme = NULL, 
                     plot = FALSE, level = "all"){
  
  
  
  tacsat =  sub_subTacsat |> filter(SI_YEAR == 2023)   
  
  
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
        } else {
          
          
          if ("means" %in% colnames(storeScheme)) {
            
            ss <- storeScheme[which(storeScheme$years == 
                                      yr & storeScheme$months == mth & storeScheme$weeks == 
                                      wk & storeScheme$analyse.by == iGr), "means"]
            
            print ( c( yr, '-', mth, '-', wk, '-', iGr))
            print( ss )
            
            sigma <- anf(storeScheme[which(storeScheme$years == 
                                             yr & storeScheme$months == mth & storeScheme$weeks == 
                                             wk & storeScheme$analyse.by == iGr), "sigma0"])
            fixPeaks <- ac(storeScheme[which(storeScheme$years == 
                                               yr & storeScheme$months == mth & storeScheme$weeks == 
                                               wk & storeScheme$analyse.by == iGr), "fixPeaks"])
            #browser()
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
          } else {
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
        browser()
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
          } else {
            
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
