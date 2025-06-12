## fiadb_api_GET			Get EVALIDator url.
## getpltdom.prop	Calculate proportion of points by domain
## getattnbr - get attribute number for estimation from POP table in database
## getAPIest - get estimate from EVALIDator API
## getFIESTAest - get estimate from FIESTA
## compareAPI - compare outputs from EVALIDator and FIESTA
## compareAPI_evalid - generate and compare output from EVALIDator and FIESTA 
##                     for one or more evalids
## getFIADBpop - ets pop tables from FIA DataMart or dsn or dbconn
## checkpop - checks pop tables
## compareADJ - compares adjustment factors for evalids


#########################################################################
## DEFINE FUNCTIONS
#########################################################################

fiadb_api_GET <- function(url){
  ## DESCRIPTION: Get EVALIDator url
  resp <- httr::GET(url=url)
  respObj <- httr::content(resp, "parsed", encoding = "ISO-8859-1")
  outputList <- list()
  outputList[['estimates']] <- as.data.frame(do.call(rbind,respObj$estimates))
  
  # if estimate includes totals and subtotals, add those data frames to output list
  if ('subtotals' %in% names(respObj)){
    subtotals <- list()
    for (i in names(respObj$subtotals)){
      subtotals[[i]] <- as.data.frame(do.call(rbind,respObj$subtotals[[i]]))
    }
    outputList[['subtotals']] <- subtotals
    outputList[['totals']] <- as.data.frame(do.call(rbind,respObj$totals))
  }
  outputList[['metadata']] <- respObj$metadata
  return(outputList)
}


getattnbr <- function(popType = "CURR",
                      estvar = NULL, 
                      estvar.filter = NULL,
                      landarea = "Forest", 
                      chng_type = "total",
                      chng_measurements = "both",
                      woodland = "Y",
                      dia5inch = TRUE,
                      saplings = FALSE,
                      dsn = NULL, conn = NULL,
                      EVALIDATOR_POP_ESTIMATE = NULL) {
  
  ## DESCRIPTION: 
  ## Get attribute number for estimation from POP table in database
  ## ARGUMENTS:
  ## popType - c('CURR', 'VOL', 'CHNG', 'P2VEG')      
  ## estvar - c('AREA', 'CHNG', 'VOLCFNET', 'VOLBFNET', 'TPA_UNADJ', 'DRYBIO_AG/2000')
  ## landarea - c('FOREST', 'TIMBERLAND, 'ALL')
  ## estn_type - c('GS', 'STGD', 'AL', 'SL') - (growing-stock; standing-dead; all live; saw-log)
  ## estimate - c('AREA CHANGE ANNUAL', 'AREA CHANGE")
  ## chng_type - c('total', 'annual')
  ## chng_measurements - c('both', 'either')
  ## woodland - c('Y', 'N', 'only')
  ## dia5inch Logical. If TRUE, gets attribute with 5 inches and above
  ## dsn - data source name of SQLite database with EVALIDATOR_POP_ESTIMATE table
  ## conn - open connection of SQLite database with EVALIDATOR_POP_ESTIMATE table
  ## EVALIDATOR_POP_ESTIMATE - table with attribute numbers
  
  
  landarealst <- c("FOREST", "TIMBERLAND", "ALL")
  landarea <- FIESTAutils::pcheck.varchar(var2check = landarea, varnm = "landarea",
                  checklst = landarealst, caption = "landarea", 
                  stopifnull = TRUE)
  
  
  ## Get LAND_BASIS from landarea
  ## LAND_BASIS - c('Forest land', Timberland', 'All land')
  land_basis <- ifelse (landarea == "FOREST", "Forest land", 
                        ifelse(landarea == "TIMBERLAND", "Timberland",
                               "All land"))
  
  if (is.null(estvar) && popType == "VOL") {
    message("check popType...")
  }
  if (!is.null(estvar) && popType != "VOL") {
    message("check popType... ")
    popType <- "VOL"
  }
  
  ## Get EVALIDATOR_POP_ESTIMATE
  popvars <- tolower(c("ATTRIBUTE_NBR", "ATTRIBUTE_DESCR", "ESTN_TYPE"))
  if (is.null(EVALIDATOR_POP_ESTIMATE)) {
    if (is.null(conn)) {
      if (is.null(dsn)) {
        ## Get attribute table from DataMart
        fn <- "https://apps.fs.usda.gov/fia/datamart/CSV/EVALIDATOR_POP_ESTIMATE.csv"
        EVALIDATOR_POP_ESTIMATE <- fread(fn, integer64 = "character")
      } else {
        ## Check database connection
        conn <- DBI::dbCanConnect(RSQLite::SQLite(), dsn)
        ## Get attribute table from database
        EVALIDATOR_POP_ESTIMATE <- DBI::dbReadTable(conn, "EVALIDATOR_POP_ESTIMATE")
        #EVALIDATOR_POP_ESTIMATE[EVALIDATOR_POP_ESTIMATE$ESTN_ATTRIBUTE %in% attributes, c(1,2,7,20,15,18)]
      }
    } else {
      EVALIDATOR_POP_ESTIMATE <- DBI::dbReadTable(conn, "EVALIDATOR_POP_ESTIMATE")
      #EVALIDATOR_POP_ESTIMATE[EVALIDATOR_POP_ESTIMATE$ESTN_ATTRIBUTE %in% attributes, c(1,2,7,20,15,18)]
    }
    names(EVALIDATOR_POP_ESTIMATE) <- tolower(names(EVALIDATOR_POP_ESTIMATE))
  } else {
    if (!all(popvars %in% names(EVALIDATOR_POP_ESTIMATE))) {
      names(EVALIDATOR_POP_ESTIMATE) <- tolower(names(EVALIDATOR_POP_ESTIMATE))
      message("EVALIDATOR_POP_ESTIMATE missing key variables: ", toString(popvars))
    }
  }
  EVALIDATOR_POP_ESTIMATE <- data.table::setDF(EVALIDATOR_POP_ESTIMATE)
  
  ## Remove spaces for consistency
  EVALIDATOR_POP_ESTIMATE$estn_attribute <- gsub(" ", "", EVALIDATOR_POP_ESTIMATE$estn_attribute)
  EVALIDATOR_POP_ESTIMATE$estn_attribute <- gsub("\n", "", EVALIDATOR_POP_ESTIMATE$estn_attribute)
  
  tmpvars <- tolower(c("ATTRIBUTE_NBR", "ATTRIBUTE_DESCR", "EVAL_TYP", "LAND_BASIS", 
                       "ESTIMATE_GRP_DESCR", "ESTIMATE", "ESTN_TYPE", "ESTN_TREE_PORTION", 
                       "ESTN_ATTRIBUTE", "ESTN_UNITS_ATTRIBUTE", "ESTN_UNITS_DISPLAY", 
                       "ESTIMATE_VARIANT", "GROWTH_ACCT"))
  tmpdf <- EVALIDATOR_POP_ESTIMATE[, tmpvars]

  
  ###############################################################################
  ## Get attribute number for EVALIDator estimate
  ###############################################################################
  if (popType == "CURR") {
    
    eval_typ <- "EXPCURR"
    estimate <- "AREA"
    
    attribute_nbr <- tmpdf[tmpdf$estimate == estimate & 
                           tmpdf$land_basis == land_basis & 
                           tmpdf$eval_typ == eval_typ, "attribute_nbr"]
    
  } else if (popType == 'CHNG') {
    
    eval_typ <- "EXPCHNG"
    if (tolower(chng_type) == "total") {
      estimate <- "AREA CHANGE"
    } else {
      estimate <- "AREA CHANGE ANNUAL"
    }
    if (chng_measurements == "both") {
      att_search <- "both measurements"
    } else {
      att_search <- "either measurement"
    }
    
    tmpdf <- tmpdf[tmpdf$estimate == estimate & 
                       tmpdf$land_basis == land_basis & 
                       tmpdf$eval_typ == eval_typ,]
   
    if (nrow(tmpdf) > 1) {
      attribute_nbr <- tmpdf[tmpdf$estimate == estimate & 
                             grepl(att_search, tmpdf$attribute_descr) &
                             tmpdf$land_basis == land_basis & 
                             tmpdf$eval_typ == eval_typ, "attribute_nbr"]
    }
    
  } else if (popType == "VOL") {
    
    if (is.null(estvar)) {
      stop("must include estvar")
    }
    
    ## Define estn_type
    if (is.null(estvar.filter)) {
      estn_type <- "AL"
    } else {
      estvar.filter <- gsub(" ", "", estvar.filter)
      if (estvar == "VOLBSNET") {
        estn_type <- "SL"
      } else if (grepl("STATUSCD==1", estvar.filter, ignore.case = TRUE)) {
        estn_type <- "AL"
      } else if (grepl("STATUSCD==2", estvar.filter, ignore.case = TRUE)) {
        estn_type <- "STGD"
      } else if (grepl("TREECLCD==2", estvar.filter, ignore.case = TRUE)) {
        estn_type <- "GS"
      } else {
        stop("estvar.filter not supported: ", estvar.filter)
      }
    } 
    
    if (estvar == "BA") {
      estn_attribute <- "TPA_UNADJ"
    } else {
      estn_attribute <- estvar
    }
    

    eval_typ <- "EXPVOL"
    if (!is.null(estn_attribute) && estn_attribute %in% c("DRYBIO_AG", "CARBON_AG")) {
      estn_attribute <- paste0(estn_attribute, "/2000")
    }  
    
    estimate_options <- sort(unique(tmpdf$estn_attribute))
    if (!estn_attribute %in% estimate_options) {
      message("estn_attribute not in EVALIDATOR_POP_ESTIMATE table:")
      print(paste(estimate_options, sep = "\n"))
    }

    tmpdf <- tmpdf[
      (!is.na(tmpdf$estn_attribute) & tmpdf$estn_attribute == estn_attribute),]
    if (nrow(tmpdf) == 0) {
      stop("invalid estn_attribute: ", estn_attribute)
    }

    if (estn_attribute == "TPA_UNADJ") {
      if (estvar == "BA") {
        tmpdf <- tmpdf[tmpdf$estimate_grp_desc == "Tree basal area",]
      } else {
        tmpdf <- tmpdf[tmpdf$estimate_grp_desc == "Tree number",]
      }
    }

    if (!land_basis %in% unique(tmpdf$land_basis)) {
      land_basis <- "Forest land"
    }
    tmpdf <- tmpdf[tmpdf$land_basis == land_basis,]
    if (nrow(tmpdf) == 0) {
      stop("invalid land_basis: ", land_basis)
    }

    tmpdf <- tmpdf[tmpdf$eval_typ == eval_typ,]
    if (nrow(tmpdf) == 0) {
      stop("invalid eval_typ: ", eval_typ)
    }
    
    tmpdf <- tmpdf[tmpdf$estn_type == estn_type,]
    if (nrow(tmpdf) == 0) {
      stop("invalid estn_type: ", estn_type)
    }
    
    if (estn_attribute %in% c("VOLTSGRS","VOLTSSND") || startsWith(estn_attribute, "DRYBIO")) {
      if (woodland == "only") {
        tmpdf <- tmpdf[(grepl("woodland", tmpdf$attribute_descr) & 
                          !grepl("timber", tmpdf$attribute_descr)), ]
      } else {
        if (estn_attribute == "VOLTSGRS") {
          tmpdf <- tmpdf[((grepl("timber", tmpdf$attribute_descr) & grepl("woodland", tmpdf$attribute_descr))
                         | (!grepl("timber", tmpdf$attribute_descr) & !grepl("woodland", tmpdf$attribute_descr))),]
        } else {
          tmpdf <- tmpdf[(!grepl("woodland", tmpdf$attribute_descr)),]
        }
      }
    } 
    
    if (nrow(tmpdf) > 1) {
      if (dia5inch) {
        if (!any(grepl("at least 5 inches", tmpdf$attribute_descr))) {
          message("no attribute exists for dia5inch")
          message(paste0(utils::capture.output(tmpdf[,popvars]), collapse = "\n"))
        } else {
          tmpdf <- tmpdf[grepl("at least 5 inches", tmpdf$attribute_descr), ]
        }
      } else {
        if (saplings) {
          tmpdf <- tmpdf[grepl("saplings", tmpdf$attribute_descr), ]
        } else {
          if (any(grepl("at least 1 inch", tmpdf$attribute_descr))) {
            tmpdf <- tmpdf[grepl("at least 1 inch", tmpdf$attribute_descr), ]
          }
        }
      }
    }

    if (nrow(tmpdf) == 0) {
      stop("invalid estn_type: ", estn_type)
    } else {
      attribute_nbr <- tmpdf[, "attribute_nbr"]
      df <- tmpdf[, c("attribute_nbr", "attribute_descr")]
      message(paste0(utils::capture.output(df), collapse = "\n"))
    }
  }
  
  if (length(attribute_nbr) == 0) {
    stop("no attribute number exists in database")
  } else if (length(attribute_nbr) > 1) {
    message("more than one attributes exist...")
    df <- tmpdf[tmpdf$attribute_nbr %in% attribute_nbr[[1]], popvars]
    message(paste0(utils::capture.output(df), collapse = "\n"))
    stop()
  }
  attribute_desc <- tmpdf[tmpdf$attribute_nbr %in% attribute_nbr, "attribute_descr"]
  message("attribute number ", attribute_nbr, ": \n", attribute_desc)  
  
  return(attribute_nbr) 
}  

#' Get estimate from EVALIDator API
#'
#' @param evalid 6 digit Evaluation ID
#' @param attribute_nbr EVALIDator attribute number for estimation variable. Using attribute_nbr replaces: estvar, estvar.filter, landarea, and chng_type
#' @param estvar estimation variable
#' @param estvar.filter estimation filter ('TREECLCD = 2', 'STATUSCD = 2', 'STATUSCD = 1'). Similar to EVALIDator estn_type ('GS'-growing-stock; 'STGD'-standing-dead; 'AL'-all live)
#' @param landarea ('FOREST', 'TIMBERLAND', 'ALL'). Similar to EVALIDator land_basis ('Forest land', Timberland', 'All land')
#' @param rowvar row domain to estimate by
#' @param colvar column domain to estimate by
#' @param ratio if ratio estimation
#' @param ratiotype type of ratio ('PERACRE', 'PERTREE')
#' @param attribute_nbrd EVALIDator attribute number for denominator estimation variable.
#' @param estvard estimation variable for denominator
#' @param estvard.filter estimation filter for denominator ('TREECLCD = 2', 'STATUSCD = 2', 'STATUSCD = 1'). Similar to EVALIDator estd_type ('GS'-growing-stock; 'STGD'-standing-dead; 'AL'-all live)
#' @param chng_type c('total', 'annual')
#' @param chng_measurements ('both', 'either')
#' @param woodland ('Y', 'N', 'only') - whether to include woodland tree species
#' @param saplings Saplings.
#' @param dia5inch Logical. If TRUE, gets attribute with 5 inches and above
#' @param dsn data source name of SQLite database with EVALIDATOR_POP_ESTIMATE table (if attribute_nbr is NULL)
#' @param conn open connection of SQLite database with EVALIDATOR_POP_ESTIMATE table (if attribute_nbr is NULL)
#' @param strFilter an additional filter to add. Include table prefix (e.g., 'cond.adforcd = 0410')
#' @param EVALIDATOR_POP_ESTIMATE table with attribute numbers (if attribute_nbr is NULL)
#'             
#' @export
getAPIest <- function(evalid, 
                      estvar = NULL,
                      estvar.filter = NULL,
                      landarea = "FOREST",
                      rowvar = NULL,
                      colvar = NULL,
                      ratio = FALSE,
                      ratiotype = "PERACRE",
                      estvard = NULL,
                      estvard.filter = NULL,
                      chng_type = "total",
                      chng_measurements = "both",
                      woodland = "Y",
                      saplings = FALSE,
                      dia5inch = FALSE, 
                      dsn = NULL, conn = NULL,
                      attribute_nbr = NULL,
                      attribute_nbrd = NULL,
                      strFilter = NULL,
                      EVALIDATOR_POP_ESTIMATE = NULL) {
  
  ## LAND_BASIS - c('Forest land', Timberland', 'All land')
  ## estimate - c('AREA CHANGE ANNUAL', 'AREA CHANGE")
  messagerowcol <- ""
  rowvarstr=colvarstr <- NULL
  
  ## Check evalid
  if (is.null(evalid)) {
    stop("must include evalid")
  }
  
  ## Get evalType from evalid
  evalType <- unique(substr(evalid, nchar(evalid)-1, nchar(evalid)))
  
  ## Get popType from evalType
  if (!evalType %in% c("01", "03", "10")) {
    popType <- "VOL"
  } else {
    popType <- ifelse (evalType == "01", "VOL",
                       ifelse (evalType == "03", "CHNG",
                               ifelse (evalType == "10", "P2VEG")))
  }
  ## Check popType
  if (popType == "VOL" && is.null(estvar)) {
    popType <- "CURR"
  }
  
  
  ## Define row/col variables for FIESTA (ROWCOL) and EVALIDator (ROWCOLVAL)
  rowcoldf <- data.frame(
    ROWCOL = c('ALSTKCD', 'DSTRDCD', 'DSTRBCD1', 'FORTYPCD', 'FORTYPGRPCD', 
               'LANDUSE', 'COND_CLASS_CD', 'LANDUSE', 
               'ADFORCD', 'ALP_ADFORCD', 
               'OWNCD', 'OWNGRPCD', 
               'RESERVCD', 'STDSZCD', 'TRTCD1',
               'SPCD', 'SPGRPCD', 'TREECLCD',
               'RDDISTCD', 'PHYSCLCD', 'COUNTYCD', 'INVYR'),
    EVALROWCOL = c('All live stocking', 'Distance to road', 'Disturbance 1', 'Forest type', 'Forest type group', 
                   'Land Use - Major', 'Land class', 'Land use', 
                   'National Forests', 'National Forests: ALP',
                   'Ownership class', 'Ownership group', 
                   'Reserved status class', 'Stand-size class', 'Stand treatment 1',
                   'Species', 'Species group', 'Tree class',
                   'Distance to road', 'Physiographic class', 'County code and name', 'Inventory year'))
  
  
  #  ## Define estn_type
  #  if (!is.null(estvar.filter)) {
  #    estvar.filter <- gsub(" ", "", estvar.filter)
  #    if (grepl("STATUSCD==1", estvar.filter, ignore.case = TRUE)) {
  #      estn_type <- "AL"
  #    } else if (grepl("STATUSCD==2", estvar.filter, ignore.case = TRUE)) {
  #      estn_type <- "STGD"
  #    } else if (grepl("TREECLCD==2", estvar.filter, ignore.case = TRUE)) {
  #      estn_type <- "GS"
  #    } else {
  #      stop("estvar.filter not supported: ", estvar.filter)
  #    }
  #  } 
  
  
  # Check strFilter
  if (!is.null(strFilter)) {
    strFilter <- FIESTAutils::RtoSQL(strFilter)
    # if (grepl("SPCD", strFilter, ignore.case = TRUE) && !grepl("tree.SPCD", strFilter, ignore.case = TRUE)) {
    #   if (grepl("SPCD", strFilter, ignore.case = FALSE)) {
    #     strFilter <- gsub("SPCD", "tree.SPCD", strFilter)
    #   } else {
    #     strFilter <- gsub("spcd", "tree.SPCD", strFilter)
    #   }
    # }
    strFilter <- gsub(" ", "%20", strFilter)
  }

  ###############################################################################
  ## Get attribute number for EVALIDator estimate
  ###############################################################################
  if (is.null(attribute_nbr)) {
    attribute_nbr <- 
      getattnbr(popType = popType,
                conn = conn,
                estvar = estvar,
                estvar.filter = estvar.filter,
                landarea = landarea, 
                chng_type = chng_type,
                chng_measurements = chng_measurements,
                woodland = woodland,
                saplings = saplings,
                dia5inch = dia5inch,
                EVALIDATOR_POP_ESTIMATE = EVALIDATOR_POP_ESTIMATE)
    
    if (ratio) {
      if (ratiotype == "PERACRE") {
        attribute_nbrd <- 
          getattnbr(popType = "CURR",
                    conn = conn,
                    landarea = landarea, 
                    EVALIDATOR_POP_ESTIMATE = EVALIDATOR_POP_ESTIMATE)
        
      } else {
        
        ## Define estn_type
        if (!is.null(estvard.filter)) {
          estvard.filter <- gsub(" ", "", estvard.filter)
          if (grepl("STATUSCD==1", estvard.filter, ignore.case = TRUE)) {
            estn_type <- "AL"
          } else if (grepl("STATUSCD==2", estvard.filter, ignore.case = TRUE)) {
            estn_type <- "STGD"
          } else if (grepl("TREECLCD==2", estvard.filter, ignore.case = TRUE)) {
            estn_type <- "GS"
          } else {
            stop("estvard.filter not supported: ", estvard.filter)
          }
        } 
        
        attribute_nbrd <- 
          getattnbr(popType = popType,
                    conn = conn,
                    estvar = estvard,
                    estvar.filter = estvard.filter,
                    landarea = landarea, 
                    chng_type = chng_type,
                    chng_measurements = chng_measurements,
                    dia5inch = dia5inch,
                    saplings = saplings,
                    EVALIDATOR_POP_ESTIMATE = EVALIDATOR_POP_ESTIMATE)
      }
    }
  }  else {
    message("getting estimates from EVALIDator for attribute number ", attribute_nbr, ":")
  }
  
  ###############################################################################
  ## Get rowvar / colvar for EVALIDator from rowcoldf
  ###############################################################################
  totals <- FALSE
  if (!is.null(rowvar) && !rowvar %in% rowcoldf$ROWCOL) {
    stop("rowvar must be in following list: ", toString(rowcoldf$ROWCOL))
  } else {
    evalrow <- rowcoldf[rowcoldf$ROWCOL == rowvar, "EVALROWCOL"]
  }
  if (is.null(colvar) && popType %in% c("CHNG", "GRM")) {
    colvar <- rowvar
    evalcol <- rowcoldf[rowcoldf$ROWCOL == colvar, "EVALROWCOL"]
  } else if (!is.null(colvar) && !colvar %in% rowcoldf$ROWCOL) {
    stop("colvar must be in following list: ", toString(rowcoldf$ROWCOL))
  } else {
    evalcol <- rowcoldf[rowcoldf$ROWCOL == colvar, "EVALROWCOL"]
  }
  
  if (!is.null(rowvar)) {
    if (popType %in% c("CHNG", "GRM")) {
      messagerowcol <- paste0("    by Previous ", evalrow)
    } else {
      messagerowcol <- paste0("    by ", evalrow)
    }
    ## Create rowvar  string for url
    rowvarstr <- gsub(" ", "%20", evalrow)
  }
  if (!is.null(colvar)) {
    messagerowcol <- paste0(messagerowcol, " and ", evalcol)
    
    ## Create colvar string for url
    colvarstr <- gsub(" ", "%20", evalcol)
  }
  message(messagerowcol, "\n")
  
  ############################################################################
  ## EVALIDator estimates
  ############################################################################
  
  # define function
  getevalgrp <- function(x) {
    ## DESCRIPTION: get evalgrp from an evalid (x)
    x2 <- substr(x, 1, nchar(x)-4) 
    if (nchar(x2) == 1) x2 <- paste0("0", x2)
    x2 <- paste0(x2, "20", substr(x, nchar(x)-3, nchar(x)-2))
    return(x2)
  }
  evalgrp <- sapply(evalid, getevalgrp)
  evalgrp <- gsub(" ", "", toString(evalgrp))
  if (evalgrp == "642016") {
    evalgrp <- "646416"
  }

  # get url for evalidator api call
  if (popType %in% c("CURR", "VOL")) {
    if (popType == "CURR") {
      attribute_nbrd <- NULL
    }
     
    url <- paste0("https://apps.fs.usda.gov/fiadb-api/fullreport?",
             "pselected=State%20code",
             "&rselected=", rowvarstr,
             "&cselected=", colvarstr,
             "&snum=", attribute_nbr,
             "&sdenom=", attribute_nbrd,
             "&strFilter=", strFilter,
             "&wc=", evalgrp,
             "&outputFormat=NJSON")
    
  } else if (popType == "CHNG") {
    
    url <- paste0("https://apps.fs.usda.gov/fiadb-api/fullreport?",
             "pselected=State%20code",
             "&rselected=", rowvarstr, 
             "&rtime=Previous",
             "&cselected=", colvarstr,
             "&snum=", attribute_nbr,
             "&wc=", evalgrp, 
             "&outputFormat=NJSON")
    
  } else {
    stop()
  }

  # call api and get total estimate for 
  res <- tryCatch(
    fiadb_api_GET(url = url),
    error = function(e) {
      message(e,"\n")
      return(NULL) })
  
  if (is.null(res)) {
    message("invalid url for: ", toString(evalid))
    message(url)
    if (!is.null(strFilter)) {
      message("check if table prefixes are included with variables (e.g., tree.SPCD %in% c(122,202)")
    }
    stop()
  }
  
  eval_res_estimates <- res[['estimates']]
  eval_res_rowvar <- res[['subtotals']][['GRP2']]
  eval_res_colvar <- res[['subtotals']][['GRP3']]
  eval_res_grpvar <- res[['subtotals']][['GRP2xGRP3']]
  eval_res_totals <- res[['totals']]
  
  returnlst <- list(eval_estimate = eval_res_estimates,
                    eval_rowest = eval_res_rowvar,
                    eval_colest = eval_res_colvar,
                    eval_grpest = eval_res_grpvar,
                    eval_totest = eval_res_totals)
  
  returnlst$sql <- res$metadata$sql
  returnlst$numEstDesc <- res$metadata$numEstDesc
  returnlst$url <- url
  returnlst$attribute_nbr = attribute_nbr
  if (ratio) {
    returnlst$attribute_nbrd = attribute_nbrd
  }
  
  return(returnlst)
}

#' Get FIESTA estimates
#' 
#' @param evalid 6 digit Evaluation ID
#' @param estvar estimation variable
#' @param estvar.filter estimation filter ('TREECLCD = 2', 'STATUSCD = 2', 'STATUSCD = 1') Similar to EVALIDator estn_type ('GS'-growing-stock; 'STGD'-standing-dead; 'AL'-all live)
#' @param landarea ('FOREST', 'TIMBERLAND', 'ALL'). Similar to EVALIDator land_basis ('Forest land', Timberland', 'All land')
#' @param rowvar row domain to estimate by
#' @param colvar column domain to estimate by
#' @param ratio if ratio estimation
#' @param ratiotype type of ratio ('PERACRE', 'PERTREE')
#' @param estvard estimation variable for denominator
#' @param estvard.filter estimation filter for denominator ('TREECLCD = 2', 'STATUSCD = 2', 'STATUSCD = 1') Similar to EVALIDator estd_type ('GS'-growing-stock; 'STGD'-standing-dead; 'AL'-all live)
#' @param chng_type c('total', 'annual')
#' @param chng_measurements ('both', 'either')
#' @param woodland ('Y', 'N', 'only') - whether to include woodland tree species
#' @param defaultVars Bool. Whether or not to return default variables.
#' @param dsn data source name of SQLite database with EVALIDATOR_POP_ESTIMATE table (if attribute_nbr is NULL)
#' @param conn open connection of SQLite database with EVALIDATOR_POP_ESTIMATE table (if attribute_nbr is NULL)
#'
#' @export
getFIESTAest <- function(evalid, 
                         estvar = NULL,
                         estvar.filter = NULL,
                         landarea = "FOREST",
                         rowvar = NULL,
                         colvar = NULL,
                         ratio = FALSE,
                         ratiotype = "PERACRE",
                         estvard = NULL,
                         estvard.filter = NULL,
                         chng_type = "total",
                         chng_measurements = "both",
                         woodland = "Y",
                         defaultVars = FALSE,
                         dsn = NULL, conn = NULL) {
  
  
  ## Check database connection
  if (is.null(conn)) {
    conn <- DBtestSQLite(dsn, dbconnopen = TRUE, showlist = FALSE)
  } else {
    if (!DBI::dbIsValid(conn)) {
      stop("database connection is invalid")
    }
  }
  
  ## Check evalid
  if (is.null(evalid) || (!is.null(evalid) && length(evalid) > 1)) {
    stop("invalid evalid")
  }
  
  ## Get evalType from evalid
  evalType <- substr(evalid, nchar(evalid)-1, nchar(evalid))
  
  ## Get popType from evalType
  if (!evalType %in% c("01", "03", "10")) {
    popType <- "VOL"
  } else {
    popType <- ifelse (evalType == "01", "VOL",
                       ifelse (evalType == "03", "CHNG",
                               ifelse (evalType == "10", "P2VEG")))
  }
  
  ## Check popType
  if (popType == "VOL" && is.null(estvar)) {
    popType <- "CURR"
  }

  ###############################################################################
  ## Get FIESTA estimate 
  ###############################################################################
  popdat <- tryCatch(
    FIESTA::modGBpop(popType = popType,
             popFilter = list(evalid = evalid),
             pltassgn = "POP_PLOT_STRATUM_ASSGN",
             dsn = dsn,
             dbconn = conn,
             defaultVars = defaultVars,
             unit_opts=list(unitvar2 = "STATECD", minplotnum.unit=2),
             stratalut = "POP_STRATUM",
             unitarea = "POP_ESTN_UNIT",
             unitvar = "ESTN_UNIT",
             areavar = "AREA_USED",
             strata = TRUE,
             strata_opts = list(minplotnum.strat = 2, stratcombine = F,
                                getwt=TRUE, getwtvar="P1POINTCNT")),
    error = function(e) {
      message(e,"\n")
      return(NULL) })
  if (is.null(popdat)) {
    message("popdat is invalid for ", evalid)
    stop()
  }

  ## Get estimates
  if (popType == "CURR") {
    est <- tryCatch(
      FIESTA::modGBarea(GBpopdat = popdat, 
                sumunits = TRUE, 
                rowvar = rowvar,
                colvar = colvar,
                landarea = landarea),
      error = function(e) {
        message(e,"\n")
        return(NULL) })
    if (is.null(est)) {
      message("modGBarea is invalid for ", evalid)
    }
    
  } else if (popType == "VOL") {
    
    if (ratio) {
      est <- tryCatch(
        FIESTA::modGBratio(GBpopdat = popdat,
                   ratiotype = ratiotype,
                   sumunits = TRUE, 
                   landarea = landarea,
                   estvarn = estvar, 
                   estvarn.filter = estvar.filter,
                   estvard = estvard, 
                   estvard.filter = estvard.filter,
                   woodland = woodland,
                   rowvar = rowvar,
                   colvar = colvar),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    } else {
      est <- tryCatch(
        FIESTA::modGBtree(GBpopdat = popdat, 
                  sumunits = TRUE, 
                  landarea = landarea,
                  estvar = estvar, 
                  estvar.filter = estvar.filter,
                  woodland = woodland,
                  rowvar = rowvar,
                  colvar = colvar),
        error = function(e) {
          message(e,"\n")
          return(NULL) })
    }
    if (is.null(est)) {
      message("modGBtree is invalid for ", evalid)
    }
    
  } else if (popType == "CHNG") {
    est <- tryCatch(
      FIESTA::modGBchng(GBpopdat = popdat,
                sumunits = TRUE,
                landarea = landarea,
                rowvar = rowvar,
                colvar = colvar,
                returntitle = TRUE,
                savedata = FALSE,
                chngtype = chng_type),
      error = function(e) {
        message(e,"\n")
        return(NULL) })
    if (is.null(est)) {
      message("modGBchng is invalid for ", evalid)
    }
  }
  
  
  estimate <- est$est
  totest <- est$raw$totest
  rowest <- est$raw$rowest
  colest <- est$raw$colest
  grpest <- est$raw$grpest
  
  #message(est$raw$domdatqry)
  
  returnlst <- list(fiesta_estimate = estimate,
                    fiesta_rowest = rowest,
                    fiesta_colest = colest,
                    fiesta_grpest = grpest,
                    fiesta_totest = totest)
  return(returnlst)
}

#' Compare outputs from EVALIDator and FIESTA
#' @param EVALIDatorlst List of estimates output from EVALIDator
#' @param FIESTAlst List of estimates output from FIESTA
#' @param compareType String ('TOTALS', 'ROW', 'COL', 'GRP')
#' @param precision_threshold Numeric. How close do estimates have to be to be considered a match?
#' 
#' @export
compareAPI <- function(EVALIDatorlst, 
                       FIESTAlst,
                       compareType = "ROW",
                       precision_threshold = 1) {
  
  ## Define empty list
  compare_res <- list()
  diffcnt <- 0
  
  ## EVALIDator estimates
  #eval_estimate <- EVALIDatorlst$eval_estimate
  
  ## FIESTA estimates
  #fiesta_estimate <- FIESTAlst$est
  
  
  compareTypelst <- c("TOTALS", "ROW", "COL", "GRP")
  compareType <- FIESTAutils::pcheck.varchar(var2check = compareType, 
                             varnm = "compareType",
                             checklst = compareTypelst, caption = "compareType", 
                             stopifnull = TRUE)
  
  
  if (compareType == "TOTALS") {
    eval_totest <- EVALIDatorlst$eval_totest
    if ("fiesta_totest" %in% names(FIESTAlst)) {
      fiesta_totest <- FIESTAlst$fiesta_totest
    } else {
      fiesta_totest <- FIESTAlst$raw$totest
    }
    
    df <- data.frame(Estimate.fiesta = fiesta_totest$est, 
                     ESTIMATE = unlist(eval_totest$ESTIMATE))
    
  } else if (compareType == "ROW") {
    eval_rowest <- EVALIDatorlst$eval_rowest
    if (is.null(eval_rowest)) {
      stop("no eval_rowest exists")
    }
    if ("fiesta_rowest" %in% names(FIESTAlst)) {
      fiesta_rowest <- FIESTAlst$fiesta_rowest
    } else {
      fiesta_rowest <- FIESTAlst$raw$rowest
    }
    fiesta_rowest <- fiesta_rowest[fiesta_rowest$est > 0,]
    if (names(fiesta_rowest)[1] == "INVYR") {
      fiesta_rowest <- fiesta_rowest[order(fiesta_rowest$INVYR, decreasing=TRUE),]
    }
    
    if (nrow(eval_rowest) != nrow(fiesta_rowest)) {
      message("number of rows are not equal")
      if (nrow(eval_rowest) < nrow(fiesta_rowest)) {
        fiesta_rowest <- fiesta_rowest[1:nrow(eval_rowest),]
      } else {
        eval_rowest <- eval_rowest[1:nrow(fiesta_rowest),]
      }
    }
    #fiesta_rowest <- fiesta_rowest[!is.na(as.character(fiesta_rowest[[1]])),]
    df <- data.frame(fiesta_rowest[,1, drop=FALSE], 
                     Estimate.fiesta = fiesta_rowest$est,
                     ESTIMATE = unlist(eval_rowest$ESTIMATE))
    
  } else if (compareType == "COL") {
    eval_colest <- EVALIDatorlst$eval_colest
    if (is.null(eval_colest)) {
      stop("no eval_colest exists")
    }
    if ("fiesta_colest" %in% names(FIESTAlst)) {
      fiesta_colest <- FIESTAlst$fiesta_colest
    } else {
      fiesta_colest <- FIESTAlst$raw$colest
    }
    fiesta_colest <- fiesta_colest[fiesta_colest$est > 0,]
    if (names(fiesta_colest)[1] == "INVYR") {
      fiesta_colest <- fiesta_colest[order(fiesta_colest$INVYR, decreasing=TRUE),]
    }
    
    df <- data.frame(fiesta_colest[,1, drop=FALSE], 
                     Estimate.fiesta = fiesta_colest$est,
                     ESTIMATE = unlist(eval_colest$ESTIMATE))
    
  } else if (compareType == "GRP") {
    eval_grpest <- EVALIDatorlst$eval_grpest
    if (is.null(eval_grpest)) {
      stop("no eval_grpest exists")
    }
    if ("fiesta_grpest" %in% names(FIESTAlst)) {
      fiesta_grpest <- FIESTAlst$fiesta_grpest
    } else {
      fiesta_grpest <- FIESTAlst$raw$grpest
    }
    fiesta_grpest <- fiesta_grpest[fiesta_grpest$est > 0,]
    fiesta_grpest <- fiesta_grpest[!is.na(as.character(fiesta_grpest[[1]])),]
    if ("INVYR" %in% names(fiesta_grpest)) {
      if ("INVYR" %in% names(fiesta_grpest)[1]) {
        fiesta_grpest <- data.table::setorderv(fiesta_grpest, cols=names(fiesta_grpest)[1:2], order=c(-1,1), na.last=TRUE)
      } else if ("INVYR" %in% names(fiesta_grpest[2])) {
        fiesta_grpest <- data.table::setorderv(fiesta_grpest, cols=names(fiesta_grpest)[1:2], order=c(1,-1), na.last=TRUE)
      }
    }
    
    df <- data.frame(fiesta_grpest[,1, drop=FALSE], 
                     Estimate.fiesta = fiesta_grpest$est,
                     ESTIMATE = unlist(eval_grpest$ESTIMATE))
    
  } else {
    message("compareType is invalid... must be in following list: 'TOTALS','ROW','COL','GRP'")
  }
  
  ## Subset data frame of estimate differences
  df[["Estimate.diff"]] <- round(df[["Estimate.fiesta"]] - df[["ESTIMATE"]], 8)
  df[["Estimate.pdiff"]] <- round(df[["Estimate.diff"]] / df[["ESTIMATE"]] * 100, 6)
  dfdiff <- df[with(df, abs(Estimate.diff) > precision_threshold),]
  if (nrow(dfdiff) > 0) {
    diffcnt <- diffcnt + 1
    #compare_res[[evalid]] <- dfdiff
  }
  
  if (diffcnt > 0) {
    #msg <- paste0("\nFound differences > ", precision_threshold, " in ",
    #              diffcnt, "/", length(evalid), " evalid")
    msg <- paste0("\nFound differences > ", precision_threshold)
    message(msg)
    return(dfdiff)
  } else {
    message("\nAll estimates match")
    return(NULL)
  }
}


compareAPI_evalid <- function(evalids, 
                              popType,
                              estvar = NULL,
                              estvar.filter = NULL,
                              landarea = "FOREST",
                              rowvar = NULL,
                              colvar = NULL,
                              ratio = FALSE,
                              ratiotype = "PERACRE",
                              estvard = NULL,
                              estvard.filter = NULL,
                              chng_type = "total",
                              chng_measurements = "both",
                              woodland = "Y",
                              dia5inch = FALSE,
                              dsn = NULL, conn = NULL,
                              EVALIDATOR_POP_ESTIMATE = NULL,
                              precision_threshold = 1) {
  
  ## DESCRIPTION: get attribute_nbr from POP table in database
  ##
  ## ARGUMENTS:
  ## evalids - one or more 6 digit Evaluation ID
  ## attribute_nbr - EVALIDator attribute number for estimation variable
  ##   Using attribute_nbr replaces: estvar, estvar.filter, landarea, and chng_type
  ## estvar - estimation variable
  ## estvar.filter - estimation filter ('TREECLCD = 2', 'STATUSCD = 2', 'STATUSCD = 1')
  ##   Similar to EVALIDator estn_type ('GS'-growing-stock; 'STGD'-standing-dead; 'AL'-all live)
  ## landarea - ('FOREST', 'TIMBERLAND', 'ALL'). 
  ## Similar to EVALIDator land_basis ('Forest land', Timberland', 'All land')
  ## rowvar - row domain to estimate by
  ## colvar - column domain to estimate by
  ## ratio - if ratio estimation
  ## ratiotype - type of ratio ('PERACRE', 'PERTREE')
  ## estvard - estimation variable for denominator
  ## estvard.filter - estimation filter for denominator ('TREECLCD = 2', 'STATUSCD = 2', 'STATUSCD = 1')
  ##   Similar to EVALIDator estd_type ('GS'-growing-stock; 'STGD'-standing-dead; 'AL'-all live)
  ## chng_type - c('total', 'annual')
  ## chng_measurements - c('both', 'either')
  ## woodland - ('Y', 'N', 'only') - whether to include woodland tree species
  ## dia5inch Logical. If TRUE, gets attribute with 5 inches and above
  ## dsn - data source name of SQLite database with EVALIDATOR_POP_ESTIMATE table (if attribute_nbr is NULL)
  ## conn - open connection of SQLite database with EVALIDATOR_POP_ESTIMATE table (if attribute_nbr is NULL)
  ## EVALIDATOR_POP_ESTIMATE - table with attribute numbers (if attribute_nbr is NULL)
  
  ## Define empty list
  attribute_nbr=attribute_nbrd <- NULL
  
  
  ## Define function
  getdiff <- function(df, precision_threshold, compareType) {
    ## DESCRIPTION: gets differences based on precision_threshold
    
    ## Subset data frame of estimate differences
    diffcnt <- 0
    df[["Estimate.diff"]] <- round(df[["Estimate.fiesta"]] - df[["ESTIMATE"]], 8)
    df[["Estimate.pdiff"]] <- round(df[["Estimate.diff"]] / df[["ESTIMATE"]] * 100, 6)
    dfdiff <- df[with(df, abs(Estimate.diff) > precision_threshold),]
    if (nrow(dfdiff) > 0) {
      diffcnt <- diffcnt + 1
      #compare_res[[evalid]] <- dfdiff
    }
    
    if (diffcnt > 0) {
      msg <- paste0("\nFound differences > ", precision_threshold)
      message(msg)
      return(dfdiff)
    } else {
      message("\nAll estimates match for ", compareType)
      return(NULL)
    }
  }

  ###############################################################################
  ## Get attribute number for EVALIDator estimate
  ###############################################################################
  attribute_nbr <- 
    getattnbr(popType = popType,
              estvar = estvar,
              estvar.filter = estvar.filter,
              landarea = landarea, 
              chng_type = chng_type,
              woodland = woodland,
              dia5inch = dia5inch,
              conn = conn,
              EVALIDATOR_POP_ESTIMATE = EVALIDATOR_POP_ESTIMATE)
  if (ratio) {
    if (ratiotype == "PERACRE") {
      sdenom <- 2
    } else {
      
      attribute_nbrd <- 
        getattnbr(popType = popType,
                  estvar = estvard,
                  estvar.filter = estvard.filter,
                  landarea = landarea, 
                  chng_type = chng_type,
                  woodland = woodland,
                  dia5inch = dia5inch,
                  conn = conn,
                  EVALIDATOR_POP_ESTIMATE = EVALIDATOR_POP_ESTIMATE)
    }
  }
  message("getting estimates from EVALIDator for attribute number ", attribute_nbr)

 
  ## Loop through evalids
  evalidlst <- vector(mode = "list", length = length(evalids))
  names(evalidlst) <- evalids
  for (i in 1:length(evalids)) {
    evalid <- as.character(evalids[i])
    message("checking evalid ", evalid, "...\n")
    
    
    ###############################################################################
    ## Get EVALIDator estimate from API
    ###############################################################################
    EVALIDatorlst <- 
      getAPIest(evalid = evalid, 
                rowvar = rowvar,
                colvar = colvar,
                conn = conn,
                attribute_nbr = attribute_nbr,
                attribute_nbrd = attribute_nbrd)
    
    if (is.null(EVALIDatorlst)) {
      message("no EVALIDator estimate for ", evalid)
      stop()
    }
    eval_estimate <- EVALIDatorlst$eval_estimate
    eval_rowest <- EVALIDatorlst$eval_rowest
    eval_colest <- EVALIDatorlst$eval_colest
    eval_grpest <- EVALIDatorlst$eval_grpest
    eval_totest <- EVALIDatorlst$eval_totest
    
    
    ###############################################################################
    ## Get FIESTA estimate 
    ###############################################################################
    message("getting estimates from FIESTA...\n")
    FIESTAlst <- 
      getFIESTAest(evalid = evalid,
                   conn = conn,
                   dsn = dsn,
                   landarea = landarea,
                   estvar = estvar,
                   estvar.filter = estvar.filter,
                   chng_type = chng_type,
                   rowvar = rowvar,
                   colvar = colvar,
                   ratiotype = ratiotype,
                   estvard = estvard,
                   estvard.filter = estvard.filter)
    
    if (is.null(FIESTAlst)) {
      message("no FIESTA estimate for ", evalid)
      stop()
    }
    fiesta_estimate <- FIESTAlst$fiesta_estimate
    fiesta_rowest <- FIESTAlst$fiesta_rowest
    fiesta_colest <- FIESTAlst$fiesta_colest
    fiesta_grpest <- FIESTAlst$fiesta_grpest
    fiesta_totest <- FIESTAlst$fiesta_totest
    
    
    comparelst <- list()

    ## TOTALS
    TOTAL <- data.frame(Estimate.fiesta = fiesta_totest$est, 
                        ESTIMATE = unlist(eval_totest$ESTIMATE))
    comparelst$TOTAL <- getdiff(TOTAL, precision_threshold, "TOTAL")
      
    ## ROWS
    fiesta_rowest <- fiesta_rowest[fiesta_rowest$est != 0,]
    ROW <- data.frame(fiesta_rowest[,1, drop=FALSE], 
                      Estimate.fiesta = fiesta_rowest$est,
                      ESTIMATE = unlist(eval_rowest$ESTIMATE))
    comparelst$ROW <- getdiff(ROW, precision_threshold, "ROW")
    
    ## COLUMNS
    fiesta_colest <- fiesta_colest[fiesta_colest$est != 0,]
    COL <- data.frame(fiesta_colest[,1, drop=FALSE], 
                      Estimate.fiesta = fiesta_colest$est,
                      ESTIMATE = unlist(eval_colest$ESTIMATE))
    comparelst$COL <- getdiff(COL, precision_threshold, "COL")
    
    ## GROUP
    fiesta_grpest <- fiesta_grpest[fiesta_grpest$est != 0,]
    GRP <- data.frame(fiesta_grpest[,1, drop=FALSE], 
                      Estimate.fiesta = fiesta_grpest$est,
                      ESTIMATE = unlist(eval_grpest$ESTIMATE))
    comparelst$GRP <- getdiff(GRP, precision_threshold, "GRP")
    
    #if (length(comparelst) > 0) {
      evalidlst[[evalid]] <- comparelst
    #}
  }  
  return(evalidlst)
}

#' Get FIADB pop
#' 
#' @param state State name
#' @param evaltype Evaluation Type  c('ALL', 'CURR', 'VOL', 'LULC', 'P2VEG', 'INV', 'DWM', 'CHNG', 'GRM', 'GROW', 'MORT', 'REMV')
#' @param evalyr Evaluation year (Two digits e.g. 19 for 2019)
#' @param evalid EVALID
#' @param datsource Data source ('datamart', 'sqlite')
#' @param dsn Data source name
#' @param dbconn Open Database connection
#' @param schema. Schema name. Optional
#' @param dbconnopen Bool. Leave dbconn open?
#' 
#' @export
getFIADBpop <- function(state = NULL,
                        evaltype = NULL,
                        evalyr = NULL,
                        evalid = NULL,
                        datsource = "datamart",
                        dsn = NULL,
                        dbconn = NULL,
                        schema. = "",
                        dbconnopen = TRUE) {
  ## DESCRIPTION: Gets pop tables from FIA DataMart or dsn or dbconn
  ## state - State name.
  ## evaltype - Evaluation type - c('ALL', 'CURR', 'VOL', 'LULC', 
  ##                                'P2VEG', 'INV', 'DWM', 'CHNG', 
  ##								'GRM', 'GROW', 'MORT', 'REMV')
  ## evalyr - Evaluation year - 2 digits (e.g., 19)
  ## datsource - Data source ('datamart', 'sqlite')
  ## dsn - Data source name. If datsource = 'sqlite', sqlite filename.
  ## dbconn - Open database connection
  
  
  ## Define evalid
  if (is.null(evalid)) {
    stcd <- ref_statecd[ref_statecd$MEANING == state, "VALUE"]
    
    ## Check evalyr
    ndigits <- nchar(as.character(evalyr))
    if (ndigits == 4) {
      evalyr <- substr(as.character(evalyr), 3, 4)
    } else if (ndigits != 2) {
      stop("evalyr is invalid")
    }
    evalid <- as.numeric(paste0(stcd, evalyr, evaltype))
  } else {
    stcd <- as.numeric(substr(evalid[1], 1, nchar(evalid[1]) - 4))
    evaltype <- substr(evalid[1], nchar(evalid[1]) - 1, nchar(evalid[1]))
    state <- FIESTAutils::pcheck.states(stcd)
  }
  
  ## Define variables to subset
  FIADBpopvars <- c("STATECD", "ESTN_UNIT", "STRATUMCD", "EXPNS", "P2POINTCNT")
  if (evaltype == "01") {
    FIADBpopvars <- c(FIADBpopvars, "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MACR", "ADJ_FACTOR_MICR")
  }
  if (evaltype == "03") {
    FIADBpopvars <- c(FIADBpopvars, "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MACR", "ADJ_FACTOR_MICR")
  }
  if (evaltype == "10") {
    FIADBpopvars <- c(FIADBpopvars, "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MACR", "ADJ_FACTOR_P2VEG_SUBP")
  }
  if (evaltype == "07") {
    FIADBpopvars <- c(FIADBpopvars, "ADJ_FACTOR_CWD", "ADJ_FACTOR_FWD_SM", 
                      "ADJ_FACTOR_FWD_LG")
  }
  if (evaltype == "09") {
    FIADBpopvars <- c(FIADBpopvars, "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MACR", "ADJ_FACTOR_INV_SUBP")
  }
  if (evaltype == "08") {
    FIADBpopvars <- c(FIADBpopvars, "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MACR", "ADJ_FACTOR_REGEN_MICR")
  }
  FIADBpopvars <- tolower(FIADBpopvars)

  if (is.null(dbconn) && !is.null(dsn)) {
    dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE)
  } 
  if (is.null(dbconn) || !DBI::dbIsValid(dbconn)) {
    
    if (datsource == "datamart") {
      POP_STRATUM <- DBgetCSV("POP_STRATUM", state)
      POP_ESTN_UNIT <- DBgetCSV("POP_ESTN_UNIT", state)
    } else {
      stop("dsn is invalid")
    }
  } 
  
  pop_estn_unit_qry <- 
    paste0("SELECT STATECD, ESTN_UNIT, ESTN_UNIT_DESCR, AREA_USED", 
           "\nFROM ", schema., "POP_ESTN_UNIT", 
           "\nWHERE evalid = ", evalid, 
           "\nORDER BY ESTN_UNIT")
  
  pop_stratum_qry <- 
    paste0("SELECT *",
           "\nFROM ", schema., "POP_STRATUM", 
           "\nWHERE evalid = ", evalid, 
           "\nORDER BY ESTN_UNIT, STRATUMCD")
  
  if (!is.null(dbconn)) {
    message("querying pop tables from database...")
    ## Get data from database
    pop_estn_unit <- DBI::dbGetQuery(dbconn,
                                     pop_estn_unit_qry)
    pop_stratum <- DBI::dbGetQuery(dbconn, 
                                   pop_stratum_qry)
  } else {
    pop_estn_unit <- sqldf::sqldf(pop_estn_unit_qry)
    pop_stratum <- sqldf::sqldf(pop_stratum_qry)
  }
  names(pop_estn_unit) <- tolower(names(pop_estn_unit))
  names(pop_stratum) <- tolower(names(pop_stratum))
  
  pop_stratum <- pop_stratum[, FIADBpopvars]
  pop_stratum <- pop_stratum[order(pop_stratum$statecd, pop_stratum$estn_unit, pop_stratum$stratumcd), ]
  pop_estn_unit <- pop_estn_unit[order(pop_estn_unit$statecd, pop_estn_unit$estn_unit), ]
  pop_stratum$stratumcd <- as.character(pop_stratum$stratumcd)
  pop_stratum <- pop_stratum[order(pop_stratum$estn_unit, pop_stratum$stratumcd),]
  
  if (!dbconnopen && !is.null(dbconn)) {
    DBI::dbDisconnect(dbconn)
  }
  
  return(list(pop_stratum=pop_stratum, pop_estn_unit=pop_estn_unit))
  #return(pop_stratum)
}


#' Check FIADB and FIESTA population data
#' @param FIADBpop population data FIADB
#' @param FIESTApop population data FIESTA
#' @param evalendType String. last two characters of the evalid identifying the type of response 
#' @param rnd Integer. rnd
#' 
#' @export
checkpop <- function(FIADBpop, FIESTApop, evalendType = "01", rnd = 10) {
  ## DESCRIPTION: compare population data generated by FIESTA with 
  ##              population data from FIADB.
  
  FIADBpop <- FIESTAutils::pcheck.table(FIADBpop)
  FIESTApop <- FIESTAutils::pcheck.table(FIESTApop)
  
  ## change names to lowercase
  names(FIADBpop) <- tolower(names(FIADBpop))
  names(FIESTApop) <- tolower(names(FIESTApop))
  
  ## Define variables to compare
#  popvars <- c("EXPNS", "P2POINTCNT",
#               "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR", "ADJ_FACTOR_MACR")
  popvars <- c("EXPNS", "P2POINTCNT")
  if (evalendType == "10") {
    #popvars <- c(popvars, "ADJ_FACTOR_P2VEG_SUBP")
    popvars <- c("ADJ_FACTOR_P2VEG_SUBP")
  }
  
  popvars <- c("EXPNS", "P2POINTCNT")
  if (evalendType == "01") {
    popvars <- c(popvars, "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MACR", "ADJ_FACTOR_MICR")
  }
  if (evalendType == "03") {
    popvars <- c(popvars, "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MACR", "ADJ_FACTOR_MICR")
  }
  if (evalendType == "10") {
    popvars <- c(popvars, "ADJ_FACTOR_P2VEG_SUBP")
  }
  if (evalendType == "07") {
    popvars <- c(popvars, "ADJ_FACTOR_CWD", "ADJ_FACTOR_FWD_SM", 
                      "ADJ_FACTOR_FWD_LG")
  }
  if (evalendType == "09") {
    popvars <- c(popvars, "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MACR", "ADJ_FACTOR_INV_SUBP")
  }
  if (evalendType == "08") {
    popvars <- c(popvars, "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MACR", "ADJ_FACTOR_REGEN_MICR")
  }
  popvars <- tolower(popvars) 
  
  ## Change names in FIADBpop
  names(FIADBpop)[names(FIADBpop) %in% popvars] <- 
    paste0(names(FIADBpop)[names(FIADBpop) %in% popvars], "_fiadb")

  FIESTApopvars <- popvars[popvars %in% names(FIESTApop)]
  
  
  stratvars <- c("estn_unit", "stratumcd")
  FIESTApop <- data.table::setDT(FIESTApop)[, c(stratvars, FIESTApopvars), with=FALSE]

  chkclass <- FIESTAutils::check.matchclass(FIADBpop, FIESTApop, stratvars)
  FIADBpop <- chkclass$tab1
  FIESTApop <- chkclass$tab2
  
  pop <- merge(FIADBpop, FIESTApop[, c(stratvars, FIESTApopvars), with=FALSE], 
               by=stratvars)
  
  adjvars <- popvars[grepl("adj_", FIESTApopvars)]
  

  for (i in 1:length(FIESTApopvars)) {
    popvar <- FIESTApopvars[i] 
    if (popvar %in% adjvars) {
      poptyp <- sub("adj_factor_", "", popvar)
    } else {
      poptyp <- popvar
    }
    if (sum(pop[[paste0(popvar, "_fiadb")]]) == 0 || sum(pop[[popvar]]) == 0) {
      pop[[paste0("diff_", poptyp)]] <- 0
    } else {
      pop[[paste0("diff_", poptyp)]] <- round(pop[[paste0(popvar, "_fiadb")]] - pop[[popvar]], rnd)
    }
  }
 #message(paste0(utils::capture.output(data.frame(pop)), collapse = "\n"))
  
  cols <- names(pop)[startsWith(names(pop), "diff_")]
  popdiff <- pop[pop[, rowSums(.SD) != 0, .SDcols = cols],]
  
  return(popdiff)
}



compareADJ <- function(evalids, 
                       dsn = NULL, conn = NULL,
                       rnd = 6) {
  
  ## Check database connection
  if (is.null(conn)) {
    conn <- DBtestSQLite(dsn, dbconnopen = TRUE, showlist = FALSE)
  }
  
  ## Loop through evalids
  compare_adj <- list()
  for (i in 1:length(evalids)) {
    evalid <- evalids[i]
    message("checking evalid ", evalids[i], "...\n")
    
    
    ## Get evalType from evalid
    evalType <- substr(evalid, nchar(evalid)-1, nchar(evalid))
    if (evalType %in% c("01", "03", "10")) {
      ## Get popType from evalType
      popType <- ifelse (evalType == "01", "VOL",
                         ifelse (evalType == "03", "CHNG",
                                 ifelse (evalType == "10", "P2VEG")))
    } else {
      popType <- "VOL"
    } 
    
    ## Get POP_STRATUM with adjustment factors
    FIADBpop <- getFIADBpop(evalid = evalid, 
                            dbconn = conn)$pop_stratum
    
    ## Get POP data (stratalut) from FIESTA
    popdat <- tryCatch(
      FIESTA::modGBpop(popType = popType,
               popFilter = list(evalid=evalids[i]),
               pltassgn = "POP_PLOT_STRATUM_ASSGN",
               dbconn = conn,
               unit_opts=list(unitvar2 = "STATECD", minplotnum.unit=2),
               stratalut = "POP_STRATUM",
               unitarea = "POP_ESTN_UNIT",
               unitvar = "ESTN_UNIT",
               areavar = "AREA_USED",
               strata = TRUE,
               strata_opts = list(minplotnum.strat = 2, stratcombine = F,
                                  getwt=TRUE, getwtvar="P1POINTCNT")),
      error = function(e) {
        message(e,"\n")
        return(NULL) })
    if (is.null(popdat)) {
      message("popdat is invalid for ", evalid)
      stop()
    } 
    
    ## Compare adjustment factors
    pop_compare <- checkpop(FIADBpop, 
                            FIESTApop = popdat$adjfactors, 
                            evalendType = evalType, 
                            rnd = rnd)
    if (nrow(pop_compare) == 0) {
      message("all adjustment factors match...")
    } else {
      message("some adjustment factors do not match...")
      FIESTAutils::messagedf(pop_compare)
      compare_adj[[as.character(evalid)]] <- pop_compare
    }
  }
  return(compare_adj)
}

