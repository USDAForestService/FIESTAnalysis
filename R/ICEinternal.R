ICEproject_compile <- function(proj_name, 
                               proj_obs = 1, 
                               invyrs, 
                               image_src, 
                               image_res, 
                               image_date, 
                               interpreter, 
                               interp_date, 
                               ICEproject = NULL) {
  ########################################################################
  ## DESCRIPTION: compiles or appends ICE project-level data to a 
  ##              database table for a given observation.
  ## RETURNS: Data frame or appended dataframe (if ICEproject != NULL)
  ##
  ## ARGUMENTS:
  ## proj_name - String. Name of project (e.g., Indiana).
  ## T1 - Integer. Time 1 of interpretation.
  ## T2 - Integer. Time 2 of interpretation.
  ## proj_obs - Integer. Number of observation for the project
  ## image_src - String vector. Image source information (T1, T2).
  ## image_res - String vector. Image resolution (T1, T2).
  ## image_date - String vector. Image data (T1, T2).
  ## interpreter - String. Name of interpreter.
  ## ICEproject - Data frame. An existing ICEproject table to append to.
  ########################################################################
  
  if (is.null(ICEproject)) {
    ICEproject <- data.frame(	
      CN = integer(),			## Consecutive row number
      PROJ_NAME = character(),	## Name of project (e.g., Indiana)
      PROJ_OBS = integer(),		## Observation number (coincide with INTERP_DATE)
      INVYR = integer(),			## Inventory year or Time of photo
      PREV_INVYR = integer(),		## Previous inventoy year
      image_src = character(),		## Image source (e.g., 'NAIP')
      IMAGE_RES = character(),		## Image resolution (e.g., '1m')
      IMAGE_DATE = as.Date(character()),	## Image Date (more detail to INVYR)
      INTERPRETER = character(),			## Interpreter
      INTERP_DATE = as.Date(character()),	## Interpretation date
      stringsAsFactors=FALSE)
    
    ## Define number of rows in ICEproject
    lastrow <- 0
    
  } else {
    cols <- c("CN", "PROJ_NAME", "PROJ_OBS", "INVYR", "PREV_INVYR", 
              "image_src", "IMAGE_RES", "IMAGE_DATE", 
              "INTERPRETER", "INTERP_DATE")
    if (!all(cols %in% names(ICEproject))) {
      cols.miss <- cols[!cols %in% names(ICEproject)]
      stop("columns are missing in ICEproject table: ", toString(cols.miss))
    }
    
    ## Get number of rows in ICEproject
    lastrow <- nrow(ICEproject)
  }


  ## Check variables
  ###################################################################

  ## Check proj_name
  if (!is.character(proj_name) || length(proj_name) > 1) {
    stop("proj_name must be a character vector of length 1")
  }

  ## Check proj_obs
  if (!is.numeric(proj_obs) || length(proj_obs) > 1) {
    stop("proj_obs must be a character vector of length 1")
  }

  ## Check invyrs
  if (!is.numeric(invyrs) || length(invyrs) < 2) {
    stop("invyrs must be a numeric vector of 2 or more years")
  } else if (any(invyrs < 1000) || any(invyrs > 3000)) {
    stop("invyrs must be in YYYY format")
  }

  ## Get number of inventory years
  nbrINVYR <- length(invyrs)
  
  ## Get previous inventory years for observation
  PREV_INVYR <- c(NA, invyrs[length(invyrs)-1])

  ## Check image source
  if (!is.null(image_src)) {
    if (all(is.na(image_src))) {
      message("no image source included")
    } else if (!is.character(image_src) || length(image_src) > nbrINVYR) {
      stop("image source must be a character vector of 1 or ", nbrINVYR, " image sources")
    } else if (length(image_src) == 1) {
      image_src <- rep(image_src, nbrINVYR)
      message("assuming image_src is the same for all invyrs")
    }
  }

  ## Check image resolution
  if (!is.null(image_res)) {
    if (all(is.na(image_res))) {
      message("no image res included")
    } else if (length(image_res) > nbrINVYR) {
      stop("image res must be a numeric vector of 1 or ", nbrINVYR, " image sources")
    } else if (length(image_res) == 1) {
      image_res <- rep(image_res, nbrINVYR)
      message("assuming image_res is the same for all invyrs")
    }
  }

  ## Check image date
  if (!is.null(image_date)) {
    if (all(is.na(image_date))) {
      message("no image date included")
    } else if (length(image_date) > nbrINVYR) {
      stop("image date must be a numeric vector of 1 or ", nbrINVYR, " image sources")
    } else if (length(image_date) == 1) {
      image_date <- rep(image_date, nbrINVYR)
      message("assuming image_date is the same for all invyrs")
    }
  }

  ## Check interpreter
  if (is.na(interpreter) || is.null(interpreter)) {
    interpreter <- NA
  } else if (!is.character(interpreter) || length(interpreter) > 1) {
    stop("interpreter must be a character vector of length 1")
  }

  ## Check interp_date
  if (is.na(interp_date) || is.null(interp_date)) {
    interp_date <- NA
  } else if (length(interp_date) > 1) {
    stop("interp_date must be a character vector of length 1")
  }


  ## Create data frame
  df <- data.frame(CN = seq(lastrow+1, lastrow+nbrINVYR, 1), 
		        PROJ_NAME = rep(proj_name, nbrINVYR), 
                   PROJ_OBS = rep(proj_obs, nbrINVYR),
		        INVYR = invyrs, 
                   PREV_INVYR = PREV_INVYR, 
		        image_src = image_src, 
                   IMAGE_RES = image_res, 
		        IMAGE_DATE = image_date,
		        INTERPRETER = rep(interpreter, nbrINVYR), 
                   INTERP_DATE = rep(interp_date, nbrINVYR))
  
  message("appending ", nrow(df), " rows to ICEproject...")
  
  ICEproject <- rbind(ICEproject, df)
  
  return(ICEproject)
}




ICEpoint_compile <- function(ICEpnt, 
                             proj_name, 
                             proj_obs, 
                             T1, T2, 
                             ICEpoint = NULL) {
  ########################################################################
  ## DESCRIPTION: compiles table for UNIMS from ICE data
  ##
  ## ARGUMENTS:
  ## ICEpnt - Data frame. ICE point data.
  ## proj_name - String. Name of project (e.g., Indiana).
  ## T1 - Integer. Time 1 of interpretation.
  ## T2 - Integer. Time 2 of interpretation.
  ## ICEpoint.lastrow - Integer. Row number of last row in ICEpoint 
  ##		(i.e., nrow(ICEpoint).
  ########################################################################
  
  if (is.null(ICEpoint)) {
    ICEpoint <- data.frame(
			CN = integer(), 			## Consecutive row number
			PROJ_NAME = character(), 	## Name of project (e.g., Indiana)
			PROJ_OBS = integer(),		## Observation number
			PLOT_ID = integer(),		## Unique identifier of plot
			DOT_ID = integer(),		## Unique identifier of point
			INVYR = integer(),		## Inventory year or Time of photo
			COVER = integer(),		## Land cover code
			USE = integer(),			## Land use code
			CHANGE = integer(),		## Did plot change (1/0)
			CHANGE_AGENT = integer(),	## Change agent of changed point
			FLAG = integer(),		## Flag if mistake in point call
			FLAG_NOTE = character(),	## Note for flag
			stringsAsFactors=FALSE) 
   
    ## Define number of rows in ICEproject
    lastrow <- 0
    
  } else {
    cols <- c("CN", "PROJ_NAME", "PROJ_OBS", "PLOT_ID", "DOT_ID", 
              "INVYR", "COVER", "USE", "CHANGE", "CHANGE_AGENT",
              "FLAG", "FLAG_NOTE")
    if (!all(cols %in% names(ICEpoint))) {
      cols.miss <- cols[!cols %in% names(ICEpoint)]
      stop("columns are missing in ICEpoint table: ", toString(cols.miss))
    }
    
    ## Get number of rows in ICEpoint
    lastrow <- nrow(ICEpoint)
  }



  ## Get number of points in ICEpnt
  nbrpnts <- nrow(ICEpnt)

  ## Define columns in data.frame - Time 1
  CN.1 <- seq(lastrow+1, lastrow+nbrpnts, 1)
  PROJ_NAME.1 <- rep(proj_name, nbrpnts)
  PROJ_OBS.1 <- rep(proj_obs, nbrpnts)
  PLOT_ID.1 <- ICEpnt$plot_id
  DOT_ID.1 <- ICEpnt$dot_id
  INVYR.1 <- T1
  COVER.1 <- ICEpnt$cover_1
  USE.1 <- ICEpnt$use_1
  CHANGE.1 <- NA
  CHANGE_AGENT.1 <- NA
  FLAG.1 <- ICEpnt$FLAG
  FLAG_NOTE.1 <- ICEpnt$FLAG_NOTE
 
  df.1 <- data.frame(CN = CN.1, 
		          PROJ_NAME = PROJ_NAME.1, 
                     PROJ_OBS = PROJ_OBS.1,
		          PLOT_ID = PLOT_ID.1, 
                     DOT_ID = DOT_ID.1, 
                     INVYR = INVYR.1,
		          COVER = COVER.1, 
                     USE = USE.1, 
		          CHANGE = CHANGE.1, 
                     CHANGE_AGENT = CHANGE_AGENT.1,
		          FLAG = FLAG.1, 
                     FLAG_NOTE = FLAG_NOTE.1, 
		          stringsAsFactors = FALSE)
 

  ## Define columns in data.frame - Time 2
  CN.2 <- seq(max(CN.1)+1, max(CN.1)+nbrpnts, 1)
  PROJ_NAME.2 <- rep(proj_name, nbrpnts)
  PROJ_OBS.2 <- rep(proj_obs, nbrpnts)
  PLOT_ID.2 <- ICEpnt$plot_id
  DOT_ID.2 <- ICEpnt$dot_id
  INVYR.2 <- T2
  COVER.2 <- ICEpnt$cover_2
  USE.2 <- ICEpnt$use_2
  CHANGE.2 <- ICEpnt$change_1_2
  CHANGE_AGENT.2 <- ICEpnt$chg_ag_2
  FLAG.2 <- ICEpnt$FLAG
  FLAG_NOTE.2 <- ICEpnt$FLAG_NOTE

  df.2 <- data.frame(CN = CN.2, 
		          PROJ_NAME = PROJ_NAME.2, 
                     PROJ_OBS = PROJ_OBS.2,
		          PLOT_ID = PLOT_ID.2, 
                     DOT_ID = DOT_ID.2, 
                     INVYR = INVYR.2,
                     COVER = COVER.2, 
                     USE = USE.2, 
                     CHANGE = CHANGE.2, 
                     CHANGE_AGENT = CHANGE_AGENT.2, 
                     FLAG = FLAG.2, 
                     FLAG_NOTE = FLAG_NOTE.2, 
                     stringsAsFactors = FALSE)

  df <- rbind(df.1, df.2) 

  message("appending ", nrow(df), " rows to ICEpoint...")
  
  ICEpoint <- rbind(ICEpoint, df)
  
  return(ICEpoint)
}




getICEdata <- function(proj_name, T1, T2, autoedit=FALSE) {
  ########################################################################
  ## DESCRIPTION: extracts data from ICEproject and ICEpoints for
  ## 		two specified time frames
  ##
  ## ARGUMENTS:
  ## proj_name - String. Name of project (e.g., Indiana).
  ## T1 - Integer. Time 1 of interpretation.
  ## T2 - Integer. Time 2 of interpretation.
  ## autoedit - if TRUE, backpopulates data based on most current data.
  ########################################################################
  ## Set global variables
  pntcheck <- NULL

  ## Set table names to change
  names2change <- c("CN", "PROJ_OBS", "INVYR", "COVER", "USE")


  db.invyrs <- unique(ICEpoint$INVYR)
  if (!T1 %in% db.invyrs) {
    stop("T1 not in database")
  }
  if (!T2 %in% db.invyrs) {
    stop("T2 not in database")
  }

  ## Time 1 query - most current observation
  ###############################################
  T1points.qry <- 
  paste("select p.* from ICEpoint p
		INNER JOIN 
		(select PROJ_NAME, PLOT_ID, DOT_ID, max(PROJ_OBS) maxobs
		from ICEpoint 
			where PROJ_NAME = ", paste0("'", proj_name,"'"),
				"and INVYR =", T1, 
                      "and COVER != -1", 		
			"group by PROJ_NAME, PLOT_ID, DOT_ID) pp
		ON p.PLOT_ID = pp.PLOT_ID 
			and p.DOT_ID = pp.DOT_ID 
			and p.PROJ_NAME = pp.PROJ_NAME
			and p.INVYR = ", T1,
			"and p.PROJ_OBS = pp.maxobs")
  T1points <- sqldf::sqldf(T1points.qry)
  #dim(T1points)
  #head(T1points)


  ## Append "_1" to names to identify Time 1 observations
  names(T1points)[names(T1points) %in% names2change] <- paste0(names2change, "_1")
  #head(T1points)

  ## Check 1 plot (Note: should be 45 records - most current observationsfor 2014)
  #T1points[T1points$PLOT_ID == id,]


  ## Time 2 query - most current observation
  ###############################################
  T2points.qry <- 
  paste("select p.* from ICEpoint p
		INNER JOIN 
		(select PROJ_NAME, PLOT_ID, DOT_ID, max(PROJ_OBS) maxobs
		from ICEpoint 
			where PROJ_NAME = ", paste0("'",proj_name,"'"),
				"and INVYR =", T2,
                      "and COVER != -1", 		
			"group by PROJ_NAME, PLOT_ID, DOT_ID) pp
		ON p.PLOT_ID = pp.PLOT_ID 
			and p.DOT_ID = pp.DOT_ID 
			and p.PROJ_NAME = pp.PROJ_NAME
			and p.INVYR = ", T2,
			"and p.PROJ_OBS = pp.maxobs")
  T2points <- sqldf::sqldf(T2points.qry)
  #dim(T2points)
  #head(T2points)


  ## Append "_2" to names to identify Time 2 observations
  names(T2points)[names(T2points) %in% names2change] <- paste0(names2change, "_2")
  #head(T2points)

  ## Check 1 plot (Note: should be 45 records - most current observationsfor 2016)
  #T2points[T2points$PLOT_ID == id,]


  #################################################################################
  ## Combine most current observations T1 with most current observations T2 into 1 table
  #################################################################################
  T1T2points <- merge(T1points[, c("PROJ_NAME", "PLOT_ID", "DOT_ID", 
			"CN_1", "INVYR_1", "COVER_1", "USE_1")], 
		T2points[, c("PROJ_NAME", "PLOT_ID", "DOT_ID", 
			"CN_2", "INVYR_2", "COVER_2", "USE_2",
			"CHANGE", "CHANGE_AGENT", "FLAG", "FLAG_NOTE")],
	by=c("PROJ_NAME", "PLOT_ID", "DOT_ID"))
  names(T1T2points)[names(T1T2points) == "CHANGE"] <- "CHANGE_1_2"
  T1T2points <- T1T2points[order(T1T2points$PLOT_ID, T1T2points$DOT_ID), ]
  #head(T1T2points)
  #dim(T1T2points)


  ## Check 1 plot (Note: should be 45 records - most current observationsfor 2016)
  #T1T2points[T1T2points$PLOT_ID == id,]

  ## Project-level data for T1 and T2
  T1T2project <- ICEproject[ICEproject$INVYR %in% c(T1, T2),]
  #T1T2project

  ## Return points and project data
  returnlst <- list(ICEpnt=T1T2points, ICEprj=T1T2project)


  ## Check data from discrepancies
  ####################################################################
  pntcheck <- T1T2points[((T1T2points$USE_1 != T1T2points$USE_2 |
				T1T2points$COVER_1 != T1T2points$COVER_2)) & 
			(!is.na(T1T2points$CHANGE_AGENT) & T1T2points$CHANGE_AGENT == 0), ]
  if (nrow(pntcheck) > 0) {
    pntcheckid <- unique(pntcheck$PLOT_ID)
    length(pntcheckid)
    message("there are ", length(pntcheckid), " plots with code descrepancies... ",
		nrow(pntcheck), " points changed, but no change agent identified")
    returnlst$pntcheck <- pntcheck

    if (autoedit) {
      message(nrow(pntcheck), 
	 	" points edited to reflect no change, based on most current time")
      T1T2points[((T1T2points$USE_1 != T1T2points$USE_2 |
				T1T2points$COVER_1 != T1T2points$COVER_2)) & 
			(!is.na(T1T2points$CHANGE_AGENT) & T1T2points$CHANGE_AGENT == 0), 
			"USE_1"] <- T1T2points[((T1T2points$USE_1 != T1T2points$USE_2 |
				T1T2points$COVER_1 != T1T2points$COVER_2)) & 
			(!is.na(T1T2points$CHANGE_AGENT) & T1T2points$CHANGE_AGENT == 0), 
			"USE_2"]
      T1T2points[((T1T2points$USE_1 != T1T2points$USE_2 |
				T1T2points$COVER_1 != T1T2points$COVER_2)) & 
			(!is.na(T1T2points$CHANGE_AGENT) & T1T2points$CHANGE_AGENT == 0), 
			"COVER_1"] <- T1T2points[((T1T2points$USE_1 != T1T2points$USE_2 |
				T1T2points$COVER_1 != T1T2points$COVER_2)) & 
			(!is.na(T1T2points$CHANGE_AGENT) & T1T2points$CHANGE_AGENT == 0), 
			"COVER_2"]
      returnlst$ICEpnt <- T1T2points
    }
    pltcheck <- setDT(pntcheck)[, .N, by="PLOT_ID"]
    setnames(pltcheck, c("PLOT_ID", "NBRPNTS"))  
    Nplots <- setDT(T1T2points)[, .N, by="PLOT_ID"]
    pltcheck <- merge(pltcheck, Nplots, by="PLOT_ID")
    pltcheck$PCTPNTS <- round(pltcheck$NBRPNTS / pltcheck$N * 100, 2)
    returnlst$pltcheck <- pltcheck
  }

  return(returnlst)
}
