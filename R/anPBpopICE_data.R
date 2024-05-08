#' ANALYSIS - Photo-based estimates for Image Change Estimation (ICE).
#'
#' Generates percent or acre estimates by domain (and estimation unit).
#'
#' domlut - the domain look up table
#'
#' \tabular{llll}{ \tab \bold{DOMCODE} \tab \bold{DOMNAME} \tab \bold{DOMTITLE}
#' \cr \tab change_1_2 \tab change_1_2_nm \tab Change \cr \tab change_1_2_GRP
#' \tab change_1_2_GRP_nm \tab Change \cr \tab cover_1 \tab cover_1_nm \tab
#' T1-Cover \cr \tab cover_2 \tab cover_2_nm \tab T2-Cover \cr \tab use_1 \tab
#' use_1_nm \tab T1-Use \cr \tab use_2 \tab use_2_nm \tab T2-Use \cr \tab
#' chg_ag_2 \tab chg_ag_2_nm \tab Change Agent \cr \tab chg_ag_2_GRP \tab
#' chg_ag_2_GRP_nm \tab Change Agent-GRP \cr \tab cover_1_GRP \tab
#' cover_1_GRP_nm \tab T1-Cover-GRP \cr \tab cover_2_GRP \tab cover_2_GRP_nm
#' \tab T2-Cover-GRP \cr \tab use_1_FOR \tab use_1_FOR_nm \tab T1-Use-FOR \cr
#' \tab use_2_FOR \tab use_2_FOR_nm \tab T2-Use-FOR \cr \tab change_pnt \tab
#' change_pnt_nm \tab Change \cr \tab use_1_2 \tab use_1_2_nm \tab Use_Change
#' \cr \tab cover_1_2 \tab cover_1_2_nm \tab Cover_Change \cr \tab use_1_2_FOR
#' \tab use_1_2_FOR_nm \tab Use_FOR_Change \cr }
#'
#' @param ice.pntfn Comma-delimited file (*.csv). The full path file name for
#' the ICE point-level data.
#' @param ice.pltfn Comma-delimited file (*.csv). The full path file name for
#' the ICE plot-level data.
#' @param plotid String. Name of unique identifier for plots.
#' @param pntid String. Name of unique identifier for points.
#' @param changelut Data frame or comma-delimted file. A different look up
#' table for land use or land cover change (see ref_ICE_change).
#' @param coverlut Data frame or comma-delimited file. A different look up
#' table for land cover (see ref_ICE_cover).
#' @param uselut Data frame or comma-delimited file. A different look up table
#' for land cover (see ref_ICE_use).
#' @param agentlut Data frame or comma-delimited file. A different look up
#' table for causal agent (see ref_ICE_agent).
#' @param appendluts Logical. If TRUE, appends all look up tables to ice.dots.
#' @param domlut Data frame or comma-delimited file. A look up table for
#' defining domain codes, names, and titles. If NULL, a default table is used.
#' @param savedata Logical. If TRUE, saves tables to outfolder.
#' @param saveobj Logical. If TRUE, saves returned list object to outfolder.
#' @param objnm String. Name of *.rds object.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @return \item{ice.dots}{ DF. ICE point data, including look up table data,
#' if appendluts=TRUE.  } \item{changelut}{ DF. The look up table for land use
#' or land cover change. } \item{coverlut}{ DF. The look up table for land
#' cover change. } \item{uselut}{ DF. The look up table for land use change. }
#' \item{agentlut}{ DF. The look up table for causal agent. } \item{domlut}{
#' DF. The look up table for domain information. }
#'
#' If savedata=TRUE, the ice.dots data frame is written to outfolder, including
#' look up table data, if appendluts=TRUE.
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anPBpopICE_data <- function(ice.pntfn, 
                            ice.pltfn = NULL, 
                            plotid = "plot_id", 
                            pntid = "dot_cnt", 
                            changelut = NULL, 
                            coverlut = NULL, 
                            uselut = NULL, 
                            agentlut = NULL, 
                            appendluts = TRUE, 
                            domlut = NULL, 
                            savedata = FALSE, 
                            saveobj = FALSE, 
                            objnm = "PBpopICE",
                            savedata_opts = NULL){

  ##########################################################################
  ## DESCRIPTION: Set up ICE data with code classes
  ## ice.pntfn	- point-level data file name
  ## ice.pltfn	- plot-level data file name
  ## ice.QAQCfn	- compiled QAQC data file name
  ## plotid	- unique identifier of plots in ice.pnt and ice.plt
  ## pntid		- unique identifier of points in ice.pnt
  ## changelut	- different look up table for land use or land cover change
  ##				(see ref_ICE_change)
  ## coverlut	- different look up table for land cover (see ref_ICE_cover)
  ## uselut	- different look up table for land use (see ref_ICE_use)
  ## agentlut	- different look up table for causal agent (see ref_ICE_agent)
  ## appendluts	- logical. if TRUE, append all look up tables to ice.pnt
  ## domlut	- data frame defining domain codes, names, and titles
  ## savedata	- logical. if TRUE, saves data to outfolder
  ## ... 		- other parameters to datExportdata
  ##########################################################################

  ## Set global variables
  cover_1=cover_2=use_1=use_2=chg_ag_2=change_1_2=use_1_2=use_1_2_nm=
	use_1_nm=use_2_nm=cover_1_2=cover_1_2_nm=cover_1_nm=cover_2_nm=
	use_1_2_FOR=use_1_FOR=use_2_FOR=use_1_2_FOR_nm=use_1_FOR_nm=
	use_2_FOR_nm=COLOR <- NULL
  gui <- FALSE
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################

    ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(anPBpopICE_data))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  ## Check parameter lists
  pcheck.params(savedata_opts=savedata_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      assign(names(savedata_opts)[[i]], savedata_opts[[i]])
    }
  }
  

  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  
  ## Import ICE dots
  ice.pnt <- pcheck.table(ice.pntfn)

  ## Import ICE plots
  ice.plt <- pcheck.table(ice.pltfn)

#  ## Import ICE QAQC data
#  ice.QAQC <- pcheck.table(ice.QAQCfn)
#
#  if (!is.null(ice.QAQC)) {
#    if (!all(c("QAQC_check", "Result") %in% names(ice.QAQC))) {
#      warning("ice.QAQC table must include QAQC_check and Result columns")
#      ice.QAQC <- NULL
#    } else {
#      check.vals <- c("PC_self_check", "PC_cross_check", "LC1_2_self_check",
#		"LC1_2_cross_check", "LU1_2_self_check", "LU1_2_cross_check")
#      if (!all(check.vals %in% ice.QAQC[["QAQC_check"]])) {
#        miss.vals <- check.vals[!check.vals %in% ice.QAQC[["QAQC_check"]]]
#        warning("ice.QAQC table must include following values in QAQC_check column: ",
#		toString(miss.vals))
#        ice.QAQC <- NULL
#      }
#    }
#  }
  
  ## Check savedata 
  savedata <- pcheck.logical(savedata, varnm="savedata", 
              title="Save data?", first="NO", gui=gui, stopifnull=TRUE)
  
  ## Check saveobj 
  saveobj <- pcheck.logical(saveobj, varnm="saveobj", 
                    title="Save object?", first="NO", gui=gui, stopifnull=TRUE)
  
  ## Check objnm
  if (is.null(objnm)) {
    objnm <- "PBpopICE"
  }
  
  ## Check output
  ########################################################
  if (savedata || saveobj) {
    outlst <- pcheck.output(outfolder=outfolder, out_dsn=out_dsn, 
                out_fmt=out_fmt, outfn.pre=outfn.pre, outfn.date=outfn.date, 
                overwrite_dsn=overwrite_dsn, overwrite_layer=overwrite_layer,
                add_layer=add_layer, append_layer=append_layer, gui=gui)
    outfolder <- outlst$outfolder
    out_dsn <- outlst$out_dsn
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    append_layer <- outlst$append_layer
    outfn.date <- outlst$outfn.date
    outfn.pre <- outlst$outfn.pre
  }
  

  #########################################################################
  ## Remove points that were not observed or there was seasonal change or
  ## vegetation expansion (change_1_2 == c(0,2,3))
  ## This gives us 45 points per plot where there was observed change,
  ## and 5 points per plots where there was no change observed.
  #########################################################################
  ice.pnt <- ice.pnt[(change_1_2 %in% c(0,2,3) & get(eval(pntid)) %in%
	c(1,26,31,36,41)) | change_1_2 == 1,]
  n.ice.pnt <- nrow(ice.pnt)

  ## Check to make sure there are either 5 points or 45 points
  idtab <- table(ice.pnt[[plotid]])
  lt5 <- names(idtab)[idtab < 5]
  if (length(lt5) > 0) {
    stop("check data:", paste(lt5, collapse=", "))
  }
  
  ## Remove all points that were uninterpretable at Time 1 or Time 2
  ## This makes sure all estimates of change add up.
  ice.pnt <- ice.pnt[cover_1 != 999 & cover_2 != 999 & use_1 != 999 &
		use_2 != 999 & chg_ag_2 != 99,]
  if (nrow(ice.pnt) < n.ice.pnt) {
    message(nrow(ice.pnt) - n.ice.pnt,
		" uninterpretable points were removed from dataset")
  }


  #########################################################################
  ## LOAD LUTS and merge to ice.pnt table
  #########################################################################

  ## changelut - table defining change
  ##########################################################################
  if (is.null(changelut)) {
    changelut <- data.table(FIESTAnalysis::ref_ICE_change)
  } else {
    changelut <- pcheck.table(changelut, tabnm="changelut", nullcheck=TRUE, gui=gui)
  }

  ## coverlut - table defining land cover codes
  ##########################################################################
  if (is.null(coverlut)) {
    coverlut <- data.table(FIESTAnalysis::ref_ICE_cover)
  } else {
    coverlut <- pcheck.table(coverlut, tabnm="coverlut", nullcheck=TRUE, gui=gui)
  }
 
  ## uselut - table defining land use codes
  ##########################################################################
  if (is.null(uselut)) {
    uselut <- data.table(FIESTAnalysis::ref_ICE_use)
  } else {
    uselut <- pcheck.table(uselut, tabnm="uselut", nullcheck=TRUE, gui=gui)
  }

  ## agentlut - table defining causal agent codes
  ##########################################################################
  if (is.null(agentlut)) {
    agentlut <- data.table(FIESTAnalysis::ref_ICE_agent)
  } else {
    agentlut <- pcheck.table(agentlut, tabnm="agentlut", nullcheck=TRUE, gui=gui)
  }


  ##########################################################################
  ## domlut - table defining domain codes, names, and titles
  ##########################################################################
  if (is.null(domlut)) {
    domlut <- data.table::data.table(
	DOMCODE = c("change_1_2", "change_1_2_GRP", "cover_1", "cover_2",
		"use_1", "use_2", "chg_ag_2", "chg_ag_2_GRP", "cover_1_GRP", "cover_2_GRP",
		"use_1_FOR", "use_2_FOR", "change_pnt"),
	DOMNAME = c("change_1_2_nm", "change_1_2_GRP_nm", "cover_1_nm", "cover_2_nm",
		"use_1_nm", "use_2_nm", "chg_ag_2_nm", "chg_ag_2_GRP_nm", "cover_1_GRP_nm",
		"cover_2_GRP_nm", "use_1_FOR_nm", "use_2_FOR_nm", "change_pnt_nm"),
	DOMTITLE = c("Change", "Change", "T1-Cover", "T2-Cover", "T1-Use", "T2-Use",
		"Change Agent", "Change Agent-GRP", "T1-Cover-GRP", "T2-Cover-GRP",
		"T1-Use-FOR", "T2-Use-FOR", "Change"), stringsAsFactors = FALSE)
  } else {
    domlut <- pcheck.table(domlut, tabnm="domlut", nullcheck=TRUE, gui=gui)
    domlutnames <- c("DOMCODE", "DOMNAME", "DOMTITLE")
    namesnot <- domlutnames[!which(domlutnames %in% names(domlut))]
    if (length(namesnot) > 0)
      stop("domlut must have column names of DOMCODE, DOMNAME, DOMTITLE")
  }


  ##########################################################################
  ## Check for missing codes and append to ice.pnt (if appendlut=TRUE)
  ##########################################################################
  colorder <- c(plotid, pntid, "change_1_2")

  ## Check if all values exist in cover_LUT
  misscd <- unique(ice.pnt$cover_1[which(!ice.pnt$cover_1 %in% coverlut$cover)])
  if (length(misscd) > 0)
    print(paste("missing attribute values in cover_LUT: ", addcommas(misscd)))

  if (appendluts) {
    ## cover_1
    col <- "cover_1"
    new_LUT <- data.table::copy(coverlut)
    names(new_LUT) <- sub("cover", col, names(new_LUT))

    ## Get names in new_LUT that are not in ice.pnt
    newnames1 <- names(new_LUT)[!names(new_LUT) %in% c(col, "COLOR")] 
    if (any(newnames1 %in% names(ice.pnt))) {
      newnames1 <- newnames1[!newnames1 %in% names(ice.pnt)]
    }
    if (length(newnames1) > 0) {
      ice.pnt <- merge(ice.pnt, new_LUT[, c(col, newnames1), with=FALSE], by=col)
    }
    colorder <- c(colorder, col, newnames1)
    data.table::setcolorder(ice.pnt, 
			c(colorder, names(ice.pnt)[which(!names(ice.pnt) %in% colorder)]))

    ## cover_2
    col <- "cover_2"
    new_LUT <- data.table::copy(coverlut)
    names(new_LUT) <- sub("cover", col, names(new_LUT))

    ## Get names in new_LUT that are not in ice.pnt
    newnames2 <- names(new_LUT)[!names(new_LUT) %in% c(col, "COLOR")] 
    if (any(newnames2 %in% names(ice.pnt))) {
      newnames2 <- newnames2[!newnames2 %in% names(ice.pnt)]
    }
    if (length(newnames2) > 0) {
      ice.pnt <- merge(ice.pnt, new_LUT[, c(col, newnames2), with=FALSE], by=col)
    }
    colorder <- c(colorder, col, newnames2)
    data.table::setcolorder(ice.pnt, c(colorder, names(ice.pnt)[which(!names(ice.pnt) %in% colorder)]))
  }

  ##########################################################################
  ## uselut
  ##########################################################################

  ## Check if all values exist in use_LUT
  misscd <- unique(ice.pnt$use_1[which(!ice.pnt$use_1 %in% uselut$use)])
  if (length(misscd) > 0)
    print(paste("missing attribute values in use_LUT: ", addcommas(misscd)))

  if (appendluts) {
    ## use_1
    col <- "use_1"
    new_LUT <- data.table::copy(uselut)
    names(new_LUT) <- sub("use", col, names(new_LUT))

    ## Get names in new_LUT that are not in ice.pnt
    newnames1 <- names(new_LUT)[!names(new_LUT) %in% c(col, "COLOR")] 
    if (any(newnames1 %in% names(ice.pnt))) {
      newnames1 <- newnames1[!newnames1 %in% names(ice.pnt)]
    }
    if (length(newnames1) > 0) {
      ice.pnt <- merge(ice.pnt, new_LUT[, c(col, newnames1), with=FALSE], by=col)
    }
    colorder <- c(colorder, col, newnames1)
    data.table::setcolorder(ice.pnt, c(colorder, names(ice.pnt)[which(!names(ice.pnt) %in% colorder)]))

    ## use_2
    col <- "use_2"
    new_LUT <- data.table::copy(uselut)
    names(new_LUT) <- sub("use", col, names(new_LUT))

    ## Get names in new_LUT that are not in ice.pnt
    newnames2 <- names(new_LUT)[!names(new_LUT) %in% c(col, "COLOR")] 
    if (any(newnames2 %in% names(ice.pnt))) {
      newnames2 <- newnames2[!newnames2 %in% names(ice.pnt)]
    }
    if (length(newnames2) > 0) {
      ice.pnt <- merge(ice.pnt, new_LUT[, c(col, newnames2), with=FALSE], by=col)
    }
    colorder <- c(colorder, col, newnames2)
    data.table::setcolorder(ice.pnt, 
		c(colorder, names(ice.pnt)[which(!names(ice.pnt) %in% colorder)]))
  }

  ##########################################################################
  ## change agent
  ##########################################################################

  ## Check if all values exist in chg_ag_2_LUT
  misscd <- unique(ice.pnt$chg_ag_2[which(!ice.pnt$chg_ag_2 %in% agentlut$chg_ag_2)])
  if (length(misscd) > 0)
    print(paste("missing attribute values in agentlut: ", paste(misscd, collapse=", ")))

  if (appendluts) {
    col <- "chg_ag_2"

    ## Get names in new_LUT that are not in ice.pnt
    newnames <- names(agentlut)[!names(agentlut) %in% c(col, "COLOR")] 
    if (any(newnames %in% names(ice.pnt))) {
      newnames <- newnames[!newnames %in% names(ice.pnt)]
    }
    if (length(newnames) > 0) {
      ice.pnt <- merge(ice.pnt, agentlut[, c(col, newnames), with=FALSE], by=col)
    }
    colorder <- c(colorder, col, newnames)
    data.table::setcolorder(ice.pnt, 
		c(colorder, names(ice.pnt)[which(!names(ice.pnt) %in% colorder)]))
  }

  ###############################################################################
  ## DEFINE change variables (use_1_2, cover_1_2, use_1_2_FOR)
  ###############################################################################
  if ("use_1" %in% names(ice.pnt) && "use_2" %in% names(ice.pnt)) {
    ice.pnt[, use_1_2 := paste(use_1, use_2, sep="_to_")]
    if ("use_1_nm" %in% names(ice.pnt) & "use_2_nm" %in% names(ice.pnt)) {
      ice.pnt[, use_1_2_nm := paste(use_1_nm, use_2_nm, sep="_to_")]

      ## Add to domlut
      if (!is.null(domlut))
        domlut <- rbind(domlut, list("use_1_2", "use_1_2_nm", "Use_Change"))
    }
  }
  if ("cover_1" %in% names(ice.pnt) && "cover_2" %in% names(ice.pnt)) {
    ice.pnt[, cover_1_2 := paste(cover_1, cover_2, sep="_to_")]
    if ("cover_1_nm" %in% names(ice.pnt) & "cover_2_nm" %in% names(ice.pnt)) {
      ice.pnt[, cover_1_2_nm := paste(cover_1_nm, cover_2_nm, sep="_to_")]

      ## Add to domlut
      if (!is.null(domlut))
        domlut <- rbind(domlut, list("cover_1_2", "cover_1_2_nm", "Cover_Change"))
    }
  }

  if ("use_1_FOR" %in% names(ice.pnt) && "use_2_FOR" %in% names(ice.pnt)) {
    if ("use_FOR_nm" %in% names(uselut))
      use_FORlut <- unique(uselut[, c("use_FOR", "use_FOR_nm")])

    ice.pnt[, use_1_2_FOR:= paste(use_1_FOR, use_2_FOR, sep="_to_")]
    if ("use_1_FOR_nm" %in% names(ice.pnt) & "use_2_FOR_nm" %in% names(ice.pnt)) {
      ice.pnt[, use_1_2_FOR_nm := paste(use_1_FOR_nm, use_2_FOR_nm, sep="_to_")]

      ## Add to domlut
      if (!is.null(domlut))
        domlut <- rbind(domlut, list("use_1_2_FOR", "use_1_2_FOR_nm", "Use_FOR_Change"))
    }
  }

  ## Remove NA values
  ice.pnt <- ice.pnt[!is.na(ice.pnt$plot_id), ]
  
  #################################################################################
  ## Variables change_pnt and change_pnt_nm were added to the chg_ag_2_LUT file
  ## These variables indicate whether a point has change. The change_1_2 variable
  ## only indicates what interpretation method was used in the process, but a true
  ## indication of change must be identified by the points. The causal agent is
  ## the best identifier of change on a point because you name a causal agent
  ## even if you don't identify a specific change between T1/T2 LU or T1/T2 LC.
  ## An example would be seeing fire occurring where the land use does not change
  ## and the cover remains the same as well.
  #################################################################################

  ## Set key, ordering by plotid and pntid
  data.table::setkeyv(ice.pnt, c(plotid, pntid))


  ## Save data to outfolder
  if (savedata) {
    ice.pntlutfn <- paste0(basename.NoExt(ice.pntfn), "_lut.csv")
    datExportData(ice.pnt, 
              savedata_opts=list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer=ice.pntlutfn,
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer,
                                  add_layer=TRUE))

    if (!is.null(ice.plt)) {
      ice.pltlutfn <- paste0(basename.NoExt(ice.pltfn), "_lut.csv")
      datExportData(ice.pnt, 
                savedata_opts=list(outfolder=outfolder, 
                                  out_fmt=out_fmt, 
                                  out_dsn=out_dsn, 
                                  out_layer=ice.pltlutfn,
                                  outfn.pre=outfn.pre, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite_layer,
                                  append_layer=append_layer,
                                  add_layer=TRUE))
    }
  }

  ## Return data
  returnlst <- list(ice.pnt=ice.pnt)
  if (!is.null(ice.plt)) {
    returnlst$ice.plt <- ice.plt
  }
  returnlst$plotid <- plotid
  returnlst$pntid <- pntid

  reflst <- list(changelut=changelut, coverlut=coverlut, uselut=uselut,
				agentlut=agentlut)
  if (!is.null(use_FORlut)) {
    reflst$use_FORlut <- use_FORlut
  }
  returnlst$reflst <- reflst
  returnlst$domlut <- domlut


  return(returnlst)
}

