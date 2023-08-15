#' ANALYSIS - Generate population data for ICE.
#'
#' Generates a set of population data for area of interest.
#'
#' If variables are NULL, then it will prompt user to input variables.
#'
#' @param ice.pntfn Comma-delimited file (*.csv). The full path file name of
#' ICE point-level data.
#' @param ice.pltfn Comma-delimited file (*.csv). The full path file name of
#' ICE plot-level data (optional).
#' @param T1 Integer. Year of Time 1 imagery (YYYY).
#' @param T2 Integer. Year of Time 2 imagery (YYYY).
#' @param plotid String. Name of unique identifier for ice45 plots.
#' @param pntid String. Name of unique identifier for the points.
#' @param pltassgn DF/DT or comma-delimited file (*.csv). Plot-level table with
#' plot assignments for strata (optional).
#' @param pltassgnid String. Name of variable for unique identifier of
#' pltassgn.
#' @param unitvar String. Name of the estimation unit variable in cond or
#' pltassgn with estimation unit assignment for each plot (e.g., 'ESTN_UNIT').
#' If one estimation unit, set unitvar=NULL.
#' @param unitvar2 String. Name of a second estimation unit variable in cond or
#' pltassgn with assignment for each plot (e.g., 'STATECD').
#' @param unitarea Numeric or DF. Total area by estimation unit. If only 1
#' estimation unit, include number of total acreage for the area of interest or
#' a data frame with areavar. If more than one estimation unit, provide a data
#' frame of total area by estimation unit, including unitvar and areavar.
#' @param areavar String. Name of area variable in unitarea. Default="ACRES".
#' @param unit.action String. What to do if number of plots in an estimation
#' unit is less than minplotnum.unit ('keep', 'remove' 'combine'). If
#' unit.action='combine', combines estimation unit to the following estimation
#' unit in unitlut.
#' @param strata Logical. If TRUE, applies post-stratification to estimation.
#' @param stratalut DF/DT. Look-up table with strata weights (proportions) or
#' area by strata (estimation unit). If no strata weights, set getwt=TRUE to
#' calculate.  If strata=FALSE, include total area by estimation unit in
#' unitarea - a stratalut will be generated with one strata (ONESTRAT) and
#' strwt = 1.
#' @param strvar String. Name of the strata variable in stratalut and pnt or
#' plt.  The default="STRATA".
#' @param getwt Logical. If TRUE, calculates strata weights from stratatlut
#' areavar.
#' @param getwtvar String. Name of variable in stratalut to calculate weights
#' (strwt).  Default="P1POINTCNT".
#' @param stratcombine Logical. If TRUE, automatically combines estimation
#' units if less than 2 plots in any one estimation unit. See notes for more
#' info.
#' @param sumunits Logical. If TRUE, sums Estimation unit results. (Note: Need
#' to include area by estimation unit).
#' @param saveobj Logical. If TRUE, saves object to outfolder.
#' @param objnm String. Object name.
#' @param savedata Logical. If TRUE, saves data tables to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param PBdataICE R Object. Output data object from anPBpopICE_data()
#' function.
#' @return List object of population data for ICE estimation.
#'
#' \item{PBx}{ Photo-based interpretation data for estimation. }
#' \item{pltassgnx}{ Data frame. Plot-level data with plot assignments of
#' Estimation unit as stratum value. } \item{plotid}{ String. Unique identifier
#' of plot. } \item{pntid}{ String. Unique identifier of points. }
#' \item{pltassgnid}{ String. Unique identifier of plot in pltassgn. }
#' \item{tabtype}{ String. Table type ('PCT', 'AREA').  } \item{sumunits}{
#' Logical. If TRUE, sums estimation unit results. } \item{unitvar}{ String.
#' Name of variable defining estimation units. } \item{strlut}{ Data frame.
#' Strata-level information (e.g., pixel counts) by estimation unit. }
#' \item{strvar}{ String. Name of variable defining strata. }
#' \item{plotsampcnt}{ NULL. } \item{getprop}{ Logical. If TRUE, gets
#' proportions from PBx data. } \item{reflst}{ List object of reference tables
#' for ICE (changelut, coverlut, uselut, agentlut, use_FORlut). }
#' \item{domlut}{ Domain reference for table output. }
#' @author Tracey S. Frescino
#' @references Frescino, Tracey S.; Moisen, Gretchen G.; Megown, Kevin A.;
#' Nelson, Val J.; Freeman, Elizabeth A.; Patterson, Paul L.; Finco, Mark;
#' Brewer, Ken; Menlove, James 2009.  Nevada Photo-Based Inventory Pilot (NPIP)
#' photo sampling procedures. Gen. Tech. Rep.  RMRS-GTR-222. Fort Collins, CO:
#' U.S. Department of Agriculture, Forest Service, Rocky Mountain Research
#' Station. 30 p.
#'
#' Patterson, Paul L. 2012. Photo-based estimators for the Nevada photo-based
#' inventory.  Res. Pap. RMRS-RP-92. Fort Collins, CO: U.S. Department of
#' Agriculture, Forest Service, Rocky Mountain Research Station. 14 p.
#' @keywords data
#' @export
anPBpopICE <- function(ice.pntfn = NULL, 
                       ice.pltfn = NULL, 
                       T1, 
                       T2, 
                       plotid = "plot_id", 
                       pntid = "dot_cnt", 
                       pltassgn = NULL, 
                       pltassgnid = NULL, 
                       unitvar = NULL, 
                       unitvar2 = NULL, 
                       unitarea = NULL, 
                       areavar = "ACRES", 
                       unit.action = "keep", 
                       strata = FALSE, 
                       stratalut = NULL, 
                       strvar = NULL, 
                       getwt = TRUE, 
                       getwtvar = "P1POINTCNT", 
                       stratcombine = TRUE, 
                       sumunits = FALSE, 
                       saveobj = FALSE,
                       objnm = "PBpopICE",
                       savedata = FALSE, 
                       savedata_opts = NULL, 
                       PBdataICE = NULL){


  ##################################################################
  ## INITIALIZE SETTINGS AND VARIABLES
  ##################################################################

  ## Set global variables
  #datstrat=datSTRATA=datACRES <- NULL

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- FALSE
  returnlst <- list()
  
  
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(anPBpopICE))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
 
  ## Check parameter lists
  pcheck.params(input.params=input.params, savedata_opts=savedata_opts)
  
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
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }
  
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################

  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata",
		title="Save data?", first="NO", gui=gui)

  ## Check savedata
  saveobj <- pcheck.logical(saveobj, varnm="savedata",
                    title="Save output object?", first="NO", gui=gui)
  
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
  ## Get ICE data
  #########################################################################
  if (is.null(PBdataICE)) {
    PBdataICE <- anPBpopICE_data(ice.pntfn, ice.pltfn=ice.pltfn, 
                    plotid="plot_id", pntid="dot_cnt", 
                    appendluts=TRUE, savedata=savedata, 
                    savedata_opts= list(outfolder=outfolder,
			                    overwrite_layer=overwrite_layer, out_dsn=out_dsn, 
			                    out_fmt=out_fmt, overwrite_layer=overwrite_layer,
			                    append_layer=append_layer, outfn.date=outfn.date, 
			                    outfn.pre=outfn.pre))
    returnlst$PBdataICE <- PBdataICE
  } else {
    PBdataICE <- pcheck.object(PBdataICE, objnm="PBdataICE",
		list.items=c("ice.pnt", "ice.plt", "plotid", "pntid"))
  }
  names(PBdataICE)
  ice.pnt <- PBdataICE$ice.pnt
  ice.plt <- PBdataICE$ice.plt
  plotid <- PBdataICE$plotid
  pntid <- PBdataICE$pntid
  domlut <- PBdataICE$domlut
  changelut <- PBdataICE$reflst$changelut
  coverlut <- PBdataICE$reflst$coverlut
  uselut <- PBdataICE$reflst$uselut
  agentlut <- PBdataICE$reflst$agentlut
  rm(PBdataICE)
  gc()

  ## Check data in changelut
  if (!is.null(changelut)) {
    agent_cols <- c("chg_ag_2_GRP", "chg_ag_2_GRP_nm", "change_pnt")
    if (!all(agent_cols %in% names(agentlut))) {
      miss <- agent_cols[!agent_cols %in% names(agentlut)]
      message("missing columns in agentlut: ", toString(miss))
    }
  }
  ## Check data in coverlut
  if (!is.null(coverlut)) {
    cover_cols <- c("cover", "cover_nm", "cover_GRP", "cover_GRP_nm",
		"cover_GRP2", "cover_GRP2_nm")
    if (!all(cover_cols %in% names(coverlut))) {
      miss <- cover_cols[!cover_cols %in% names(coverlut)]
      message("missing columns in coverlut: ", toString(miss))
    }
  }
  ## Check data in uselut
  if (!is.null(uselut)) {
    use_cols <- c("use", "use_nm", "use_FOR", "use_FOR_nm")
    if (!all(use_cols %in% names(uselut))) {
      miss <- use_cols[!use_cols %in% names(uselut)]
      message("missing columns in uselut: ", toString(miss))
    }
  }
  ## Check other columns in ice.pnt
  other_cols <- c("use_1_2", "cover_1_2", "use_1_2_FOR")
  if (!all(other_cols %in% names(ice.pnt))) {
    miss <- other_cols[!other_cols %in% names(ice.pnt)]
    message("missing columns in ice.pnt: ", toString(miss))
  }

  ## Check T1 and T2
  if ((T2 - T1) < 0) {
    stop("T1 is after T2")
  }
  if (!is.null(ice.plt)) {
    if (all(c("Date1", "Date2") %in% names(ice.plt))) {
      ## Check Date1 and Date2
      ice.plt$Date2 <- as.Date(ice.plt$Date2, format="%m/%d/%Y")
      ice.plt$Date1 <- as.Date(ice.plt$Date1, format="%m/%d/%Y")

      Date1miss <- sum((is.na(ice.plt$Date1) | ice.plt$Date1 == ""), na.rm=TRUE)
      Date2miss <- sum((is.na(ice.plt$Date2) | ice.plt$Date2 == ""), na.rm=TRUE)

      ## if no Date1 values, fill with Aug 1, T1 year
      if (Date1miss == nrow(ice.plt)) {
        ice.plt$Date1 <- paste0(T1, "-08-01")
      }
      ## if no Date2 values, fill with Aug 1, T2 year
      if (Date2miss == nrow(ice.plt)) {
        ice.plt$Date2 <- paste0(T2, "-08-01")
      }

      Date1avg <- mean(ice.plt$Date1, na.rm=TRUE)
      Date2avg <- mean(ice.plt$Date2, na.rm=TRUE)

 
      ## Get remeasurement period (REMPER) for each plot 
      ## Using lubridate package
      #ice.plt$REMPER <- 
      #   lubridate::time_length(difftime(ice.plt$Date2, ice.plt$Date1), "years")

      ## Using base R (estimate for leap years)
      ice.plt$REMPER <- as.numeric(difftime(ice.plt$Date2, 
                          ice.plt$Date1, units="weeks")) / 52.25
      REMPERavg <- round(mean(ice.plt$REMPER, na.rm=TRUE),1)

      if ((REMPERavg / (T2 - T1)) > 2) {
        message("plot dates are different than T1 and T2 dates:\n")
      }
    } else {
      Date1avg <- T1
      Date2avg <- T2

      REMPERavg <- T2 - T1
    }
  } 

  #REMPERavg <- as.numeric(difftime(Date2avg, 
  #                    Date1avg, unit="weeks")) / 52.25


  #########################################################################
  ## Get population data
  ##################################################################
  PBpopdatICE <- modPBpop(pntdat=ice.pnt, plt=ice.plt, 
                    plotid=plotid, pntid=pntid, 
                    puniqueid=plotid, pltassgn=pltassgn, pltassgnid=pltassgnid, 
                    unitarea=unitarea, unitvar=unitvar, areavar=areavar, 
                    unit_opts=list(unit.action=unit.action), 
                    strata=strata, strvar=strvar, 
                    strata_opts=list(getwt=getwt, getwtvar=getwtvar, 
                    stratcombine=stratcombine), sumunits=sumunits, 
                    savedata=savedata, savedata_opts=list(outfolder=outfolder))
  PBpopdatICE$reflst <- PBdataICE$reflst
  PBpopdatICE$domlut <- domlut
  PBpopdatICE$REMPERavg <- REMPERavg

  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, outfolder=outfolder,
		      overwrite=overwrite_layer, ext="rda")
    save(PBpopdatICE, file=objfn)
    message("saving object to: ", objfn)
  }

  return(PBpopdatICE)
}

