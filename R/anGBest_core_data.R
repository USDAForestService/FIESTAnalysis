#' Extract data for core table from FIA DataMart.
#'
#' Extract data from FIA Datamart for input to FIESTAnalysis::anGBest_core
#' function. The function generates a set of core tables for a state 
#' (https://apps.fs.usda.gov/fia/datamart/datamart.html).
#'
#' If variables are NULL, then it will prompt user to input variables.
#'
#' @param state String. Name of a state for tables (e.g., 'Utah')
#' @param datsource String. Source of data ('datamart', 'sqlite').
#' @param data_dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param evalCur Logical. If TRUE, uses the most current FIA Evaluation in
#' FIA DataMart for the state.
#' @param evalEndyr Integer. Last year of the FIA Evaluation of interest.
#' @param evalType String vector. The type(s) of evaluation of interest ('ALL',
#' 'CURR', 'VOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN').  The evalType
#' 'ALL' includes nonsampled plots; 'CURR' includes plots used for area
#' estimates; 'VOL' includes plots used for area and/or tree estimates; The
#' evalType 'GRM' includes plots used for growth, removals, mortality, and
#' change estimates (eval_typ %in% c(GROW, MORT, REMV, CHNG)).  Multiple types
#' are accepted. See details below and FIA database manual for regional
#' availability and/or differences. Note: do not use if EVALID is specified.
#' @param savedata Logical. If TRUE, saves data objects to outfolder (*.rda)
#' @param outfolder String. The path of folder to output tables.
#' @param outfn.date Logical. If TRUE, appends current date to outfile name.
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anGBest_core_data <- function(state, 
                              datsource = "datamart",
                              data_dsn = NULL, 
                              evalCur = TRUE, 
                              evalEndyr = NULL, 
                              evalType = c("CURR", "VOL", "GRM"),
                              savedata = TRUE, 
                              outfolder = NULL, 
                              outfn.date = TRUE) {

  ## Check state
  #######################################################
  state <- pcheck.states(state)
  if (length(state) == 0) stop("must include state")
  evalType <- c("ALL", "VOL", "GRM")

  ref_statecd <- FIESTAutils::ref_statecd
  stcd <- ref_statecd[ref_statecd$MEANING == state, "VALUE"]
  st <- ref_statecd[ref_statecd$MEANING == state, "ABBR"]

  ## Notes:
  ## 1. If want to add row groups with no subtotals, use GBest*() with rowgrp=TRUE.
  ## 2. If want to add row groups with subtotals, use rowgrptab().
  ## 3. If want to add SE columns side-by-side, use write2xlsx() with addSEcol=TRUE.
  ## 4. If want to add SE values in same cell, use GBest*() with allin1=TRUE or
  ##	rowgrptab() with allin1=TRUE if subtotals and write2xlsx() with allin1=TRUE.
  ## 5. If want all columns in ref table, use GBest*() or rowgrptab() with col.add0=TRUE.

  ## Check outfolder
  ########################################################
  outfolder <- pcheck.outfolder(outfolder)

  ## Create eval_opts from parameters
  eval_opts <- eval_options(Cur = evalCur, Endyr = evalEndyr, evalType = evalType)

  ## Get plot data from FIA dataMart
  ############################################
  datPlots <- DBgetPlots(states = state, 
                         datsource = datsource, 
                         data_dsn = data_dsn, 
                         eval = "FIA",
                         eval_opts = eval_opts, 
                         istree = TRUE, 
                         getxy = FALSE,
                         savePOP = TRUE)
  plt <- datPlots$plt
  pop_plot_stratum_assgn <- datPlots$pop_plot_stratum_assgn
  evalid <- datPlots$evalid

  if (is.null(evalEndyr)) {
    EVALID <- evalid[[1]][1]
    evalEndyr <- paste0("20", substr(EVALID, nchar(EVALID) - 3, nchar(EVALID)-2))
  }

  ## Get strata information from FIA dataMart
  ############################################
  datStrata <- DBgetStrata(datsource = datsource, 
                           data_dsn = data_dsn, 
                           eval_opts = list(evalid = evalid),
                           pop_plot_stratum_assgn = pop_plot_stratum_assgn)


  ## Get county and survey unit reference tables
  #############################################################
  unitqry <- paste("select VALUE, MEANING from ref_unit where STATECD = ", stcd)
  ref_countycd <- sf::st_drop_geometry(stunitco[stunitco$STATECD == stcd, c("COUNTYCD", "COUNTYNM")])
  ref_unitcd <- sqldf::sqldf(unitqry)

############ End CSV only

  datCore <- list()
  datCore$datPlots <- datPlots
  datCore$datStrata <- datStrata
  datCore$ref_countycd <- ref_countycd
  datCore$ref_unitcd <- ref_unitcd

  if (savedata) {
    datCorefn <- "datCore"
    if (outfn.date)
      datCorefn <- paste(datCorefn, format(Sys.time(), "%Y%m%d"), sep="_")

    if (!is.null(evalEndyr)) {
      fn <- paste0(outfolder, "/", st, "_eval", evalEndyr, "_", datCorefn, ".rda")
    } else {
      paste0(outfolder, "/", st, "_", datCorefn, ".rda")
    }

    save(datCore, file=fn)
  }

  if (savedata) {
    cat(
    " #################################################################################",
    "\n", paste("Data written to: ", outfolder), "\n",
    "#################################################################################",
    "\n" )
  }

  return(datCore)
}

