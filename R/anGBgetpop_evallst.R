#' @title
#' Analysis - Generate Green-Book estimates for one or more FIA Evaluations.
#'
#' @description 
#' Wrapper to generate state-level estimates using FIESTA's Green-Book
#' module, for one or more FIA Evaluations. 
#'
#'
#' @param states String vector. Name(s) of states in evaluation.
#' @param RS String vector. Name of research station(s)
#' ('RMRS','SRS','NCRS','NERS','PNWRS').  Do not use if states is populated.
#' @param datsource String. Source of data ('datamart', 'sqlite').
#' @param data_dsn String. If datsource='sqlite', the name of SQLite database
#' (*.sqlite).
#' @param dbTabs List of database tables the user would like returned.
#'  See help(dbTables) for a list of options.
#' @param eval String. Type of evaluation time frame for data extraction 
#' ('FIA', 'custom'). See eval_opts for more further options. 
#' @param eval_opts List of evaluation options for 'FIA' or 'custom'
#' evaluations to determine the set of data returned. See help(eval_options)
#' for a list of options.
#' @param byEndyr Logical. If TRUE, organizes evalidlst by evalEndyr.  Set TRUE
#' if estimates include more than one state (e.g., if area of interest crosses
#' more than one state - Manti-LaSal).
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param outfolder String. The output folder path. If NULL and savedata=TRUE,
#' outfolder is the working directory.
#' @param ...  Parameters to modGBpop().
#' 
#' @return Set of population data from modGBpop() for each Evaluation
#' specified.
#' @author Tracey S. Frescino
#' @keywords data
#' 
#' @examples
#'   ## Calculate population data for Utah, area evaluations 2015-2019
#'   GBpoplst <- anGBgetpop_evallst(states="Utah", 
#'                         eval_opts=list(Endyr=c(2015:2019), evalType="CURR"))
#'
#'   ## Get acres by forest type from each Evaluation in GBpoplst
#'   lapply(GBpoplst, function(x) modGBarea(GBpopdat=x, rowvar="FORTYPGRPCD",
#' 	landarea="FOREST", row.FIAname=TRUE))
#'
#' @export
anGBgetpop_evallst <- function(states = NULL, 
                         RS = NULL,
                         datsource = "datamart", 
                         data_dsn = NULL, 
                         dbTabs = dbTables(),
                         eval = "FIA",
                         eval_opts = eval_options(Type = "VOL"),
                         byEndyr = FALSE, 
                         savedata = FALSE, 
                         outfolder = NULL, 
                         ...) {
  ## DESCRIPTION: estimates for each evalid in list

  ## Set global variables
  gui <- FALSE
  GBpop_evallst <- list()
  

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
 
  ## Check parameter lists
  #pcheck.params(input.params, eval_opts=eval_opts)
  
  ## Set eval_options defaults
  eval_defaults_list <- formals(eval_options)[-length(formals(eval_options))] 
  for (i in 1:length(eval_defaults_list)) {
    assign(names(eval_defaults_list)[[i]], eval_defaults_list[[i]])
  } 
  ## Set user-supplied eval_opts values
  if (length(eval_opts) > 0) {
    for (i in 1:length(eval_opts)) {
      if (names(eval_opts)[[i]] %in% names(eval_defaults_list)) {
        assign(names(eval_opts)[[i]], eval_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(eval_opts)[[i]]))
      }
    }
  } else {
    message("no evaluation timeframe specified...")
    message("see eval and eval_opts parameters (e.g., eval='custom', eval_opts=eval_options(Cur=TRUE))\n")
    stop()
  }


  ## Set dbTables defaults
  dbTables_defaults_list <- formals(dbTables)[-length(formals(dbTables))]
  for (i in 1:length(dbTables_defaults_list)) {
    assign(names(dbTables_defaults_list)[[i]], dbTables_defaults_list[[i]])
  }
  ## Set user-supplied dbTables values
  if (length(dbTabs) > 0) {
    for (i in 1:length(dbTabs)) {
      if (names(dbTabs)[[i]] %in% names(dbTables_defaults_list)) {
        assign(names(dbTabs)[[i]], dbTabs[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(dbTabs)[[i]]))
      }
    }
  }
  

  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata", 
                             title="Save data extraction?", 
                             first="NO", gui=gui)
  ## Check outfolder
  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder, gui=gui)
  }
  ## Check byEndyr
  byEndyr <- pcheck.logical(byEndyr, varnm="byEndyr", 
                            title="By Endyr?", 
                            first="NO", gui=gui)
  if (byEndyr) {
    if (is.null(Endyr)) {
      stop("must include evalEndyrlst if byEndyr=TRUE")
    }
  }
  ## datsource
  datsourcelst <- c("sqlite", "datamart")
  datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
                              checklst=datsourcelst, gui=gui, 
                              caption="Data source?")


  if (datsource == "sqlite") {
    if (is.null(data_dsn)) {
      stop("must include data_dsn")
    }

    dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
    tablst <- DBI::dbListTables(dbconn)
    pop_stratumnm <- chkdbtab(tablst, dbTabs$popstratum_layer)
    if (is.null(pop_stratumnm)) {
      message("must include ", dbTabs$popstratum_layer, " in data_dsn to use strata for estimation")
      strata <- FALSE
    }
    pop_estn_unitnm <- chkdbtab(tablst, dbTabs$popestnunit_layer)
    if (is.null(pop_estn_unitnm)) {
      stop("must include ", dbTabs$popestnunit_layer, " in data_dsn to use for estimation")
    }
    ppsanm <- chkdbtab(tablst, dbTabs$ppsa_layer, stopifnull=TRUE)
    if (is.null(ppsanm)) {
      stop("must include ", dbTabs$ppsa_layer, " in data_dsn to use for estimation")
    }
  } else {
    pop_stratumnm <- "POP_STRATUM"
    pop_estn_unitnm <- "POP_ESTN_UNIT"
    ppsanm <- "POP_PLOT_STRATUM_ASSGN"
  }  
 

  ## Get plot data 
  ####################################################################
  pltdat <- DBgetPlots(states = states, 
                     invtype = "ANNUAL",
                     datsource = datsource, 
                     data_dsn = data_dsn,
                     dbTabs = dbTables(ppsa_layer = ppsanm, 
                                       popstratum_layer = pop_stratumnm,
                                       popestnunit_layer = pop_estn_unitnm),
                     eval = eval, 
                     eval_opts = eval_opts,
                     getxy = FALSE,
                     returndata = TRUE, 
                     savePOP = TRUE, 
                     ...
                     )
  #load("pltdat.rds")
  evalidlst <- pltdat$evalid
  evalEndyrlst <- eval_opts$Endyr


  if (byEndyr) {
    if (unique(unlist(lapply(evalidlst, length))) != length(evalEndyrlst)) {
      message("list of evalids does not match evalEndyrlst")
      message("evalidlst: ", toString(evalidlst), "\n", 
              "evalEndyrlst: ", toString(evalEndyrlst))
      stop()
    }
    evalidlst <- data.table::transpose(evalidlst)
    names(evalidlst) <- paste0("eval", evalEndyrlst)
  } else {
    evalidlst <- unlist(evalidlst)
    #evalidstcds <- substr(unlist(evalidlst), nchar(evalidlst)-5, nchar(evalidlst)-4)
    #evalidstabbr <- sapply(evalidstcds, pcheck.states, statereturn="ABBR")
    #names(evalidlst) <- paste0(evalidstabbr, evalidlst)
    names(evalidlst) <- paste0("eval", evalidlst)
  }

  ## Get population from the sqlite database with pop tables
  #########################################################################
  if (byEndyr) {
    evalidloop <- names(evalidlst)
  } else {
    evalidloop <- unlist(evalidlst)
  }

  for (i in 1:length(evalidloop)) {
    evalid <- evalidloop[i]
    evalnm <- names(evalid)
    if (byEndyr) {
      evalnm <- evalid
      evalid <- evalidlst[[evalid]]
    }
    message("getting population data for ", toString(evalnm))
    message("evalid: ", toString(evalid))

    GBpopdat <- modGBpop(popType = Type, 
                         popTabs = list(cond = pltdat$tabs$cond, 
                                        plt = pltdat$tabs$plt, 
                                        tree = pltdat$tabs$tree, 
                                        seed = pltdat$tabs$seed), 
                         pltassgn = pltdat$pop_plot_stratum_assgn, 
                         pltassgnid = "PLT_CN", 
                         popFilter = list(evalid = evalid),
                         strata = TRUE, 
                         unitvar = "ESTN_UNIT", 
                         unitarea = pltdat$pop_estn_unit, 
                         areavar = "AREA_USED", 
                         unit_opts = list(unitvar2="STATECD"), 
                         stratalut = pltdat$pop_stratum, 
                         strvar = "STRATUMCD", 
                         strata_opts = list(getwt = TRUE, 
                                            getwtvar = "P1POINTCNT", 
                                            stratcombine = TRUE), 
                         savedata = savedata, 
                         savedata_opts = list(outfolder=outfolder, 
                           				            outfn.pre=evalnm))
    GBpop_evallst[[evalnm]] <- GBpopdat
  }

  return(GBpop_evallst)
}
