#' @title
#' Analysis - Generate Green-Book estimates for one or more FIA Evaluations.
#'
#'#' @description 
#' Wrapper to generate state-level estimates using FIESTA's Green-Book
#' module, for one or more FIA Evaluations. 
#'
#'
#' @param states String vector. Name(s) of states in evaluation.
#' @param RS String vector. Name of research station(s)
#' ('RMRS','SRS','NCRS','NERS','PNWRS').  Do not use if states is populated.
#' @param evalidlst Integer vector. Vector of one or more evalid (e.g, 491801).
#' @param evalCur Logical. If TRUE, gets the most current evaluation for each
#' state.
#' @param evalEndyrlst Integer vector. Vector of one or more Evaluation end
#' years to generate estimates (YYYY).
#' @param evalType String vector. The type(s) of evaluation of interest ('ALL',
#' 'CURR', VOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN').  The evalType
#' 'ALL' includes nonsampled plots; 'CURR' includes plots used for area
#' estimates; 'VOL' includes plots used for area and/or tree estimates; The
#' evalType 'GRM' includes plots used for growth, removals, mortality, and
#' change estimates (eval_typ %in% c(GROW, MORT, REMV, CHNG)).  Multiple types
#' are accepted. See details below and FIA database manual for regional
#' availability and/or differences.
#' @param datsource String. Source of data ('datamart', 'sqlite').
#' @param data_dsn If datsource='sqlite', the file name (data source name) of
#' the sqlite database (*.sqlite).
#' @param isseed Logical. If TRUE, add seedling data from Evaluation(s).
#' @param ppsanm String. Name of pop_plot_stratum_assgn table in database.
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
#'   GBpoplst <- anGBgetpop_evallst(evalEndyrlst=c(2015:2019), states="Utah",
#' 	evalType="CURR")
#'
#'   ## Get acres by forest type from each Evaluation in GBpoplst
#'   lapply(GBpoplst, function(x) modGBarea(GBpopdat=x, rowvar="FORTYPGRPCD",
#' 	landarea="FOREST", row.FIAname=TRUE))
#'
#' @export
anGBgetpop_evallst <- function(states = NULL, 
                         RS = NULL,
                         evalidlst = NULL, 
                         evalCur = FALSE, 
                         evalEndyrlst = NULL,  
                         evalType = "VOL", 
                         datsource = "datamart", 
                         data_dsn = NULL, 
                         isseed = FALSE, 
                         ppsanm = "pop_plot_stratum_assgn", 
                         byEndyr = FALSE, 
                         savedata = FALSE, 
                         outfolder = NULL, 
                         ...) {
  ## DESCRIPTION: estimates for each evalid in list

  ## Set global variables
  tree=seed=seed_layer=unitvar2 <- NULL
  gui <- FALSE
  istree=evalAll <- FALSE
  GBpop_evallst <- list()

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
    if (is.null(evalEndyrlst)) {
      stop("must include evalEndyrlst if byEndyr=TRUE")
    }
  }
  
  ## datsource
  datsourcelst <- c("sqlite", "datamart")
  datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
                              checklst=datsourcelst, gui=gui, 
                              caption="Data source?")
  
  
  ## Get list of evalids from database
  ########################################################
  if (datsource == "sqlite") {
    if (is.null(data_dsn)) {
      stop("must include data_dsn")
    }

    dbconn <- DBtestSQLite(data_dsn, dbconnopen=TRUE, showlist=FALSE)
    tablst <- DBI::dbListTables(dbconn)
    stratalut <- chkdbtab(tablst, "pop_stratum")
    if (is.null(stratalut)) {
      message("must include stratalut in data_dsn to use strata for estimation")
      strata <- FALSE
    }
    unitarea <- chkdbtab(tablst, "pop_estn_unit")
    if (is.null(unitarea)) {
      stop("must include unitarea in data_dsn to use for estimation")
    }
    pltassgn <- chkdbtab(tablst, ppsanm, stopifnull=TRUE)
    if (is.null(pltassgn)) {
      stop("must include ", ppsanm, " in data_dsn to use for estimation")
    }
    
    if (is.null(evalidlst) && !evalCur && is.null(evalEndyrlst)) {
      evalAll <- TRUE
    }
    evaliddat <- DBgetEvalid(datsource = datsource, 
                             data_dsn = data_dsn, 
                             RS = RS, 
                             states = states, 
                             evalAll = evalAll, 
                             evalid = evalidlst, 
                             evalCur = !evalAll, 
                             evalEndyr = evalEndyrlst, 
                             evalType = evalType) 
    evalidlst <- evaliddat$evalidlist

  } else if (datsource == "datamart") {   ## datsource="datamart"
    evaliddat <- DBgetEvalid(evalid = evalidlst, 
                             evalEndyr = evalEndyrlst, 
                             states = states, 
                             RS = RS, 
                             evalAll = FALSE, 
                             evalType = evalType)
    evalidlst <- evaliddat$evalidlist
    
  } else if (datsource %in% c("obj", "csv")) { ## datsource %in% c("obj", "csv")
    stcds <- pcheck.states(states, statereturn="VALUE")
    yrlst <- sapply(evalEndyrlst, substr, 3, 4)
    evalTypecd <- ifelse(evalType == "CURR", "01",
                         ifelse(evalType == "VOL", "01", "01"))
    evalidlst <- as.list(sort(paste0(apply(expand.grid(stcds, yrlst), 1,
                                           function(x) paste0(x[1], x[2])), evalTypecd)))
    names(evalidlst) <- states
  }
 
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
  if (any(evalType %in% c("VOL", "CHNG"))) {
    istree <- TRUE
  }

  ## Get population from the sqlite database with pop tables
  #########################################################################
  if (datsource == "sqlite") {
    if (isseed) seed_layer <- "seed"

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

      GBpopdat <- modGBpop(popType = evalType, 
                           popTabs = list(cond = "cond", 
                                          plt = "plot", 
                                          tree = "tree", 
                                          seed = seed_layer), 
                           pltassgn = pltassgn, 
                           pltassgnid = "PLT_CN", 
                           popFilter = list(evalid=evalid),
                           dsn = data_dsn, 
                           pjoinid = "CN", 
                           strata = TRUE, 
                           unitvar = "ESTN_UNIT", 
                           unitarea = unitarea, 
                           areavar = "AREA_USED", 
                           unit_opts = list(unitvar2="STATECD"), 
                           stratalut = stratalut, 
                           strvar="STRATUMCD", 
                           strata_opts = list(getwt=TRUE, 
                                              getwtvar="P1POINTCNT", 
                                              stratcombine=TRUE), 
                           savedata=savedata, 
                           savedata_opts = list(outfolder=outfolder, 
                           				outfn.pre=evalnm))
      GBpop_evallst[[evalnm]] <- GBpopdat
    }
  } else {
    ## Get population from datamart or sqlite database without pop tables
    #########################################################################
    pltdat <- spGetPlots(datsource = datsource, 
                         data_dsn = data_dsn, 
                         eval_opts = list(evalid=evalidlst), 
                         istree = istree, 
                         isseed = isseed, 
                         savePOP = TRUE)
    tabs <- pltdat$tabs
    plt <- tabs$plt
    cond <- tabs$cond
    tree <- tabs$tree
    seed <- tabs$seed
    pop_plot_stratum_assgn <- tabs$pop_plot_stratum_assgn
      

    ## Get strata information for evalidlst
    GBstratdat <- DBgetStrata(eval_opts = list(evalid=evalidlst), 
                              pop_plot_stratum_assgn=pop_plot_stratum_assgn)

    if (byEndyr) {
      evalidloop <- names(evalidlst)
      evalidloop <- evalidlst
    } else {
      evalidloop <- unlist(evalidlst)
      names(evalidloop) <- names(evalidlst)
    }

    for (i in 1:length(evalidloop)) {
      evalid <- evalidloop[i]
      evalnm <- names(evalid)
      message("getting population data for ", toString(evalnm))
      message("evalid: ", toString(evalid))

      GBpopdat <- modGBpop(popType=evalType, 
                           popTabs = list(cond=cond, plt=plt, 
                           				tree=tree, seed=seed), 
                           popFilter = list(evalid = evalid), 
                           dsn = data_dsn, 
                           stratdat = GBstratdat, 
                           savedata = savedata, 
                           savedata_opts = list(outfolder=outfolder, 
                           outfn.pre = evalnm), 
                           ...)
      GBpop_evallst[[evalnm]] <- GBpopdat
    }
  }
  return(GBpop_evallst)
}
