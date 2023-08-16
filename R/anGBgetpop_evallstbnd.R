#' @title
#' Analysis - Get estimates for Green-Book module by FIA Evaluation End year.
#'
#' @description 
#' Wrapper to generate estimates for each evaluation ending in the year in
#' list.  If boundary crosses more than one Evaluation state, Evaluations for
#' each state ending in the same year are used. Data are subset to boundary.
#'
#'
#' @param evalidlst Integer vector. Vector of one or more evalid (e.g, 491801).
#' @param evalCur Logical. If TRUE, gets the most current evaluation for each
#' state.
#' @param evalEndyrlst Integer vector. Vector of one or more Evaluation end
#' years to generate estimates (YYYY).
#' @param bnd sf R object or String. Area of interest (population
#' boundary or estimation unit). Can be a spatial polygon object, full pathname
#' to a shapefile, or name of a layer within a database.
#' @param bnd_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of boundary. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if bnd is sf object.
#' @param bnd.att String. The attribute in bnd that defines the
#' estimation unit(s). If NULL, bnd is default as one estimation unit.
#' @param bnd.filter String. Filter to subset bnd SpatialPolygons layer.
#' @param evalAll Logical. If TRUE, extract all FIA Evaluations in database.
#' @param evalType String vector. The type(s) of evaluation of interest ('ALL',
#' 'CURR', VOL', 'GRM', 'P2VEG', 'DWM", 'INV', 'REGEN', 'CRWN').  The evalType
#' 'ALL' includes nonsampled plots; 'CURR' includes plots used for area
#' estimates; 'VOL' includes plots used for area and/or tree estimates; The
#' evalType 'GRM' includes plots used for growth, removals, mortality, and
#' change estimates (eval_typ %in% c(GROW, MORT, REMV, CHNG)).  Multiple types
#' are accepted. See details below and FIA database manual for regional
#' availability and/or differences.
#' @param RS String. Name of FIA research station to restrict states to
#' ('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are
#' included.
#' @param states String vector. States to extract data.
#' @param datsource String. Source of data ('datamart', 'sqlite').
#' @param data_dsn If datsource='sqlite', the file name (data source name) of
#' the sqlite database (*.sqlite).
#' @param strat_layer String. Name of raster stratification layer.
#' @param strat_lut Data frame. A look-up table of codes to aggregate. The
#' format of table includes 2 columns, one column same name as strvar.  If
#' strattype="RASTER", strvar="value".
#' @param byEndyr Logical. If TRUE, organizes evalidlst by evalEndyr.  Set TRUE
#' if estimates include more than one state (e.g., if area of interest crosses
#' more than one state - Manti-LaSal).
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param outfolder String. The output folder path. If NULL and savedata=TRUE,
#' outfolder is the working directory.
#' @param ...  Parameters to modGBpop().
#' @return Set of population data (from modGBpop()) for bnd.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#'
#'   ## Get population data and area estimates for WY Bighorn National Forest
#'   ##		for evaluations ending 2015 to 2019
#'   WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp", package="FIESTA")
#'   fornffn <- system.file("extdata", "sp_data/WYbighorn_forest_nonforest_250m.tif", package="FIESTA")
#'
#'   GBpoplst <- anGBpop_evalbnd(evalEndyrlst=c(2015:2019), bnd=WYbhfn,
#' 	xymeasCur=TRUE, xy.uniqueid="PLOT_ID", evalType="CURR", strat_layer=fornffn)
#'
#'   estlst <- lapply(GBpoplst, function(x) modGBarea(GBpopdat=x, rowvar="FORTYPGRPCD",
#' 	landarea="FOREST", row.FIAname=TRUE))
#'   names(estlst)
#'   names(estlst$eval561501)
#'
#' @export
anGBgetpop_evallstbnd <- function(evalidlst = NULL, 
                            evalCur = FALSE, 
                            evalEndyrlst = NULL, 
                            evalAll = FALSE, 
                            evalType = "VOL",
                            RS = NULL, 
                            states = NULL, 
                            bnd, 
                            bnd_dsn = NULL, 
                            bnd.att = NULL, 
                            bnd.filter = NULL, 
                            datsource = NULL, 
                            data_dsn = NULL, 
                            strat_layer = NULL, 
                            strat_lut = NULL, 
                            byEndyr = FALSE, 
                            savedata = FALSE, 
                            outfolder = NULL, 
                            ...) {

  ## DESCRIPTION: get estimates for each evalid in list

  ## Set global variables
  tree=seed=RS=pltassgn=pltassgnid <- NULL
  gui <- FALSE
  istree <- FALSE


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
  

  #############################################################################
  ## Import boundary
  #############################################################################
  bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")

   
  if (!is.null(bndx)) {
    
    ## bnd.filter
    bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf

    ## Get intersecting states
    ########################################################
    states <- spGetStates(bndx, 
                          #stbnd.att="STATENM", 
                          stbnd.att="COUNTYFIPS",
                          RS=NULL, showsteps=TRUE)$statenames
    stcds <- pcheck.states(states, statereturn = "VALUE")
  }
  
  ## datsource
  datsourcelst <- c("sqlite", "datamart", "obj")
  datsource <- pcheck.varchar(var2check=datsource, varnm="datsource", 
                       checklst=datsourcelst, gui=gui, 
                       caption="Data source?")
  

  ## Get list of evalids from database
  ########################################################
  if (datsource == "sqlite") {
    if (is.null(data_dsn)) {
      stop("must include data_dsn")
    }

    dbconn <- DBtestSQLite(data_dsn, 
                           dbconnopen=TRUE, 
                           showlist=FALSE)
    tablst <- DBI::dbListTables(dbconn)
    stratalut <- chkdbtab(tablst, "pop_stratum")
    unitarea <- chkdbtab(tablst, "pop_estn_unit")
    pltassgn <- chkdbtab(tablst, ppsanm, stopifnull=TRUE)
    if (is.null(evalidlst) && !evalCur && is.null(evalEndyrlst)) {
      evalAll <- TRUE
    }
    evaliddat <- DBgetEvalid(datsource = datsource, 
                          data_dsn = data_dsn, 
                          RS = RS, 
                          states = states, 
                          evalAll = evalAll, 
                          evalid = evalidlst, 
                          evalCur = evalAll, 
                          evalEndyr = evalEndyrlst, 
                          evalType = evalType)
    evalidlst <- evaliddat$evalidlist

  } else if (datsource == "datamart") {   
    evaliddat <- DBgetEvalid(evalid = evalidlst, 
                          evalEndyr = evalEndyrlst, 
                          RS = RS, 
                          states = states, 
                          evalAll = FALSE, 
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

  ####################################################################
  ## Get plots
  ####################################################################
  ## Note: must set returnxy=TRUE to return XY coordinates
  
  pltdat <- spGetPlots(bnd = bndx, 
                       returnxy = TRUE, 
                       evalid = evalidlst, 
                       datsource = datsource,
                       ...)
  spxy <- pltdat$spxy
  xy.uniqueid <- pltdat$xy.uniqueid
  

  ## Get strata and/or estimation unit information
  #########################################################################
  if (!is.null(strat_layer)) {
    strata <- TRUE

    ## Get strata information for boundary
    stratdat <- spGetStrata(xyplt = spxy, 
                            uniqueid = xy.uniqueid, 
                            unit_layer = bndx, 
                            unitvar = bnd.att, 
                            strat_layer = strat_layer, 
                            strat_lut = strat_lut, 
                            rast.NODATA = 0)
    pltassgn <- stratdat$pltassgn
    pltassgnid <- stratdat$pltassgnid
    unitarea <- stratdat$unitarea
    unitvar <- stratdat$unitvar
    areavar <- stratdat$areavar
    stratalut <- stratdat$stratalut
    strvar <- stratdat$strvar
  } else {
    strata <- FALSE
    stratalut <- NULL
    strvar <- NULL

    ## Get estimation unit information for boundary
    unitdat <- spGetEstUnit(xyplt = spxy, 
                            uniqueid = xy.uniqueid, 
                            unit_layer = bnd, 
                            unit_dsn = bnd_dsn, 
                            unitvar = bnd.att, 
                            unit.filter = bnd.filter)
    pltassgn <- unitdat$pltassgn
    pltassgnid <- unitdat$pltassgnid
    unitarea <- unitdat$unitarea
    unitvar <- unitdat$unitvar
    areavar <- unitdat$areavar
  }

  ## Add a temporary table to out_dsn
  #########################################################################
  if (!is.null(data_dsn)) {
    write2sqlite(pltassgn, 
                 data_dsn, 
                 out_name = "ppsatmp", 
                 overwrite = TRUE, 
                 index.unique = pltassgnid)
    ppsanm <- "ppsatmp"
  } else {
    ppsanm <- pltassgn
  }


  ## Loop through Evaluation list
  #########################################################################
  GBpop_evalEndyrlst <- list()

  for (evalnm in names(evalidlst)) {
    message("getting population data for ", toString(evalnm))

    ## Get population data for boundary
    #########################################################################
    GBpopdat <- modGBpop(popType = evalType, 
                         pltdat = pltdat, 
                         pltassgn = ppsanm, 
                         pltassgnid = pltassgnid, 
                         dsn = data_dsn, 
                         strata = strata, 
                         unitvar = unitvar, 
                         unitarea = unitarea, 
                         areavar = areavar, 
                         stratalut = stratalut, 
                         strvar = strvar, 
                         strata_opts=list(getwt=FALSE, 
                                          stratcombine=TRUE), 
                         savedata = savedata, 
                         savedata_opts=list(outfolder = NULL, 
                                            outfn.pre=evalnm))
    GBpop_evalEndyrlst[[evalnm]] <- GBpopdat
  }


  if (!is.null(data_dsn)) {
    ## Remove temporary table from data_dsn
    out_conn <- DBI::dbConnect(RSQLite::SQLite(), data_dsn)
    DBI::dbRemoveTable(out_conn, "ppsatmp")
    DBI::dbDisconnect(out_conn)
  }

  return(GBpop_evalEndyrlst)

}



