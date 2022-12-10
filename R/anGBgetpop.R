#' Analysis - Generate estimates for custom-defined boundary.
#'
#' Wrapper to get plot data, stratification data and estimates within a
#' custom-defined boundary. Includes FIESTA functions: anGBdata()
#' (spGetPlots(), spGetStrata()), and modGBpop().
#'
#'
#' @param popType String. Type of evaluation(s) to include in population data
#' ("VOL"). Note: currently only 'VOL' is available.
#' @param showsteps Logical. If TRUE, show intermediate steps.
#' @param returnxy Logical. If TRUE, saves xy coordinates to outfolder.
#' @param savedata Logical. If TRUE, saves data objects to outfolder.
#' @param saveobj Logical. If TRUE, saves GBpopdat object to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. 
#' @param GBdata R list object. FIA data for estimation.
#' @param ...  Parameters for GBdata.
#' @return Estimate.
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#'
#'
#'   ## Get data for WY Bighorn administrative boundary using
#'   WYbhfn <- system.file("extdata", "sp_data/WYbighorn_adminbnd.shp", package="FIESTA")
#'   fornffn <- system.file("extdata", "sp_data/WYbighorn_forest_nonforest_250m.tif", package="FIESTA")
#'
#'   ## Get population data for generating GB estimates
#'   GBpop <- anGBpop(WYbhfn, datsource="datamart", strat_layer=fornffn)
#'   names(GBpop)
#'
#'   names(GBpop$GBdata)
#'   names(GBpop$GBpopdat)
#'
#' @export
anGBgetpop <- function(GBdata = NULL,
                    popType = "VOL",
                    showsteps = FALSE, 
                    returnxy = TRUE, 
                    savedata = FALSE, 
                    saveobj = FALSE, 
                    savedata_opts = NULL, 
                    ...) {


  ## Set global variables
  gui <- FALSE
  returnlst <- list()
  strata <- TRUE
  istree <- FALSE

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(anGBdata)),
                 names(formals(spGetPlots)),
                 names(formals(modGBpop)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
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
  
  ## Check returnxy
  returnxy <- pcheck.logical(returnxy, varnm="returnxy",
                  title="Return XY?", first="NO", gui=gui)
  
  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata",
                  title="Save data extraction?", first="NO", gui=gui)
  
  ## Check savesteps
  # savesteps <- pcheck.logical(savesteps, varnm="savesteps",
  #                 title="Save step data?", first="YES", gui=gui)
  
  ## Check saveobj
  saveobj <- pcheck.logical(saveobj, varnm="saveobj",
                  title="Save SApopdat object?", 
                  first="YES", gui=gui, stopifnull=TRUE)
  
  if (saveobj && is.null(objnm)) {
    objnm <- "anGBpop"
  }
  
  
  ## Check output
  if (savedata) {
    outlst <- pcheck.output(out_dsn=out_dsn, 
                  out_fmt=out_fmt, 
                  outfolder=outfolder, outfn.pre=outfn.pre, 
                  outfn.date=outfn.date, overwrite_dsn=overwrite_dsn, 
                  overwrite_layer=overwrite_layer, append_layer=append_layer, 
                  createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    overwrite_dsn <- outlst$overwrite_dsn
    append_layer <- outlst$append_layer
    if (!is.null(out_layer)) {
      out_layer <- "bnd"
    }
   } else if (saveobj) {
     outfolder <- pcheck.outfolder(outfolder)
  }


  ## Check popType
  popTypelst <- c("VOL")
  popType <- pcheck.varchar(var2check=popType, 
                            varnm="popType", gui=gui, 
                            checklst=popTypelst, 
                            caption="population type", 
                            stopifnull=TRUE,
		warn="only VOL is currently available")
  if (popType %in% c("VOL", "CHNG")) {
    istree <- TRUE
  }


  ###########################################################################
  ## Extract FIA data and auxiliary data
  ###########################################################################

  if (is.null(GBdata)) {
    message("extracting data...")

    ###########################################################################
    ## Extract FIA data and model data
    ###########################################################################
    outfn.pre <- "GBdata"

    GBdata <- anGBdata(...)
    returnlst$GBdata <- GBdata
  } else {
    GBdata <- pcheck.object(GBdata, objnm="GBdata", 
                            list.items=c("bnd", "plt", "cond", "unitarea"))
  }

  ####################################################################
  ## Get population data
  ####################################################################
  GBpopdat <- modGBpop(popType=popType, 
                       GBdata=GBdata, 
                       saveobj=saveobj)
  names(GBpopdat)
  returnlst$GBpopdat <- GBpopdat


  return(returnlst)
}

