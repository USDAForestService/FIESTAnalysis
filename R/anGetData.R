#' @title
#' Data - Get plot data and auxiliary data for estimation.
#'
#' @description 
#' Wrapper to get plot data and auxiliary data within a given 
#' boundary for use in FIESTA's estimation modules.
#'
#'
#' @param bnd_layer sf R object or String. Boundary layer of estimation domain
#' units. Can be a spatial sf object, full pathname to a shapefile, or name of
#' a layer within a database.
#' @param bnd_dsn String. Name of data source name with bnd_layer, if in a
#' database.
#' @param bnd.att String. Name of attribute in boundary file to define
#' estimation domain units.
#' @param bnd.filter String. Optional filter of bnd_layer.
#' @param RS String. Name of FIA research station to restrict states to
#' ('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are
#' included.
#' @param rastfolder String. Name of the folder with raster layers. Optional.
#' Useful if all raster layers are in same folder.
#' @param rastlst.cont String vector or list. A list of raster(s) with
#' continuous data values (e.g., DEM). The list may include file name of
#' raster(s) or raster objects that are not InMemory.
#' @param rastlst.cont.name String vector. Output names for continuous rasters.
#' Optional. If NULL, name of raster is used as default or name+'_'+layer
#' number for multi-band layers.
#' @param rastlst.cont.NODATA Numeric vector. NODATA value for categorical
#' rasters (See notes). These values will be converted to NA and removed from
#' output if keepNA=FALSE. If 1 number, the same value will be used for all
#' categorical rasters. If more than 1 number, the number of values must be
#' equal to the number of rasters in rastlst.cont.
#' @param rastlst.cat String vector or list. A list of raster(s) with thematic
#' (i.e., categorical) data values. The list may include file name of raster(s)
#' or raster objects that are not InMemory.
#' @param rastlst.cat.name String vector. Output names for categorical rasters.
#' If NULL, name of raster is used as default or name+'_'+layer number for
#' multi-band layers.
#' @param rastlst.cat.NODATA Numeric vector. NODATA value for categorical
#' rasters (See notes). These values will be converted to NA and removed from
#' output if keepNA=FALSE. If 1 number, the same value will be used for all
#' categorical rasters. If more than 1 number, the number of values must be
#' equal to the number of rasters in rastlst.cat.
#' @param vars2keep String. Name of variable(s) in smallbnd to keep for output.
#' @param minplots Integer. Minimum number of plots in any on domain unit.
#' @param keepNA Logical. If TRUE, returns data frame of NA values.
#' @param ncores Integer. Number of cores to use for extracting values.
#' @param showsteps Logical. If TRUE, clipped points are displayed.  If
#' savedata=TRUE, the display is saved as *.jpg to outfolder.
#' @param returnxy Logical. If TRUE, returns XY coordinates.
#' @param savedata Logical. If TRUE, saves data to outfolder. Note:
#' includes XY data if returnxy = TRUE.
#' @param savesteps Logical. If TRUE, save steps spatial intermediate layers.
#' @param saveobj Logical. If TRUE, save SAest object to outfolder.
#' @param objnm String. Name of saved object name.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. 
#' @param pltdat R object. FIA plot data, output from spGetPlots().
#' @param ...  Other parameters for DBgetPlots.
#' 
#' @return Data.
#' 
#' @note
#' If exportsp=TRUE:\cr If out_fmt="shp", the writeOGR (rgdal) function is
#' called. The ArcGIS driver truncates variable names to 10 characters or less.
#' Variable names are changed before export using an internal function
#' If Spatial o bject has more than 1 record, it will be returned but not exported.
#'
#' The spTransform (rgdal) method is used for on-the-fly map projection
#' conversion and datum transformation using PROJ.4 arguments. Datum
#' transformation only occurs if the +datum tag is present in the both the from
#' and to PROJ.4 strings. The +towgs84 tag is used when no datum transformation
#' is needed. PROJ.4 transformations assume NAD83 and WGS84 are identical
#' unless other transformation parameters are specified.  Be aware, providing
#' inaccurate or incomplete CRS information may lead to erroneous data shifts
#' when reprojecting. See spTransform help documentation for more details.
#' @author Tracey S. Frescino
#' @keywords data
#' 
#' @export
anGetData <- function(bnd_layer, 
                      bnd_dsn = NULL, 
                      bnd.att = NULL, 
                      bnd.filter = NULL,
                      RS = NULL, 
                      rastfolder = NULL, 
                      rastlst.cont = NULL, 
                      rastlst.cont.name = NULL, 
                      rastlst.cont.NODATA = NULL, 
                      rastlst.cat = NULL, 
                      rastlst.cat.name = NULL, 
                      rastlst.cat.NODATA = NULL, 
                      vars2keep = "AOI", 
                      minplots = 0, 
                      keepNA = FALSE,
                      ncores = 1,
                      showsteps = FALSE,
                      returnxy = FALSE, 
                      savedata = FALSE, 
                      savesteps = FALSE, 
                      saveobj = FALSE, 
                      objnm = "spdat", 
                      savedata_opts = NULL, 
                      pltdat = NULL, 
                      ...) {

  ## Set global variables
  gui <- FALSE
  plt=AOI <- NULL

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- unique(c(names(formals(anGetData)),
		names(formals(spGetPlots)), names(formals(DBgetPlots))))
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
  savesteps <- pcheck.logical(savesteps, varnm="savesteps",
		title="Save step data?", first="YES", gui=gui)

  ## Check saveobj
  saveobj <- pcheck.logical(saveobj, varnm="saveobj",
		title="Save SApopdat object?", first="YES", gui=gui, stopifnull=TRUE)

  
  ## Check output
  if (savedata) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
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
      out_layer <- "plots"
    }
    
  } else if (savesteps || saveobj) {
    outfolder <- pcheck.outfolder(outfolder)
    if (is.null(out_layer)) {
      out_layer <- "plots"
    }
    if (!is.null(outfn.pre)) {
      out_layer <- paste0(outfn.pre, "_", out_layer)
    }
  }

  if (savesteps) {
    stepfolder <- file.path(outfolder, "SAdoms_steps")
    if (!dir.exists(stepfolder)) dir.create(stepfolder)
    step_dsn <- NULL
    step_fmt <- "shp"
  }
 
  ####################################################################
  ## Get FIA plot data from SQLite within boundary
  ####################################################################
  if (is.null(pltdat)) {
    pltdat <- spGetPlots(bnd = bnd_layer, 
                         bnd_dsn = bnd_dsn,
                         bnd.filter = bnd.filter, 
                         RS = RS, 
                         returnxy = TRUE, 
                         ...)
    if (is.null(pltdat)) return(NULL)
    if (saveobj) {
      message("saving pltdat object to: ", 
			file.path(outfolder, "pltdat.rda"), "...")
      save(pltdat, file=file.path(outfolder, "pltdat.rda"))
    }
  } else {
    pltdat.names <- c("bnd", "xy.uniqueid", "pjoinid", "tabs", "tabIDs")
    if (!all(pltdat.names %in% names(pltdat))) {
      stop("missing components in pltdat list: ",
		toString(pltdat.names[!pltdat.names %in% names(pltdat)]))
    }
  }
 
  ## Extract list objects
  spxy <- pltdat$spxy
  xy.uniqueid <- pltdat$xy.uniqueid
  pjoinid <- pltdat$pjoinid
  tabs <- pltdat$tabs
  tabIDs <- pltdat$tabIDs
  bnd <- pltdat$bnd
  states <- pltdat$states
  invyrs <- pltdat$invyrs

  ## Check bnd.att
  bnd.att <- pcheck.varchar(var2check=bnd.att, varnm="bnd.att",
                            checklst=names(bnd), gui=gui, 
                            caption="Boundary attribute", stopifnull=FALSE)

  ## Check bnd.att
  bnd.att <- pcheck.varchar(var2check=bnd.att, varnm="bnd.att",
	 checklst=names(bnd), gui=gui, caption="Boundary attribute", stopifnull=FALSE)
  if (is.null(bnd.att)) {
    if ("DOMAIN" %in% names(bnd)) {
      bnd.att <- "DOMAIN"
    } else {
      message("only one domain unit")
      bnd$ONEUNIT <- 1
      bnd.att <- "ONEUNIT"
    }
  }

  ## Check raster data
  if (is.null(rastlst.cont) && is.null(rastlst.cat)) {
    stop("must include raster data")
  }


  if (showsteps && !is.null(spxy)) {
    ## Set plotting margins
    mar <-  graphics::par("mar")
    graphics::par(mar=c(1,1,1,1))

    plot(sf::st_geometry(bnd), border="grey")
    plot(sf::st_geometry(spxy), add=TRUE, col="blue", cex=.25)
    graphics::par(mar=mar)
  }

  if (savesteps && !is.null(spxy)) {
    ## Set plotting margins
    mar <-  graphics::par("mar")
    graphics::par(mar=c(1,1,1,1))

    jpg_layer <- paste0(out_layer, "_plots")
    jpgfn <- paste0(stepfolder, "/", jpg_layer, ".jpg")
    grDevices::jpeg(jpgfn, res=400, units="in", width=8, height=10)

    plot(sf::st_geometry(bnd), border="grey")
    plot(sf::st_geometry(spxy), add=TRUE, col="blue", cex=.25)
    grDevices::dev.off()
    message("Writing jpg to ", jpgfn, "\n")
    graphics::par(mar=mar)
  }

  ## Check number of plots (Note: must be greater than minplots)
  ## If all AOIs have less than 2 plots, return NULL
  polyatts <- c(bnd.att, "AOI")
  polyvarlst <- unique(polyatts[polyatts %in% names(bnd)])

  if (length(polyvarlst) > 0) {
    extpoly <- tryCatch(spExtractPoly(xyplt = spxy, 
                                    polyvlst = bnd, 
                                    xy.uniqueid = xy.uniqueid, 
                                    polyvarlst = polyvarlst, 
                                    keepNA = FALSE),
                        error=function(e) {
                          message(e, "\n")
                          return(NULL) })
    if (is.null(extpoly)) {
      return(NULL)
      stop()
    }
    test <- data.table::data.table(sf::st_drop_geometry((extpoly$spxyext)))
  } else {
    test <- data.table::data.table(sf::st_drop_geometry((spxy)))
  }
  if (!"AOI" %in% names(test)) {
    test$AOI <- 1
    if (any(vars2keep == "AOI")) {
      vars2keep <- vars2keep[vars2keep != "AOI"]
    }
  }
 
  pltcnt <- test[AOI == 1, .N, by=bnd.att]
  message("checking number of plots by domain...")
  message(paste0(utils::capture.output(pltcnt), collapse = "\n"))

  if (nrow(pltcnt) == 0) {
    message("No plots in AOI... no estimates generated")
    #return(NULL)
  } else if (all(pltcnt$N < minplots)) {
    message("ALL AOIs have ", minplots, " plots or less... no estimates generated")
    #return(NULL)
  }

  ####################################################################
  ## Get model data
  ####################################################################
  message("summarizing auxiliary model data...")
  auxdat <- spGetAuxiliary(xyplt = spxy,
                           uniqueid = xy.uniqueid,
                           unit_layer = bnd, 
                           unitvar = bnd.att, 
                           rastfolder = rastfolder,
                           rastlst.cont = rastlst.cont, 
                           rastlst.cont.name = rastlst.cont.name,
                           rastlst.cont.NODATA = rastlst.cont.NODATA,
                           rastlst.cat = rastlst.cat, 
                           rastlst.cat.name = rastlst.cat.name,
                           rastlst.cat.NODATA = rastlst.cat.NODATA, 
                           keepNA = keepNA, 
                           npixels = TRUE,
                           vars2keep = vars2keep, 
                           savedata = FALSE, 
                           ncores = ncores, 
                           addN = TRUE)
  pltassgn <- auxdat$pltassgn
  unitzonal <- auxdat$unitzonal
  unitvar <- auxdat$unitvar
  prednames <- auxdat$prednames
  zonalnames <- auxdat$zonalnames
  predfac <- auxdat$predfac
  unitarea <- auxdat$unitarea
  areavar <- auxdat$areavar
  pltassgnid <- auxdat$pltassgnid
  npixelvar <- auxdat$npixelvar

  # if ("N" %in% names(unitzonal)) {
  #   message("checking number of plots by domain...")
  #   pltcnt <- unitzonal[, c(unitvar, "N")]
  #   message(paste0(utils::capture.output(pltcnt), collapse = "\n"))
  # 
  #   if (nrow(pltcnt) == 0) {
  #     message("No plots in AOI... no estimates generated")
  #     return(NULL)
  #   } else if (all(pltcnt$N <= minplots)) {
  #     message("ALL AOIs have ", minplots, " plots or less... no estimates generated")
  #     print(pltcnt)
  #     return(NULL)
  #   }
  #   unitzonal$N <- NULL
  # }

  
  ##########################################
  ## Create output list
  ##########################################
  returnlst <- list(bnd=bnd, tabs = tabs, tabIDs = tabIDs, 
                    pltassgn = pltassgn, 
                    pltassgnid = pltassgnid, pjoinid = pjoinid, 
                    unitarea = unitarea, 
                    unitvar = unitvar, areavar=areavar, 
                    unitzonal = unitzonal, 
                    prednames = prednames, predfac = predfac, 
                    zonalnames = zonalnames, npixelvar = npixelvar, pltcnt = pltcnt,
                    states = states, invyrs = invyrs, vars2keep = vars2keep)
  if (length(predfac) > 0) {
    returnlst$predfac.levels <- auxdat$predfac.levels
  }

  if (returnxy) {
    returnlst$spxy <- spxy
    returnlst$xy.uniqueid <- xy.uniqueid
  }

  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, ext="rda", outfolder=outfolder, 
                      overwrite=overwrite_layer, outfn.pre=outfn.pre, outfn.date=TRUE)
    saveRDS(returnlst, file=objfn)
    message("saving object to: ", objfn)
  }


  #################################################################
  ## Write data to file
  #################################################################
  if (savedata) {
    if (!is.null(RS)) {
      datExportData(sf::st_drop_geometry(bnd), 
            savedata_opts=list(outfolder=outfolder, 
                               out_fmt=out_fmt, 
                               out_dsn=out_dsn, 
                               out_layer=out_layer, 
                               outfn.date=outfn.date, 
                               overwrite_layer=overwrite_layer))
    }
    if (returnxy) {
      datExportData(sf::st_drop_geometry(spxy), 
            savedata_opts=list(outfolder=outfolder, 
                               out_fmt=out_fmt, 
                               out_dsn=out_dsn, 
                               out_layer="xyplt", 
                               outfn.date=outfn.date, 
                               overwrite_layer=overwrite_layer))
    }
    datExportData(pltassgn, 
            savedata_opts=list(outfolder=outfolder, 
                               out_fmt=out_fmt, 
                               out_dsn=out_dsn, 
                               out_layer="pltassgn", 
                               outfn.date=outfn.date, 
                               overwrite_layer=overwrite_layer))
    for (tabnm in names(tabs)) {
      datExportData(tabs[[tabnm]], 
            savedata_opts=list(outfolder=outfolder, 
                               out_fmt=out_fmt, 
                               out_dsn=out_dsn, 
                               out_layer=tabnm, 
                               outfn.date=outfn.date, 
                               overwrite_layer=overwrite_layer))
    }
    datExportData(unitarea, 
            savedata_opts=list(outfolder=outfolder, 
                               out_fmt=out_fmt, 
                               out_dsn=out_dsn, 
                               out_layer="unitarea", 
                               outfn.date=outfn.date, 
                               overwrite_layer=overwrite_layer))
    datExportData(unitzonal, 
            savedata_opts=list(outfolder=outfolder, 
                               out_fmt=out_fmt, 
                               out_dsn=out_dsn, 
                               out_layer="unitzonal", 
                               outfn.date=outfn.date, 
                               overwrite_layer=overwrite_layer))
    datExportData(pltcnt, 
            savedata_opts=list(outfolder=outfolder, 
                               out_fmt=out_fmt, 
                               out_dsn=out_dsn, 
                               out_layer="pltcnt", 
                               outfn.date=outfn.date, 
                               overwrite_layer=overwrite_layer))

  }

  return(returnlst)
}
