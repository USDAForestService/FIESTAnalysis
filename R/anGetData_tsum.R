#' Data - Get plot data and auxiliary data for estimation.
#'
#' Wrapper to get plot data and auxiliary data for estimation.
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
#' @param estvarlst String (vector). Tree-level variable(s) estimation
#' variables (e.g., "TPA_UNADJ", "BA").
#' @param estvar.filter String. A filter for all variables in estvarlst.
#' @param bycond Logical. If TRUE, summaries by condition.
#' @param TPA Logical. If TRUE, tsumvarlst variable(s) are multiplied by the
#' respective trees-per-acre variable to get per-acre measurements.
#' @param getadjplot Logical. If TRUE, adjustments are calculated for
#' nonsampled conditions on plot.
#' @param polyvlst sf R object or String. Name(s) of polygon layers to extract
#' values. A spatial polygon object, full path to shapefile, or name of a layer
#' within a database.
#' @param polyv_dsn String. Data source name (dsn) where polyvlst layers are
#' found (e.g., *.sqlite, *.gdb, folder name). The dsn varies by driver.  See
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
#' @param keepNA Logical. If TRUE, NA values in data extraction are retained 
#' in output.
#' @param strata Logical. If TRUE, include stratalut in output dataset.
#' @param strvar String. Name of strata variable to include in stratalut.  Name
#' must be in rastlst.cat.name or rastlst.cat (if rastlst.cat.name= NULL).
#' @param vars2keep String. Name of variable(s) in smallbnd to keep for output.
#' @param minplots Integer. Minimum number of plots in any on domain unit.
#' @param showsteps Logical. If TRUE, clipped points are displayed.  If
#' savedata=TRUE, the display is saved as *.jpg to outfolder.
#' @param returnxy Logical. If TRUE, returns XY coordinates.
#' @param savedata Logical. If TRUE, saves data to outfolder. Note:
#' includes XY data if returnxy = TRUE.
#' @param exportsp Logical. If TRUE, exports spatial object.
#' @param savesteps Logical. If TRUE, save steps spatial intermediate layers.
#' @param saveobj Logical. If TRUE, save SAest object to outfolder.
#' @param objnm String. Name of saved object name.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param pltdat R object. FIA plot data, output from spGetPlots().
#' @param auxdat R object. Auxiliary data, output from spGetAuxiliary().
#' @param ncores R object. Number of cores to use for data extraction.
#' (Note: needs parallel package).
#' @param ...  Other parameters for DBgetPlots.
#' @return Data.
#' @note
#'
#' If exportsp=TRUE:\cr If out_fmt="shp", the writeOGR (rgdal) function is
#' called. The ArcGIS driver truncates variable names to 10 characters or less.
#' Variable names are changed before export using an internal function.
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
#' @export
anGetData_tsum <- function(bnd_layer, 
                           bnd_dsn = NULL, 
                           bnd.att = NULL, 
                           bnd.filter = NULL, 
                           RS = NULL, 
                           estvarlst = NULL, 
                           estvar.filter = "STATUSCD == 1", 
                           bycond = TRUE,
                           TPA = TRUE, 
                           getadjplot = TRUE, 
                           polyvlst = NULL,
                           polyv_dsn = NULL,
                           rastfolder = NULL, 
                           rastlst.cont = NULL, 
                           rastlst.cont.name = NULL, 
                           rastlst.cont.NODATA = NULL, 
                           rastlst.cat = NULL, 
                           rastlst.cat.name = NULL, 
                           rastlst.cat.NODATA = NULL, 
                           keepNA = TRUE,
                           strata = FALSE, 
                           strvar = NULL, 
                           vars2keep = NULL, 
                           minplots = 0, 
                           showsteps = FALSE, 
                           returnxy = FALSE, 
                           savedata = FALSE, 
                           exportsp = FALSE,
                           savesteps = FALSE, 
                           saveobj = FALSE, 
                           objnm = "tsumdat", 
                           savedata_opts = NULL, 
                           pltdat = NULL, 
                           auxdat = NULL,
                           ncores = 1,
                           ...) {

  ## Set global variables
  gui <- FALSE
  plt=AOI=outnames <- NULL
  istree <- TRUE
  addmean <- FALSE
  sumvars=pltassgn2=strvar2 <- NULL
  cround=tround=3

## To add
## 1) subset plot data


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  matchargs <- as.list(match.call()[-1])

  dotargs <- c(list(...))
  args <- as.list(environment())
  args <- args[!names(args) %in% names(dotargs)]
  args <- args[names(args) %in% names(matchargs)]
  args <- append(args, dotargs)

  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(anGetData_tsum)),
		names(formals(spGetPlots)), "istree", "isseed")
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)


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


  ## Check strata
  strata <- pcheck.logical(strata, varnm="strata",
		title="Add stratalut?", first="NO", gui=gui, stopifnull=TRUE)

  ## Check returnxy
  returnxy <- pcheck.logical(returnxy, varnm="returnxy",
                             title="Return XY?", first="NO", gui=gui)
  
  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata",
		title="Save data extraction?", first="NO", gui=gui)

  ### check exportsp
  exportsp <- pcheck.logical(exportsp, varnm="exportsp", 
		title="Export spatial layer?", first="NO", gui=gui)

  ## Check savesteps
  savesteps <- pcheck.logical(savesteps, varnm="savesteps",
		title="Save step data?", first="YES", gui=gui)

  ## Check saveobj
  saveobj <- pcheck.logical(saveobj, varnm="saveobj",
		title="Save SApopdat object?", first="YES", gui=gui, stopifnull=TRUE)

  ## Check ncores
  if (!is.null(ncores)) {
    if (length(ncores) != 1) {
      stop("ncores must be integer vector of length = 1")
    } else if (!is.numeric(ncores)) {
      stop("ncores must be integer vector of length = 1")
    } else if (ncores > 1) {
      require("parallel")
      nbrcores <- parallel::detectCores()
      if (ncores > nbrcores) {
        message("ncores is greater than number of available cores")
        message("using ", nbrcores, " ncores")
        ncores <- nbrcores
      }
    }     
  } else {
    ncores <- 1
  }

  ## Check overwrite, outfn.date, outfolder, outfn
  ########################################################
  if (savedata) {
    outlst <- pcheck.output(out_dsn=out_dsn, out_fmt=out_fmt, 
                  outfolder=outfolder, outfn.pre=outfn.pre, 
                  outfn.date=outfn.date, overwrite_dsn=overwrite_dsn, 
                  overwrite_layer=overwrite_layer, 
                  append_layer=append_layer, createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    overwrite_dsn <- outlst$overwrite_dsn
    append_layer <- outlst$append_layer

  } else if (savesteps || saveobj) {
    outfolder <- pcheck.outfolder(outfolder)
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
    if ("istree" %in% names(args)) {
      pltdat <- spGetPlots(bnd = bnd_layer, 
                         bnd_dsn = bnd_dsn,
                         bnd.filter = bnd.filter, 
                         RS = RS, 
                         returnxy = TRUE, 
                         ...)
	} else {
      pltdat <- spGetPlots(bnd = bnd_layer, 
                         bnd_dsn = bnd_dsn,
                         bnd.filter = bnd.filter, 
                         RS = RS, 
                         returnxy = TRUE, 
						             istree = TRUE,
                         ...)
    }	
    if (is.null(pltdat)) return(NULL)
    if (saveobj) {
      pltobjfn <- getoutfn(outfn="pltdat", ext="rds", outfolder=outfolder, 
                  overwrite=overwrite_layer, outfn.pre=outfn.pre, 
                  outfn.date=outfn.date)
      saveRDS(pltdat, file=pltobjfn)
      message("saving pltdat object to: ", pltobjfn)
    }
  } else {
    pltdat.names <- c("bnd", "xy.uniqueid", "puniqueid", "pjoinid", "tabs")
    pltdat <- pcheck.object(pltdat, list.items = pltdat.names)
  }

  ## Extract list objects
  spxy <- pltdat$spxy
  xy.uniqueid <- pltdat$xy.uniqueid
  puniqueid <- pltdat$puniqueid
  pjoinid <- pltdat$pjoinid
  plt <- pltdat$tabs$plt
  cond <- pltdat$tabs$cond
  tree <- pltdat$tabs$tree
  seed <- pltdat$tabs$seed
  puniqueid <- pltdat$tabIDs$plt
  cuniqueid <- pltdat$tabIDs$cond
  tuniqueid <- pltdat$tabIDs$tree
  bnd <- pltdat$bnd
  
  if (is.null(tree)) {
    stop("must include tree data")
  }

  ## Check bnd.att
  bnd.att <- pcheck.varchar(var2check=bnd.att, 
                            varnm="bnd.att",
                            checklst=names(bnd), gui=gui, 
                            caption="Boundary attribute", 
                            stopifnull=FALSE)
  if (is.null(bnd.att)) {
    if ("DOMAIN" %in% names(bnd)) {
      bnd.att <- "DOMAIN"
    } else {
      message("only one domain unit")
      bnd$ONEUNIT <- 1
      bnd.att <- "ONEUNIT"
    }
  }
  

  ## Check SAdoms
  #if (!all(c("DOMAIN", "AOI") %in% names(SAdoms))) {
  #  stop("invalid SAdoms...  need to include DOMAIN and AOI attributes")
  #}

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
    jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
    grDevices::jpeg(jpgfn, res=400, units="in", width=8, height=10)
    
    plot(sf::st_geometry(bnd), border="grey")
    plot(sf::st_geometry(spxy), add=TRUE, col="blue", cex=.25)
    grDevices::dev.off()
    message("Writing jpg to ", jpgfn, "\n")
    graphics::par(mar=mar)
  }

  ## Check number of plots (Note: must be greater than minplots)
  ## If all AOIs have less than 2 plots, return NULL
  polyvarlst <- unique(c(bnd.att, vars2keep)[!c(bnd.att, vars2keep) %in% names(spxy)])
  if (length(polyvarlst) > 0) {
    extpoly <- tryCatch(
		 spExtractPoly(xyplt = spxy,
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
  }
  test <- test[AOI == 1, .N, by=bnd.att]
  message("checking number of plots in domain...")

  if (nrow(test) == 0) {
    message("No plots in AOI... no estimates generated")
    return(NULL)
  } else if (all(test$N <= minplots)) {
    message("ALL AOIs have ", minplots, " plots or less... no estimates generated")
    messagedf(test)
    return(NULL)
  } else {
    message("all domains have >= ", minplots, " plots")
    messagedf(test)
  }
    

  ####################################################################
  ## Extract polygon vector data
  ####################################################################
  if (!is.null(polyvlst)) {
    extpoly2 <- tryCatch(
                    spExtractPoly(xyplt = spxy, 
                                  polyvlst = polyvlst, 
                                  xy.uniqueid = xy.uniqueid, 
                                  keepNA = FALSE),
     	                    error=function(e) {
			                        message(e, "\n")
			                    return(NULL) })
    if (is.null(extpoly2)) {
      stop("polyvlst is invalid")
    } else {
      spxy <- extpoly2$spxyext
      outnames <- extpoly2$outnames
    }
  }

  ####################################################################
  ## Get auxiliary data
  ####################################################################
  if (is.null(auxdat)) {
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
                             ncores = ncores,
                             vars2keep = vars2keep, 
                             savedata = FALSE)
    if (is.null(auxdat)) return(NULL)
    if (saveobj) {
      auxobjfn <- getoutfn(outfn="auxdat", ext="rds", outfolder=outfolder, 
                  overwrite=overwrite_layer, outfn.pre=outfn.pre, 
                  outfn.date=outfn.date)
      saveRDS(auxdat, file=auxobjfn)
      message("saving auxdat object to: ", auxobjfn)
    }
  } else {
    auxdat.names <- c("pltassgn", "unitarea", "unitzonal")
    auxdat <- pcheck.object(auxdat, list.items = auxdat.names)
  }
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
  npixels <- unitzonal[, c(unitvar, npixelvar)]

  if (strata) { 
    if (is.null(strvar)) {    
      if (!is.null(predfac) && length(predfac) == 1) {
        strvar <- predfac
      } else {
        stop("must include strvar if strata=TRUE")
      }
    } 
    strwtvar <- "strwt" 
    if (!is.null(unitzonal)) {
      if (length(strvar) > 1) {
        for (i in 1:length(strvar)) {
          assign(paste0("stratalut",i), 
                    strat.pivot(unitzonal, unitvars=unitvar, 
                      strvar[i], strwtvar=strwtvar))
          assign(paste0("strvar",i), strvar)
        }
      } else {
        stratalut <- strat.pivot(unitzonal, unitvars=unitvar, 
                      strvar, strwtvar=strwtvar)
      }
    }
  }

  ##########################################
  ## Create output list
  ########################################## 

  #####################################################
  ## Get population data 
  #####################################################
  popdat <- modMApop(pltdat = pltdat, 
                     auxdat = auxdat, 
                     adjplot = getadjplot, 
                     standardize = FALSE) 
  treex <- popdat$treex

  #####################################################
  ## Summarize condition data 
  #####################################################
  csumvarnm <- "FORPROP"

  conddatp <- datSumCond(cond = cond,
                         plt = plt,
                         adjcond = TRUE,
                         NAto0 = TRUE,
                         csumvar = "CONDPROP_UNADJ",
                         cfilter = "COND_STATUS_CD == 1",
                         csumvarnm = csumvarnm,
                         getadjplot = getadjplot,
                         puniqueid = puniqueid,
                         cuniqueid = cuniqueid,
                         cround = cround)
  cdatp <- conddatp$condsum
  csumvar <- conddatp$csumvarnm

  conddatc <- datSumCond(cond = cond,
                         bycond = TRUE,
                         adjcond = TRUE,
                         NAto0 = TRUE,
                         csumvar = "CONDPROP_UNADJ",
                         cfilter = "COND_STATUS_CD == 1",
                         csumvarnm = csumvarnm,
                         getadjplot = getadjplot,
                         puniqueid = puniqueid,
                         cuniqueid = cuniqueid,
                         cround = cround)
  cdatc <- conddatc$condsum

  ## Metadata for forprop
  DESCRIPTION  <- "Proportion of forest land"
  if (getadjplot) {
    DESCRIPTION <- paste(DESCRIPTION, "- adjusted for partial nonresponse at plot-level")
  }
  forprop_meta <- cbind(VARIABLE = csumvar, DESCRIPTION = DESCRIPTION, TABLE = "DERIVED")
  
  #####################################################
  ## Summarize tree data 
  #####################################################
  adjtree <- ifelse(getadjplot, TRUE, FALSE)

  ## Summarize tree data to plot-level
  treedatp <- datSumTree(tree = treex, 
                         plt = cdatp,
                         cond = cond,
                         bycond = FALSE,
                         TPA = TPA, 
                         adjtree = adjtree, 
                         NAto0 = TRUE, 
                         tsumvarlst = estvarlst, 
                         getadjplot = FALSE,
                         tfilter = estvar.filter, 
                         puniqueid = puniqueid,
                         cuniqueid = cuniqueid,
                         tround = tround)
  tdatp <- treedatp$treedat
  sumvars <- unique(c(csumvar, treedatp$sumvars))
  tdatp_meta <- treedatp$meta  
  tdatp_meta <- rbind(tdatp_meta, forprop_meta)

  ## Summarize tree data to condition-level
  treedatc <- datSumTree(tree = treex, 
                         cond = cdatc, 
                         bycond = TRUE,
                         TPA = TPA, 
                         adjtree = adjtree, 
                         NAto0 = TRUE, 
                         tsumvarlst = estvarlst, 
                         getadjplot = FALSE,
                         tfilter = estvar.filter, 
                         puniqueid = puniqueid,
                         cuniqueid = cuniqueid,
                         tround = tround)
  tdatc <- treedatc$treedat
  sumvars <- unique(c(csumvar, treedatc$sumvars))
  tdatc_meta <- treedatc$meta  
  tdatc_meta <- rbind(tdatc_meta, forprop_meta)
 
  if (addmean) {
    tpltassgn <- setDT(merge(pltassgn[, c(pltassgnid, unitvar)], tdatp, 
				by.x=pltassgnid, by.y=puniqueid))

    varnm.mean <- paste0(sumvars, "_mean")
    varnm.var <- paste0(sumvars, "_var")

    ## Add mean response to dunitlut for Area-level estimates
    datmean <- tpltassgn[, lapply(.SD, mean, na.rm=TRUE), .SDcols=sumvars, by=unitvar]
    setnames(datmean, sumvars, varnm.mean)

    datvar <- tpltassgn[, lapply(.SD, function(x) {var(x, na.rm=TRUE) / .N}), 
		.SDcols=sumvars, by=unitvar]
    setnames(datvar, sumvars, varnm.var)

    popdat$unitlut <- merge(popdat$unitlut, datvar, by=unitvar)
  }

  pltvars <- names(pltassgn)[!names(pltassgn) %in% names(tdatp)]
  pltassgn <- pltassgn[pltassgn$PLT_CN %in% tdatp$CN, unique(c(pltassgnid, pltvars, outnames))]

  if (nrow(tdatp) != length(unique(tdatc$PLT_CN))) {
    message("number of plots are different")
  }
    

  returnlst <- list(bnd=bnd, 
                    popdat=popdat,
                    pltassgn=data.frame(pltassgn), 
                    tsumdatp=data.frame(tdatp),
                    tsumdatc=data.frame(tdatc),
                    unitarea=data.frame(unitarea), 
                    unitvar=unitvar, 
                    areavar=areavar, 
                    unitzonal=data.frame(unitzonal), 
                    prednames=prednames, 
                    predfac=predfac, 
                    zonalnames=zonalnames, 
                    puniqueid=puniqueid, 
                    pjoinid=pjoinid, 
                    pltassgnid=pltassgnid,
                    npixels=npixels,
                    npixelvar=npixelvar, 
                    sumvars=sumvars)
  if (strata) {
    if (length(strvar) > 1) {
      for (i in 1:length(strvar)) {
        returnlst[[paste0("stratalut",i)]] <- get(paste0("stratalut", i))
        returnlst[[paste0("strvar",i)]] <- get(paste0("strvar", i))
      }
    } else {
      returnlst$stratalut <- stratalut
      returnlst$strvar <- strvar
    }
  }
  
#  if (istree && !is.null(treex)) {
#    returnlst$tree <- data.table::setDF(treex)
#  }
#  if (isseed && !is.null(seedx)) {
#    returnlst$seed <- data.table::setDF(seedx)
#  }
  if (length(predfac) > 0) {
    returnlst$predfac.levels <- auxdat$predfac.levels
  }

  if (returnxy) {
    returnlst$spxy <- spxy
    returnlst$xy.uniqueid <- xy.uniqueid
  }
  returnlst$tsumdatp_meta <- tdatp_meta
  returnlst$tsumdatc_meta <- tdatc_meta
  returnlst$args <- args

  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, ext="rds", outfolder=outfolder, 
                  overwrite=overwrite_layer, outfn.pre=outfn.pre, 
                  outfn.date=outfn.date)
    saveRDS(returnlst, file=objfn)
    message("saving object to: ", objfn)
  }

  if (savedata) {
    if (!is.null(RS)) {
      datExportData(sf::st_drop_geometry(bnd), 
        savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="SAdoms",
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer,
                            append_layer=append_layer,
                            add_layer=TRUE))
    }
    if (returnxy) {
      datExportData(sf::st_drop_geometry(spxy), 
            savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="xyplt", 
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer))    }
    datExportData(pltassgn, 
            savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="pltassgn", 
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer))
    datExportData(tdatp, 
            savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="tsumdatp", 
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer))

    datExportData(tdatc, 
            savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="tsumdatc", 
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer))

    
    # for (tabnm in names(tabs)) {
    #   datExportData(tabs[[tabnm]], 
    #         savedata_opts=list(outfolder=outfolder, 
    #                         out_fmt=out_fmt, 
    #                         out_dsn=out_dsn, 
    #                         out_layer=tabnm, 
    #                         outfn.date=outfn.date, 
    #                         overwrite_layer=overwrite_layer))
    # }
    datExportData(unitarea, 
            savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="unitarea", 
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer))
    datExportData(unitzonal, 
            savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="unitzonal", 
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer))  
    datExportData(tdatp_meta, 
            savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="tsumdatp_meta", 
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer)) 

    datExportData(tdatc_meta, 
            savedata_opts=list(outfolder=outfolder, 
                            out_fmt=out_fmt, 
                            out_dsn=out_dsn, 
                            out_layer="tsumdatc_meta", 
                            outfn.pre=outfn.pre, 
                            outfn.date=outfn.date, 
                            overwrite_layer=overwrite_layer))  
 

  }

  if (exportsp) {
    makesp <- TRUE
    message("exporting point data...")  
    pltvars <- c("CN", "STATECD", "UNITCD", "COUNTYCD", "PLOT",
                 "PLOT_ID", "LON_PUBLIC", "LAT_PUBLIC", 
                 "PLOT_STATUS_CD", "INVYR", 
                 "MEASYEAR", "MEASMON", "INTENSITY", 
                 "NBRCND", "NBRCNDSAMP", "NBRCNDFOR", 
                 "FORNONSAMP", "P2PANEL")
    pltvars <- pltvars[pltvars %in% names(plt)]
    if (all(c("CN", "LON_PUBLIC", "LAT_PUBLIC") %in% pltvars)) {
      xy.uniqueid <- "CN"
      xvar <- "LON_PUBLIC"
      yvar <- "LAT_PUBLIC"
    } else {
      if ("CN" %in% pltvars) {
        xy.uniqueid <- "CN"
      } else if ("PLT_CN" %in% pltvars) {
        xy.uniqueid <- "PLT_CN"
      } else {
        makesp <- FALSE
        message("CN or PLT_CN not in plt... cannot create spatial points")
      }
      if (all(c("LON", "LAT") %in% pltvars)) {
        xvar <- "LON"
        yvar <- "LAT"
      } else if (all(c("LON_ACTUAL", "LAT_ACTUAL") %in% pltvars)) {
        xvar <- "LON_ACTUAL"
        yvar <- "LAT_ACTUAL"
      } else {
        makesp <- FALSE
        message("LON and LAT variable are not in plt... cannot create spatial points")
      }
    }
      
    if (makesp) {
      spxy_pub <- spMakeSpatialPoints(
                 plt[, pltvars], 
                 xy.uniqueid = xy.uniqueid,
                 xvar = xvar, 
                 yvar = yvar,
                 exportsp = TRUE, 
                 savedata_opts = list(outfolder=outfolder, 
                                      out_layer = "spxy_PUBLIC",
                                      outfn.pre = outfn.pre))
    }
  }   

  return(returnlst)
}

