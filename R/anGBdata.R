#' @title
#' Analysis - Get data for Green-Book module.
#'
#' @description 
#' Wrapper to get plot data and stratification data within a given 
#' boundary for use in FIESTA's Green-Book module.
#'
#' @param bnd_layer sf R object or String. Area of interest (population
#' boundary or estimation unit). Can be a spatial polygon object, full pathname
#' to a shapefile, or name of a layer within a database.
#' @param bnd_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of boundary. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if bnd is sf object.
#' @param bnd.att String. The attribute in bnd_layer that defines the
#' estimation unit(s). If NULL, bnd is default as one estimation unit.
#' @param bnd.filter String. Filter to subset bnd SpatialPolygons layer.
#' @param RS String. Name of FIA research station to restrict states to.
#' @param strata Logical. If TRUE, get strata weights from strat_layer.
#' @param strattype String. Spatial layer type of stratlayer ("POLY",
#' "RASTER").  Note: raster objects must be written to file.
#' @param strat_layer String or raster object. Name of the strata spatial
#' raster layer, including fullpath and extension (e.g., *.img). If raster
#' object, must be written to file.
#' @param strat_dsn String. The data source name (dsn; i.e., folder or database
#' name, if polygon layer) of strat_layer. The dsn varies by driver. See gdal
#' OGR vector formats (https://www.gdal.org/ogr_formats.html). Optional.
#' @param strvar String. If strattype="POLY", name of strata attribute in
#' strat_layer.
#' @param rast.NODATA Numeric vector. NODATA value for strat_layer, 
#' if strattype="RASTER".
#' @param minplots Integer. Minimum number of plots in any on domain unit.
#' @param showsteps Logical. If TRUE, show intermediate steps.
#' @param cex.plots Number. Size of dots representing plots in display.
#' @param returnxy Logical. If TRUE, returns XY coordinates.
#' @param savexy Logical. If TRUE, saves XY data to outfolder.
#' @param savedata Logical. If TRUE, saves data to outfolder. 
#' @param savesteps Logical. If TRUE, saves plotting steps as jpg images to
#' outfolder.
#' @param saveobj Logical. If TRUE, save SAest object to outfolder.
#' @param objnm String. Name of *.rds object.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. 
#' @param pltdat R object. FIA plot data, output from spGetPlots().
#' @param ... Variables passed to spGetPlots.
#' 
#' @return Data.
#' @author Tracey S. Frescino
#' @keywords data
#' 
#' @examples
#'   ## Get data for WY Bighorn administrative boundary using
#'   WYbhfn <- system.file("extdata",
#'    "sp_data/WYbighorn_adminbnd.shp",
#'     package="FIESTAnalysis")
#'   fornffn <- system.file("extdata",
#'    "sp_data/WYbighorn_forest_nonforest_250m.tif",
#'     package="FIESTAnalysis")
#'
#'   ## Get data for calculating GB population data and generating GB estimates
#'   GBdata <- anGBdata(WYbhfn, datsource="datamart", strat_layer=fornffn, evalCur=TRUE)
#'   names(GBdata)
#'
#'   plot(sf::st_geometry(GBdata$bnd))
#'   plot(sf::st_geometry(GBdata$xyplt), add=TRUE, pch=18, cex=.5)
#'
#'   dim(GBdata$xyplt)
#'   dim(GBdata$plt)
#'   dim(GBdata$pltassgn)
#'
#'   ## Look at unitarea, area by estimation and stratalut, with strata pixel counts and weights
#'   GBdata$unitarea
#'   GBdata$stratalut
#'
#'   ## Display first 6 rows of pltassgn table with plot assignments for strata
#'   head(GBdata$pltassgn)
#'
#' @export
anGBdata <- function(bnd_layer, 
                     bnd_dsn = NULL, 
                     bnd.att = NULL, 
                     bnd.filter = NULL, 
                     RS = NULL, 
                     strata = TRUE, 
                     strattype = "RASTER", 
                     strat_layer = NULL, 
                     strat_dsn = NULL, 
                     strvar = NULL, 
                     rast.NODATA = 0,
                     minplots = 0, 
                     showsteps = FALSE, 
                     cex.plots = 0.5, 
                     returnxy = FALSE, 
					 savexy = FALSE,
                     savedata = FALSE, 
                     savesteps = FALSE, 
                     saveobj = FALSE, 
                     objnm = NULL,
                     savedata_opts = NULL, 
                     pltdat = NULL, 
                     ...) {


  ## Set global variables
  gui <- FALSE
  plt=stratalut=strwtvar <- NULL

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  
  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(anGBdata)),
		names(formals(spGetPlots)))
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

  ## Check returnxy
  returnxy <- pcheck.logical(returnxy, varnm="returnxy", 
                             title="Return XY?", first="NO", gui=gui)
  
  ## Check savexy
  savexy <- pcheck.logical(savexy, varnm="savexy", 
                           title="Save XY data?", first="NO", gui=gui)

  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata", 
                             title="Save data extraction?", first="NO", gui=gui)

  ## Check savesteps
  savesteps <- pcheck.logical(savesteps, varnm="savesteps", 
                              title="Save step data?", first="YES", gui=gui)

  ## Check saveobj
  saveobj <- pcheck.logical(saveobj, varnm="saveobj", 
                            title="Save SApopdat object?", 
                            first="YES", gui=gui, stopifnull=TRUE)
  
  if (saveobj && is.null(objnm)) {
    objnm <- "GBdata"
  }


  ## Check output
  if (savedata) {
    outlst <- pcheck.output(out_dsn = out_dsn, 
                            out_fmt = out_fmt, 
                            outfolder = outfolder, 
                            outfn.pre = outfn.pre,
                            outfn.date = outfn.date, 
                            overwrite_dsn = overwrite_dsn,
                            overwrite_layer = overwrite_layer, 
                            append_layer = append_layer,
                            createSQLite = FALSE, 
                            gui = gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    overwrite_dsn <- outlst$overwrite_dsn
    append_layer <- outlst$append_layer
    if (!is.null(out_layer)) {
      out_layer <- "bnd"
    }
  } else if (savesteps || saveobj) {
    outfolder <- pcheck.outfolder(outfolder)
  }
 
  ####################################################################
  ## Get FIA plot data from SQLite within boundary
  ####################################################################
  if (is.null(pltdat)) {
    pltdat <- spGetPlots(bnd=bnd_layer, 
                         bnd_dsn=bnd_dsn, bnd.filter=bnd.filter, 
                         RS=RS, returnxy=TRUE, ...)
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
  pltids <- pltdat$pltids
  xy.uniqueid <- pltdat$xy.uniqueid
  pjoinid <- pltdat$pjoinid
  tabs <- pltdat$tabs
  tabIDs <- pltdat$tabIDs
  bnd <- pltdat$bnd


  ## Check bnd.att
  bnd.att <- pcheck.varchar(var2check=bnd.att, varnm="bnd.att", 
                            checklst=names(bnd), gui=gui, 
                            caption="Boundary attribute", 
                            stopifnull=FALSE)
  if (is.null(bnd.att)) {
    bnd$ONEUNIT <- 1
    bnd.att <- "ONEUNIT"
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
    jpgfn <- paste0(outfolder, "/", jpg_layer, ".jpg")
    grDevices::jpeg(jpgfn, res=400, units="in", width=8, height=10)
    graphics::par(mar=c(1,1,1,1))

    plot(sf::st_geometry(bnd), border="grey")
    plot(sf::st_geometry(spxy), add=TRUE, col="blue", cex=.25)
    grDevices::dev.off()
    message("Writing jpg to ", jpgfn, "\n")
    graphics::par(mar=mar)
  }


  ## Check number of plots (Note: must be greater than minplots)
  ## If all AOIs have less than 2 plots, return NULL
  polyvarlst <- bnd.att
  extpoly <- spExtractPoly(xyplt = spxy, 
                           polyvlst = bnd, 
                           xy.uniqueid = xy.uniqueid, 
                           polyvarlst = polyvarlst, 
                           keepNA = FALSE)
  test <- data.table::data.table(sf::st_drop_geometry((extpoly$spxyext)))
  test <- test[, .N, by=bnd.att]
  message("checking number of plots in domain...")

  if (nrow(test) == 0) {
    message("No plots in AOI... no estimates generated")
#    return(NULL)
  } else if (all(test$N <= minplots)) {
    message("ALL AOIs have ", minplots, " plots or less... no estimates generated")
    print(test)
#    return(NULL)
  }
 

  ####################################################################
  ## Get strata data 
  ####################################################################
  if (strata) {
    message("summarizing stratification data...")
    stratdat <- spGetStrata(spxy, 
                            uniqueid = xy.uniqueid, 
                            unit_layer = bnd, 
                            unitvar = bnd.att, 
                            strattype = strattype, 
                            strat_layer = strat_layer, 
                            strat_dsn = strat_dsn, 
                            strvar = strvar, 
                            rast.NODATA = rast.NODATA)
    pltassgn <- data.table::setDT(stratdat$pltassgn)
    unitarea <- stratdat$unitarea
    unitvar <- stratdat$unitvar
    areavar <- stratdat$areavar
    stratalut <- stratdat$stratalut
    strvar <- stratdat$strvar
    strwtvar <- stratdat$strwtvar
    pltassgnid <- stratdat$pltassgnid

  } else {
    message("summarizing estimation unit data...")
    stratdat <- spGetEstUnit(spxy, unittype="POLY", 
                                     uniqueid=xy.uniqueid, 
                                     unit_layer=bnd, unitvar=bnd.att)
    pltassgn <- stratdat$pltassgn
    unitarea <- stratdat$unitarea
    unitvar <- stratdat$unitvar
    areavar <- stratdat$areavar
    pltassgnid <- stratdat$pltassgnid
  }

  ##########################################
  ## Create output list
  ##########################################
  returnlst <- list(bnd=bnd, 
                    tabs=tabs, 
                    tabIDs=tabIDs, 
                    pltassgn=data.table::setDF(pltassgn), 
                    pltassgnid=pltassgnid, 
                    pjoinid=pjoinid, 
                    unitarea=data.table::setDF(unitarea), 
                    unitvar=unitvar, 
                    areavar=areavar)
  if (strata) {
    returnlst$stratalut <- stratalut
    returnlst$strvar <- strvar
    returnlst$strwtvar <- strwtvar
  } 

  if (returnxy) {
    returnlst$spxy <- spxy
    returnlst$xy.uniqueid <- xy.uniqueid
  }

  if (saveobj) {
    objnm <- "GBpopdat.rda"
    objfn <- getoutfn(outfn=objnm, outfolder=outfolder, 
                    overwrite=overwrite_layer, outfn.date=TRUE)
    save(returnlst, file=objfn)
    message("saving object to: ", objfn)
  }


  #################################################################
  ## Write data to file
  #################################################################
   if (savexy) {
     datExportData(sf::st_drop_geometry(spxy), 
            savedata_opts=list(outfolder = outfolder, 
                               out_fmt = out_fmt, 
                               out_dsn = out_dsn, 
                               out_layer = "xyplt", 
                               outfn.date = outfn.date, 
                               overwrite_layer = overwrite_layer))
   }
   if (savedata) {
    if (!is.null(RS)) {
      datExportData(sf::st_drop_geometry(bnd), 
            savedata_opts=list(outfolder = outfolder, 
                               out_fmt = out_fmt, 
                               out_dsn = out_dsn, 
                               out_layer = "bnd", 
                               outfn.date = outfn.date, 
                               overwrite_layer = overwrite_layer))
    }
    datExportData(pltassgn, 
            savedata_opts=list(outfolder = outfolder, 
                               out_fmt = out_fmt, 
                               out_dsn = out_dsn, 
                               out_layer = "pltassgn", 
                               outfn.date = outfn.date, 
                               overwrite_layer = overwrite_layer))

    for (tabnm in names(tabs)) {
      datExportData(tabs[[tabnm]], 
            savedata_opts=list(outfolder = outfolder, 
                               out_fmt = out_fmt, 
                               out_dsn = out_dsn, 
                               out_layer = tabnm, 
                               outfn.date = outfn.date, 
                               overwrite_layer = overwrite_layer))
    }
    datExportData(unitarea, 
            savedata_opts=list(outfolder = outfolder, 
                               out_fmt = out_fmt, 
                               out_dsn = out_dsn, 
                               out_layer = "unitarea", 
                               outfn.date = outfn.date, 
                               overwrite_layer = overwrite_layer))
    if (strata) {
      datExportData(stratalut, 
            savedata_opts=list(outfolder = outfolder, 
                               out_fmt = out_fmt, 
                               out_dsn = out_dsn, 
                               out_layer = "stratalut", 
                               outfn.date = outfn.date, 
                               overwrite_layer = overwrite_layer))
    }
  }

  return(returnlst)
}

