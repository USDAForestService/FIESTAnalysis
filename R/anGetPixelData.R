#' Data - Extract pixel-level data.
#'
#' Wrapper to get pixel data, including auxiliary information.
#'
#'
#' @param ref.rastfn String. Path name of raster to use as reference for
#' pixel size.
#' @param bnd sf R object or String. Boundary layer of estimation domain
#' units. Can be a spatial sf object, full pathname to a shapefile, or name of
#' a layer within a database.
#' @param bnd_dsn String. Name of data source name with bnd_layer, if in a
#' database.
#' @param bnd.att String. Name of attribute in boundary file to define
#' estimation domain units.
#' @param bnd.filter String. Optional filter of bnd_layer.
#' @param polyvlst String vector or list. The name(s) of variable(s) to
#' extract from polygon(s). If extracting multiple variables from more than one
#' polygon, specify names in a list format, corresponding to polyvlst.
#' @param polyvarnmlst String vector or list. Output name(s) of variable(s)
#' extracted from polygon(s). If extracting multiple variables from more than
#' one polygon, specify names in a list format, corresponding to polyvlst. The
#' number of names must match the number of variables in polyvarlst.
#' @param rastfolder String. Name of the folder with raster layers. Optional.
#' Useful if all raster layers are in same folder.
#' @param rastlst String vector or list. A list of raster(s) with
#' continuous data and/or categorical values (e.g., DEM). The list may include 
#' file name of raster(s) or raster objects that are not InMemory.
#' @param rastlst.name String vector. Output names for rastlst.
#' Optional. If NULL, name of raster is used as default or name+'_'+layer
#' number for multi-band layers.
#' @param returnxy Logical. If TRUE, returns XY coordinates.
#' includes XY data if returnxy = TRUE.
#' @param exportsp Logical. If TRUE, the extracted raster point data are
#' exported to outfolder.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE.  
#' @param ncores R object. Number of cores to use for data extraction.
#' (Note: needs parallel package).
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
anGetPixelData <- function(ref.rastfn,
                           bnd, 
                           bnd_dsn = NULL, 
                           bnd.att = NULL, 
                           bnd.filter = NULL, 
                           polyvlst = NULL,
                           polyvarnmlst = NULL,
                           rastfolder = NULL, 
                           rastlst = NULL, 
                           rastlst.name = NULL, 
                           keepNA = TRUE,
                           returnxy = FALSE, 
                           exportsp = FALSE,
                           savedata_opts = NULL, 
                           ncores = 1) {

  ## Set global variables
  gui <- FALSE
  fnlst <- {}
  lut <- NULL
  savedata <- TRUE

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(anGetPixelData))
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
      assign(names(savedata_opts)[[i]], savedata_opts[[i]])
    }
  }


  ## Check returnxy
  returnxy <- pcheck.logical(returnxy, varnm="returnxy",
                             title="Return XY?", first="NO", gui=gui)
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
  } 


  ## Verify ref raster
  ########################################################
  ref.rast <- getrastlst.rgdal(ref.rastfn)

  ## Get proj4 of reference raster
  ref.rast.crs <- rasterInfo(ref.rast)$crs

 
  ####################################################################
  ## Get boundary info
  ####################################################################

  ## Import boundary
  bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")
  if (!is.null(bndx)) {
    bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf

    ## Check bnd.att
    bnd.att <- pcheck.varchar(var2check = bnd.att, 
                              varnm = "bnd.att",
                              checklst = names(bndx), gui=gui, 
                              caption = "Boundary attribute", 
                              stopifnull = TRUE)

    ## Compare crs of reference raster and reproject if different
    bndx <- crsCompare(bndx, ref.rast.crs)$x

    if (!class(bndx[[bnd.att]]) %in% c("numeric", "integer")) { 
      ## Create lookup table for Subsection values
      lut <- data.frame(BYVAR = sort(unique(bndx[[bnd.att]])), 
                  VALUE = seq(1:length(unique(bndx[[bnd.att]]))))
      bndx <- merge(bndx, lut, by.x=bnd.att, by.y="BYVAR")
      burn_value <- "VALUE"
      #bnd
    } else {
      burn_value <- bnd.att
    }

    ## Rasterize polygon to temporary file
    rastfn <- rasterFromVectorExtent(src = bndx, 
                  dstfile = tempfile("bndrast", fileext=".tif"),
                  init = 0, dtName = "Int16", res = 90)

    ## Reference raster
    rasterizePolygons(src = bndx, 
                      rasterfile = rastfn, 
                      burn_value = burn_value)
  }


  # Create temporary raster of pixel longitudes:
  lontmpfn <- tempfile("pixelLon", fileext=".tif")
  gdalraster::calc(expr = "pixelLon * 100000", rasterfiles = rastfn,
             dstfile = lontmpfn,
             dtName = "Int32", options = c("COMPRESS=DEFLATE"),
             nodata_value = -9999, usePixelLonLat = TRUE)

  # Create temporary raster of pixel latitudes:
  lattmpfn <- tempfile("pixelLat", fileext=".tif")
  gdalraster::calc(expr = "pixelLat * 100000", rasterfiles = rastfn,
             dstfile = lattmpfn, 
             dtName = "Int32", options = c("COMPRESS=DEFLATE"),
             nodata_value = -9999, usePixelLonLat = TRUE)

  ## Note: rasterCombine only allows Int* data types
  rastcombo <- gdalraster::combine(c(rastfn, lontmpfn, lattmpfn),
                             var.names = c(burn_value, "pixelLon", "pixelLat"))

  ## Convert Lon/Lat values back to original (numeric) values
  xy.uniqueid <- "cmbid"
  xycols <- c("pixelLon", "pixelLat")
  rastcombo <- setDT(rastcombo)
  rastcombo <- rastcombo[!is.na(get(burn_value)), 
 				c(xy.uniqueid, burn_value, xycols), with=FALSE]
  rastcombo[, (xycols) := lapply(.SD, function(x) x / 100000), .SDcols=xycols]


  ## Merge lut values if exists
  if (!is.null(lut)) {
    rastcombo <- merge(rastcombo, lut, by.x=burn_value, by.y="VALUE")
    rastcombo[, (burn_value) := NULL]
    setnames(rastcombo, "BYVAR", bnd.att)
    byvalues <- sort(unique(rastcombo[[bnd.att]]))
    message("looping through ", length(byvalues), " ", tolower(bnd.att), "s")
  } else {
    byvalues <- sort(unique(rastcombo$VALUE))
    message("looping through ", length(byvalues), " values")
  }


  ## For efficiency, lets subset and loop thru unique values
  for (i in 1:length(byvalues)) {

    byval <- byvalues[i]
    message("getting data for: ", byval)

    #########################################################################
    ## Make spatial point of pixelLon/pixelLat coordinates
    #########################################################################
    message("creating spatial points...")

    outsp_layer <- paste0("rastcombo_", byval)
    xyplt <- spMakeSpatialPoints(rastcombo[rastcombo[[bnd.att]] == byval, ], 
                      xy.uniqueid = xy.uniqueid,
                      xvar = "pixelLon",
                      yvar = "pixelLat",
                      xy.crs = 4269,
                      exportsp = exportsp,
                      addxy = returnxy,
                      savedata_opts = list(outfolder = outfolder,
                                           out_layer = outsp_layer))
    #dim(xyplt)
    nbrplots <- nrow(xyplt)

    #########################################################################
    ## Extract attribute values from polyvlst
    #########################################################################
    message("extracting polygon data...")

    if (!is.null(polyvlst)) {
      xyext <- tryCatch(
                 spExtractPoly(xyplt = xyplt, 
                               polyvlst = polyvlst, 
                               polyvarlst = polyvarnmlst,
                               xy.uniqueid = xy.uniqueid, 
                               keepNA = FALSE),
     	          error=function(e) {
			    message(e, "\n")
			return(NULL) })
      if (is.null(xyext)) {
        message("no points returned")
      } else {
        spxy <- xyext$spxyext

        if (nrow(spxy) < nbrplots) {
          lessplots <- nrow(spxy) - nbrplots
          message( "poly extraction resulted in ", lessplots, " less plots ")
        }
      }      
      #head(spxy)
      #dim(spxy)
    } else {
      spxy <- xyplt
    }

  
    #########################################################################
    ## Extract raster data
    #########################################################################
    message("extracting auxiliary data...")

    if (!is.null(rastlst)) {
      auxext <- tryCatch(
                  spExtractRast(xyplt = spxy, 
                                xy.uniqueid = xy.uniqueid,
                                rastfolder = rastfolder,
                                rastlst = rastlst,
                                var.name = rastlst.name, 
                                keepNA = FALSE,
                                ncores = ncores,
                                savedata = FALSE),
     	          error=function(e) {
			    message(e, "\n")
			return(NULL) })
      spxy <- auxext$sppltext

      if (nrow(spxy) < nbrplots) {
        lessplots <- nrow(spxy) - nbrplots
        message( "raster extraction resulted in ", lessplots, " less plots ")
      }      
    }

    #########################################################################
    ## Export data 
    #########################################################################
    out_layer <- paste0("pixeldat_", byval)
    if (savedata) {
      datExportData(sf::st_drop_geometry(spxy), 
            savedata_opts = list(outfolder = outfolder,
                                 out_fmt = out_fmt,
                                 out_dsn = out_dsn,
                                 out_layer = out_layer,
                                 outfn.date = outfn.date,
                                 overwrite_layer = overwrite_layer))
    }

    ## Append filename to fnlst
    fnlst[byval] <- file.path(outfolder, paste0(out_layer, ".csv"))

  }

  return(fnlst)
}

