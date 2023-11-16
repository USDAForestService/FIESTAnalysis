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
#' @param bnd.byvar String. Optional name in bnd to get pixel data by.
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
						   bnd.byvar = NULL,
                           polyvlst = NULL,
                           polyvarnmlst = NULL,						   
                           rastfolder = NULL, 
                           rastlst = NULL, 
                           rastlst.name = NULL, 
                           returnxy = FALSE, 
                           exportsp = FALSE,
                           savedata_opts = NULL, 
                           ncores = 1) {


  ## Set global variables
  gui <- FALSE
  fnlst <- list()
  lut <- NULL
  savedata <- TRUE
  lonlat <- FALSE
  savetmp <- TRUE

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

  ## Get crs of reference raster
  ref.rast.crs <- rasterInfo(ref.rast)$crs
  ref.rast.nodata <- rasterInfo(ref.rast)$nodata_value
  ref.rast.datatype <- rasterInfo(ref.rast)$datatype

 
  ####################################################################
  ## Get boundary info
  ## If a boundary is input, clip the ref raster to the boundary mask
  ####################################################################

  ## Import boundary
  bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, caption="boundary")
  if (!is.null(bndx)) {
    bndx <- datFilter(bndx, xfilter=bnd.filter, stopifnull=TRUE)$xf

    ## Compare crs of reference raster and reproject if different
    bndx <- crsCompare(bndx, ref.rast.crs)$x
	
	## Check byvar - for looping through and saving by...
    byvar <- pcheck.varchar(var2check = bnd.byvar, 
                            varnm = "bnd.byvar",
                            checklst = names(bndx), gui=gui, 
                            caption = "By Variable", 
                            stopifnull = FALSE)

    if (is.null(byvar)) {
	  bndx$BYVAR <- 1
	  byvar <- "BYVAR"
	}
	byvalues <- sort(unique(bndx[[byvar]]))


    ## Check bnd.att -  for getting pixels within each...
	if (!all(bnd.att %in% names(bndx))) {
	  bnd.att.miss <- bnd.att[!bnd.att %in% names(bndx)] 
	  message("bnd.att not in bnd: ", toString(bnd.att.miss))
	}

#    if (!class(bndx[[bnd.att]]) %in% c("numeric", "integer")) { 
#      ## Create lookup table for character values
#      lut <- data.frame(BYVAR = sort(unique(bndx[[bnd.att]])), 
#                  VALUE = seq(1:length(unique(bndx[[bnd.att]]))))
#      bndx <- merge(bndx, lut, by.x=bnd.att, by.y="BYVAR")
#      burn_value <- "VALUE"
#      #bnd
#    } else {
#      burn_value <- bnd.att
#    }

    # ## Rasterize polygon to temporary file
    # rastfn <- rasterFromVectorExtent(src = bndx, 
                  # dstfile = tempfile("bndrast", fileext=".tif"),
                  # init = 0, dtName = "Int16", res = 90)

    # ## Reference raster
    # rasterizePolygons(src = bndx, 
                      # rasterfile = rastfn, 
                      # burn_value = burn_value)
					  
					  
	## Clip reference raster to polygon boundary 
    ###########################################################################
	if (savetmp) {
	  rastfn <- file.path(outfolder, "tmp_bndrast.tif")
	} else {
	  rastfn <- tempfile("bndrast", fileext="tif")
    }
	
	## Get nodata value of ref raster.
	## If the nodata values was not assigned (NA), then use a default value.
	if (is.na(ref.rast.nodata)) {
	  nodata <- getDefaultNodata(ref.rast.datatype)
	} else {
	  nodata <- ref.rast.nodata
	}
	  
    clipRaster(src = bndx, 
             srcfile = ref.rast, 
			 src_band = 1, 
			 dstfile = rastfn, 
             fmt = "GTiff", 
			 init = nodata,
			 dstnodata = nodata,
			 maskByPolygons = TRUE, 
             options = "COMPRESS=LZW")
 
   ## Create expressions to calculate rasters of X and Y values within bnd
   #exprX <- 'ifelse(A > 0, pixelX, -9999)' 
   #exprY <- 'ifelse(A > 0, pixelY, -9999)' 
   
   ## If raster value != the nodata value, keep the center pixel X or Y value, else -9999
   exprX <- 'ifelse(A != nodata, pixelX, nodata)' 
   exprY <- 'ifelse(A != nodata, pixelY, nodata)' 

  } else {
    ## Create expressions to calculate rasters of X and Y across raster
    exprX <- 'pixelX' 
    exprY <- 'pixelY' 
  }
  
  if (savetmp) {
    pixelXfn = file.path(outfolder, "tmp_pixelX.tif")
    pixelYfn = file.path(outfolder, "tmp_pixelY.tif")
  } else {
    pixelXfn <- tempfile("pixelX", fileext=".tif")
    pixelYfn <- tempfile("pixelY", fileext=".tif")
  }
  
  
  ####################################################################
  ## Get pixel XY data using exprX/exprY above.
  ## If lonlat values are desired, use pixelLon/pixelLat and multiply
  ## by a large number to integerize, retaining significant digits.
  ####################################################################
  
  if (lonlat) {
    out.crs = 4269
    # Create temporary raster of pixel longitudes:
    gdalraster::calc(expr = "pixelLon * 1000000000", 
	                 rasterfiles = rastfn,
					 dstfile = pixelXfn,
					 dtName = "Int32", 
					 options = c("COMPRESS=DEFLATE"),
                     nodata_value = -9999, 
					 usePixelLonLat = TRUE)

    # Create temporary raster of pixel latitudes:
    gdalraster::calc(expr = "pixelLat * 1000000000", 
	                 rasterfiles = rastfn,
					 dstfile = pixelYfn,
					 dtName = "Int32", 
					 options = c("COMPRESS=DEFLATE"),
					 nodata_value = -9999, 
					 usePixelLonLat = TRUE)
  } else {
    
    ## Create a pixel X raster in the coordinate system the raster is in
    ###########################################################################
    gdalraster::calc(expr = exprX, 
                   rasterfiles = rastfn,
                   dstfile = pixelXfn,
				   dtName = "Int32", 
                   options = c("COMPRESS=DEFLATE"),
                   nodata_value = -9999, 
                   write_mode = "overwrite")


    ## Create a pixel Y raster in the coordinate system the raster is in
    ###########################################################################
    gdalraster::calc(expr = exprY, 
                   rasterfiles = rastfn,
                   dstfile = pixelYfn,
				   dtName = "Int32", 
                   options = c("COMPRESS=DEFLATE"),
                   nodata_value = -9999, 
                   write_mode = "overwrite")
  }
  
  
  ####################################################################
  ## Get combination raster of pixel X/Y values
  ## Note: rasterCombine only allows Int* data types
  ####################################################################

  combofn <- file.path(outfolder, "combo.tif")
  rastcombo <- gdalraster::combine(c(pixelXfn, pixelYfn),
                             var.names = c("pixelX", "pixelY"),
							 fmt = "GTiff",
							 dstfile = combofn,
							 options = c("COMPRESS = LZW"))



  #########################################################################
  ## Make spatial point of pixelX/pixelY coordinates
  #########################################################################
  message("creating spatial points...")

  xy.uniqueid <- "cmbid" 
  outsp_layer <- paste0("rastcombo")
  # exportsp <- FALSE
  xyplt <- spMakeSpatialPoints(rastcombo, 
                      xy.uniqueid = xy.uniqueid,
                      xvar = "pixelX",
                      yvar = "pixelY",
                      xy.crs = ref.rast.crs,
                      exportsp = exportsp,
                      addxy = returnxy,
                      savedata_opts = list(outfolder = outfolder,
                                           out_layer = outsp_layer))

  ## Extract XY pixel values from bndx
  xyext <- spExtractPoly(xyplt = xyplt, 
                         polyvlst = bndx, 
                         polyvarlst = unique(c(bnd.att, byvar)),
                         xy.uniqueid = xy.uniqueid, 
                         keepNA = FALSE)$spxyext
						 
						 
						 
  # ## Note: rasterCombine only allows Int* data types
  # rastcombo <- gdalraster::combine(c(rastfn, pixelXfn, pixelYfn),
                             # var.names = c(burn_value, "pixelX", "pixelY"))

  # ## Convert Lon/Lat values back to original (numeric) values
  # xy.uniqueid <- "cmbid"
  # xycols <- c("pixelLon", "pixelLat")
  # rastcombo <- setDT(rastcombo)
  # rastcombo <- rastcombo[!is.na(get(burn_value)), 
 				# c(xy.uniqueid, burn_value, xycols), with=FALSE]
  # rastcombo[, (xycols) := lapply(.SD, function(x) x / 10000000), .SDcols=xycols]


#  ## Merge lut values if exists
#  if (!is.null(lut)) {
#    rastcombo <- merge(rastcombo, lut, by.x=burn_value, by.y="VALUE")
#    rastcombo[, (burn_value) := NULL]
#    setnames(rastcombo, "BYVAR", bnd.att)
#    byvalues <- sort(unique(rastcombo[[bnd.att]]))
#    message("looping through ", length(byvalues), " ", tolower(bnd.att), " values")
#  } else {
#    byvalues <- sort(unique(rastcombo$VALUE))
#    message("looping through ", length(byvalues), " values")
#  }


  ## For efficiency, lets subset and loop thru unique values
  for (i in 1:length(byvalues)) {

    byval <- byvalues[i]
    if (length(byvalues) > 1) {
      message("getting data for: ", byval)
    }
	
    #########################################################################
    ## Make spatial point of pixelLon/pixelLat coordinates
    #########################################################################
    message("creating spatial points...")

    xyplt <- xyext[xyext[[byvar]] == byval, ]
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
        xyplt <- xyext$spxyext

        if (nrow(xyplt) < nbrplots) {
          lessplots <- nrow(xyplt) - nbrplots
          message( "poly extraction resulted in ", lessplots, " less plots ")
        }
      }      
      #head(xyplt)
      #dim(xyplt)
    }

  
    #########################################################################
    ## Extract raster data
    #########################################################################
    message("extracting auxiliary data...")

    if (!is.null(rastlst)) {
      auxext <- tryCatch(
                  spExtractRast(xyplt = xyplt, 
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
      xyplt <- auxext$sppltext

      if (nrow(xyplt) < nbrplots) {
        lessplots <- nrow(xyplt) - nbrplots
        message( "raster extraction resulted in ", lessplots, " less plots ")
      }      
    }

    #########################################################################
    ## Export data 
    #########################################################################
	if (length(byvalues) == 1) {
	  out_layer <- "pixeldat"
	} else {
      out_layer <- paste0("pixeldat_", byval)
	}
	xyplt$BYVAR <- NULL
	
    if (savedata) {
      datExportData(sf::st_drop_geometry(xyplt), 
            savedata_opts = list(outfolder = outfolder,
                                 out_fmt = out_fmt,
                                 out_dsn = out_dsn,
                                 out_layer = out_layer,
                                 outfn.date = outfn.date,
                                 overwrite_layer = overwrite_layer))
	  saveRDS(ref.rast.crs, file.path(outfolder, "refrast_crs.rds"))
    }

    ## Append filename to fnlst
    fnlst[[byval]] <- file.path(outfolder, paste0(out_layer, ".csv"))

  }

  if (length(byvalues) > 1) {
    return(fnlst)
  } else {
    return(fnlst[[1]])
  }
}

