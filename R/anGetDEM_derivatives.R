#' Spatial analysis - Creates derivative rasters from a digital elevation model (DEM).
#' 
#' Creates one or more derivative rasters from digital elevation model (DEM)
#' using gdalUtils::gdaldem(). The output will have the same extent, cell size, 
#' and coordinate reference system as the input DEM. See below for list of 
#' derivatives to choose from.
#' 
#' DEM Derivatives:\cr 
#' \tabular{ll}{ 
#' \tab slope, in degrees\cr 
#' \tab aspect, in degrees\cr 
#' \tab roughness\cr
#' \tab TRI (Terrain Ruggedness Index)\cr
#' \tab TPI (Terrain Position Index)\cr
#' \tab northness - aspect northness\cr
#' \tab eastness - aspect eastness\cr }
#' 
#' @param demfn String or Raster. File name of DEM.
#' @param derivatives String vector. List of derivatives to calculate from
#' DEM ('slope', 'aspect', 'roughness', 'TRI', 'TPI').
#' @param aspect.derivatives Logical. If TRUE, calculate northing and 
#' easting derivatives.
#' @param aspect.keep Logical. If aspect.derivatives = TRUE, if TRUE, 
#' returns aspect in list.
#' @param compress String. An optional compression type ('LZW', "DEFLATE',
#' "PACKBITS').
#' @param BigTIFF Logical. If TRUE, adds BIGTIFF=YES to export (for files > 4GB).
#' @param outfolder String. If exportsp=TRUE, name of output folder. If NULL,
#' the working directory is used.
#' @param outfn.pre String. Name of output prefix for raster derivative(s). 
#' If NULL, default is 'dem'.
#' @param outext String. Name of raster extension (fmt). If NULL, uses
#' extension from outfn or rastfn.
#' @param overwrite Logical. If TRUE, overwrites raster derivatives.
#' @return \item{rasterlst}{ String list. Full path name of raster files. }
#' 
#' @author Tracey S. Frescino, Chris Toney
#' @keywords spatial
#' 
#' @export anGetDEM_derivatives
anGetDEM_derivatives <- function(demfn, 
                              derivatives = c("slope", "aspect", "roughness", "TRI", "TPI"), 
                              aspect.derivatives = TRUE,
                              aspect.keep = FALSE,
                              compress = "DEFLATE", 
                              BigTIFF = FALSE,
                              outfolder = NULL, 
                              outfn.pre = "dem", 
                              outext = "tif", 
                              overwrite = FALSE){

  ##################################################################################
  ## DESCRIPTION: reproject raster. If crs.new is not defined, uses crs.default. 	
  ## Default projection: NAD83 - Conus Albers (EPSG:5070)
  ## +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96,
  ##		+x_0=0 +y_0=0", "+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  ## compress ('LZW', 'DEFLATE', 'PACKBITS')
  ##################################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Set global variables
  of=dstnodata=co=addOptions <- NULL
  gui <- FALSE
  returnlst <- list()

  drivers <- data.frame(
	fmt = c("raster", "ascii", "SAGA", "IDRISI", "CDF", "GTiff", "ENVI", 
		"EHdr", "HFA", "VRT"),
	DefaultExt = c("grd", "asc", "sdat", "rst", "nc", "tif", "envi", 
		"bil", "img", "vrt"),
	stringsAsFactors=FALSE
  )	
  
  ## Create lists
  derivativelst <- c("slope", "aspect", "roughness", "TRI", "TPI")
  compresslst <- c("LZW", "PACKBITS", "DEFLATE")
  

  ## If gui.. set variables to NULL
  if (gui) savedata <- NULL

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## Verify rasters 
  demfile <- getrastlst.rgdal(demfn, gui=gui)
  deminfo <- rasterInfo(demfile)

  ## Get names of raster 
  demnm <- basename.NoExt(demfile) 
 
  ## Check derivatives
  derivatives <- pcheck.varchar(var2check=derivatives, 
                                varnm="derivatives", gui=gui, 
                                checklst=derivativelst, 
                                caption="DEM derivatives?", 
                                multiple=TRUE)
  

  ## Check aspect.derivatives
  aspect.derivatives <- pcheck.logical(aspect.derivatives, 
                              varnm="aspect.derivatives", 
                              title="Aspect derivatives?", 
                              first="NO", gui=gui)
  if (aspect.derivatives) {
    ## Check aspect.keep
    aspect.keep <- pcheck.logical(aspect.keep, 
                                  varnm="aspect.keep", 
                                  title="Keep aspect layer?", 
                                  first="NO", gui=gui)
    
    if (!"aspect" %in% derivatives) {
      message("aspect not in derivatives... adding to list")
      derivatives <- c(derivatives, "aspect")
    }
  }
  
  
  ## Get projection of raster
  dem.crs <- deminfo$crs
  if (is.na(dem.crs) || is.null(dem.crs) || dem.crs == "") {
    if (is.null(dem.crs)) {
      stop(demnm, " does not have defined projection")
    } 
  }

  ## Check outfolder
  outfolder <- pcheck.outfolder(outfolder)
  
  ## Check outfn.pre
  if (is.null(outfn.pre)) {
    outfn.pre <- "dem"
  }

  # ## Check outext and outfilenm
  # if (is.null(outext)) {
  #   outext <- getext(outfn)
  #   if (is.na(outext) || outext == "") {
  #     outext <- "tif"
  #   }
  # }

  outext.tmp <- unlist(strsplit(outext, "\\."))
  if (length(outext.tmp) > 1) {
    outext <- outext.tmp[length(outext.tmp)]   
    if (!outext %in% drivers[["DefaultExt"]]) stop("outext is invalid") 
  }
  
  ## Get output raster format
  of <- drivers[drivers$DefaultExt == outext, "fmt"]
  
  ## Check compression
  compress <- pcheck.varchar(var2check=compress, 
                             varnm="compress", gui=gui,
                             checklst=compresslst, caption="Compress output?")
  if (!is.null(compress)) {
    co <- paste0("COMPRESS=", compress)
  }
  
  if (outext == "tif" && !is.null(co)) {
    ## Check BigTIFF
    BigTIFF <- pcheck.logical(BigTIFF, 
                              varnm="BigTIFF", 
                              title="BigTIFF compression?", 
                              first="NO", gui=gui)
    co <- c(co, "BIGTIFF=YES")
  }
  
  
  ##################################################################
  ## DO WORK
  ##################################################################
  for (d in derivatives) {
    message("creating ", d, " derivative from ", demnm, "...")
    
    outfilenm <- getoutfn(d, 
                          outfolder=outfolder, 
                          outfn.pre=outfn.pre, 
                          ext=outext,
                          overwrite=overwrite)
    message("saving ", d, " to: ", outfilenm)
    
    
    # Create derivative
    gdalUtils::gdaldem(d, 
                       input_dem=demfn, 
                       output=outfilenm, 
                       of=of, 
                       co=co)
    if (!file.exists(outfilenm)) {
      stop("an error occurred when creating derivative ", d)
    }
    if (is.na(rasterInfo(outfilenm)$crs)) {
      stop("derivative has undefined crs... need to update rgdal package")
    }

    if (d != "aspect" || aspect.keep) {
      returnlst[[d]] <- outfilenm
    }
    
    if (d == "aspect" && aspect.derivatives) {
      ## Define data type and NODATA value
      dtName <- "Int16"
      nodata.val <- gdalraster::DEFAULT_NODATA[[dtName]]
      
      ## Aspect northness
      aspnfn <- getoutfn("aspect_northness", 
                            outfolder=outfolder, 
                            outfn.pre=outfn.pre, 
                            ext=outext,
                            overwrite=overwrite)
      message("saving aspect northing to: ", outfilenm)
      returnlst[["aspect_northing"]] <- aspnfn
      
      
      gdalraster::calc(expr = "northness(A)*100", 
                 rasterfiles = outfilenm, 
                 dstfile = aspnfn, 
                 fmt = of, 
                 dtName = "Int16",
                 nodata_value = nodata.val,
                 setRasterNodataValue = TRUE,
                 options=co)
      
      ## Aspect eastness
      aspefn <- getoutfn("aspect_eastness", 
                         outfolder=outfolder, 
                         outfn.pre=outfn.pre, 
                         ext=outext,
                         overwrite=overwrite)
      message("saving aspect easting to: ", outfilenm)
      returnlst[["aspect_easting"]] <- aspefn
      
      gdalraster::calc(expr = "eastness(A)*100", 
                 rasterfiles = outfilenm, 
                 dstfile = aspefn, 
                 fmt = of, 
                 dtName = "Int16",
                 nodata_value = nodata.val,
                 setRasterNodataValue = TRUE,
                 options = co)
    }
  }
  return(returnlst)
}
