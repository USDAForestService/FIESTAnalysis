#' Data - Get list of population data for estimation.
#'
#' Wrapper to get population data for estimation.
#'
#'
#' @param SAdatalst sf R object. List object output from FIESTAnalysis::anGetData_list().
#' @param smallbndlst sf R object. List object of smallbnds associated with SAdatalst.
#' @param smallbnd.domain String. Name of unique identifier for smallbnd.
#' @param prednames String vector. One or more predictors to use for estimation. 
#' @param addxy Logical. If TRUE, adds X/Y attributes to pltassgn.
#' @param saveobj Logical. If TRUE, saves SApopdat object to outfolder.
#' @param objnm String. If savedata=TRUE, name of object to save. 
#' @param outfolder String. Name of outfolder. If NULL, saves to working directory.
#' @param overwrite Logical. If TRUE, overwrite object in outfolder.
#' @param outfn.pre String. If saveobj=TRUE, prefix for object name. 
#' @param outfn.date Logical. If TRUE, add current date to object name.
#' @return Data.
#' @note
#'
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
#' @export
anSApop_list <- function(SAdatalst, 
                         smallbndlst, 
                         smallbnd.domain,
                         largebnd.unique = NULL,
                         prednames = NULL, 
                         addxy = FALSE,
                         saveobj = FALSE,
                         objnm = "SApopdat",
                         outfolder = NULL, 
                         overwrite = FALSE, 
                         outfn.pre = NULL, 
                         outfn.date = FALSE) {

  ## Set global variables
  gui <- FALSE


  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(anSApop_list)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  
  ## Check parameter inputs
  ##########################################################################
  
  ## Check saveobj
  saveobj <- pcheck.logical(saveobj, varnm="saveobj",
                            title="Save SApopdat object?", 
                            first="YES", gui=gui, stopifnull=TRUE)
  if (saveobj && is.null(objnm)) {
    objnm <- "SApopdat"
  }
  
  if (saveobj) {
    outfolder <- pcheck.outfolder(outfolder)
    
    overwrite <- pcheck.logical(overwrite, varnm="overwrite",
                              title="Overwrite?", 
                              first="YES", gui=gui, stopifnull=TRUE)
    outfn.date <- pcheck.logical(outfn.date, varnm="outfn.date",
                                title="Add date?", 
                                first="NO", gui=gui, stopifnull=TRUE)
    
  } 
  
  ## Extract FIA data and model data
  ###########################################################################
  if (class(SAdatalst) != "list") {
    SAdatalst <- list(SAdatalst)
  }
  if (class(smallbndlst) != "list") {
    smallbndlst <- list(smallbndlst)
  }

  nbrlst <- length(SAdatalst)
  SApoplst <- vector(mode='list', length=nbrlst)
  names(SApoplst) <- names(SAdatalst)
  if (is.null(names(SApoplst))) {
    paste0("SAdom", seq(1:nbrlst))
  }

  for (i in 1:nbrlst) {
    message("getting population data for ", i, " out of ", nbrlst, " modeling domains...\n")

    SAdata <- SAdatalst[[i]]
    smallbnd <- smallbndlst[[i]]
    nm <- names(SApoplst)[[i]]
    
    pop <- modSApop(SAdata = SAdata, 
                    smallbnd = smallbnd, 
                    smallbnd.domain = smallbnd.domain,
                    largebnd.unique = largebnd.unique,
                    prednames = prednames,
                    addxy = addxy)
    if (is.null(pop)) {
      message("no data extracted for: ", SAdatanm)
      SApoplst[[nm]] <- NA
    } else {
      SApoplst[[nm]] <- pop
    }
  }
  
  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, ext="rda", outfolder=outfolder, 
                      overwrite=overwrite, outfn.pre=outfn.pre, outfn.date=outfn.date)
    save(SApoplst, file=objfn)
    message("saving object to: ", objfn)
  } 
  

  return(SApoplst)
}
