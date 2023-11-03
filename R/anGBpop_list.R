#' Data - Get list of population data for estimation.
#'
#' Wrapper to get population data for estimation.
#'
#'
#' @param datalst sf R object. List object output from FIESTAnalysis::anGetData_list().
#' @param strvar String. Name of strata variable to use for GB estimates. 
#' @param saveobj Logical. If TRUE, saves SApopdat object to outfolder.
#' @param objnm String. If savedata=TRUE, name of object to save. 
#' @param outfolder String. Name of outfolder. If NULL, saves to working directory.
#' @param overwrite Logical. If TRUE, overwrite object in outfolder.
#' @param outfn.pre String. If saveobj=TRUE, prefix for object name. 
#' @param outfn.date Logical. If TRUE, add current date to object name.
#' @param ... Additional parameters to FIESTA::modGBpop
#' @return Data.
#'
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anGBpop_list <- function(datalst, 
                         strvar = NULL, 
                         saveobj = FALSE,
                         objnm = "GBpopdat",
                         outfolder = NULL, 
                         overwrite = FALSE, 
                         outfn.pre = NULL, 
                         outfn.date = FALSE,
						 ...) {

  ## Set global variables
  gui <- FALSE


  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(anGBpop_list)), names(formals(modGBpop)))
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
  if (class(datalst) != "list") {
    datalst <- list(datalst)
  }

  nbrlst <- length(datalst)
  GBpoplst <- vector(mode='list', length=nbrlst)
  names(GBpoplst) <- names(datalst)
  if (is.null(names(GBpoplst))) {
    paste0("bnd", seq(1:nbrlst))
  }

  for (i in 1:length(datalst)) {
    message("getting population data for ", i, " out of ", nbrlst, " boundaries...\n")

    GBdata <- datalst[[i]]
    nm <- names(GBpoplst)[[i]]

    pop <- modGBpop(GBdata = GBdata, 
                    strvar = strvar, 
					...)
    if (is.null(pop)) {
      message("no data extracted for: ", SAdatanm)
      GBpoplst[[nm]] <- NA
    } else {
      GBpoplst[[nm]] <- pop
    }
  }
  
  if (saveobj) {
    objfn <- getoutfn(outfn=objnm, ext="rda", outfolder=outfolder, 
                      overwrite=overwrite, outfn.pre=outfn.pre, outfn.date=outfn.date)
    save(SApoplst, file=objfn)
    message("saving object to: ", objfn)
  } 
  

  return(GBpoplst)
}
