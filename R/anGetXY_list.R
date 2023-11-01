#' Data - Get list of XY data by bnd.
#'
#' Wrapper to get XY data.
#'
#'
#' @param bndlst sf R object. List of bnd sf objects.
#' @param saveobj Logical. If TRUE, saves xydatlst object to outfolder.
#' @param objnm String. If savedata=TRUE, name of object to save. 
#' @param outfolder String. Name of outfolder. If NULL, saves to working directory.
#' @param overwrite Logical. If TRUE, overwrite object in outfolder.
#' @param outfn.pre String. If saveobj=TRUE, prefix for object name. 
#' @param outfn.date Logical. If TRUE, add current date to object name.
#' @param ... Parameter for FIESTA::spGetXY function. 
#' @return Data.
#' @note
#'
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anGetXY_list <- function(bndlst, 
                         saveobj = FALSE,
                         objnm = "xydatlst",
						 outfolder = NULL, 
                         overwrite = FALSE, 
                         outfn.pre = NULL, 
                         outfn.date = FALSE,
						 ...) {

  ## Set global variables
  gui <- FALSE


  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(anGetXY_list)), names(formals(spGetXY)))
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
  if (class(bndlst) != "list") {
    bndlst <- list(bndlst)
  }
  nbrlst <- length(xydatlst) 
  xydatlst <- vector(mode='list', length=nbrlst)
  names(xydatlst) <- names(bndlst)

  for (i in 1:nbrlst) {
    message("getting XY data for ", i, " out of ", nbrlst, " boundaries...\n")

    bnd <- bndlst[[i]]
    nm <- names(bndlst)[[i]]
    if (is.null(nm)) {
      nm <- paste0("bnd", i)
    }
 
    xydat <- spGetXY(bnd = bnd, ...)
    if (is.null(xydat)) {
      message("no data extracted for: ", bndnm)
      xydatlst[[nm]] <- NA
    } else {
      xydatlst[[nm]] <- xydat
    }
  }
  
  if (saveobj) {
    objfn <- getoutfn(outfn = objnm, 
	                  ext = "rds", 
					  outfolder = outfolder, 
                      overwrite = overwrite, 
					  outfn.pre = outfn.pre, 
					  outfn.date = outfn.date)
    save(xydatlst, file=objfn)
    message("saving object to: ", objfn)
  } 
  

  return(xydatlst)
}
