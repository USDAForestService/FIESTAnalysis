#' Data - Get list of XY data by bnd.
#'
#' Wrapper to get XY data.
#'
#'
#' @param xydatlst sf R object. List output from FIESTAnalysis::anGetXY_list function.
#' @param saveobj Logical. If TRUE, saves xydatlst object to outfolder.
#' @param objnm String. If savedata=TRUE, name of object to save. 
#' @param outfolder String. Name of outfolder. If NULL, saves to working directory.
#' @param overwrite Logical. If TRUE, overwrite object in outfolder.
#' @param outfn.pre String. If saveobj=TRUE, prefix for object name. 
#' @param outfn.date Logical. If TRUE, add current date to object name.
#' @param ... Parameter for FIESTA::spGetAuxiliary function. 
#' @return Data.
#'
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anGetAuxiliary_list <- function(xydatlst, 
                           saveobj = FALSE,
                           objnm = "auxdatlst",
						   outfolder = NULL, 
                           overwrite = FALSE, 
                           outfn.pre = NULL, 
                           outfn.date = FALSE,
						   ...) {

  ## Set global variables
  gui <- FALSE


  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- unique(c(names(formals(anGetAuxiliary_list)), names(formals(spGetAuxiliary))))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  
  ## Check parameter inputs
  ##########################################################################
 
  ## Check saveobj
  saveobj <- pcheck.logical(saveobj, varnm="saveobj",
                            title="Save auxdatlst object?", 
                            first="YES", gui=gui, stopifnull=TRUE)
  if (saveobj && is.null(objnm)) {
    objnm <- "auxdatlst"
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
  if (class(xydatlst) != "list") {
    stop("xydatlst must be a list object output from anGetXY_list")
  }
  
  nbrlst <- length(xydatlst) 
  auxdatlst <- vector(mode='list', length=nbrlst)
  names(auxdatlst) <- names(xydatlst)

  for (i in 1:nbrlst) {
    message("getting auxiliary data for ", i, " out of ", nbrlst, " boundaries...\n")

    xydat <- xydatlst[[i]]
    nm <- names(xydatlst)[[i]]
	
	## Check xydat for necessary data
	list.items <- c("spxy", "bndx")
    xydat <- pcheck.object(xydat, "xydat", list.items=list.items)

	spxy <- xydat$spxy
	bndx <- xydat$bndx

    ## Run FIESTA::spGetAuxiliary function
    auxdat <- spGetAuxiliary(xyplt = spxy, unit_layer = bndx, ...)

    if (is.null(auxdat)) {
      message("no data extracted for: ", xydatnm)
      auxdatlst[[nm]] <- append(auxdatlst[[nm]], NA)
    } else {
      auxdatlst[[nm]] <- append(auxdatlst[[nm]], auxdat)
    }
  }
  
  objfn <- getoutfn(outfn = objnm, 
	                  ext = "rds", 
					  outfolder = outfolder, 
                      overwrite = overwrite, 
					  outfn.pre = outfn.pre, 
					  outfn.date = outfn.date)
  save(auxdatlst, file=objfn)
  message("saving object to: ", objfn) 

  return(auxdatlst)
}
