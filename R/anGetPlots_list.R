#' Data - Get list of XY data by bnd.
#'
#' Wrapper to get XY data.
#'
#'
#' @param auxdatlst sf R object. List output from FIESTAnalysis::anGetAuxiliary_list function.
#' @param saveobj Logical. If TRUE, saves xydatlst object to outfolder.
#' @param objnm String. If savedata=TRUE, name of object to save. 
#' @param outfolder String. Name of outfolder. If NULL, saves to working directory.
#' @param overwrite Logical. If TRUE, overwrite object in outfolder.
#' @param outfn.pre String. If saveobj=TRUE, prefix for object name. 
#' @param outfn.date Logical. If TRUE, add current date to object name.
#' @param ... Parameter for FIESTA::spGetPlots function. 
#' @return Data.
#'
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anGetPlots_list <- function(auxdatlst, 
                           saveobj = FALSE,
                           objnm = "pltdatlst",
						   outfolder = NULL, 
                           overwrite = FALSE, 
                           outfn.pre = NULL, 
                           outfn.date = FALSE,
						   ...) {

  ## Set global variables
  gui <- FALSE


  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(anGetPlots_list)), names(formals(spGetPlots)),
                           names(formals(DBgetPlots)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  
  ## Check parameter inputs
  ##########################################################################
  
  ## Check saveobj
  saveobj <- pcheck.logical(saveobj, varnm="saveobj",
                            title="Save pltdat object?", 
                            first="YES", gui=gui, stopifnull=TRUE)
  if (saveobj && is.null(objnm)) {
    objnm <- "pltdatlst"
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
  if (!is.list(auxdatlst)) {
    stop("auxdatlst must be a list object output from anGetAuxiliary_list")
  }

  nbrlst <- length(auxdatlst)
  pltdatlst <- vector(mode='list', length=nbrlst)
  names(pltdatlst) <- names(auxdatlst)

  for (i in 1:nbrlst) {
    message("getting plot data for ", i, " out of ", nbrlst, " boundaries...\n")

    auxdat <- auxdatlst[[i]]
    nm <- names(auxdatlst)[[i]]
	
	## Check xydat for necessary data
	list.items <- c("pltassgn", "unitarea", "unitvar")
    auxdat <- pcheck.object(auxdat, "auxdat", list.items=list.items)

	pltassgn <- auxdat$pltassgn
 
    pltdat <- spGetPlots(pltids = pltassgn, ...)
    if (is.null(pltdat)) {
      message("no data extracted for: ", auxdatnm)
      pltdatlst[[nm]] <- NA
    } else {
      pltdatlst[[nm]] <- pltdat
    }
	
	pltdatlst[[nm]] <- append(pltdatlst[[nm]], auxdat)
  }
  
  if (saveobj) {
    objfn <- getoutfn(outfn = objnm, 
	                  ext = "rds", 
					  outfolder = outfolder, 
                      overwrite = overwrite, 
					  outfn.pre = outfn.pre, 
					  outfn.date = outfn.date)
    save(pltdatlst, file=objfn)
    message("saving object to: ", objfn)
  } 
  

  return(pltdatlst)
}
