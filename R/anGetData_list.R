#' Data - Get list of plot data and auxiliary data for estimation.
#'
#' Wrapper to get plot data and auxiliary data for estimation.
#'
#'
#' @param bndlst sf R object. List object of boundary extent(s).
#' @param splitbnd Logical. If TRUE, splits list into multiple lists.
#' @param splitby numeric. If splitbnd=TRUE, the number of domains to split by.
#' @param savelistobj Logical. If TRUE, saves listobjects to listoutfolder. 
#' @param listoutfolder NULL. Folder to save list object. If NULL, saves 
#' to working directory.
#' @param listobjnm String. Name of saved object. 
#' @param ...  Other parameters for anGetData().
#' @return Data.
#' 
#'
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anGetData_list <- function(bndlst, 
                           splitbnd = TRUE, 
                           splitby = 100, 
                           savelistobj = TRUE,
                           listoutfolder = NULL, 
                           listobjnm = "datalst",
                           ...) {

  ## Set global variables
  gui <- FALSE

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- unique(c(names(formals(anGetData_list)),
		names(formals(anGetData)),
		names(formals(spGetPlots)),
		names(formals(DBgetPlots)), 
           names(formals(spGetAuxiliary))))

  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
 
  ## Check split
  splitbnd <- pcheck.logical(splitbnd, varnm="splitbnd",
		title="Split bnd list?", first="NO", gui=gui)

  ## Check splitby
  if (splitbnd) {
    if (is.null(splitby)) {
      message("splitby is null... using default")
    } else if (!is.vector(splitby) || length(splitby) != 1 || !is.numeric(splitby)) {
      message("must be numeric vector of length 1")
    } 
  }


  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  
  ## Check saveobj
  savelistobj <- pcheck.logical(savelistobj, varnm="savelistobj",
		title="Save list data objects?", first="YES", gui=gui)
  
  ## Check splitbnd
  if (splitbnd && !savelistobj) {
    message("consider saving objects (i.e., saveobj=TRUE)")
  }

  ## Check output
  ########################################################
  if (savelistobj) {
    listoutfolder <- pcheck.outfolder(listoutfolder)

    if (is.null(listobjnm)) {
      listobjnm <- "datalst"
    }
    if (!is.na(getext(listobjnm))) {
      listobjnm <- basename.NoExt(listobjnm)
    }
  }   

  ## Extract FIA data and model data
  ###########################################################################
  datalst <- list()

  if (class(bndlst) != "list") {
    bndlst <- list(bndlst)
  }
  
  nbrdom <- length(bndlst)

  if (splitbnd) {
    if (is.null(splitby)) {
      splitby <- 100
    }
    if (nbrdom < splitby) {
      message("number of domains is less than splitby: ", splitby)
      splitbnd <- FALSE
    }
  }  
 
  ## Split bndlst
  SAbndlist <- list()
  if (splitbnd) {
    lastn <- 0
    for (i in 1:(ceiling(length(bndlst) / splitby))) {
      n <- lastn + 1
      if (i == ceiling(length(bndlst) / splitby)) {
        lastn <- length(bndlst)
      } else {
        lastn <- i * splitby
      }
      SAbndlist[[paste0("SAbnd", i)]] <- bndlst[n:lastn]
      n <- lastn
    }
  } else {
    SAbndlist[[paste0("SAbnd", 1)]] <- bndlst
  }
  nbrbndlst <- length(SAbndlist)
  if (nbrbndlst > 1) {
    message("bndlst split into ", nbrbndlst, " domains")
  }

  for (j in 1:length(SAbndlist)) {
    SAbndlst <- SAbndlist[[j]]
    nbrSAbnd <- length(SAbndlst)

    if (splitbnd) {
      SAdatalist <- list()
    }
    for (i in 1:length(SAbndlst)) {
      message("getting data for ", i, " out of ", nbrSAbnd, " modeling domains...\n")

      SAbnd <- SAbndlst[[i]]
      SAbndnm <- names(SAbndlst)[[i]]
      if (is.null(SAbndnm)) {
        SAbndnm <- paste0("plots", i)
      }
      dat <- anGetData(SAbnd, ...)                

      if (is.null(dat)) {
        message("no data extracted for: ", SAbndnm)
      }
      datalst[[SAbndnm]] <- dat

      if (splitbnd) {
        SAdatalist[[j]] <- datalst
      } 
      rm(dat)
      gc()
    }
    ## Save list object
    if (savelistobj) {
      if (splitbnd) {
        datalstnm <- file.path(listoutfolder, paste0(listobjnm, j, ".rds"))
      } else {
        datalstnm <- file.path(listoutfolder, paste0(listobjnm, ".rds"))
      }
      saveRDS(datalst, datalstnm)
    }
  }
  if (splitbnd) {
    return(SAdatalist) 
  } else {
    return(datalst)
  }
}
