#' ANALYSIS - Generate a report from ICE core tables for Green-Book custom
#' population.
#' 
#' Generates a report from a set of ICE core tables based on area of interest.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' ice.QAQC - 2 columns (QAQC_check, Result):\cr \tabular{ll}{ \tab
#' PC_self_check\cr \tab PC_cross_check\cr \tab LC1_2_self_check\cr \tab
#' LC1_2_cross_check\cr \tab LU1_2_self_check\cr \tab LU1_2_cross_check\cr }
#' 
#' @param rawfolder String. Full path name of rawdata folder with estimates.
#' @param AOInm String. Name of area of interest.
#' @param T1 String. Time 1 description (e.g., YYYY).
#' @param T2 String. Time 2 description (e.g., YYYY).
#' @param REMPER Number. Remeasurement period.
#' @param QAQC.self Numeric vector. Vector of percentages corresponding to 
#' self checks of: Change, LU, LC, Agent of Change, in that order. Optional.
#' @param QAQC.cross Numeric vector. Vector of percentages corresponding to 
#' cross checks of: Change, LU, LC, Agent of Change, in that order. Optional.
#' @param outfn.pre String. Short name of file name. If NULL, using AOInm.
#' @param photofn String. Path name to photo to add to report.
#' @param icepltfn String. Path name to ICE plot data.
#' @param outfolder String. The path of folder to output tables.
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anPBpopICE_report <- function(rawfolder, 
                              AOInm, 
                              T1, 
                              T2, 
                              REMPER = NULL,
                              QAQC.self = NULL,
                              QAQC.cross = NULL,
                              outfn.pre = NULL, 
                              photofn = NULL, 
                              icepltfn = NULL,
                              outfolder = NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.

  wkdir <- getwd()

  outfolder <- pcheck.outfolder(outfolder)
  #outfolder <- normalizePath(outfolder)
  reportfolder <- tempdir()

  ## Check QAQC data
  if (is.null(QAQC.self) || all(is.na(QAQC.self)) || 
			all(QAQC.self == "NA") || all(QAQC.self == "")) {
#  if (is.null(QAQC.self) || all(QAQC.self == "")) {
    #QAQC.self <- NULL
    QAQC.self <- "NA"
  } else {
    if (all(!is.vector(QAQC.self), !is.numeric(QAQC.self), length(QAQC.self) != 4)) {
      warning("QAQC.self must be a vector of 4 numbers")
      QAQC.self <- NULL
    } else if (sum(is.na(QAQC.self)) < 4) {
      QAQC.chk <- QAQC.self[!is.na(QAQC.self)]
      if (any(QAQC.chk < 0, QAQC.chk > 100)) {
        warning("all values of QAQC.self must be between 0 and 100")
      }     
    } else if (any(QAQC.self < 0, QAQC.self > 100)) {
      warning("all values of QAQC.self must be between 0 and 100")
      QAQC.self <- NULL
    }    
  }
  if (is.null(QAQC.cross) || all(is.na(QAQC.cross)) || 
		all(QAQC.cross == "NA") || all(QAQC.cross == "")) {
#  if (is.null(QAQC.cross) || all(QAQC.cross == "")) {
    #QAQC.cross <- NULL
    QAQC.cross <- "NA"
  } else {
    if (all(!is.vector(QAQC.cross), !is.numeric(QAQC.cross), length(QAQC.cross) != 4)) {
      warning("QAQC.cross must be a vector of 4 numbers")
      QAQC.cross <- NULL
    } else if (sum(is.na(QAQC.cross)) < 4) {
      QAQC.chk <- QAQC.cross[!is.na(QAQC.cross)]
      if (any(QAQC.chk < 0, QAQC.chk > 100)) {
        warning("all values of QAQC.cross must be between 0 and 100")
      }     
    } else if (any(QAQC.cross < 0, QAQC.cross > 100)) {
      warning("all values of QAQC.cross must be between 0 and 100")
      ice.QAQC <- NULL
    }    
  }
 
#  if (any(lapply(strsplit(outfolder, "/"), substr, 1, 1)[[1]] == "_")) {
#    reportfolder <- tempdir()
#  }
    
  #reportfolder <- file.path(outfolder, "report")
  #if (!dir.exists(reportfolder)) {
  #  dir.create(reportfolder)
  #}
  rawfolder <- normalizePath(rawfolder, mustWork=TRUE)

  if (is.null(outfn.pre)) {
    outfn.pre <- gsub(" ", "_", AOInm)
  }
  
  
  ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(AOInm, '_report.Rmd'))
  #reportnm <- paste0(AOInm, '_report.docx')
  reportnm <- paste0(AOInm, '_report.html')
  reportfn <- file.path(reportfolder, reportnm)
  file.copy(system.file("rmd", "anPBpopICE_report.Rmd", package="FIESTAnalysis"),
	rmdfn, overwrite=TRUE)
#  file.copy(system.file("rmd", "anPBtemplateICE.docx", package="FIESTAnalysis"),
#	file.path(reportfolder, "anPBtemplateICE.docx"), overwrite=TRUE)
  if (is.null(photofn)) {
    file.copy(system.file("rmd", "ICE.PNG", package="FIESTAnalysis"), 
		file.path(reportfolder, "ICE.PNG"), overwrite=TRUE)
  } else {
    file.copy(photofn, file.path(reportfolder, "ICE.PNG"))
  }

  ## TESTING
  #file.copy("C:/_tsf/_GitHub/FIESTA/inst/rmd/anPBpopICE_report.Rmd", rmdfn, overwrite=TRUE)
  #file.copy("C:/_tsf/_GitHub/FIESTA/inst/rmd/anPBtemplateICE.docx", reportfn, overwrite=TRUE)
  #file.copy("C:/_tsf/_GitHub/FIESTA/inst/rmd/ICE.PNG", file.path(reportfolder, "ICE.PNG"), overwrite=TRUE)

#  if (all(is.null(QAQC.self), is.null(QAQC.cross))) {
    # Lines 2168-2241 contain text for QAQC
    # Starting before the following:
    ############
    ############## Quality Assessment and Quality Control (QAQC) report
    # And ending before:
    ##############
    ############## Reference Information
#    system(paste("sed -i '2168,2241d'", rmdfn)) 
#  }

  ## Set working directory to reportfolder
  setwd(reportfolder) 
 
  test <- tryCatch(
    rmarkdown::render(
      input = rmdfn,
      output_format = "html_document",
      output_file = reportfn,
      params = list(rawfolder=rawfolder, AOInm=AOInm, T1=T1, T2=T2,
		           REMPER=REMPER, outfn.pre=outfn.pre, 
                      QAQC.self=QAQC.self, QAQC.cross=QAQC.cross),
      envir = parent.frame()
    ),
    error=function(e) {
      message(e, "\n")
      return(NULL) 
    })
  if (is.null(test)) {
    setwd(wkdir)
  }

  ## Copy report from temporary folder to outfolder
  file.copy(reportfn, outfolder, overwrite=TRUE)
  message("saving report to ", file.path(outfolder, reportnm))
  file.copy("figs", outfolder, recursive=TRUE, overwrite=TRUE)
  message("saving figures to ", file.path(outfolder, "figs")) 

  ## Set working directory back to original working directory
  setwd(wkdir) 

}
