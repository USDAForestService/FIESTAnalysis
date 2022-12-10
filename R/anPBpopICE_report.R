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
#' @param outfn.pre String. Short name of file name. If NULL, using AOInm.
#' @param ice.QAQCfn String. Name of file with summarize QAQC data (See details
#' for file structure). If no file is given, the QAQC section is removed from
#' report.
#' @param photofn String. Path name to photo to add to report.
#' @param outfolder String. The path of folder to output tables.
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anPBpopICE_report <- function(rawfolder, 
                              AOInm, 
                              T1, 
                              T2, 
                              outfn.pre = NULL, 
                              photofn = NULL, 
                              outfolder = NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.

  wkdir <- getwd()

  outfolder <- pcheck.outfolder(outfolder)
  #outfolder <- normalizePath(outfolder)
  reportfolder <- tempdir()


# ice.QAQCfn String. Name of file with summarize QAQC data (See details for 
# file structure). If no file is given, the QAQC section is removed from report.
  ice.QAQCfn = NULL


#  if (any(lapply(strsplit(outfolder, "/"), substr, 1, 1)[[1]] == "_")) {
#    reportfolder <- tempdir()
#  }
    
  #reportfolder <- file.path(outfolder, "report")
  #if (!dir.exists(reportfolder)) {
  #  dir.create(reportfolder)
  #}
  rawfolder <- normalizePath(rawfolder, mustWork=TRUE)
  if (!is.null(ice.QAQCfn)) {
    ice.QAQCfn <- normalizePath(ice.QAQCfn, mustWork=TRUE)
  }

  if (is.null(outfn.pre)) {
    outfn.pre <- gsub(" ", "_", AOInm)
  }
  
  
  ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(AOInm, '_report.Rmd'))
  reportnm <- paste0(AOInm, '_report.docx')
  reportfn <- file.path(reportfolder, reportnm)
  file.copy(system.file("rmd", "anPBpopICE_report.Rmd", package="FIESTAnalysis"),
	rmdfn, overwrite=TRUE)
  file.copy(system.file("rmd", "anPBtemplateICE.docx", package="FIESTAnalysis"),
	file.path(reportfolder, "anPBtemplateICE.docx"), overwrite=TRUE)
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

#  if (is.null(ice.QAQCfn)) {
#    # Lines 1889-1908 contain text for QAQC
#    # Starting before the following:
#    ############
#    ############## Quality Assessment and Quality Control (QAQC) report
#    # And ending before:
#    ##############
#    ############## Reference Information
#    system(paste("sed -i '1902,1922d'", rmdfn)) 
#  }

  ## Set working directory to reportfolder
  setwd(reportfolder) 

  test <- tryCatch(
    rmarkdown::render(
      input = rmdfn,
      output_file = reportfn,
      params = list(rawfolder=rawfolder, AOInm=AOInm, T1=T1, T2=T2,
		  outfn.pre=outfn.pre, ice.QAQCfn=ice.QAQCfn),
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
  file.copy(reportfn, outfolder)
  message("saving report to ", file.path(outfolder, reportnm))
  

  ## Set working directory back to original working directory
  setwd(wkdir) 

}
