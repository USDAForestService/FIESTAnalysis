#' ANALYSIS - Compare and diagnose estimates.
#' 
#' Generates a report for estimator comparisions and diagnoses.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' @param estimates List containing FIESTA estimation output. 
#' @param attribute String. Name of estimated attribute, should be the name 
#' used in `data` containing the estimates you would like to compare. 
#' @param small_area String. Column name in `multest` containing small area of
#' interest codes. Defaults to "DOMAIN"
#' @param large_area String. Column name in `multest` containing large area that 
#' strength was borrowed out to.
#' @param outfolder String. The path of folder to output tables.
#' @author Tracey S. Frescino, Grayson W. White
#' @keywords data
#' @export
anDiagnostic_report <- function(estimates,
                                attribute,
                                small_area = "DOMAIN",
                                large_area,
                                outfolder = NULL) {
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(anDiagnostic_report)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  wkdir <- getwd()
  
  ## set global variables
  report_imagefn <- NULL
  gui <- FALSE
  
  ## Check for tidyverse library
  if (!"tidyverse" %in% rownames(installed.packages())) {
      message("This report requires the installation of the 'tidyverse' library.
              Please install with: 'install.packages('tidyverse')'.")
  }
  
  ## Check outfolder 
  outfolder <- pcheck.outfolder(outfolder)
  
  ## Create temporary folder for report
  reportfolder <- tempdir()
  reportrmd <- system.file("rmd", "anDiagnostic_report.Rmd", package="FIESTAnalysis")
  
  ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(small_area, '_report.Rmd'))
  reportnm <- paste0(small_area, '_report.docx')
  reportfn <- file.path(reportfolder, reportnm)
  file.copy(reportrmd, rmdfn, overwrite=TRUE)
  file.copy(system.file("rmd", "anGBtemplate.docx", package="FIESTAnalysis"),
            file.path(reportfolder, "anGBtemplate.docx"), overwrite=TRUE)
  
  ## Set working directory to reportfolder
  setwd(reportfolder) 
  
  test <- tryCatch(
    rmarkdown::render(
      input = rmdfn,
      output_file = reportfn,
      params = list(estimates = estimates,
                    small_area = small_area,
                    large_area = large_area,
                    attribute = attribute),
      envir = parent.frame()
    ),
    error=function(err) {
      message(err)
      return(NULL)
    } )
  if (is.null(test)) {
    setwd(wkdir) 
    stop()
  }
  
  
  ## Copy report from temporary folder to outfolder
  tmp <- file.copy(reportfn, outfolder, overwrite=TRUE)
  if (tmp) {
    message("saving report to ", file.path(outfolder, reportnm))
  } else {
    message("error when copying to ", outfolder)
  }
  
  ## Set working directory back to original working directory
  setwd(wkdir) 
}
