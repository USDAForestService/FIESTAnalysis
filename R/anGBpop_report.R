#' ANALYSIS - Generate a report for Green-Book custom population.
#' 
#' Generates a standard report based on area of interest.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' @param GBpopdat List object. Population data from modGBpop().
#' @param AOInm String. Name of area of interest.
#' @param photofn String. Path name to photo to add to report.
#' @param photo_author String. Name of photo author (Optional).
#' @param EVALIDator_match Logical. If TRUE, estimates match EVALIDator.
#' @param pcfilter String. Name of variable or string to filter plots or
#' conditions.  Must be in R syntax (e.g., ADFORCD == 408, COUNTYCD == 1).
#' Optional.
#' @param spcd Integer. A species code to use as focus species of the report.
#' Multiple codes are accepted if the focus species is a combined group.
#' Default is 746 (quaking aspen).
#' @param title.ref String. Reference information for title and output file
#' names.
#' @param outfolder String. The path of folder to output tables.
#' @param reportnm String. The name of the output report. If NULL, 
#' defaults to AOInm '_rport.docx'. 
#' @author Tracey S. Frescino
#' @keywords data
#' 
#' @export
anGBpop_report <- function(GBpopdat, 
                           AOInm, 
                           photofn = NULL, 
                           photo_author = NULL, 
                           EVALIDator_match = FALSE, 
                           pcfilter = NULL, 
                           spcd = 746, 
                           title.ref = NULL, 
                           outfolder = NULL,
                           reportnm = NULL) {
  
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.

  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(anGBpop_report)) 
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

  ## Check GBpopdat
  GBpopdat <- pcheck.object(GBpopdat, "GBpopdat", 
		list.items=c("treex", "seedx"))

  outfolder <- pcheck.outfolder(outfolder)
  reportfolder <- tempdir()
  reportrmd <- system.file("rmd", "anGBpop_report.Rmd", package="FIESTAnalysis")
  
  ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(AOInm, '_report.Rmd'))
  if (is.null(reportnm)) {
    reportnm <- paste0(gsub(" ", "_", AOInm), '_report.docx')
  } else {
    if (!is.character(reportnm)) {
      stop("reportnm is invalid")
    }
    if (is.na(getext(reportnm)) && getext(reportnm) != "docx") {
      reportnm <- paste0(reportnm, ".docx")
    }
  }  
  reportfn <- file.path(reportfolder, reportnm)
  file.copy(reportrmd, rmdfn, overwrite=TRUE)
  file.copy(system.file("rmd", "anGBtemplate.docx", package="FIESTAnalysis"),
	file.path(reportfolder, "anGBtemplate.docx"), overwrite=TRUE)
  if (!is.null(photofn)) {
    if (!file.exists(photofn)) {
      stop(photofn, " does not exist")
    }
    file.copy(photofn, file.path(reportfolder, "report_image.PNG"), overwrite=TRUE) 
    report_imagefn <- file.path(reportfolder, "report_image.PNG")
  }

  ## Add BA if not already in dataset
  if (!"BA" %in% names(GBpopdat$treex)) {
    GBpopdat$treex$BA <- GBpopdat$treex$DIA * GBpopdat$treex$DIA * 0.005454
  }
  

#  ref_spcd <- ref_codes[ref_codes$VARIABLE == "SPCD", ]
  spcdlst <- table(GBpopdat$seedx$SPCD)[table(GBpopdat$seedx$SPCD) > 5]
  if (!is.null(spcd) && length(spcd) > 0) {
    if (!all(spcd %in% names(spcdlst))) {
      message("SPCD not in popdat: ", toString(spcd))
      spcd <- names(spcdlst[spcdlst != 999 & spcdlst == max(spcdlst)])
    } else {
      nplt <- sum(spcdlst[names(spcdlst) %in% spcd])
      if (nplt < 10) {
        spcd <- names(spcdlst[spcdlst != 999 & spcdlst == max(spcdlst)])
      }
    }
  } else {
    spcd <- names(spcdlst[spcdlst != 999 & spcdlst == max(spcdlst)])
  }
  
#  spnm <- ref_spcd[!is.na(ref_spcd$VALUE) & 
#				ref_spcd$VALUE == spcd, "MEANING"]

  if (is.null(photofn)) {
    # Lines 101 to 105
    system(paste("sed -i '101,105d'", rmdfn)) 
    #system(paste("sed -i '92,96d'", rmdfn)) 
  }

  ## Set working directory to reportfolder
  setwd(reportfolder) 

  rmarkdown::render(
    input = rmdfn,
    output_file = reportfn,
    params = list(GBpopdat=GBpopdat, AOInm=AOInm, photofn=photofn,
                  photo_author=photo_author, pcfilter=pcfilter, 
                  spcd=spcd, EVALIDator_match=EVALIDator_match, 
                  title.ref=title.ref),
    envir = parent.frame()
  )

  ## Copy report from temporary folder to outfolder
  tmp <- file.copy(reportfn, outfolder, overwrite=TRUE)
  if (tmp) {
    message("saving report to ", file.path(outfolder, reportnm))
  } else {
    message("error when copying to ", outfolder)
  }

  ## Set working directory back to original working directory
  setwd(wkdir) 
  return(reportfn)

}
