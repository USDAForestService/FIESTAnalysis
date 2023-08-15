#' ANALYSIS - Generate a report for Green-Book custom population.
#' 
#' Generates a standard report based on area of interest.
#' 
#' If variables are NULL, then it will prompt user to input variables.
#' 
#' @param MApopdat List object. Population data from modMApop().
#' @param MAmethod String. mase (i.e., model-assisted) method to use 
#' ('greg', 'gregEN', 'ratio').
#' @param AOInm String. Name of area of interest.
#' @param photofn String. Path name to photo to add to report.
#' @param photo_author String. Name of photo author (Optional).
#' @param pcfilter String. Name of variable or string to filter plots or
#' conditions.  Must be in R syntax (e.g., ADFORCD == 408, COUNTYCD == 1).
#' Optional.
#' @param spcd Integer. A species code to use as focus species of the report.
#' Multiple codes are accepted if the focus species is a combined group.
#' Default is 746 (quaking aspen).
#' @param title.ref String. Reference information for title and output file
#' names.
#' @param totals Logical. If TRUE, per acre estimates are multiplied by acres.
#' @param outfolder String. The path of folder to output tables.
#' @author Tracey S. Frescino
#' @keywords data
#' 
#' @export
anMApop_report <- function(MApopdat, 
                           MAmethod = "greg", 
                           AOInm, 
                           photofn = NULL, 
                           photo_author = NULL, 
                           pcfilter = NULL, 
                           spcd = NULL, 
                           title.ref = NULL,
                           totals = TRUE, 
                           outfolder) {
  
  ## DESCRIPTION: Creates a report using Rmarkdown 
  ## 		Adds a folder named report in the outfolder and copies all 
  ##		components of report into folder.
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(anMApop_report)) 
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

  ## Check MApopdat
  MApopdat <- pcheck.object(MApopdat, "MApopdat",
                            list.items=c("treex", "seedx"))

  outfolder <- pcheck.outfolder(outfolder)
  reportfolder <- tempdir()
  reportrmd <- system.file("rmd", "anMApop_report.Rmd", package="FIESTAnalysis")

  # ## Copy files to outfolder
  rmdfn <- file.path(reportfolder, paste0(AOInm, '_report.Rmd'))
  reportnm <- paste0(gsub(" ", "_", AOInm), '_report.docx')
  reportfn <- file.path(reportfolder, reportnm)
  file.copy(reportrmd, rmdfn, overwrite=TRUE)
  file.copy(system.file("rmd", "anGBtemplate.docx", package="FIESTAnalysis"),
            file.path(reportfolder, "anGBtemplate.docx"), overwrite=TRUE)
  if (!is.null(photofn)) {
    if (!file.exists(photofn)) {
      stop(photofn, "does not exist")
    }
    file.copy(photofn, file.path(reportfolder, "report_image.PNG"), overwrite=TRUE)
    report_imagefn <- file.path(reportfolder, "report_image.PNG")
  }

  ## Check MAmethod 
  MAmethodlst <- c("HT", "PS", "greg", "gregEN", "ratio")
  MAmethod <- pcheck.varchar(var2check=MAmethod, varnm="MAmethod",  
                    checklst=MAmethodlst, caption="MAmethod", 
                    multiple=FALSE, stopifnull=TRUE, gui=gui)
  
  #  ref_spcd <- ref_codes[ref_codes$VARIABLE == "SPCD", ]
  spcdlst <- table(MApopdat$seedx$SPCD)[table(MApopdat$seedx$SPCD) > 5]
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
    # Lines 105 to 109
    #system(paste("sed -i '102,106d'", rmdfn))
    system(paste("sed -i '92,96d'", rmdfn))
  }

  ## Set working directory to reportfolder
  setwd(reportfolder)
  
  rmarkdown::render(
    input = rmdfn,
    output_file = reportfn,
    params = list(MApopdat = MApopdat, MAmethod=MAmethod, AOInm=AOInm,
                  photo_author=photo_author, pcfilter=pcfilter, 
                  spcd=spcd, title.ref=title.ref, totals=totals),
    envir = parent.frame()
  )
  
  # ## Copy report from temporary folder to outfolder
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
