#' ANALYSIS - Generate core tables for Small Area module.
#'
#' Generates a set of core tables based on a small area of interest.
#'
#' If variables are NULL, then it will prompt user to input variables.
#'
#' @param SApopdatlst R object. Population data from database, returned from
#' modSApop() function for a specified area of interest.
#' @param SApackage String. Small area package to use ('JoSAE', 'sae', 'hbsae')
#' @param SAmethod String. Small area method to use ('unit', 'area')
#' @param title.ref String. Reference information for title and output file
#' names.
#' @param xlsx Logical. If TRUE, adds all tables to Excel spreadsheet.
#' @param outfolder String. The path of folder to output tables.
#' @param outfn.pre String. Add a prefix to output name (e.g., "01").
#' @param outfn.date Logical. If TRUE, adds current date to outfile name.
#' @param overwrite Logical. If TRUE, overwrites file if it exists.
#' @param fortypgrpcd Integer vector. Forest type group code for focus for
#' estimation.
#' @param divideby.vol String. divideby for volume tables ('hundred',
#' 'thousand', 'million').
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anSApop_core <- function(SApopdatlst, 
                         SApackage,
                         SAmethod,
                         title.ref, 
                         xlsx = FALSE, 
                         outfolder = NULL, 
                         outfn.pre = NULL, 
                         outfn.date = FALSE, 
                         overwrite = FALSE, 
                         fortypgrpcd = 220, 
                         divideby.vol = "million") {

  ## Set global variables
  SPGRPCD=footnote3=footnote4 <- NULL
  divideby <- "thousand"

  ## Check fortypgrpcd
  ref_codes <- FIESTAutils::ref_codes
  ref_fortypgrpcd <- ref_codes[ref_codes$VARIABLE == "FORTYPGRPCD",]
  cdlst <- stats::na.omit(sort(ref_fortypgrpcd$VALUE))
  nmlst <- ref_fortypgrpcd$MEANING[ref_fortypgrpcd$VALUE %in% cdlst]
  if (!all(fortypgrpcd %in% cdlst)) {
    stop("fortypgrpcd is invalid")
  } else {
    message("using fortyprpcd ", toString(cdlst), " for: ", toString(nmlst))
  }
  fortypgrpnm <- ref_fortypgrpcd$MEANING[ref_fortypgrpcd$VALUE %in% fortypgrpcd]


  if (xlsx) {
    if (Sys.getenv("JAVA_HOME")!="")
      Sys.setenv(JAVA_HOME="")
    #library(rJava)
    #library(xlsx)
  }
  gui <- FALSE
  estround <- 1
  fill <- TRUE
  addSEcol <- FALSE
  returntitle <- TRUE
  filtervar <- "FORTYPGRPCD"
  modelselect <- TRUE
  na.fill <- "DIR"


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(anSApop_core)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  
  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################

  ## Check SApopdatlst
  if (class(SApopdatlst) != "list") {
    SApopdatlst <- list(SApopdatlst)
  } else if ("condx" %in% names(SApopdatlst)) {
    SApopdatlst <- list(SApopdatlst)
  }  
  
  ## Check SApopdat
  for (SApopdat in SApopdatlst) {
    ## Check SApopdat
    SApopdat <- pcheck.object(SApopdat, "SApopdat",
		list.items=c("treex", "seedx"))
  }
  
  ## Check SApackage 
  SApackagelst <- c("JoSAE", "sae", "hbsae")
  SApackage <- pcheck.varchar(var2check=SApackage, varnm="SApackage", gui=gui, 
                              checklst=SApackagelst, caption="SApackage", multiple=FALSE, stopifnull=TRUE)
  
  ## Check for JoSAE library
  if (SApackage == "JoSAE") {
    if (!"JoSAE" %in% rownames(installed.packages())) {
      message("SApackage JoSAE requires package JoSAE")
    }
  } else {
    if (!"sae" %in% rownames(installed.packages())) {
      message("SApackage sae requires package sae")
    }
  }
  
  ## Check SAmethod 
  SAmethodlst <- c("unit", "area", "combo")
  SAmethod <- pcheck.varchar(var2check=SAmethod, varnm="SAmethod", gui=gui, 
                             checklst=SAmethodlst, caption="SAmethod", 
                             multiple=FALSE, stopifnull=TRUE)

  ## Check outfolder
  outfolder <- pcheck.outfolder(outfolder)

  ## Check xlsx
  xlsx <- pcheck.logical(xlsx, varnm="xlsx",
		title="Excel?", first="NO", gui=gui)

  savedata <- ifelse(xlsx, FALSE, TRUE)
  rawdata <- ifelse(xlsx, FALSE, TRUE)
  allin1 <- TRUE

  if (xlsx) {
    ## Set workbook name
    outfn <- paste0(gsub(" ", "", gsub(",", "", title.ref)), "_core_tables")

    ## Check Excel workbook
    wbnm <- pcheck.xlsx(wbnm=NULL, savewb=TRUE, outfn=outfn, outfolder=outfolder,
			overwrite=overwrite)

    ## Define footnotes
    footnote1 <- "Numbers in rows and columns may not sum to totals due to rounding."
    footnote2 <- "A dash (--) indicates no sample for the cell"

    ## Set up Excel Workbook
    wb <- xlsx::loadWorkbook(file=wbnm)
    sheetnm <- "TOC"

    newsheet <- xlsx::createSheet(wb, sheetName=as.character(as.character(sheetnm)))
    datsheet <- xlsx::getSheets(wb)[[sheetnm]]
    message(paste("saving to sheet", sheetnm))

    #######################################################################################
    ## Table of Contents (TOC)
    #######################################################################################
    t01 <- "Table 01. Number of live trees on forest land"
    t02 <- "Table 02. Net cuft volume of live trees on forest land"
    t03 <- "Table 03. Basal area (square feet) of live trees on forest land"
    t04 <- "Table 04. Area by distance to road and land class, on all land"
    t05 <- "Table 05. Number of live trees by species and disturbance group, on forest land"
    t06 <- "Table 06. Number of live trees by species and diameter class, on forest land"
    t07 <- "Table 07. Number of dead trees by species by diameter class, on forest land"
    t08 <- "Table 08. Net cuft volume of live trees by species and diameter class, on forest land"
    t09 <- "Table 09. Net cuft volume of dead trees by species and diameter class, on forest land"
    t10 <- "Table 10. Basal area of dead trees by agent code, on forest land"
    t11 <- "Table 11. Number of live trees per acre by species and disturbance group, on forest land"
    t12 <- "Table 12. Basal area per acre of dead trees by agent code, on forest land"
    toc <- data.frame(rbind(t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12), stringsAsFactors=FALSE)

    ## Create row and cells for title
    toctitle.row <- xlsx::createRow(datsheet, 1)
    toctitle.cells <- xlsx::createCell(toctitle.row, colIndex=1)
    xlsx::setRowHeight(toctitle.row, multiplier=2)
    xlsx::setCellValue(toctitle.cells[[1]], "Table of Contents")

    xlsx::addDataFrame(toc, datsheet, col.names=FALSE, row.names=FALSE, startRow=3)

    ## SAVE EXCEL WORKBOOK
    ###############################################################
    xlsx::saveWorkbook(wb, wbnm)
  }

  #######################################################################################
  ## New Variables
  #######################################################################################

  ## Diameter class for DIA - DIACL
  for (i in 1:length(SApopdatlst)) {
    treedia.brks=c(0,5,10,15,20,25,50,100)
    datlut <- datLUTclass(x=SApopdatlst[[i]]$treex, xvar="DIA", cutbreaks=treedia.brks)
    SApopdatlst[[i]]$treex <- datlut$xLUT
  }


  #######################################################################################
  ## 01 - Number of live trees on forest land
  #######################################################################################
  tabnm <- "01"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  estvar <- "TPA_UNADJ"
  estvar.filter <- "STATUSCD == 1"

  estdat <- modSAtree(SApopdatlst=SApopdatlst,
                      landarea=landarea,
                      estvar=estvar,
                      estvar.filter=estvar.filter,
                      modelselect=modelselect,
				              multest=TRUE,
                      na.fill=na.fill,
		                  savedata=savedata,
		                  returntitle=returntitle,
		                  table_opts = table_options(allin1=allin1),
		                  title_opts = title_options(
		                               title.ref=title.ref,
		                               title.unitvar="fire"),
		                  savedata_opts = savedata_options(
		                                  outfolder=outfolder,
		                                  outfn.pre=outfn.pre2,
		                                  outfn.date=outfn.date,
		                                  overwrite_layer=overwrite)
		                  )
  #estdat$est
  esttab <- estdat$est
  tabtitle <- estdat$titlelst$title.estpse

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, 
               tabtitle=paste0("Table ", tabnm, ". ", tabtitle), 
               outfolder=outfolder, fill=fill, allin1=allin1, addSEcol=addSEcol, 
               coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, 
               footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }

  
  #######################################################################################
  ## 02 - Net cuft volume of live trees on forest land
  #######################################################################################
  tabnm <- "02"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  estvar <- "VOLCFNET"
  estvar.filter <- "STATUSCD == 1 & DIA >= 5"

  estdat <- modSAtree(SApopdatlst=SApopdatlst,
                      landarea=landarea,
                      estvar=estvar,
                      estvar.filter=estvar.filter,
                      modelselect=modelselect,
				              multest=TRUE,
                      na.fill=na.fill,
                      savedata=savedata,
		                  returntitle=returntitle,
		                  table_opts = table_options(allin1=allin1),
		                  title_opts = title_options(
		                               title.ref=title.ref,
		                               title.unitvar="fire"),
		                  savedata_opts = savedata_options(
		                                  outfolder=outfolder,
		                                  outfn.pre=outfn.pre2,
		                                  outfn.date=outfn.date,
		                                  overwrite_layer=overwrite)
		                  )
		                          
  #estdat$est
  esttab <- estdat$est
  tabtitle <- estdat$titlelst$title.estpse

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, 
               tabtitle=paste0("Table ", tabnm, ". ", tabtitle),
		           outfolder=outfolder, fill=fill, allin1=allin1, addSEcol=addSEcol, 
		           coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, 
		           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 03 - Basal area (square feet) of live trees on forest land
  #######################################################################################
  tabnm <- "03"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  estvar <- "BA"
  estvar.filter <- "STATUSCD == 1 & DIA >= 1"

  devtools::load_all("C:/_tsf/_GitHub/FIESTA/R")
  
  estdat <- modSAtree(SApopdatlst=SApopdatlst,
                      landarea=landarea,
                      estvar=estvar, 
                      estvar.filter=estvar.filter,
                      modelselect=modelselect,
                      na.fill=na.fill,
                      savedata=savedata, 
		                  returntitle=returntitle,
		                  table_opts = table_options(allin1=allin1),
		                  title_opts = title_options(
		                               title.ref=title.ref,
		                               title.unitvar="fire"),
		                  savedata_opts = savedata_options(
		                                  outfolder=outfolder,
		                                  outfn.pre=outfn.pre2,
		                                  outfn.date=outfn.date,
		                                  overwrite_layer=overwrite)
		                  )
  #estdat$est
  esttab <- estdat$est
  tabtitle <- estdat$titlelst$title.estpse

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, 
               tabtitle=paste0("Table ", tabnm, ". ", tabtitle), 
               outfolder=outfolder, fill=fill, allin1=allin1, addSEcol=addSEcol, 
               coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, 
               footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 04 - Basal area (square feet) of live trees on forest land - FORTYPGRPCD
  #######################################################################################
  tabnm <- "04"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  pcfilter <- getfilter(filtervar, fortypgrpcd)
  estvar <- "BA"
  estvar.filter <- "STATUSCD == 1 & DIA >= 1"

  
  estdat <- tryCatch(
    modSAtree(SApopdatlst=SApopdatlst,
					    landarea=landarea,
		          pcfilter=pcfilter, 
		          estvar=estvar, 
		          estvar.filter=estvar.filter,
		          modelselect=modelselect,
		          na.fill=na.fill,
		          savedata=savedata,
		          returntitle=returntitle,
		          table_opts = table_options(allin1=allin1),
		          title_opts = title_options(
		                       title.ref=title.ref, 
		                       title.unitvar="fire",
		                       title.filter=fortypgrpnm),
		          savedata_opts = savedata_options(
		                          outfolder=outfolder,
		                          outfn.pre=outfn.pre2,
		                          outfn.date=outfn.date,
		                          overwrite_layer=overwrite)),
      error = function(e) {
      message(e,"\n")
      return(NULL) })
  
  if (!is.null(estdat)) {
    esttab <- estdat$est
    tabtitle <- estdat$titlelst$title.estpse

    if (xlsx) {
      coltottxt <- "All forest land"
      cellwidth <- 14
      write2xlsx(esttab=esttab, 
               tabtitle=paste0("Table ", tabnm, ". ", tabtitle), 
               outfolder=outfolder, fill=fill, allin1=allin1, addSEcol=addSEcol, 
               coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm, 
               footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
    }
  }
}
