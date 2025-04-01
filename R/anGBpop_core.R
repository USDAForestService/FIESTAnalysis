#' ANALYSIS - Generate core tables for Green-Book custom population.
#'
#' Generates a set of core tables based on area of interest.
#'
#' If variables are NULL, then it will prompt user to input variables.
#'
#' @param GBpopdat R object. Population data from database, returned from
#' modGBpop() function for a specified area of interest.
#' @param title.ref String. Reference information for title and output file
#' names.
#' @param xlsx Logical. If TRUE, adds all tables to Excel spreadsheet.
#' @param outfolder String. The path of folder to output tables.
#' @param outfn.pre String. Add a prefix to output name (e.g., "01").
#' @param outfn.date Logical. If TRUE, adds current date to outfile name.
#' @param overwrite Logical. If TRUE, overwrites file if it exists.
#' @param treedia.brks Integer vector. Numeric breaks for tree diameters.
#' @param divideby.vol String. divideby for volume tables ('hundred',
#' 'thousand', 'million').
#' @author Tracey S. Frescino
#' @keywords data
#'
#' @export
anGBpop_core <- function(GBpopdat, 
                         title.ref, 
                         xlsx = FALSE, 
                         outfolder = NULL, 
                         outfn.pre = NULL, 
                         outfn.date = FALSE, 
                         overwrite = FALSE, 
                         treedia.brks = c(0,5,10,15,20,25,50,100), 
                         divideby.vol = "million") {

  ## Set global variables
  SPGRPCD=footnote3=footnote4 <- NULL
  divideby <- "thousand"


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

  ## Notes:
  ## 1. If want to add row groups with no subtotals, use GBest*() with rowgrp=TRUE.
  ## 2. If want to add row groups with subtotals, use rowgrptab().
  ## 3. If want to add SE columns side-by-side, use write2xlsx() with addSEcol=TRUE.
  ## 4. If want to add SE values in same cell, use GBest*() with allin1=TRUE or
  ##	rowgrptab() with allin1=TRUE if subtotals and write2xlsx() with allin1=TRUE.
  ## 5. If want all columns in ref table, use GBest*() or rowgrptab() with col.add0=TRUE.

  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(anGBpop_core)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  

  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################

  ## Check GBpopdat
  ########################################################
  GBpopdat <- pcheck.object(GBpopdat, "GBpopdat",
		list.items=c("treex", "seedx"))


  ## Check outfolder
  ########################################################
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
    t01 <- "Table 01. Area by land class and reserved status"
    t02 <- "Table 02. Area by forest type and stand-size class, on forest land"
    t03 <- "Table 03. Area by forest type group and disturbance class, on forest land"
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

    xlsx:: addDataFrame(toc, datsheet, col.names=FALSE, row.names=FALSE, startRow=3)

    ## SAVE EXCEL WORKBOOK
    ###############################################################
    xlsx::saveWorkbook(wb, wbnm)
  }

  #######################################################################################
  ## New Variables
  #######################################################################################

  ## Disturbance group for primary disturbance (DSTRBCD1) - DSTRBCD
#  GBpopdat$pltcondx <- merge(GBpopdat$pltcondx,
#		ref_codes[ref_codes$VARIABLE == "DSTRBCD", c("VALUE", "GROUPCD")],
#		by.x="DSTRBCD1", by.y="VALUE")
#  names(GBpopdat$pltcondx)[names(GBpopdat$pltcondx) == "GROUPCD"] <- "DSTRBGRPNM"

  ## Diameter class for DIA - DIACL
  if (is.null(treedia.brks)) {
    treedia.brks=c(0,5,10,15,20,25,50,100)
  }
  datlut <- datLUTclass(x=GBpopdat$treex, xvar="DIA", cutbreaks=treedia.brks)
  GBpopdat$treex <- datlut$xLUT

  ## Add BA if not already in dataset
  if (!"BA" %in% names(GBpopdat$treex)) {
    GBpopdat$treex$BA <- GBpopdat$treex$DIA * GBpopdat$treex$DIA * 0.005454
  }
  

  #######################################################################################
  ## 01 - Area by land class and reserved status
  #######################################################################################
  tabnm <- "01"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "ALL"
  rowvar <- "COND_STATUS_CD"
  colvar <- "RESERVCD"
 
  estdat <- modGBarea(
    GBpopdat=GBpopdat,
    sumunits=TRUE,
		landarea=landarea,
		rowvar=rowvar,
		colvar=colvar, 
		savedata=savedata,
		returntitle=returntitle,
		table_opts = table_options(
		  row.FIAname=TRUE,
		  col.FIAname=TRUE, 
		  row.add0=TRUE,
		  col.add0=TRUE,
		  allin1=allin1
		),
		title_opts = title_options(title.ref=title.ref),
		savedata_opts = savedata_options(
		  outfolder=outfolder,
		  outfn.pre=outfn.pre2,
		  outfn.date=outfn.date,
		  overwrite_layer=overwrite
		))
  esttab <- estdat$est

  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- NULL
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle), 
               title.colvar=title.colvar, outfolder=outfolder, 
               title.rowvar=title.rowvar, fill=fill, allin1=allin1, 
               addSEcol=addSEcol, coltottxt=coltottxt, cellwidth=cellwidth, 
               wbnm=wbnm, sheetnm=tabnm, footnote1=footnote1, footnote2=footnote2, 
               footnote3=footnote3)
  }

  #######################################################################################
  ## 02 - Area by forest type and stand-size class, on forest land
  #######################################################################################
  tabnm <- "02"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  rowvar <- "FORTYPCD"
  colvar <- "STDSZCD"

  estdat <- modGBarea(
    GBpopdat=GBpopdat,
    sumunits=TRUE,
		landarea=landarea,
		rowvar=rowvar,
		colvar=colvar,
		savedata=savedata,
		returntitle=returntitle,
		table_opts = table_options(
		  row.FIAname=TRUE,
		  col.FIAname=TRUE,
		  col.add0=TRUE,
		  allin1=allin1
		),
		title_opts = title_options(title.ref=title.ref),
		savedata_opts = savedata_options(
		  outfolder=outfolder,
		  outfn.pre=outfn.pre2,
		  outfn.date=outfn.date,
		  overwrite_layer=overwrite
		))
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle), 
            title.colvar=title.colvar, outfolder=outfolder, 
            title.rowvar=title.rowvar, fill=fill, allin1=allin1, 
            addSEcol=addSEcol, coltottxt=coltottxt, cellwidth=cellwidth, 
            wbnm=wbnm, sheetnm=tabnm, 
            footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 03 - Area by forest type group and disturbance class, on forest land
  #######################################################################################
  tabnm <- "03"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  rowvar <- "FORTYPGRPCD"
  colvar <- "DSTRBCD1"

  estdat <- modGBarea(GBpopdat=GBpopdat,
                              sumunits=TRUE,
                              landarea=landarea, 
                              pcfilter="DSTRBCD1 > 0",
                              rowvar=rowvar,
                              colvar=colvar, 
                              savedata=savedata,
                              returntitle=returntitle,
                              table_opts = table_options(
                                row.FIAname=TRUE,
                                col.FIAname=TRUE,
                                col.add0=FALSE,
                                allin1=allin1
                              ),
                              title_opts = title_options(title.ref=title.ref),
                              savedata_opts = savedata_options(
                                outfolder=outfolder,
                                outfn.pre=outfn.pre2,
                                outfn.date=outfn.date,
                                overwrite_layer=overwrite
                              ))
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle), 
               title.colvar=title.colvar, outfolder=outfolder, 
               title.rowvar=title.rowvar, fill=fill, allin1=allin1, 
               addSEcol=addSEcol, coltottxt=coltottxt, cellwidth=cellwidth, 
               wbnm=wbnm, sheetnm=tabnm, 
               footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 04 - Area by distance to road and land class, on all land
  #######################################################################################
  tabnm <- "04"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "ALL"
  rowvar <- "RDDISTCD"
  colvar <- "COND_STATUS_CD"

  estdat <- modGBarea(GBpopdat=GBpopdat,
                              sumunits=TRUE,
                              landarea=landarea,
                              rowvar=rowvar,
                              colvar=colvar,
                              savedata=savedata,
                              returntitle=returntitle,
                              table_opts = table_options(
                                row.FIAname=TRUE,
                                col.FIAname=TRUE,
                                col.add0=FALSE,
                                allin1=allin1
                              ),
                              title_opts = title_options(title.ref=title.ref),
                              savedata_opts = savedata_options(
                                outfolder=outfolder,
                                outfn.pre=outfn.pre2,
                                outfn.date=outfn.date,
                                overwrite_layer=overwrite
                              ))
		                          
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle), 
               title.colvar=title.colvar, outfolder=outfolder, 
               title.rowvar=title.rowvar, fill=fill, allin1=allin1, 
               addSEcol=addSEcol, coltottxt=coltottxt, cellwidth=cellwidth, 
               wbnm=wbnm, sheetnm=tabnm, 
               footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 05 - Number of live trees by species and disturbance group, on forest land
  #######################################################################################
  tabnm <- "05"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  rowvar <- "SPCD"
  colvar <- "DSTRBGRP"
  estvar <- "TPA_UNADJ"
  estvar.filter <- "STATUSCD == 1"

  estdat <- modGBtree(GBpopdat=GBpopdat,
                              sumunits=TRUE,
		                          landarea=landarea,
                              estvar=estvar,
		                          estvar.filter=estvar.filter,
		                          rowvar=rowvar,
		                          colvar=colvar, 
		                          savedata=savedata,
		                          returntitle=returntitle,
		                          table_opts = table_options(
		                            row.FIAname=TRUE,
		                            col.FIAname=TRUE,
		                            col.add0=FALSE,
		                            allin1=allin1
		                          ),
		                          title_opts = title_options(title.ref=title.ref),
		                          savedata_opts = savedata_options(
		                            outfolder=outfolder,
		                            outfn.pre=outfn.pre2,
		                            outfn.date=outfn.date,
		                            overwrite_layer=overwrite
		                          ))
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle), 
            title.colvar=title.colvar, outfolder=outfolder, 
            title.rowvar=title.rowvar, fill=fill, allin1=allin1, 
            addSEcol=addSEcol, coltottxt=coltottxt, cellwidth=cellwidth, 
            wbnm=wbnm, sheetnm=tabnm, 
            footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 06 - Number of live trees by species and diameter class, on forest land
  #######################################################################################
  tabnm <- "06"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  rowvar <- "SPCD"
  colvar <- "DIACL"
  estvar <- "TPA_UNADJ"
  estvar.filter <- "STATUSCD == 1"

  estdat <- modGBtree(GBpopdat=GBpopdat,
                              sumunits=TRUE,
		                          landarea=landarea,
                              estvar=estvar,
		                          estvar.filter=estvar.filter,
		                          rowvar=rowvar,
		                          colvar=colvar,
		                          savedata=savedata, 
		                          returntitle=returntitle,
		                          table_opts = table_options(
		                            row.FIAname=TRUE,
		                            col.FIAname=FALSE, 
		                            col.add0=FALSE,
		                            allin1=allin1
		                          ),
		                          title_opts = title_options(title.ref=title.ref),
		                          savedata_opts = savedata_options(
		                            outfolder=outfolder,
		                            outfn.pre=outfn.pre2,
		                            outfn.date=outfn.date,
		                            overwrite_layer=overwrite
		                          ))
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),
		title.colvar=title.colvar, outfolder=outfolder,
		title.rowvar=title.rowvar, fill=fill, allin1=allin1, addSEcol=addSEcol,
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 07 - Number of dead trees by species by diameter class, on forest land
  #######################################################################################
  tabnm <- "07"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  rowvar <- "SPCD"
  colvar <- "DIACL"
  estvar <- "TPA_UNADJ"
  estvar.filter <- "STATUSCD == 2 & STANDING_DEAD_CD == 1"

  estdat <- modGBtree(GBpopdat=GBpopdat,
                              sumunits=TRUE,
		                          landarea=landarea,
                              estvar=estvar,
		                          estvar.filter=estvar.filter,
		                          rowvar=rowvar,
		                          colvar=colvar,
		                          savedata=savedata,
		                          returntitle=returntitle,
		                          table_opts = table_options(
		                            row.FIAname=TRUE,
		                            col.FIAname=FALSE,
		                            col.add0=FALSE,
		                            allin1=allin1
		                          ),
		                          title_opts = title_options(title.ref=title.ref),
		                          savedata_opts = savedata_options(
		                            outfolder=outfolder,
		                            outfn.pre=outfn.pre2,
		                            outfn.date=outfn.date,
		                            overwrite_layer=overwrite
		                          ))
		                          
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),
		title.colvar=title.colvar, outfolder=outfolder,
		title.rowvar=title.rowvar, fill=fill, allin1=allin1, addSEcol=addSEcol,
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 08 - Net cuft volume of live trees by species and diameter class, on forest land
  #######################################################################################
  tabnm <- "08"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  rowvar <- "SPCD"
  colvar <- "DIACL"
  estvar <- "VOLCFNET"
  estvar.filter <- "STATUSCD == 2 & STANDING_DEAD_CD == 1"

  estdat <- modGBtree(GBpopdat=GBpopdat,
                              sumunits=TRUE,
		                          landarea=landarea,
                              estvar=estvar,
		                          estvar.filter=estvar.filter,
		                          rowvar=rowvar,
		                          colvar=colvar,
		                          savedata=savedata,
		                          returntitle=returntitle,
		                          table_opts = table_options(
		                            row.FIAname=TRUE,
		                            col.FIAname=FALSE,
		                            col.add0=FALSE,
		                            allin1=allin1, 
		                          ),
		                          title_opts = title_options(title.ref=title.ref),
		                          savedata_opts = savedata_options(
		                            outfolder=outfolder,
		                            outfn.pre=outfn.pre2,
		                            outfn.date=outfn.date, 
		                            overwrite_layer=overwrite
		                          ))
		                          
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),
		title.colvar=title.colvar, outfolder=outfolder,
		title.rowvar=title.rowvar, fill=fill, allin1=allin1, addSEcol=addSEcol,
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 09 - Net cuft volume of dead trees by species and diameter class, on forest land
  #######################################################################################
  tabnm <- "09"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  rowvar <- "SPCD"
  colvar <- "DIACL"
  estvar <- "VOLCFNET"
  estvar.filter <- "STATUSCD == 1 & DIA >= 5"
  estdat <- modGBtree(GBpopdat=GBpopdat,
                              sumunits=TRUE,
		                          landarea=landarea,
                              estvar=estvar,
		                          estvar.filter=estvar.filter,
		                          rowvar=rowvar, 
		                          colvar=colvar,
		                          savedata=savedata, 
		                          returntitle=returntitle,
		                          table_opts = table_options(
		                            row.FIAname=TRUE,
		                            col.FIAname=FALSE,
		                            col.add0=FALSE,
		                            allin1=allin1
		                          ),
		                          title_opts = title_options(title.ref=title.ref),
		                          savedata_opts = savedata_options(
		                            outfolder=outfolder,
		                            outfn.pre=outfn.pre2, 
		                            outfn.date=outfn.date, 
		                            overwrite_layer=overwrite
		                          ))
		                          
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),
		title.colvar=title.colvar, outfolder=outfolder,
		title.rowvar=title.rowvar, fill=fill, allin1=allin1, addSEcol=addSEcol,
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 10 - Basal area of dead trees by agent code, on forest land
  #######################################################################################
  tabnm <- "10"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  rowvar <- "SPCD"
  colvar <- "AGENTCD"
  estvar <- "BA"
  estvar.filter <- "STATUSCD == 2 & STANDING_DEAD_CD == 1"
  
  estdat <- modGBtree(GBpopdat=GBpopdat,
                              sumunits=TRUE,
		                          landarea=landarea,
                              estvar=estvar,
		                          estvar.filter=estvar.filter,
		                          rowvar=rowvar, 
		                          colvar=colvar,
		                          savedata=savedata, 
		                          returntitle=returntitle,
		                          table_opts = table_options(
		                            row.FIAname=TRUE,
		                            col.FIAname=TRUE,
		                            col.add0=FALSE,
		                            allin1=allin1
		                          ),
		                          title_opts = title_options(title.ref=title.ref),
		                          savedata_opts = savedata_options(
		                            outfolder=outfolder,
		                            outfn.pre=outfn.pre2,
		                            outfn.date=outfn.date, 
		                            overwrite_layer=overwrite
		                          ))
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),
		title.colvar=title.colvar, outfolder=outfolder,
		title.rowvar=title.rowvar, fill=fill, allin1=allin1, addSEcol=addSEcol,
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 11 - Number of live trees per acre by species and disturbance group, on forest land
  #######################################################################################
  tabnm <- "11"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  rowvar <- "SPCD"
  colvar <- "DSTRBGRP"
  estvar <- "TPA_UNADJ"
  estvar.filter <- "STATUSCD == 1"

  estdat <- modGBratio(GBpopdat=GBpopdat,
                               sumunits=TRUE,
		                           landarea=landarea,
                               estvarn=estvar,
		                           estvarn.filter=estvar.filter,
		                           rowvar=rowvar,
		                           colvar=colvar,
		                           savedata=savedata,
		                           returntitle=returntitle,
		                           table_opts = table_options(
		                             row.FIAname=TRUE,
		                             col.FIAname=TRUE,
		                             col.add0=FALSE,
		                             allin1=allin1 
		                           ),
		                           title_opts = title_options(title.ref=title.ref),
		                           savedata_opts = savedata_options(
		                             outfolder=outfolder,
		                             outfn.pre=outfn.pre2,
		                             outfn.date=outfn.date,
		                             overwrite_layer=overwrite
		                           ))
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),
		title.colvar=title.colvar, outfolder=outfolder,
		title.rowvar=title.rowvar, fill=fill, allin1=allin1, addSEcol=addSEcol,
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }


  #######################################################################################
  ## 12 - Basal area per acre of dead trees by agent code, on forest land
  #######################################################################################
  tabnm <- "12"
  outfn.pre2 <- ifelse(is.null(outfn.pre), tabnm, paste0(tabnm, "_", outfn.pre))
  landarea <- "FOREST"
  rowvar <- "SPCD"
  colvar <- "AGENTCD"
  estvar <- "BA"
  estvar.filter <- "STATUSCD == 2 & STANDING_DEAD_CD == 1"

  estdat <- modGBratio(GBpopdat=GBpopdat,
                               sumunits=TRUE,
		                           landarea=landarea,
                               estvarn=estvar, 
		                           estvarn.filter=estvar.filter,
	                             rowvar=rowvar,
		                           colvar=colvar,
		                           savedata=savedata,
		                           returntitle=returntitle,
		                           table_opts = table_options(
		                             row.FIAname=TRUE,
		                             col.FIAname=TRUE,
		                             col.add0=FALSE,
		                             allin1=allin1
		                           ),
		                           title_opts = title_options(title.ref=title.ref),
		                           savedata_opts = savedata_options(
		                             outfolder=outfolder,
		                             outfn.pre=outfn.pre2, 
		                             outfn.date=outfn.date, 
		                             overwrite_layer=overwrite
		                           ))
  #estdat$est
  esttab <- estdat$est
  tabtitle <- ifelse (allin1, estdat$titlelst$title.estpse, estdat$titlelst$title.est)
  title.colvar <- estdat$titlelst$title.colvar
  title.rowvar <- estdat$titlelst$title.rowvar

  if (xlsx) {
    coltottxt <- "All forest land"
    cellwidth <- 14
    write2xlsx(esttab=esttab, tabtitle=paste0("Table ", tabnm, ". ", tabtitle),
		title.colvar=title.colvar, outfolder=outfolder,
		title.rowvar=title.rowvar, fill=fill, allin1=allin1, addSEcol=addSEcol,
		coltottxt=coltottxt, cellwidth=cellwidth, wbnm=wbnm, sheetnm=tabnm,
           footnote1=footnote1, footnote2=footnote2, footnote3=footnote3)
  }

}
