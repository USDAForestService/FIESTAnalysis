#' ANALYSIS - Generate core tables for ICE.
#'
#' Generates a set of core tables based on area of interest.
#'
#' If variables are NULL, then it will prompt user to input variables.
#'
#' @param PBpopdatICE List object. Population data output from
#' PBpopdatICE().
#' @param AOInm String. The name of the area of interest (i.e., population) for
#' titles and output names.
#' @param T1 Integer. Year of Time 1 imagery (YYYY).
#' @param T2 Interger. Year of Time 2 imagery (YYYY).
#' @param tabtype String. Type of units for the table ("AREA", "PCT").
#' @param sumunits Logical. If TRUE, estimation units are summed and returned
#' in one table.
#' @param pnt1 Logical. If TRUE, use only center point when no change observed.
#' @param domlut Data frame. Lookup table of domain categories and titles for
#' estimation.  If NULL, default from PBpopdatICE().
#' @param outfolder String. The path of folder to output tables.
#' @param outfn.pre String. Add a prefix to output name (e.g., "01").
#' @param outfn.date Logical. If TRUE, adds current date to outfile name.
#' @param overwrite Logical. If TRUE, overwrites file if it exists.
#' 
#' @return The following tables are saved to a folder named 'estimates' in the
#' outfolder.  If rawdata=TRUE, the rawdata is saved to a folder named
#' 'rawdata' in the estimates folder.
#'
#' \item{table01}{ Estimates of change_pnt on all lands. } \item{table02}{
#' Estimates of chg_ag_2_GRP on all lands. } \item{table03}{ Estimates of land
#' use at Time 1 on all lands. } \item{table04}{ Estimates of land use at Time
#' 2 on all lands. } \item{table05}{ Estimates of land cover at Time 1 all
#' lands. } \item{table06}{ Estimates of land cover at Time 2 on all lands. }
#' \item{table07}{ Percent of forest land use change. } \item{table08}{ Percent
#' of land cover change where land use was forest at Time 1. } \item{table09}{
#' Percent of land use change from Time 1 to Time 2. } \item{table10}{ Percent
#' of land cover change from Time 1 to Time 2. } \item{table11}{ Percent of
#' land cover by land use at Time 1. } \item{table12}{ Percent of land cover by
#' land use at Time 2. } \item{table13}{ Percent change of land use at Time 1
#' within land use at Time 2. } \item{table13a}{ Percent change of land use at
#' Time 1 within land cover at Time 2. } \item{table14}{ Percent change of land
#' cover at Time 1 within land use at Time 2. } \item{table15}{ Percent change
#' of land use at Time 1 within within change agent group. } \item{table16}{
#' Percent change of land cover at Time 1 within change agent group. }
#' \item{table17}{ Percent change of Land Use from Time 1 to Time 2. }
#' \item{table18}{ Percent change of Land Cover from Time 1 to Time 2. }
#' \item{table19}{ Percent change of Land Use (Forest/Nonforest) from Time 1 to
#' Time 2. }
#' @author Tracey S. Frescino
#' @references Frescino, Tracey S.; Moisen, Gretchen G.; Megown, Kevin A.;
#' Nelson, Val J.; Freeman, Elizabeth A.; Patterson, Paul L.; Finco, Mark;
#' Brewer, Ken; Menlove, James 2009.  Nevada Photo-Based Inventory Pilot (NPIP)
#' photo sampling procedures. Gen. Tech. Rep.  RMRS-GTR-222. Fort Collins, CO:
#' U.S. Department of Agriculture, Forest Service, Rocky Mountain Research
#' Station. 30 p.
#'
#' Patterson, Paul L. 2012. Photo-based estimators for the Nevada photo-based
#' inventory.  Res. Pap. RMRS-RP-92. Fort Collins, CO: U.S. Department of
#' Agriculture, Forest Service, Rocky Mountain Research Station. 14 p.
#' @keywords data
#' @export
anPBpopICE_core <- function(PBpopdatICE, 
                            AOInm, 
                            T1, 
                            T2, 
                            tabtype = "PCT", 
                            sumunits = FALSE, 
                            pnt1 = FALSE, 
                            domlut = NULL, 
                            outfolder = NULL,
                            outfn.pre = NULL, 
                            outfn.date = FALSE, 
                            overwrite = FALSE){ 
 
  ##################################################################
  ## INITIALIZE SETTINGS AND VARIABLES
  ##################################################################

  ## Set global variables
  #datstrat=datSTRATA=datACRES <- NULL

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- FALSE
  savedata=returntitle=rawdata <- TRUE


  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  input.params <- names(as.list(match.call()))[-1]
  formallst <- names(formals(anPBpopICE_core)) 
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  

  ##################################################################
  ## CHECK PARAMETER INPUTS
  ##################################################################
  
  ## Check GBpopdat
  ########################################################
  PBpopdatICE <- pcheck.object(PBpopdatICE, "PBpopdatICE",
                            list.items=c("PBx"))

  ## Check outfolder
  outfolder <- pcheck.outfolder(outfolder)
  outfolder <- paste0(outfolder, "/estimates")
  if (!dir.exists(outfolder)) dir.create(outfolder)

  ## Create outfn.pre
  if (is.null(outfn.pre)) outfn.pre <- AOInm

  ## Generate title.ref
  title.ref <- paste0(AOInm, paste0(", ", T1, "-", T2))
  if (pnt1) title.ref <- paste(title.ref, "- 1pt")


  
  #########################################################################
  ## Get luts from PBpopdatICE
  #########################################################################
  changelut <- data.table::setDT(PBpopdatICE$reflst$changelut)
  coverlut <- data.table::setDT(PBpopdatICE$reflst$coverlut)
  uselut <- data.table::setDT(PBpopdatICE$reflst$uselut)
  agentlut <- data.table::setDT(PBpopdatICE$reflst$agentlut)

  plotid <- PBpopdatICE$plotid
  pntid <- PBpopdatICE$pntid

  if (is.null(domlut)) {
    domlut <- PBpopdatICE$domlut
  }

  ###############################################################################
  ## 01. Estimates of change_pnt on all lands
  ## Percent changed.
  ###############################################################################
  outfn.pre2 <- paste0("01_", outfn.pre)
  rowvar <- "change_pnt"

  est.change_pnt <- modPB(PBpopdat=PBpopdatICE,
                          tabtype=tabtype, rowvar=rowvar,
                          sumunits=sumunits, domlut=domlut,
                          returntitle=returntitle, savedata=savedata,
                          savedata_opts=list(outfolder=outfolder,
                                  outfn.date=outfn.date,
                                  overwrite_layer=overwrite,
                                  outfn.pre=outfn.pre2),
                          title_opts = list(title.ref=title.ref))


  ###############################################################################
  ## 02. Estimates of chg_ag_2_GRP on all lands
  ## Percent change by change agent
  ###############################################################################
  outfn.pre2 <- paste0("02_", outfn.pre)
  rowvar <- "chg_ag_2_GRP"
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(agentlut[!agentlut[[rowvar]] %in% c(99,0),
			c(rowvar, rowvarnm), with=FALSE])
  pntfilter <- "change_pnt == 1"

  est.chg_ag_2grp <- modPB(PBpopdat=PBpopdatICE,
                            tabtype=tabtype, rowvar=rowvarnm,
                            domlut=domlut, sumunits=sumunits,
                            pntfilter=pntfilter,
                            table_opts=list(rowlut=rowlut, row.add0=TRUE),
                            returntitle=returntitle, savedata=savedata,
                            savedata_opts=list(outfolder=outfolder,
                                    outfn.date=outfn.date,
                                    overwrite_layer=overwrite,
                                    outfn.pre=outfn.pre2),
                            title_opts=list(title.ref=title.ref))
  #est.chg_ag_2grp$est


  ###############################################################################
  ## 03. Estimates of land use at Time 1 on all lands
  ###############################################################################
  outfn.pre2 <- paste0("03_", outfn.pre)
  rowvar <- "use_1"
  rowvar2 <- sub("_1", "", rowvar)
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(uselut[!uselut[[rowvar2]] %in% c(-1,0,999),
			c(rowvar2, paste(rowvar2, "nm", sep="_")), with=FALSE])
  data.table::setnames(rowlut, c(rowvar, rowvarnm))

  use_1 <- modPB(PBpopdat=PBpopdatICE,
                  tabtype=tabtype, rowvar=rowvar,
                  domlut=domlut, sumunits=sumunits,
                  table_opts=list(rowlut=rowlut, row.add0=TRUE),
                  returntitle=returntitle, savedata=savedata,
                  savedata_opts=list(outfolder=outfolder,
                          outfn.date=outfn.date,
                          overwrite_layer=overwrite,
                          outfn.pre=outfn.pre2),
                  title_opts=list(title.ref=title.ref))
  #use_1$est


  ###############################################################################
  ## 04. Estimates of land use at Time 2 on all lands
  ###############################################################################
  outfn.pre2 <- paste0("04_", outfn.pre)
  rowvar <- "use_2"
  rowvar2 <- sub("_2", "", rowvar)
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(uselut[!uselut[[rowvar2]] %in% c(-1,0,999),
			c(rowvar2, paste(rowvar2, "nm", sep="_")), with=FALSE])
  data.table::setnames(rowlut, c(rowvar, rowvarnm))

  use_2 <- modPB(PBpopdat=PBpopdatICE,
                  tabtype=tabtype, rowvar=rowvar,
                  domlut=domlut, sumunits=sumunits,
                  table_opts=list(rowlut=rowlut, row.add0=TRUE),
                  returntitle=returntitle, savedata=savedata,
                  savedata_opts=list(outfolder=outfolder,
                              outfn.date=outfn.date,
                              overwrite_layer=overwrite,
                              outfn.pre=outfn.pre2),
                  title_opts=list(title.ref=title.ref))
  #use_2$est


  ###############################################################################
  ## 05. Estimates of land cover at Time 1 all lands
  ###############################################################################
  outfn.pre2 <- paste0("05_", outfn.pre)
  rowvar <- "cover_1"
  rowvar2 <- sub("_1", "", rowvar)
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(coverlut[!coverlut[[rowvar2]] %in% c(-1,0,999),
			c(rowvar2, paste(rowvar2, "nm", sep="_")), with=FALSE])
  data.table::setnames(rowlut, c(rowvar, rowvarnm))

  cover_1 <- modPB(PBpopdat=PBpopdatICE,
                    tabtype=tabtype, rowvar=rowvar,
                    domlut=domlut, sumunits=sumunits,
                    table_opts=list(rowlut=rowlut, row.add0=TRUE),
                    returntitle=returntitle, savedata=savedata,
                    savedata_opts=list(outfolder=outfolder,
                                outfn.date=outfn.date,
                                overwrite_layer=overwrite,
                                outfn.pre=outfn.pre2),
                    title_opts=list(title.ref=title.ref))


  ###############################################################################
  ## 06. Estimates of land cover at Time 2 on all lands
  ###############################################################################
  outfn.pre2 <- paste0("06_", outfn.pre)
  rowvar <- "cover_2"
  rowvar2 <- sub("_2", "", rowvar)
  rowvarnm <- paste(rowvar, "nm", sep="_")
  rowlut <- unique(coverlut[!coverlut[[rowvar2]] %in% c(-1,0,999),
			c(rowvar2, paste(rowvar2, "nm", sep="_")), with=FALSE])
  data.table::setnames(rowlut, c(rowvar, rowvarnm))

  cover_2 <- modPB(PBpopdat=PBpopdatICE,
                    tabtype=tabtype, rowvar=rowvar,
                    domlut=domlut, sumunits=sumunits,
                    table_opts=list(rowlut=rowlut, row.add0=TRUE),
                    returntitle=returntitle, savedata=savedata,
                    savedata_opts=list(outfolder=outfolder,
                                outfn.date=outfn.date,
                                overwrite_layer=overwrite,
                                outfn.pre=outfn.pre2),
                    title_opts=list(title.ref=title.ref))


  ###############################################################################
  ## 07. Estimates of use_2_FOR by use_1_FOR
  ## Percent of forest land use change.
  ###############################################################################
  outfn.pre2 <- paste0("07_", outfn.pre)
  rowvar <- "use_1_FOR"
  colvar <- "use_2_FOR"

  use_1_2_FOR <- modPB(PBpopdat=PBpopdatICE, 
                        tabtype=tabtype, 
                        rowvar=rowvar, colvar=colvar, 
                        domlut=domlut, sumunits=sumunits, 
                        gainloss=TRUE,
                        table_opts=list(row.add0=TRUE, col.add0=TRUE),
                        returntitle=returntitle, savedata=savedata, 
                        savedata_opts=list(outfolder=outfolder, 
                                    outfn.date=outfn.date, 
                                    overwrite_layer=overwrite, 
                                    outfn.pre=outfn.pre2),
                        title_opts=list(title.ref=title.ref))
  #use_1_2_FOR$est


  ###############################################################################
  ## 08. Estimates of cover_2_GRP by cover_1_GRP
  ## Percent of land cover change where land use was forest at Time 1.
  ###############################################################################
  outfn.pre2 <- paste0("08_", outfn.pre)
  rowvar <- "cover_1_GRP"
  colvar <- "cover_2_GRP"

  cover_1_2_GRP <- modPB(PBpopdat=PBpopdatICE, 
                        tabtype=tabtype, 
                        rowvar=rowvar, colvar=colvar, 
                        domlut=domlut, sumunits=sumunits, 
                        gainloss=TRUE,
                        table_opts=list(row.add0=TRUE, col.add0=TRUE),
                        returntitle=returntitle, savedata=savedata, 
                        savedata_opts=list(outfolder=outfolder, 
                                    outfn.date=outfn.date, 
                                    overwrite_layer=overwrite, 
                                    outfn.pre=outfn.pre2),
                        title_opts=list(title.ref=title.ref))
  #cover_1_2_GRP$est


  ###############################################################################
  ## 09. Estimates of use_2 by use_1
  ## Percent of land use change from Time 1 to Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("09_", outfn.pre)
  rowvar <- "use_1"
  colvar <- "use_2"

  use_1_2 <- modPB(PBpopdat=PBpopdatICE, 
                    tabtype=tabtype,
                    rowvar=rowvar, colvar=colvar, 
                    domlut=domlut, sumunits=sumunits, 
                    gainloss=TRUE,
                    table_opts=list(row.add0=TRUE, col.add0=TRUE),
                    returntitle=returntitle, savedata=savedata, 
                    savedata_opts=list(outfolder=outfolder, 
                                outfn.date=outfn.date, 
                                overwrite_layer=overwrite, 
                                outfn.pre=outfn.pre2),
                    title_opts=list(title.ref=title.ref))
  #use_1_2$est


  ###############################################################################
  ## 10. Estimates of cover_2 by cover_1
  ## Percent of land cover change from Time 1 to Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("10_", outfn.pre)
  rowvar <- "cover_1"
  colvar <- "cover_2"

  cover_1_2 <- modPB(PBpopdat=PBpopdatICE, 
                      tabtype=tabtype,
                      rowvar=rowvar, colvar=colvar, 
                      domlut=domlut, sumunits=sumunits, 
                      gainloss=TRUE,
                      table_opts=list(row.add0=TRUE, col.add0=TRUE),
                      returntitle=returntitle, savedata=savedata, 
                      savedata_opts=list(outfolder=outfolder, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite, 
                                  outfn.pre=outfn.pre2),
                      title_opts=list(title.ref=title.ref))
  #cover_1_2$est


  ###############################################################################
  ## 11. Estimates of cover_1 by use_1
  ## Percent of land cover by land use at Time 1.
  ###############################################################################
  outfn.pre2 <- paste0("11_", outfn.pre)
  rowvar <- "use_1"
  colvar <- "cover_1"

  use_1_cover_1 <- modPB(PBpopdat=PBpopdatICE,
                          tabtype=tabtype,
                          rowvar=rowvar, colvar=colvar, 
                          domlut=domlut, sumunits=sumunits, 
                          gainloss=TRUE,
                          table_opts=list(row.add0=TRUE, col.add0=TRUE),
                          returntitle=returntitle, savedata=savedata, 
                          savedata_opts=list(outfolder=outfolder, 
                                      outfn.date=outfn.date, 
                                      overwrite_layer=overwrite, 
                                      outfn.pre=outfn.pre2),
                          title_opts=list(title.ref=title.ref))
  #use_1_cover_1$est


  ###############################################################################
  ## 12. Estimates of cover_2 by use_2
  ## Percent of land cover by land use at Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("12_", outfn.pre)
  rowvar <- "use_2"
  colvar <- "cover_2"

  use_2_cover_2 <- modPB(PBpopdat=PBpopdatICE, 
                          tabtype=tabtype,
                          rowvar=rowvar, colvar=colvar, 
                          domlut=domlut, sumunits=sumunits, 
                          gainloss=TRUE,
                          table_opts=list(row.add0=TRUE, col.add0=TRUE),
                          returntitle=returntitle, savedata=savedata, 
                          savedata_opts=list(outfolder=outfolder, 
                                      outfn.date=outfn.date, 
                                      overwrite_layer=overwrite, 
                                      outfn.pre=outfn.pre2),
                          title_opts=list(title.ref=title.ref))
  #use_2_cover_2$est


  ###############################################################################
  ## 13. Ratio estimates of use_2 within use_1
  ## Percent change of land use at Time 1 within land use at Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("13_", outfn.pre)
  rowvar <- "use_1"
  colvar <- "use_2"

  use_1_use_2_rat <- modPB(PBpopdat=PBpopdatICE, 
                            tabtype=tabtype, ratio=TRUE,
                            rowvar=rowvar, colvar=colvar, 
                            domlut=domlut, sumunits=sumunits, 
                            table_opts=list(row.add0=TRUE, col.add0=TRUE),
                            returntitle=returntitle, savedata=savedata, 
                            savedata_opts=list(outfolder=outfolder, 
                                        outfn.date=outfn.date, 
                                        overwrite_layer=overwrite, 
                                        outfn.pre=outfn.pre2),
                            title_opts=list(title.ref=title.ref))
  #use_1_use_2_rat$est


  ###############################################################################
  ## 13a. Ratio estimates of use_2 within cover_2
  ## Percent change of land use at Time 2 within land cover at Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("13a_", outfn.pre)
  rowvar <- "use_2"
  colvar <- "cover_2"

  use_2_cover_2_rat <- modPB(PBpopdat=PBpopdatICE, 
                              tabtype=tabtype, ratio=TRUE,
                              rowvar=rowvar, colvar=colvar, 
                              domlut=domlut, sumunits=sumunits, 
                              table_opts=list(row.add0=TRUE, col.add0=TRUE),
                              returntitle=returntitle, savedata=savedata, 
                              savedata_opts=list(outfolder=outfolder, 
                                          outfn.date=outfn.date, 
                                          overwrite_layer=overwrite, 
                                          outfn.pre=outfn.pre2),
                              title_opts=list(title.ref=title.ref))
  
  #use_2_cover_2_rat$est


  ###############################################################################
  ## 13b. Ratio estimates of cover_2 within use_2
  ## Percent change of land cover at Time 2 within land use at Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("13b_", outfn.pre)
  rowvar <- "cover_2"
  colvar <- "use_2"

  cover_2_use_2_rat <- modPB(PBpopdat=PBpopdatICE, 
                              tabtype=tabtype, ratio=TRUE,
                              rowvar=rowvar, colvar=colvar, 
                              domlut=domlut, sumunits=sumunits, 
                              table_opts=list(row.add0=TRUE, col.add0=TRUE),
                              returntitle=returntitle, savedata=savedata, 
                              savedata_opts=list(outfolder=outfolder, 
                                          outfn.date=outfn.date, 
                                          overwrite_layer=overwrite, 
                                          outfn.pre=outfn.pre2),
                              title_opts=list(title.ref=title.ref))
  
  #use_2_cover_2_rat$est



  ###############################################################################
  ## 14. Ratio estimates of cover_2 within cover_1
  ## Percent change of land cover at Time 1 within land use at Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("14_", outfn.pre)
  rowvar <- "cover_1"
  colvar <- "cover_2"

  cover_1_cover_2_rat <- modPB(PBpopdat=PBpopdatICE, 
                                tabtype=tabtype, ratio=TRUE,
                                rowvar=rowvar, colvar=colvar, 
                                domlut=domlut, sumunits=sumunits, 
                                table_opts=list(row.add0=TRUE, col.add0=TRUE),
                                returntitle=returntitle, savedata=savedata, 
                                savedata_opts=list(outfolder=outfolder, 
                                            outfn.date=outfn.date, 
                                            overwrite_layer=overwrite, 
                                            outfn.pre=outfn.pre2),
                                title_opts=list(title.ref=title.ref))
  #cover_1_cover_2_rat$est


  ###############################################################################
  ## 15. Ratio estimates of use_1 within chg_ag_2_GRP
  ## Percent change of land use at Time 1 within within change agent group.
  ###############################################################################
  outfn.pre2 <- paste0("15_", outfn.pre)
  rowvar <- "chg_ag_2_GRP"
  colvar <- "use_1"
  pntfilter <- "use_1 != use_2"

  agent_use_1_rat <- modPB(PBpopdat=PBpopdatICE, 
                            tabtype=tabtype, ratio=TRUE,
                            pntfilter=pntfilter, 
                            rowvar=rowvar, colvar=colvar, 
                            domlut=domlut, sumunits=sumunits, 
                            table_opts=list(row.add0=TRUE, col.add0=TRUE),
                            returntitle=returntitle, savedata=savedata, 
                            savedata_opts=list(outfolder=outfolder, 
                                        outfn.date=outfn.date, 
                                        overwrite_layer=overwrite, 
                                        outfn.pre=outfn.pre2),
                            title_opts=list(title.ref=title.ref))
  #agent_use_1_rat$est


  ###############################################################################
  ## 16. Ratio estimates of cover_1 within chg_ag_2_GRP
  ## Percent change of land cover at Time 1 within change agent group.
  ###############################################################################
  outfn.pre2 <- paste0("16_", outfn.pre)
  rowvar <- "chg_ag_2_GRP"
  colvar <- "cover_1"
  pntfilter <- "cover_1 != cover_2"

  agent_cover_1_rat <- modPB(PBpopdat=PBpopdatICE, 
                              tabtype=tabtype, ratio=TRUE,
                              pntfilter=pntfilter,
                              rowvar=rowvar, colvar=colvar, 
                              domlut=domlut, sumunits=sumunits, 
                              table_opts=list(row.add0=TRUE, col.add0=TRUE),
                              returntitle=returntitle, savedata=savedata, 
                              savedata_opts=list(outfolder=outfolder, 
                                          outfn.date=outfn.date, 
                                          overwrite_layer=overwrite, 
                                          outfn.pre=outfn.pre2),
                              title_opts=list(title.ref=title.ref))
  #agent_cover_1_rat$est


  ###############################################################################
  ## 17. Transitions - Land Use
  ## Percent change of Land Use from Time 1 to Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("17_", outfn.pre)
  rowvar <- "use_1_2"
  pntfilter <- "use_1 != use_2"

  use_1_2t <- modPB(PBpopdat=PBpopdatICE, 
                      tabtype=tabtype,
                      pntfilter=pntfilter, 
                      rowvar=rowvar,  
                      domlut=domlut, sumunits=sumunits, 
                      table_opts=list(row.add0=TRUE),
                      returntitle=returntitle, savedata=savedata, 
                      savedata_opts=list(outfolder=outfolder, 
                                  outfn.date=outfn.date, 
                                  overwrite_layer=overwrite, 
                                  outfn.pre=outfn.pre2),
                      title_opts=list(title.ref=title.ref))
  #use_1_2t$est


  ###############################################################################
  ## 18. Transitions - Land Cover
  ## Percent change of Land Cover from Time 1 to Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("18_", outfn.pre)
  rowvar <- "cover_1_2"
  pntfilter <- "cover_1 != cover_2"

  cover_1_2t <- modPB(PBpopdat=PBpopdatICE, 
                        tabtype=tabtype,
                        pntfilter=pntfilter, 
                        rowvar=rowvar,  
                        domlut=domlut, sumunits=sumunits, 
                        table_opts=list(row.add0=TRUE),
                        returntitle=returntitle, savedata=savedata, 
                        savedata_opts=list(outfolder=outfolder, 
                                    outfn.date=outfn.date, 
                                    overwrite_layer=overwrite, 
                                    outfn.pre=outfn.pre2),
                        title_opts=list(title.ref=title.ref))
  #cover_1_2t$est


  ###############################################################################
  ## 19. Transitions - Land Use - Forest/Nonforest
  ## Percent change of Land Use (Forest/Nonforest) from Time 1 to Time 2.
  ###############################################################################
  outfn.pre2 <- paste0("19_", outfn.pre)
  rowvar <- "use_1_2_FOR"
  pntfilter <- "use_1_FOR != use_2_FOR"

  use_1_2_FORt <- modPB(PBpopdat=PBpopdatICE, 
                          tabtype=tabtype,
                          pntfilter=pntfilter, 
                          rowvar=rowvar,  
                          domlut=domlut, sumunits=sumunits, 
                          table_opts=list(row.add0=TRUE),
                          returntitle=returntitle, savedata=savedata, 
                          savedata_opts=list(outfolder=outfolder, 
                                      outfn.date=outfn.date, 
                                      overwrite_layer=overwrite, 
                                      outfn.pre=outfn.pre2),
                          title_opts=list(title.ref=title.ref))
  #use_1_2_FORt$est

}

