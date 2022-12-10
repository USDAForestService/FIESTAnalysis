#' ANALYSIS - Translate FIA data to Open Foris Collect/Calc data sets.
#'
#' Creates comma-delimited files that can be input into an Open Foris Collect,
#' FIA survey design to use as input data into the Open Foris Calc estimation
#' tool.
#'
#'
#' @param pltstrat DF/DT, comma-delimited file(*.csv), SpatialDataFrame, or
#' shapefile(*.shp) with one record per plot and strata/estimation unit
#' variable(s).
#' @param cond DF/DT or comma-delimited file (*.csv). The condition-level table
#' with one record per condtions and including nonsampled conditions.
#' @param tree DF/DT or comma-delimited file(*.csv). The tree table with tree
#' data to aggregate to condition-level. See details for necessary variables to
#' include.
#' @param seed DF/DT or comma-delimited file (*.csv). Seedling table.
#' @param unitarea Numeric or DF. Total area by estimation unit. If only 1
#' estimation unit, include number of total acreage for the area of interest or
#' a data frame with areavar. If more than one estimation unit, provide a data
#' frame of total area by estimation unit.
#' @param strlut DF/DT. If strata=TRUE, look-up table with strata proportions
#' ('strwt') by strata (and estimation unit). To calculate 'strwt', set
#' getwt=TRUE and getwtvar= name of variable with information to calculate
#' weights from (e.g., pixel counts).
#' @param level_1_code String. Name of level 1 estimation unit variable with
#' code values (e.g., state).
#' @param level_1_label String. Name of level 1 estimation unit variable with
#' code value descriptions.
#' @param level_2_code String. Name of level 2 estimation unit variable with
#' code values (e.g., county).
#' @param level_2_label String. Name of level 2 estimation unit variable with
#' code value descriptions.
#' @param calc Logical. If TRUE, creates tables for use in Open Foris Calc.
#' @param collect Logical. If TRUE, creates tables for use in Open Foris
#' Collect.
#' @param savedata Logical. If TRUE, saves table(s) to outfolder.
#' @param outfolder String. The outfolder to write files to. If NULL, files are
#' written to working directory, or if gui, a window to browse.
#' @author Tracey S. Frescino
#' @keywords data
#' @export
#' @importFrom data.table ":="

anFIA2OpenForis <- function(pltstrat, cond, tree, seed, unitarea, strlut,
	level_1_code, level_1_label=NULL, level_2_code=NULL, level_2_label=NULL,
	calc=TRUE, collect=TRUE, savedata=FALSE, outfolder=NULL) {


  ###############################################################
  ## DESCRIPTION:
  ## Translate FIA data to Open Foris Collect/Calc data sets
  ###############################################################

  ## Set global variables
  STRATUM_DESCR=P1POINTCNT=strwt=V1=AREA <- NULL

  returnlst <- list()


  ### Check calc
  calc <- pcheck.logical(calc, varnm="calc",
		title="Calc tables?", first="YES", stopifnull=TRUE)

  ### Check collect
  collect <- pcheck.logical(collect, varnm="collect",
		title="Collect tables?", first="YES", stopifnull=TRUE)

  ### Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata",
		title="Save data tables?", first="YES", stopifnull=TRUE)

  ## Check outfolder
  ########################################################
  if (savedata)
    outfolder <- pcheck.outfolder(outfolder)



  if (calc) {
    ## Create an empty list to hole return calc data
    calcdat <- list()

    ## Create a new folder inside outfolder to hold output calc data
    if (savedata) {
      calcfolder <- paste(outfolder, "calc", sep="/")
      if (!file.exists(calcfolder)) dir.create(calcfolder)
    }

    ## Define variables
    level1.vars.fia <- c(level_1_code, level_1_label)
    level1.vars.calc <- c("level_1_code")
    if (!is.null(level_1_label))
      level1.vars.calc <- c("level_1_code", "level_1_label")

    level2.vars.fia <- c(level_2_code, level_2_label)
    if (!is.null(level_2_label))
      level2.vars.calc <- c("level_2_code", "level_2_label")

    areavar.fia <- "ACRES"
    areavar.calc <- "level_2_area"
    strwtvar <- "stratum_area"


    #####################################################################################
    ## Compose calc-aois data - CALC  (unitarea)
    #####################################################################################

    ## level 2 - by county
    calc.aois.level2 <- stats::aggregate(unitarea[[areavar.fia]],
			unitarea[, c(level1.vars.fia, level2.vars.fia)], sum)
    names(calc.aois.level2) <- c(level1.vars.calc, level2.vars.calc, areavar.calc)
    print(utils::head(calc.aois.level2))

    calcdat$calc.aois.level2 <- data.table::setDF(calc.aois.level2)
    if (savedata)
      write2csv(calc.aois.level2, outfolder=calcfolder, outfilenm="calc-aois_level2")

    ## level 1 - by state
    calc.aois.level1 <- stats::aggregate(unitarea[[areavar.fia]], unitarea[, level1.vars.fia], sum)
    names(calc.aois.level1) <- c(level1.vars.calc, areavar.calc)
    print(utils::head(calc.aois.level1))

    calcdat$calc.aois.level1 <- data.table::setDF(calc.aois.level1)
    if (savedata)
      write2csv(calc.aois.level1, outfolder=calcfolder, outfilenm="calc-aois_level1")


    #####################################################################################
    ## Compose calc-stratum data (stratalut)
    ## - level1 (by state) and level2 (by county)
    #####################################################################################
    strlut <- data.table::setDT(merge(strlut,
	  	unitarea[, c("ESTN_UNIT", "ESTN_UNIT_DESCR", "STATE_NAME", "ACRES")],
		by="ESTN_UNIT"))
    strlut[strlut$STRATUMCD == 2, STRATUM_DESCR := "Brown"]


    ## level1 (by state)
    ####################################
    stratum.level1.vars.fia <- c(level1.vars.fia, "STRATUMCD", "STRATUM_DESCR")
    stratum.level1.vars.calc <- c(level1.vars.calc, "stratum_code", "stratum_label")

    calc.stratum.level1 <- strlut[, sum(P1POINTCNT), by=stratum.level1.vars.fia]
    calc.stratum.level1[, strwt:=prop.table(V1)]
    data.table::setnames(calc.stratum.level1, "V1", "P1POINTCNT")
    calc.stratum.level1[, AREA := strwt * sum(unitarea[[areavar.fia]])]

    calc.stratum.level1 <- calc.stratum.level1[, c(stratum.level1.vars.fia, "AREA"), with=FALSE]
    names(calc.stratum.level1) <- c(stratum.level1.vars.calc, strwtvar)
    print(calc.stratum.level1)

    calcdat$calc.stratum.level1 <- data.table::setDF(calc.stratum.level1)
    if (savedata)
      write2csv(calc.stratum.level1, outfolder=calcfolder, outfilenm="calc-stratum_level1")


    ## level2 (by county)
    ####################################
    stratum.level2.vars.fia <- c(level1.vars.fia, level2.vars.fia, "STRATUMCD", "STRATUM_DESCR")
    stratum.level2.vars.calc <- c(level1.vars.calc, level2.vars.calc, "stratum_code", "stratum_label")

    calc.stratum.level2 <- strlut[, sum(P1POINTCNT), by=stratum.level2.vars.fia]
    calc.stratum.level2[, strwt:=prop.table(V1), by=level2.vars.fia]
    data.table::setnames(calc.stratum.level2, "V1", "P1POINTCNT")
    calc.stratum.level2 <- merge(calc.stratum.level2,
		unitarea[, c(level2.vars.fia, areavar.fia)], by=level2.vars.fia)
    calc.stratum.level2[, (strwtvar) := strwt * get(areavar.fia)]
    calc.stratum.level2 <- calc.stratum.level2[, c(stratum.level2.vars.fia, strwtvar), with=FALSE]
    names(calc.stratum.level2) <- c(stratum.level2.vars.calc, strwtvar)
    print(utils::head(calc.stratum.level2))

    calcdat$calc.stratum.level2 <- data.table::setDF(calc.stratum.level2)
    if (savedata)
      write2csv(calc.stratum.level2, outfolder=calcfolder, outfilenm="calc-stratum_level2")


    returnlst$calcdat <- calcdat
  }


  if (collect) {
    ## Create an empty list to hole return calc data
    collectdat <- list()

    ## Create a new folder inside outfolder to hold output calc data
    if (savedata) {
      collectfolder <- paste(outfolder, "collect", sep="/")
      if (!file.exists(collectfolder)) dir.create(collectfolder)
    }

    #####################################################################################
    ## Create Sample Design data for Sampling point data - COLLECT
    #####################################################################################
    ## level1 - plt
    ## level2 - subplot
    ## x - LON_PUBLIC		NOTE: only 1 coordinate for cluster - center plot
    ## y - LAT_PUBLIC		NOTE: only 1 coordinate for cluster - center plot
    sampdesign <- unique(pltstrat[, c("STATECD", "ESTN_UNIT", "CN", "LON_PUBLIC", "LAT_PUBLIC")])
    names(sampdesign) <- c("level1_code", "level2_code", "level3_code", "x", "y")
    sampdesign$srs_id <- "EPSG:4326"
    print(utils::head(sampdesign))

    collectdat$sampdesign <- data.table::setDF(sampdesign)
    if (savedata)
      write2csv(sampdesign, outfolder=collectfolder, outfilenm="sampdesign")


    ## Create cluster table for stand table - COLLECT
    ####################################################################
    ## NOTE: cluster names must be lowercase
    puniqueid.fia <- "CN"
    puniqueid.calc <- "cn"

    pltvars <- c("UNITCD", "STATECD", "COUNTYCD", "INVYR", "MEASYEAR",
		"PLOT_STATUS_CD", "ESTN_UNIT", "STRATUMCD")
    plt <- pltstrat[, c(puniqueid.fia, pltvars)]

    pltnames <- pltvars
    names(plt) <- c(puniqueid.calc, tolower(pltnames))
    print(utils::head(plt))

    collectdat$plot <- data.table::setDF(plt)
    if (savedata)
      write2csv(plt, outfolder=collectfolder, outfilenm="plot")


    ## Create stand table for stand table - COLLECT
    ####################################################################
    puniqueid.calc2 <- "plot_cn"
    cuniqueid.fia <- c("PLT_CN", "CONDID")
    cuniqueid.calc <- c(puniqueid.calc2, "condid")

    condvars <- c("CONDPROP_UNADJ", "SUBPPROP_UNADJ", "MICRPROP_UNADJ",
		"COND_STATUS_CD", "OWNGRPCD", "FORTYPCD", "STDSZCD",
		"RESERVCD", "ADFORCD")
    condnames <- condvars

    cond <- cond[, c(cuniqueid.fia, condvars)]
    names(cond) <- c(cuniqueid.calc, tolower(condnames))

    domnames <- tolower(c("FORTYPCD", "OWNGRPCD", "STDSZCD", "RESERVCD", "ADFORCD"))
    cond[is.na(cond)] <- -1
    cond <- cond[cond[[puniqueid.calc2]] %in% plt[[puniqueid.calc]],]
    print(utils::head(cond))

    ## Define concatenated variable  (This is done through Collect)
    #cond$fortypcd_stdszcd <- paste(cond$fortypcd, cond$stdszcd, sep="_")

    collectdat$cond <- data.table::setDF(cond)
    if (savedata)
      write2csv(cond, outfolder=collectfolder, outfilenm="cond")


    ## Create tree data for tree table - COLLECT
    ####################################################################
    tuniqueid.fia <- c("PLT_CN", "CONDID")
    tuniqueid.calc <- c(puniqueid.calc2, "cond_condid")

    tree$tree_no <- paste(tree$SUBP, tree$TREE, sep="_")
    treevars <- c("tree_no", "STATUSCD", "SPCD", "DIA", "HT",
			"VOLCFNET", "TPA_UNADJ", "DRYBIO_AG")
    tree <- tree[, c(tuniqueid.fia, treevars)]
    names(tree) <- c(tuniqueid.calc, tolower(treevars))
    print(utils::head(tree))

    ## Define concatenated variable  (This is done through Collect)
    #tree <- merge(tree, cond[, c("plot_cn", "condid", "fortypcd")],
	#		by.x=c("plot_cn", "cond_condid"), by.y=c("plot_cn", "condid"))
    #tree$fortypcd_spcd <- paste(tree$fortypcd, tree$spcd, sep="_")

    collectdat$tree <- data.table::setDF(tree)
    if (savedata)
      write2csv(tree, outfolder=collectfolder, outfilenm="tree")


    ## Create seed data for seed table - COLLECT
    ####################################################################
    seed <- seed[order(seed$PLT_CN, seed$SUBP), ]
    seed$SEED <- sequence(tabulate(as.factor(paste(seed$PLT_CN, seed$SUBP))))
    seed$seed_no <- paste(seed$SUBP, seed$SEED, sep="_")
    seedvars <- c("seed_no", "SPCD", "TREECOUNT_CALC", "TOTAGE", "TPA_UNADJ")
    seed <- seed[, c(tuniqueid.fia, seedvars)]
    names(seed) <- c(tuniqueid.calc, tolower(seedvars))
    print(utils::head(seed))

    collectdat$seed <- data.table::setDF(seed)
    if (savedata)
      write2csv(seed, outfolder=collectfolder, outfilenm="seed")


    returnlst$collectdat <- collectdat
  }

  return(returnlst)
}

