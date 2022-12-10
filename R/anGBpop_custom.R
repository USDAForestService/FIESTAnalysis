#' Analysis - Generate estimates for custom-defined boundary.
#'
#' Wrapper to get plot data, stratification data and estimates within a
#' custom-defined boundary.
#'
#'
#' @param GBpopdat R list object. FIA population data and calculations for
#' estimation.
#' @param esttype String. Type of estimation ("AREA", "TREE", "RATIO").
#' @param estseed String. What to do with seedling information ("none", "only",
#' "add").
#' @param landarea String. The sample area filter for estimates ('ALL',
#' 'FOREST', 'TIMBERLAND').  If landarea=FOREST, filtered to COND_STATUS_CD =
#' 1; If landarea=TIMBERLAND, filtered to SITECLCD in(1:6) and RESERVCD = 0.
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param estvar String. Name of the tree estimate variable.
#' @param estvar.filter String. A tree filter for the estimate variable. Must
#' be R syntax (e.g,. "STATUSCD == 1").
#' @param rowvar String. Name of row domain variable in cond or tree. If
#' rowvar=NULL, only total estimates are output. If rowvar="DIACL",
#' treedia.brks are used for class breaks.
#' @param colvar String. Name of column domain variable in cond or tree. If
#' colvar="DIACL", treedia.brks are used for class breaks.
#' @param treedia.brks Integer vector. Vector of breaks to define diameter
#' classes (e.g., c(0,5,10,20,50,100). Define rowvar or colvar = "DIACL".
#' @param sumunits Logical. If TRUE, estimation units are summed and returned
#' in one table.
#' @param divideby String. Conversion number for output ('hundred', 'thousand',
#' 'million').
#' @param title.ref String. Table title for reference.
#' @param title.main String. Main title for table and barplot.
#' @param getbarplot Logical. If TRUE, generates a barplot of table rows or
#' columns.
#' @param barplot.row Logical. If TRUE, generate barplot based on table rows.
#' If FALSE, columns are used.
#' @param barplot.ord String. The order of x based on y values ('DESC', "ASC')
#' for barplot. If NULL, the default order of the table is used.
#' @param barplot.color String. The base color palette for barplot ('rainbow',
#' 'heat', 'terrain', 'topo', 'cm', 'hcl1', 'hcl2', 'BrewerDark2',
#' 'BrewerPaired', 'BrewerBlues'). Note: All colors beginning with 'Brewer' are
#' color palettes from the RColorBrewer package.
#' @param barplot.ylim Number. A vector of min and max values, c(min,max) for
#' the barplot y axis. If NULL, defaults to maximum y value plus the standard
#' error.
#' @param barplot.nplt Logical. If TRUE, Adds number of plots on top of bar.
#' @param savedata Logical. If TRUE, saves data objects to outfolder.
#' @param outfolder String. If savedata=TRUE or exportsp=TRUE, name of output
#' folder.  If NULL, the working directory is used.
#' @param outfn.pre String. Add a prefix to output name (e.g., "01").
#' @param outfn.date Logical. If TRUE, adds current date to outfile name.
#' @param overwrite Logical. If TRUE, overwrites output table and barplot.
#' @param ...  Parameters for modGB*() functions.
#' @return Estimate and barplot (if getbarplot=TRUE).
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anGBpop_custom <- function(GBpopdat, 
                           esttype = "TREE", 
                           estseed = "none", 
                           landarea = "FOREST",
                           pcfilter = NULL, 
                           estvar = NULL, 
                           estvar.filter = NULL,
                           rowvar = NULL, 
                           colvar = NULL, 
                           treedia.brks = c(0,5,10,20,50,100), 
                           sumunits = TRUE,
                           divideby = NULL, 
                           title.ref = NULL, 
                           title.main = NULL, 
                           getbarplot = FALSE,
                           barplot.row = TRUE, 
                           barplot.ord = NULL, 
                           barplot.color = NULL, 
                           barplot.ylim = NULL,
                           barplot.nplt = FALSE, 
                           savedata = FALSE, 
                           outfolder = NULL, 
                           outfn.pre = NULL,
                           outfn.date = FALSE, 
                           overwrite = FALSE, 
                           ...) {


  ## Set global variables
  gui <- FALSE
  returnlst <- list()
  row.FIAname=col.FIAname <- TRUE
  gettable <- FALSE


  ## Check esttype
  ########################################################
  esttypelst <- c("AREA", "TREE", "RATIO")
  esttype <- pcheck.varchar(var2check=esttype, varnm="esttype",
		checklst=esttypelst, caption="Estimation type", stopifnull=TRUE)


  ## Check gettable
  gettable <- pcheck.logical(gettable, varnm="gettable",
		title="Create kable table?", first="YES", gui=gui)

  ## Check getbarplot
  getbarplot <- pcheck.logical(getbarplot, varnm="getbarplot",
		title="Create barplot?", first="YES", gui=gui)

  if (getbarplot) {
    toplabelvar <- NULL

    ## Check barplot.row
    barplot.row <- pcheck.logical(barplot.row, varnm="barplot.row",
		title="Rows for barplot?", first="NO", gui=gui)
    if (!barplot.row && is.null(colvar))
      stop("must include colvar if barplot.row=FALSE")

    ## Check barplot.nplots
    barplot.nplt <- pcheck.logical(barplot.nplt, varnm="barplot.nplt",
		title="Add number of plots?", first="NO", gui=gui)
  }

  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata",
		title="Save data extraction?", first="NO", gui=gui)

  ## Check outfolder
  ########################################################
  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder=outfolder, gui=gui)
  }


  ###########################################################################
  ## Extract FIA data and model data
  ###########################################################################
  if (is.null(GBpopdat))
    stop("must include population data - anGBpop()")


  ## Add variable for tree diameter class
  if (esttype != "AREA") {
    if ((!is.null(rowvar) && rowvar == "DIACL") || (!is.null(colvar) && colvar == "DIACL")) {
      ## Check treedia.brks
      if (!is.null(treedia.brks)) {
        datlut <- datLUTclass(x=GBpopdat$treex, xvar="DIA", cutbreaks=treedia.brks)
        GBpopdat$treex <- datlut$xLUT

        print(table(GBpopdat$treex$DIACL))
        if (rowvar == "DIACL") row.FIAname <- FALSE
        if (!is.null(colvar) && colvar == "DIACL") col.FIAname <- FALSE
      }
    }
  }

  if (esttype == "AREA") {
    ####################################################################
    ## Get estimates
    ####################################################################
    MODest <- modGBarea(GBpopdat=GBpopdat, landarea=landarea,
		pcfilter=pcfilter,
		rowvar=rowvar, row.FIAname=row.FIAname, colvar=colvar,
		col.FIAname=col.FIAname, sumunits=sumunits, rawdata=TRUE,
		returntitle=TRUE, title.ref=title.ref, savedata=savedata,
		outfolder=outfolder, overwrite_layer=overwrite, outfn.pre=outfn.pre,
		outfn.date=outfn.date, divideby=divideby, ...)

  } else if (esttype == "TREE") {
    ####################################################################
    ## Get estimates
    ####################################################################
    MODest <- modGBtree(GBpopdat=GBpopdat, estseed=estseed, landarea=landarea,
		pcfilter=pcfilter,
		estvar=estvar, estvar.filter=estvar.filter,
		rowvar=rowvar, row.FIAname=row.FIAname, colvar=colvar,
		col.FIAname=col.FIAname, sumunits=sumunits, rawdata=TRUE,
		returntitle=TRUE, title.ref=title.ref, savedata=savedata,
		outfolder=outfolder, overwrite_layer=overwrite, outfn.pre=outfn.pre,
 		outfn.date=outfn.date, divideby=divideby, ...)

  } else if (esttype == "RATIO") {
    MODest <- modGBratio(GBpopdat=GBpopdat, estseed=estseed, landarea=landarea,
		pcfilter=pcfilter,
		estvarn=estvar, estvarn.filter=estvar.filter,
		rowvar=rowvar, row.FIAname=row.FIAname, colvar=colvar,
		col.FIAname=col.FIAname, sumunits=sumunits, rawdata=TRUE,
		returntitle=TRUE, title.ref=title.ref, savedata=savedata,
		outfolder=outfolder, overwrite_layer=overwrite, outfn.pre=outfn.pre,
 		outfn.date=outfn.date, divideby=divideby, ...)
  }
  est <- MODest$est

  if (!is.null(colvar))
    pse <- MODest$pse
  raw <- MODest$raw
  titlelst <- MODest$titlelst


  if (gettable) {
    kable.table(est=est, title.row=titlelst$title.row,
            title.rowvar=titlelst$title.rowvar)
  }


  ####################################################################
  ## Get barplot
  ####################################################################
  if (getbarplot) {
    anMOD_barplot(MODest=MODest, barplot.row=barplot.row,
		barplot.ord=barplot.ord, barplot.color=barplot.color,
		barplot.ylim=barplot.ylim, barplot.nplt=barplot.nplt,
		savedata=savedata, outfolder=outfolder, outfn.pre=outfn.pre,
		outfn.date=outfn.date, overwrite=overwrite,
		title.ref=title.ref, title.main=title.main, divideby=divideby)
  }

  returnlst$est <- est
  if (!is.null(colvar))
    returnlst$pse <- pse
  returnlst$raw <- raw
  returnlst$titlelst <- titlelst

  return(returnlst)
}

