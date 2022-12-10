#' Analysis - Generate estimates for custom-defined boundary.
#'
#' Wrapper to get plot data, stratification data and estimates within a
#' custom-defined boundary.
#'
#'
#' @param MODest R list object. FIA estimation data from mod*est() functions.
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
#' @param overwrite Logical. If TRUE and exportshp=TRUE, overwrite files in
#' outfolder.
#' @param title.ref String. Table title for reference.
#' @param title.main String. Main title for table and barplot.
#' @param divideby String. Conversion number for output ('hundred', 'thousand',
#' 'million').
#' @param ...  Added parameters to datBarplot().
#' @return Estimate.
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anMOD_barplot <- function(MODest, 
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
                          title.ref = NULL, 
                          title.main = NULL, 
                          divideby = NULL, 
                          ...) {


  ## Set global variables
  gui <- FALSE
  ref_titles <- ref_titles
  returnlst <- list()
  toplabelvar <- NULL
  barplot.tot <- FALSE


  ## Check GBest
  ########################################################
  if (!all(c("est", "raw", "titlelst") %in% names(MODest))) {
    stop("MODest is invalid")
  }


  ## Get data from MODest
  est <- MODest$est
  pse <- MODest$pse
  raw <- MODest$raw
  esttype <- raw$esttype
  rowvar <- raw$rowvar
  colvar <- raw$colvar
  if (esttype == "TREE") {
    estvar <- raw$estvar
  }
  if (esttype == "RATIO") {
    estvar <- raw$estvarn
  }
  titlelst <- MODest$titlelst


  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata",
		title="Save data extraction?", first="NO", gui=gui)

  ## Check overwrite, outfn.date, outfolder, outfn
  ########################################################
  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder, gui=gui)
    
    outfn.date <- pcheck.logical(outfn.date , varnm="outfn.date", 
                      title="Add date to outfiles?", first="NO", gui=gui)
    
    overwrite <- pcheck.logical(overwrite, varnm="overwrite", 
                      title="Overwrite?", first="NO", gui=gui)

    if (!is.null(outfn.pre)) {
      outfolder <- file.path(outfolder, outfn.pre)
      if (!dir.exists(outfolder)) dir.create(outfolder)
    }
  }

  ## Check barplot.row
#  barplot.row <- pcheck.logical(barplot.row, varnm="barplot.row",
#		title="Rows for barplot?", first="NO", gui=gui)

  ## Check barplot.nplots
  barplot.nplt <- pcheck.logical(barplot.nplt, varnm="barplot.nplt",
		title="Add number of plots?", first="NO", gui=gui)


  ####################################################################
  ## Get barplot
  ####################################################################
  estcol <- ifelse(esttype == "RATIO", "rhat", "est")
  secol <- ifelse(esttype == "RATIO", "rhat.se", "est.se")
  if (esttype != "RATIO" && !is.null(divideby)) {
    estcol <- paste0("est.", divideby)
    secol <- paste0("est.se.", divideby)
  }

  ## Check barplot.ord
  ########################################################
  orderlst <- c('DESC', 'ASC')
  barplot.ord <- pcheck.varchar(var2check=barplot.ord, varnm="barplot.ord",
		checklst=orderlst, caption="Barplot order")

  ## Check barplot.color
  ########################################################
  colorlst <- c('rainbow', 'heat', 'terrain', 'topo', 'cm', 'hcl1', 'hcl2',
	'BrewerDark2', 'BrewerPaired', 'BrewerBlues')
  barplot.color <- pcheck.varchar(var2check=barplot.color, varnm="barplot.color",
		checklst=colorlst, caption="Barplot color")

  ## Check barplot.row
  if (!is.null(colvar) && colvar != "NONE") {
    barplot.row <- pcheck.logical(barplot.row, varnm = "barplot.row",
        	title = "Row values?", first = "YES", stopifnull = TRUE)
  } else if (rowvar == "TOTAL") {
    barplot.tot <- TRUE
  } else {
    barplot.row <- TRUE
  }

  if (barplot.tot) {
    xvar <- titlelst$title.unitvar
    if (!is.null(raw$totest)) {
      btab <- raw$totest
    } else {
      if ("dunit_totest" %in% names(raw)) {
        btab <- raw$dunit_totest
      } else {
        btab <- raw$unit_totest
      }
    }
  } else if (barplot.row) {
    xvar <- titlelst$title.rowvar
    if (!is.null(raw$rowest)) {
      btab <- raw$rowest
    } else {
      btab <- raw$unit_rowest
    }
  } else {
    xvar <- titlelst$title.colvar
    if (!is.null(raw$colest)) {
      btab <- raw$colest
    } else {
      btab <- raw$unit_colest
    }
  }

  btab.cols <- c(xvar, estcol, secol)
  if ("NBRPLT.gt0" %in% names(btab)) {
    btab.cols <- c(btab.cols, "NBRPLT.gt0")
  }
  bpest <- btab[, btab.cols]


  xvar <- names(bpest)[1]
  nbrx <- nrow(bpest)

  if (esttype %in% "AREA") {
    ylabel <- "Area - Acres"
    if (!is.null(divideby))
      ylabel <- paste0(ylabel, ", in ", divideby, "s")
    bplotfn <- paste0(esttype, "_", rowvar, "_barplot")

  } else {
    if (estvar %in% ref_titles$DOMVARNM) {
      ylabel <- ref_titles[ref_titles$DOMVARNM == estvar, "DOMTITLE"]
    } else {
      ylabel <- estvar
    }

    if (esttype == "RATIO") {
      ylabel <- paste(ylabel, "per acre")
    } else if (!is.null(divideby)) {
      ylabel <- paste0(ylabel, ", in ", divideby, "s")
    }
    bplotfn <- paste0(esttype, "_", estvar, "_", rowvar, "_barplot")
  }


  if (!is.null(barplot.color) &&
	barplot.color %in% c("BrewerDark2", "BrewerPaired", "BrewerBlues")) {
    if (!"RColorBrewer" %in% rownames(utils::installed.packages())) {
      message("RColorBrewer package is required using Brewer colors")
    }
  }


  if (is.null(barplot.color)) {
    bplot.col <- NULL
  } else if (barplot.color == "rainbow") {
    bplot.col <- grDevices::rainbow(nbrx)
  } else if (barplot.color == "heat") {
    bplot.col <- grDevices::heat.colors(nbrx)
  } else if (barplot.color == "terrain") {
    bplot.col <- grDevices::terrain.colors(nbrx)
  } else if (barplot.color == "topo") {
    bplot.col <- grDevices::topo.colors(nbrx)
  } else if (barplot.color == "cm") {
    bplot.col <- grDevices::cm.colors(nbrx)
  } else if (barplot.color == "hcl1") {
    bplot.col <- grDevices::hcl.colors(nbrx)
  } else if (barplot.color == "hcl2") {
    bplot.col <- grDevices::hcl.colors(nbrx, "Set 2")
  } else if (barplot.color == "BrewerDark2") {
    bplot.col <- RColorBrewer::brewer.pal(nbrx, name="Dark2")
  } else if (barplot.color == "BrewerPaired") {
    bplot.col <- RColorBrewer::brewer.pal(nbrx, name="Paired")
  } else if (barplot.color == "BrewerBlues") {
    bplot.col <- RColorBrewer::brewer.pal(nbrx, name="Blues")
  } else {
    bplot.col <- NULL
  }

  if (is.null(title.main)) {
    main <- wraptitle(titlelst$title.row, 60)
  } else {
    main <- wraptitle(title.main, 60)
  }

  if (barplot.nplt) toplabelvar <- "NBRPLT.gt0"
  datBarplot(x=bpest, xvar=xvar, yvar=estcol, errbars=TRUE, sevar=secol,
		savedata=savedata, outfolder=outfolder, x.order=barplot.ord,
		outfn=bplotfn, outfn.date=outfn.date, overwrite=overwrite,
		device.height=7, las.xnames=2, ylabel=ylabel, col=bplot.col,
		main=main, ylim=barplot.ylim, toplabelvar=toplabelvar, ...)

}

