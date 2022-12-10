#' Analysis - Get estimates for Green-Book module.
#'
#' Wrapper to generate estimates for each evalid in list for one or more
#' estimation variables and one or more estimation variable filters.
#'
#'
#' @param GBpopdatlst List of list. One or more population data objects
#' returned from modGBpop(), anGBgetpop_evallst() or anGBpop_evalcustom().
#' @param esttype String. Estimation type ('AREA', 'TREE', 'RATIO'). Only one
#' estimation type is allowed at a time.
#' @param estseed String. Use seedling data only or add to tree data. Seedling
#' estimates are only for counts (estvar='TPA_UNADJ')-('none', 'only', 'add').
#' @param landarea String. The condition-level filter for defining land area
#' ('ALL', 'FOREST', 'TIMBERLAND'). If landarea='FOREST', COND_STATUS_CD = 1;
#' if landarea='TIMBERLAND', SITECLCD in(1:6) & RESERVCD = 0.
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param estvarlst String vector. One or more tree-level estimate variable
#' (e.g., 'VOLCFNET').
#' @param estvar.filterlst String vector. One or more tree-level filter for
#' estvar.  Must be R syntax (e.g., 'STATUSCD == 1').
#' @param rowvar String. Optional. Name of domain variable to group estvar by
#' for rows in table output. Rowvar must be included in an input data frame
#' (i.e., plt, cond, tree). If no rowvar is included, an estimate is returned
#' for the total estimation unit. Include colvar for grouping by 2 variables.
#' @param colvar String. Optional. If rowvar != NULL, name of domain variable
#' to group estvar by for columns in table output. Colvar must be included in
#' an input data frame (i.e., plt, cond, tree).
#' @param sumunits Logical. If TRUE, sums estimation units to population.
#' @param savedata Logical. If TRUE, saves data to outfolder.
#' @param raw_dsn String. Data source name for rawdata output. If extension is
#' not included, out_fmt is used. Use full path if outfolder=NULL.
#' @param raw_fmt String. File format for rawdata output ('csv', 'sqlite',
#' 'db', 'sqlite3', 'db3', 'gpkg', 'gdb'). If out_fmt='gdb', must have ArcGIS
#' license and install arcgisbinding package.
#' @param outfolder String. The output folder path. If NULL and savedata=TRUE,
#' outfolder is the working directory.
#' @param outfn.pre String. The name used for prefix of outfiles (e.g.,
#' outfn.pre'_plt*').
#' @param outfn.date Logical. If TRUE, add date to end of outfile (e.g.,
#' outfn_'date'.csv).
#' @param overwrite_dsn Logical. If TRUE, overwrites out_dsn.
#' @param overwrite_layer Logical. If TRUE, overwrites layers in out_dsn or
#' files if out_fmt = 'csv'.
#' @param append_layer Logical. If TRUE, appends layers to existing out_dsn or
#' files if out_fmt = 'csv'. Note: currently cannot append layers if out_fmt =
#' "gdb".
#' @param title.filterlst String vector. A title corresponding to
#' estvar.filterlst.
#' @param ...  If GBpopdatlst=NULL, parameters for anGBest_eval().
#' @return The output area organized by estimation type and estimation
#' variable.\cr
#'
#' If savedata=TRUE, data are export to specified format (out_fmt).
#' @author Tracey S. Frescino
#' @keywords data
#' @examples
#'
#'
#'   ## Get population data for
#'  # GBpoplst <- anGBgetpop_evallst(evalEndyrlst=c(2015:2019), states=c("AZ","NM"),
#' 	# evalType="VOL")
#'
#'   ## Get area estimates by forest type and stand-size class
#'  # GBestarea <- anGBest_batch(GBpopdatlst=GBpoplst, esttype="AREA",
#' 	# rowvar="FORTYPCD", colvar="STDSZCD", savedata=FALSE)
#'  # names(GBestarea)
#'  # names(GBestarea$AREA)
#'
#'   ## Look at area estimates for totals, row totals, and groups
#'  # GBestarea$AREA$totest
#'  # head(GBestarea$AREA$rowest)
#'  # head(GBestarea$AREA$grpest)
#'
#'
#'   ## Get tree estimates for volume, basal area, number of trees, biomass and carbon
#'   ##    for live trees and standing dead trees by forest type
#'  # GBesttree <- anGBest_batch(GBpopdatlst=GBpoplst, esttype="TREE",
#' 	# estvarlst=c("VOLCFNET", "BA", "TPA_UNADJ", "DRYBIO_AG", "CARBON_AG"),
#' 	# estvar.filterlst=c("STATUSCD==1", "STATUSCD == 2 & STANDING_DEAD_CD == 1 & DIA >= 5.0"),
#' 	# rowvar="FORTYPCD",
#' 	# savedata=FALSE, title.filterlst=c("live", "standing_dead"))
#'  # names(GBest)
#'  # names(GBest$VOLCFNET_live_FOREST)
#'
#'   ## Look at tree estimates for totals and row totals for live cuft volume on forest land
#'  #  GBest$VOLCFNET_live_FOREST$totest
#'  # GBest$VOLCFNET_live_FOREST$rowest
#'
#' @export
anGBpoplst_batch <- function(GBpopdatlst = NULL, 
                          esttype = "AREA", 
                          estseed = "none", 
                          landarea = "FOREST", 
                          pcfilter = NULL, 
                          estvarlst = NULL, 
                          estvar.filterlst = NULL, 
                          rowvar = NULL, 
                          colvar = NULL, 
                          sumunits = TRUE, 
                          savedata = FALSE, 
                          raw_dsn = "estdat", 
                          raw_fmt = "sqlite", 
                          outfolder = NULL, 
                          outfn.pre = NULL, 
                          outfn.date = FALSE, 
                          overwrite_dsn = FALSE, 
                          overwrite_layer = TRUE, 
                          append_layer = FALSE, 
                          title.filterlst = NULL, 
                          ...) {
  ## DESCRIPTION: estimates for each evalid in list

  ## Set global variables
  tree=seed=seed_layer=unitvar2 <- NULL
  gui <- FALSE
  istree=FALSE
  rawonly <- TRUE
  RS=NULL
  GBpop_evalEndyrlst <- list()
  if (sumunits) {
    exportlst <- c("totest", "rowest", "colest", "grpest")
  } else {
    exportlst <- c("unit_totest", "unit_rowest", "unit_colest", "unit_grpest")
  }

  ## Check esttype
  ########################################################
  esttypelst <- c("AREA", "TREE", "RATIO")
  esttype <- pcheck.varchar(var2check=esttype, varnm="esttype", 
                            checklst=esttypelst, 
                            caption="Estimation type", stopifnull=TRUE)
  if (esttype == "AREA") {
    estvarlst <- "AREA"
    estvar.filterlst <- NULL
  }

  ## Check savedata
  savedata <- pcheck.logical(savedata, varnm="savedata",
		title="Save data extraction?", first="NO", gui=gui)

  ## If savedata, check output file names
  ################################################################
  if (savedata) {
    outlst <- pcheck.output(out_dsn=raw_dsn, out_fmt=raw_fmt, 
                            outfolder=outfolder, outfn.date=outfn.date, 
                            overwrite_dsn=overwrite_dsn, 
                            overwrite_layer=overwrite_layer, 
                            append_layer=append_layer, 
                            createSQLite=FALSE, gui=gui)
    out_dsn <- outlst$out_dsn
    outfolder <- outlst$outfolder
    out_fmt <- outlst$out_fmt
    overwrite_layer <- outlst$overwrite_layer
    overwrite_dsn <- outlst$overwrite_dsn
    append_layer <- outlst$append_layer

    if (out_fmt == "csv") {
      rawfolder <- paste(outfolder, "rawdata", sep="/")
      if (!file.exists(rawfolder)) dir.create(rawfolder)
    } else {
      rawfolder <- outfolder
    }
  }

  ## If population data
  ################################################################
  if (is.null(GBpopdatlst)) {
    GBpopdatlst <- anGBgetpop_evallst(...)
    names(GBpopdatlst)
  }


  ## check estvarlst
  ################################################################
  nbrestvar <- length(estvarlst)
  if (!is.null(title.filterlst) && length(title.filterlst) < nbrestvar) {
    title.filterlst <- rep(title.filterlst, nbrestvar)
  }
  #if (!is.null(estvar.filterlst) && length(estvar.filterlst) < nbrestvar) {
  #  estvar.filterlst <- rep(estvar.filterlst, nbrestvar)
  #}
  if (is.null(estvar.filterlst)) {
    estvar.filterlst <- "NONE"
  }


  ####################################################################
  ## Get estimates
  ####################################################################
  GBest <- list()

  states <- names(GBpopdatlst)
  for (st in 1:length(states)) {
    state <- states[st]
    GBpopdat <- GBpopdatlst[[state]]
    evalidst <- GBpopdat$evalid
    stabbr <- pcheck.states(state, statereturn="ABBR")
    layer.pre <- NULL
    if (st > 1) {
      append_layer <- TRUE
    }

    for (i in 1:length(evalidst)) {
      eval <- evalidst[i]
      outfn.pre <- paste0(stabbr, "_", eval)

      for (j in 1:length(estvarlst)) {
        msg <- paste0("getting estimates for ", state, ", evaluation ", eval, "...")
        estvar <- estvarlst[j]
        estvarnm <- ifelse(estvar == "TPA_UNADJ", "COUNT", 
                           ifelse(estvar == "CONDPROP_UNADJ", "AREA", estvar))
        for (k in 1:length(estvar.filterlst)) {
          msg2 <- paste(msg, estvar)
          estvar.filter <- estvar.filterlst[k]
          if (estvar.filter != "NONE") {
            title.filter <- title.filterlst[k]
            if (is.null(title.filter)) {
              title.filter <- gsub(" ", "", estvar.filter)
            }
            msg2 <- paste0(msg2, " - ", title.filter, "...")
            layer.pre <- paste(estvarnm, title.filter, landarea, sep="_")
          } else {
            layer.pre <- paste(estvarnm)
          }
          message(msg2)

          if (esttype == "AREA") {
            GBestdat <- tryCatch(
		            modGBarea(GBpopdat = GBpopdat, 
		                      landarea = landarea, 
		                      pcfilter = pcfilter, 
		                      rowvar = rowvar, colvar = colvar, 
		                      table_opts = list(row.FIAname = TRUE, 
		                                        col.FIAname = TRUE,
		                                        rawonly = rawonly), 
		                      savedata = FALSE, 
		                      returntitle = TRUE),
				        error=function(err) {
					          message(err,"\n")
					      return(NULL)
				} )
          } else if (esttype == "TREE") {
            GBestdat <- tryCatch(
		            modGBtree(GBpopdat = GBpopdat, 
		                     landarea = landarea, 
		                     pcfilter = pcfilter, 
		                     estvar = estvar, estvar.filter = estvar.filter, 
		                     rowvar = rowvar, colvar = colvar, 
		                     table_opts = list(row.FIAname = TRUE, 
		                                       col.FIAname = TRUE,
		                                       rawonly = rawonly), 
		                     savedata = FALSE, 
		                     returntitle = TRUE, 
		                     title_opts = list(title.filter = title.filter)),
				        error=function(err) {
					          message(err,"\n")
					      return(NULL)
				} )
          } else if (esttype == "RATIO") {
            GBestdat <- tryCatch(
                modGBratio(GBpopdat = GBpopdat, 
                          landarea = landarea, 
                          pcfilter = pcfilter, 
                          estvarn = estvar, estvarn.filter = estvar.filter, 
                          rowvar = rowvar, colvar = colvar, 
                          table_opts = list(row.FIAname = TRUE, 
                                            col.FIAname = TRUE,
                                            rawonly = rawonly), 
                          savedata = FALSE, 
                          returntitle = TRUE, 
                          title_opts = list(title.filter = title.filter)),
                error=function(err) {
					          message(err,"\n")
					      return(NULL)
				} )
          }
          exportlst <- exportlst[exportlst %in% names(GBestdat$raw)]
          if (length(exportlst) == 0) {
            stop("no tables to export")
          }
          for (tabnm in exportlst) {
            tab <- data.table::data.table(EVALID=eval[[1]], GBestdat$raw[[tabnm]])
            if (savedata) {
              datExportData(tab, 
			savedata_opts = list(outfolder = rawfolder,
                           out_fmt = out_fmt, out_dsn = out_dsn,
                           overwrite_dsn = overwrite_dsn,
                           overwrite_layer = overwrite_layer,
                           append_layer = append_layer,
                           layer.pre = layer.pre, out_layer = tabnm))
            } else {
              GBest[[layer.pre]][[tabnm]] <-
				data.table::rbindlist(list(GBest[[layer.pre]][[tabnm]], tab))
            }
          }
        }  ## k loop - estvar.filter
      }  ## j loop - estvar
    } ## i loop
  }
  return(GBest)
}
