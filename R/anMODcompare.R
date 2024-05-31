#' Analysis - Compare estimates for custom-defined boundary.
#'
#' Wrapper to compare estimates from FIESTA's SAmodule, MAmodule, 
#' and GBmodule.
#'
#' @param bnd sf R object or String. Area of interest (population
#' boundary or estimation unit). Can be a spatial polygon object, full pathname
#' to a shapefile, or name of a layer within a database.
#' @param bnd_dsn String. Data source name (dsn; e.g., sqlite or shapefile
#' pathname) of boundary. The dsn varies by driver. See gdal OGR vector formats
#' (https://www.gdal.org/ogr_formats.html). Optional if bnd is sf object.
#' @param bnd.att String. The attribute in bnd_layer that defines the
#' estimation unit(s). If NULL, bnd is default as one estimation unit.multestvar
#' @param bnd.filter String. Filter to subset bnd SpatialPolygons layer.
#' @param largebnd.unique String. The FIESTAnalysis::ecomap layer is used
#' as default for the borrowing large extent. The unique id can be
#' 'SECTION', 'PROVINCE', or NULL.
#' @param strvar String. Name of strata variable. Note. Must be in rastlst.cat. 
#' @param landarea String. The sample area filter for estimates ('ALL',
#' 'FOREST', 'TIMBERLAND').  If landarea=FOREST, filtered to COND_STATUS_CD =
#' 1; If landarea=TIMBERLAND, filtered to SITECLCD in(1:6) and RESERVCD = 0.
#' @param pcfilter String. A filter for plot or cond attributes (including
#' pltassgn).  Must be R logical syntax.
#' @param estvarlst String vector. One or more tree-level estimate variable
#' (e.g., 'VOLCFNET').
#' @param tfilterlst String vector. One or more tree filters ('live', 'dead').
#' @param intensity1 Logical. If TRUE, uses only INTENSITY=1 (i.e., P2 plots)
#' for GB and MA estimates, to assure an equal probability sample.
#' @param forprop Logical. If TRUE, adds forest estimates of forest proportion.
#' @param totals Logical. If TRUE, sums per acre values to total values by 
#' by multiplying by total acres.
#' @param pse Logical. Add pse columns to table.
#' @param estround Integer. Number of digits to round estimates and estimates.se.
#' @param byeach Logical. If TRUE, creates an SAdom for each smallbnd polygon.
#' @param prednames String vector. One or more predictors to use for estimation. 
#' Names must match names in SAdatalst$prednames
#' @param modelselect Logical. If TRUE, model selection occurs. 
#' @param showsteps Logical. If TRUE, intermediate steps of selection process
#' are displayed.
#' @param savedata Logical. If TRUE, save SAdoms spatial layer to outfolder.
#' @param savesteps Logical. If TRUE, save steps spatial intermediate layers
#' and JPG images. All spatial layers are output as *.shp format in a separate
#' folder (SAdoms_steps).
#' @param saveobj Logical. If TRUE, save objects.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options. Only used when savedata = TRUE. 
#' @param SAdomdat List object. Output from spGetSAdoms(). 
#' @param SAdatalst List object. Output from anGetData_list(). 
#' @param ... Parameters for input to anGetData_list().
#' @return Estimates
#' @author Tracey S. Frescino
#' @keywords data
#' @export
anMODcompare <- function(bnd = NULL, 
                         bnd_dsn = NULL, 
                         bnd.att = NULL, 
                         bnd.filter = NULL, 
                         largebnd.unique = "SECTION",
                         strvar, 
                         landarea = "FOREST", 
                         pcfilter = NULL, 
                         estvarlst = c("BA", "VOLCFNET", "VOLBFNET", "TPA_UNADJ", "DRYBIO_AG", "CARBON_AG"),
                         tfilterlst = "live",
						             intensity1 = TRUE,
						             forprop = TRUE,
						             totals = FALSE,
						             pse = FALSE,
						             estround = 2,
                         byeach = FALSE, 
                         prednames = NULL,
                         modelselect = TRUE,
                         showsteps = FALSE,
                         savedata = FALSE, 
                         savesteps = FALSE, 
                         saveobj = FALSE, 
                         savedata_opts = NULL, 
                         SAdomdat = NULL,
                         SAdatalst = NULL,
                         ...) {

  ## Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- unique(c(names(formals(anMODcompare)), names(formals(anGetData_list)), 
			names(formals(anGetData)), names(formals(spGetPlots)), 
			names(formals(spGetAuxiliary))))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check parameter lists
  pcheck.params(input.params, savedata_opts=savedata_opts)
  
  ## Set savedata defaults
  savedata_defaults_list <- formals(savedata_options)[-length(formals(savedata_options))]
  
  for (i in 1:length(savedata_defaults_list)) {
    assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  }
  
  ## Set user-supplied savedata values
  if (length(savedata_opts) > 0) {
    if (!savedata) {
      message("savedata=FALSE with savedata parameters... no data are saved")
    }
    for (i in 1:length(savedata_opts)) {
      if (names(savedata_opts)[[i]] %in% names(savedata_defaults_list)) {
        assign(names(savedata_opts)[[i]], savedata_opts[[i]])
      } else {
        stop(paste("Invalid parameter: ", names(savedata_opts)[[i]]))
      }
    }
  }

  ## Check strvar
  if (!exists("strvar") || is.null(strvar) || !is.character(strvar) || strvar == "") {
    stop("invalid strvar")
  }

  ## Get modeling domains
  ##############################################################
  if (is.null(SAdomdat)) {
    if (is.null(bnd)) {
	    stop("must include smallbnd")
	  }
    SAdomdat <- spGetSAdoms(smallbnd = bnd, 
                          smallbnd_dsn = bnd_dsn,
                          smallbnd.unique = bnd.att, 
                          smallbnd.filter = bnd.filter,
                          helperbnd = FIESTAnalysis::ecomap, 
                          helperbnd.unique = "SUBSECTION",
                          largebnd = ecomap, 
                          largebnd.unique = largebnd.unique,
                          showsteps = showsteps, 
                          savesteps = savesteps, 
                          savedata = savedata, 
                          savedata_opts = list(outfolder=outfolder),
                          helper_autoselect = TRUE, 
                          maxbnd.threshold = 1,
                          addstate = TRUE,
                          multiSAdoms = TRUE,
                          byeach = byeach)
    #names(SAdomdat$SAdomlst$SAbnd)
    #head(SAdomdat$SAdomlst$SAbnd)
    if (saveobj) {
      message("saving SAdomdat to ", file.path(outfolder, "SAdomdat.rds"))
      saveRDS(SAdomdat, file.path(outfolder, "SAdomdat.rds"))
    }
  }
  smallbnd.domain <- SAdomdat$smallbnd.domain
  
  if (is.null(SAdatalst)) {
    SAdatalst <- anGetData_list(SAdomdat$SAdomlst, 
                              showsteps=showsteps, ...)
    if (saveobj) {
      message("saving SAdatalst to ", file.path(outfolder, "SAdomdat.rds"))
      saveRDS(SAdatalst, file.path(outfolder, "SAdatalst.rds"))
    }
  }
  #names(SAdatalst)
 
  ####################################################################
  ## Get population data for SA
  ####################################################################
  message("generating population data for SA...\n")

  ## Small area - population - All
  SApopdatlst <- lapply(names(SAdatalst), function(x)
                  modSApop(SAdata = SAdatalst[[x]], 
                           smallbnd = SAdomdat$smallbndlst[[x]],
                           smallbnd.domain = SAdomdat$smallbnd.domain,
                           prednames = prednames,
						               unit_opts = list(minplotnum.unit = 2)))
  names(SApopdatlst) <- names(SAdatalst)
#head(SApopdatlst[[1]]$dunitlut)

  ####################################################################
  ## Get population data for GB and MA
  ####################################################################
  message("generating population data for GB and MA...\n")
  
  if (intensity1) {
    popFilter <- list(AOIonly = TRUE, intensity = 1)
  } else {
    popFilter <- list(AOIonly = TRUE)
  }  
 
  ## Get population data for Model-Assisted estimates
  MApopdatlst <- lapply(names(SAdatalst), function(x)
                  modMApop(MAdata = SAdatalst[[x]], 
                           popFilter = popFilter,
						               unit_opts = list(minplotnum.unit = 2)))
  names(MApopdatlst) <- names(SAdatalst)
#head(MApopdatlst[[1]]$unitlut)
 
  ## Get population data for Green-Book estimates - no strata
  GBpopdatlst1 <- lapply(names(SAdatalst), function(x)
                  modGBpop(GBdata = SAdatalst[[x]], 
                           popFilter = popFilter,
						               strata = FALSE,
						               adj = "plot",
						               unit_opts = list(minplotnum.unit = 2)))
  names(GBpopdatlst1) <- names(SAdatalst)

  ## Get population data for Green-Book estimates - with strata
  GBpopdatlst2 <- lapply(names(SAdatalst), function(x)
                  modGBpop(GBdata = SAdatalst[[x]], 
                           popFilter = popFilter,
						   strata = TRUE,
						   strvar = strvar,
						   adj = "plot",
						   unit_opts = list(minplotnum.unit = 2)))
  names(GBpopdatlst2) <- names(SAdatalst)
#head(GBpopdatlst[[1]]$stratalut)
 
  ####################################################################
  ## Get estimates
  ####################################################################
  multest <- list()
  predselect <- list()
  output <- list()
  multestvarlst <- {}
  unitvar <- "DOMAIN"

  MAmethod = "greg"
  estcols1 <- c(unitvar, "nhat", "nhat.var", "NBRPLT.gt0", "AREAUSED")
  estcols2 <- c(unitvar, "nhat", "nhat.se", "NBRPLT.gt0", "AREAUSED")
   
  SAestvar <- sort(c("DIR", "JU.GREG",  
	             "JU.EBLUP", "hbsaeU",
				 "JFH", "JA.synth", "saeA", "hbsaeA"))
  SAsevar <- sort(c("DIR.se", "JU.GREG.se", 
				 "JU.EBLUP.se.1", "hbsaeU.se",
				 "JFH.se", "JA.synth.se",
				 "saeA.se", "hbsaeA.se"))
  SAestvars <- c("JU.Synth")
  for (i in 1:length(SAestvar)) {
    SAestvars <- c(SAestvars, SAestvar[i], SAsevar[i])
	  if (pse) {
	    SAestvars <- c(SAestvars, paste0(SAestvar[i], ".pse"))
	  }
  }	
  MAestvar <- c("maseGREG")
  MAsevar <- c("maseGREG.se")
  MAestvars <- c(MAestvar, MAsevar)
  if (pse) {
    MAestvars <- c(MAestvars, paste0(MAestvar, ".pse"))
  }
  GBestHTvar <- c("HT")
  GBseHTvar <- c("HT.se") 
  GBestHTvars <- c(GBestHTvar, GBseHTvar)
  if (pse) {
    GBestHTvars <- c(GBestHTvars, paste0(GBestHTvar, ".pse"))
  } 
  GBestPSvar <- c("PS")
  GBsePSvar <- c("PS.se")
  GBestPSvars <- c(GBestPSvar, GBsePSvar)
  if (pse) {
    GBestPSvars <- c(GBestPSvars, paste0(GBestPSvar, ".pse"))
  }
  #GBestvars <- c(GBestHTvars, GBestPSvars)
 
  #estvar <- c("JU.Synth", SAestvar, SAsevar, MAestvar, MAsevar, GBestHTvar, GBsetHTvar, GBestPSvar, GBsetPSvar)
  estvars <- c(SAestvars, MAestvars, GBestHTvars, GBestPSvars)

  if (forprop) {
    message("generating estimates of forest proportion...\n")

    ## Area estimates
    ##############################################
    multestvarlst <- "FORPROP"
 
    ## Small area
    SAareaALL <- modSAarea(SApopdatlst = SApopdatlst, 
                           landarea = landarea, 
                           multest = TRUE, 
                           modelselect = modelselect, 
                           table_opts = list(rawonly = TRUE, totals = totals),
                           largebnd.unique = SAdomdat$largebnd.unique)
    SAarea.multest <- SAareaALL$multest	
	  names(SAarea.multest)[names(SAarea.multest) %in% c("NBRPLT.gt0", "AREAUSED")] <- 
	                c("NBRPLT.gt0.SA", "AREAUSED.SA")
					
    SAarea.predselect.area <- SAareaALL$raw$predselect.area
    SAarea.predselect.unit <- SAareaALL$raw$predselect.unit
    output[["FORPROP"]][["raw"]] <- SAareaALL$raw[c("SAobjlst", "areaunits")]

    rm(SAareaALL)
    gc()

    if ("AOI" %in% names(SAarea.multest)) {
      SAarea.multest <- SAarea.multest[SAarea.multest$AOI == 1, ]
	    SAarea.multest$AOI <- NULL
	    SAarea.multest$LARGEBND <- NULL
    }
    if (pse) {
      SAarea.multest[, paste0(SAestvar, ".pse")] <- 
	                round(SAarea.multest[, SAsevar] / SAarea.multest[, SAestvar] * 100, 2)
	  }
	  SAarea.multest <- SAarea.multest[, c("DOMAIN",  "NBRPLT", SAestvars, 
	                                       "NBRPLT.gt0.SA", "AREAUSED.SA")]
	
	  ## Model-assisted - Greg
    MAareaGREG <- lapply(names(MApopdatlst), function(x)
                            modMAarea(MApopdat = MApopdatlst[[x]], 
				                    MAmethod = MAmethod,
                            landarea = landarea, 
							              modelselect = modelselect, 
                            table_opts = list(rawonly = TRUE, totals = totals)))
    MAarea.est <- do.call(rbind, lapply(MAareaGREG, function(x) 
									x$raw$unit_totest[,estcols1]))
    MAarea.est$nhat.se <- sqrt(MAarea.est$nhat.var)
    MAarea.est <- MAarea.est[, estcols2]
	  data.table::setnames(MAarea.est, 
	           c("DOMAIN", "maseGREG", "maseGREG.se", "NBRPLT_gt0.MA", "AREAUSED.MA")) 			   
    if (pse) {
      MAarea.est[, paste0(MAestvar, ".pse")] <- 
	             round(MAarea.est[, MAsevar] / MAarea.est[, MAestvar] * 100, 2)
	  }
	  MAarea.est <- MAarea.est[, c("DOMAIN",  MAestvars, 
	                               "NBRPLT_gt0.MA", "AREAUSED.MA")]

	  MAtree.predselect.greg <- rbindlist(lapply(MAareaGREG, function(x) 
	    x$raw$predselectlst$totest), fill=TRUE)
    MAarea.predselect.greg[is.na(MAarea.predselect.greg)] <- 0

    ## Create final table of estimates
    area.est <- merge(SAarea.multest, MAarea.est, by="DOMAIN", all.x=TRUE)


    ## Green-book - Post-strat
    GBareaHT <- lapply(names(GBpopdatlst1), function(x)
                  modGBarea(GBpopdat = GBpopdatlst1[[x]], 
                            landarea = landarea, 
                            table_opts = list(rawonly = TRUE, totals = totals)))
    names(GBareaHT) <- names(SAdatalst)
    GBareaHT.est <- do.call(rbind, lapply(GBareaHT, function(x) 
									x$raw$unit_totest[,estcols1[1:3]]))
    GBareaHT.est$nhat.se <- sqrt(GBareaHT.est$nhat.var)
    GBareaHT.est <- GBareaHT.est[, estcols2[1:3]]
    data.table::setnames(GBareaHT.est, c("DOMAIN", "HT", "HT.se"))

    if (pse) {
      GBareaHT.est[, paste0(GBestHTvar, ".pse")] <- 
	             round(GBareaHT.est[, GBseHTvar] / GBareaHT.est[, GBestHTvar] * 100, 2)
	  }
	  GBareaHT.est <- GBareaHT.est[, c("DOMAIN",  GBestHTvars)]

    ## Green-book - Post-strat
    GBareaPS <- lapply(names(GBpopdatlst2), function(x)
                  modGBarea(GBpopdat = GBpopdatlst2[[x]], 
                            landarea = landarea, 
							              returntitle = TRUE,
                            table_opts = list(rawonly = TRUE, totals = totals)))
    names(GBareaPS) <- names(SAdatalst)
    GBareaPS.est <- do.call(rbind, lapply(GBareaPS, function(x) 
									x$raw$unit_totest[,estcols1]))
    GBareaPS.est$nhat.se <- sqrt(GBareaPS.est$nhat.var)
    GBareaPS.est <- GBareaPS.est[, estcols2]
    data.table::setnames(GBareaPS.est, 
	             c("DOMAIN", "PS", "PS.se", "NBRPLT_gt0.GB", "AREAUSED.GB"))
    if (pse) {
      GBareaPS.est[, paste0(GBestPSvar, ".pse")] <- 
	          round(GBareaPS.est[, GBsePSvar] / GBareaPS.est[, GBestPSvar] * 100, 2)
	  }
	  GBareaPS.est <- GBareaPS.est[, c("DOMAIN",  GBestPSvars, "NBRPLT_gt0.GB", "AREAUSED.GB")]

    ## Merge estimates to final table
    area.est <- merge(area.est, GBareaHT.est, by="DOMAIN", all.x=TRUE)
    area.est <- merge(area.est, GBareaPS.est, by="DOMAIN", all.x=TRUE)

    ## Get totals
	  if (totals) {
	    area.est[, c("JU.Synth", SAestvar, SAsevar)] <- 
	               round(area.est[, c("JU.Synth", SAestvar, SAsevar)] * area.est$AREAUSED.SA, estround)           
	    area.est[, c(MAestvar, MAsevar)] <- round(area.est[,c(MAestvar, MAsevar)] * area.est$AREAUSED.MA, estround)            
	    area.est[, c(GBestHTvar, GBseHTvar, GBestPSvar, GBsePSvar)] <- 
	               round(area.est[, c(GBestHTvar, GBseHTvar, GBestPSvar, GBsePSvar)] * area.est$AREAUSED.GB, estround)           
    }
    ## Round area variables
    area.est$AREAUSED.SA <- round(area.est$AREAUSED.SA)
    area.est$AREAUSED.MA <- round(area.est$AREAUSED.MA)
    area.est$AREAUSED.GB <- round(area.est$AREAUSED.GB)
 
    multest[["FORPROP"]] <- area.est
  #  predselect[["FORPROP"]] <- list(SAest.area=SAarea.predselect.area,
  #						SAest.unit=SAarea.predselect.unit,
  #						MAest.greg=MAarea.predselect.greg)
  #  raw[["FORPROP"]] <- SAareaALL$raw[c("SAobjlst", "estunits")]
  #  raw[["FORPROP"]][["MAmethod"]] <- MAareaGREG$raw$MAmethod
  
    output[["FORPROP"]][["multest"]] <- area.est
	  output[["FORPROP"]][["estvars"]] <- c(SAestvars, MAestvars, GBestHTvars, GBestPSvars)
    output[["FORPROP"]][["raw"]][["MAmethod"]] <- MAareaGREG$raw$MAmethod
    output[["FORPROP"]][["raw"]][["predselect"]] <- list(SAest.area=SAarea.predselect.area,
                                              SAest.unit=SAarea.predselect.unit,
                                              MAest.greg=MAarea.predselect.greg)
	  output[["FORPROP"]][["titles"]][["title.main"]] <- strsplit(GBareaPS[[1]]$titlelst$title.tot, ";")[[1]][1]
	  output[["FORPROP"]][["titles"]][["title.yvar"]] <- GBareaPS[[1]]$titlelst$title.yvar
    rm(GBareaHT)
    rm(GBareaPS)
    rm(MAareaGREG)
    rm(MAarea.est)
    gc()
  }

  ## Tree estimates
  ##############################################
  for (j in 1:length(estvarlst)) {
    estvar <- estvarlst[j]	
	  estvarnm <- ifelse(estvar == "TPA_UNADJ", "COUNT", estvar)

    for (k in 1:length(tfilterlst)) {
      tfilter <- tfilterlst[k]
      message("generating estimates for ", estvar, " - ", tfilter, "...\n")
	  
	    estvar.filter <- ifelse(tfilter == "live", "STATUSCD == 1",
	    ifelse(tfilter == "dead", "STATUSCD == 2 & STANDING_DEAD_CD == 1", NULL))
	  
	    ## Add filter to multestvar name
	    multestvar <- paste0(estvarnm, "_", tfilter)
      multestvarlst <- c(multestvarlst, multestvar)
  
      ## Small area
      SAtreeALL <- modSAtree(SApopdatlst = SApopdatlst, 
                           landarea = landarea, 
                           estvar = estvar, 
                           estvar.filter = estvar.filter, 
                           multest = TRUE, 
                           modelselect = modelselect, 
                           table_opts = list(rawonly = TRUE, totals = totals), 
                           largebnd.unique = SAdomdat$largebnd.unique)
						   
      SAtree.multest <- SAtreeALL$multest
	    names(SAtree.multest)[names(SAtree.multest) %in% c("NBRPLT.gt0", "AREAUSED")] <- 
	                c("NBRPLT.gt0.SA", "AREAUSED.SA")

      SAtree.predselect.area <- SAtreeALL$raw$predselect.area
      SAtree.predselect.unit <- SAtreeALL$raw$predselect.unit
      SAobjlst <- SAtreeALL$raw$SAobjlst$"1"
      output[[multestvar]][["raw"]] <- SAtreeALL$raw[c("SAobjlst", "areaunits")]

      rm(SAtreeALL)
      gc()

      if ("AOI" %in% names(SAtree.multest)) {
        SAtree.multest <- SAtree.multest[SAtree.multest$AOI == 1, ]
		    SAtree.multest$AOI <- NULL
	      SAtree.multest$LARGEBND <- NULL
      }
      if (pse) {
        SAtree.multest[, paste0(SAestvar, ".pse")] <- 
		            round(SAtree.multest[, SAsevar] / SAtree.multest[, SAestvar] * 100, 2)
	    }
	    SAtree.multest <- SAtree.multest[, c("DOMAIN",  "NBRPLT", SAestvars, "NBRPLT.gt0.SA", "AREAUSED.SA")]

      ## Model-assisted - Greg
      MAtreeGREG <- lapply(names(MApopdatlst), function(x)
                  modMAtree(MApopdat = MApopdatlst[[x]], 
				                    MAmethod = MAmethod,
                            landarea = landarea, 
							              estvar=estvar, 
                            estvar.filter = estvar.filter, 
							              modelselect = modelselect, 
                            table_opts = list(rawonly = TRUE, totals = totals)))
      MAtree.est <- do.call(rbind, lapply(MAtreeGREG, function(x) 
									x$raw$unit_totest[,estcols1]))
      MAtree.est$nhat.se <- sqrt(MAtree.est$nhat.var)
      MAtree.est <- MAtree.est[, estcols2]
      data.table::setnames(MAtree.est, 
	           c("DOMAIN", "maseGREG", "maseGREG.se", "NBRPLT_gt0.MA", "AREAUSED.MA")) 	
			   
      if (pse) {
        MAtree.est[, paste0(MAestvar, ".pse")] <- 
		            round(MAtree.est[, MAsevar] / MAtree.est[, MAestvar] * 100, 2)
	    }
	    MAtree.est <- MAtree.est[, c("DOMAIN",  MAestvars, "NBRPLT_gt0.MA", "AREAUSED.MA")]

      MAtree.predselect.greg <- rbindlist(lapply(MAtreeGREG, function(x) 
									x$raw$predselectlst$totest), fill=TRUE)
      MAtree.predselect.greg[is.na(MAtree.predselect.greg)] <- 0

      ## Create final table
      tree.est <- merge(SAtree.multest, MAtree.est, by="DOMAIN", all.x=TRUE)
	
	
	    ## Green-book - HT
      GBtreeHT <- lapply(names(GBpopdatlst1), function(x)
                  modGBtree(GBpopdat = GBpopdatlst1[[x]], 
                            landarea = landarea, 
							              estvar = estvar, 
                            estvar.filter = estvar.filter, 
 							              strata = FALSE,
                            table_opts = list(rawonly = TRUE, totals = totals)))
      names(GBtreeHT) <- names(SAdatalst)
      GBtreeHT.est <- do.call(rbind, lapply(GBtreeHT, function(x) 
									x$raw$unit_totest[,estcols1[1:3]]))
      GBtreeHT.est$nhat.se <- sqrt(GBtreeHT.est$nhat.var)
	    GBtreeHT.est <- GBtreeHT.est[, estcols2[1:3]]
      data.table::setnames(GBtreeHT.est, 
	                  c("DOMAIN", "HT", "HT.se"))

      if (pse) {
        GBtreeHT.est[, paste0(GBestHTvar, ".pse")] <- 
		               round(GBtreeHT.est[, GBseHTvar] / GBtreeHT.est[, GBestHTvar] * 100, 2)
	    }
	    GBtreeHT.est <- GBtreeHT.est[, c("DOMAIN",  GBestHTvars)]

	    ## Green-book - Post-strat
      GBtreePS <- lapply(names(GBpopdatlst2), function(x)
                  modGBtree(GBpopdat = GBpopdatlst2[[x]], 
                            landarea = landarea, 
							              estvar = estvar, 
                            estvar.filter = estvar.filter, 
							              strata = TRUE,
							              strvar = strvar,
							              returntitle = TRUE,
                            table_opts = list(rawonly = TRUE, totals = totals)))
      names(GBtreePS) <- names(SAdatalst)
      GBtreePS.est <- do.call(rbind, lapply(GBtreePS, function(x) 
									x$raw$unit_totest[,estcols1]))
      GBtreePS.est$nhat.se <- sqrt(GBtreePS.est$nhat.var)
	    GBtreePS.est <- GBtreePS.est[, estcols2]
      data.table::setnames(GBtreePS.est, 
	             c("DOMAIN", "PS", "PS.se", "NBRPLT_gt0.GB", "AREAUSED.GB"))
				 
      if (pse) {
        GBtreePS.est[, paste0(GBestPSvar, ".pse")] <- 
		         round(GBtreePS.est[, GBsePSvar] / GBtreePS.est[, GBestPSvar] * 100, 2)
	    }
	    GBtreePS.est <- GBtreePS.est[, c("DOMAIN",  GBestPSvars, "NBRPLT_gt0.GB", "AREAUSED.GB")]

      ## Merge to final table
      tree.est <- merge(tree.est, GBtreeHT.est, by="DOMAIN")
      tree.est <- merge(tree.est, GBtreePS.est, by="DOMAIN")

      ## Get totals
	    if (totals) {
	  	  tree.est[, c("JU.Synth", SAestvar, SAsevar)] <- 
	               round(tree.est[, c("JU.Synth", SAestvar, SAsevar)] * tree.est$AREAUSED.SA, estround)           
	      tree.est[, c(MAestvar, MAsevar)] <- round(tree.est[,c(MAestvar, MAsevar)] * tree.est$AREAUSED.MA, estround)            
	      tree.est[, c(GBestHTvar, GBseHTvar, GBestPSvar, GBsePSvar)] <- 
	               round(tree.est[, c(GBestHTvar, GBseHTvar, GBestPSvar, GBsePSvar)] * tree.est$AREAUSED.GB, estround)           
      }
      ## Round tree variables
      tree.est$AREAUSED.SA <- round(tree.est$AREAUSED.SA)
      tree.est$AREAUSED.MA <- round(tree.est$AREAUSED.MA)
      tree.est$AREAUSED.GB <- round(tree.est$AREAUSED.GB)
    
      #multest[[multestvar]] <- tree.est
      #    predselect[[multestvar]] <- list(SAest.area=SAtree.predselect.area,
      #						SAest.unit=SAtree.predselect.unit,
      #						MAest.greg=MAarea.predselect.greg)
      #    raw[[multestvar]] <- SAtreeALL$raw[c("SAobjlst", "estunits")]
      #    raw[[multestvar]][["MAmethod"]] <- MAtreeGREG$raw$MAmethod
    
      output[[multestvar]][["multest"]] <- tree.est
	    output[[multestvar]][["estvars"]] <- estvars
      output[[multestvar]][["raw"]][["MAmethod"]] <- MAmethod
      output[[multestvar]][["raw"]][["predselect"]] <- list(SAest.area=SAtree.predselect.area,
                                                SAest.unit=SAtree.predselect.unit,
                                                MAest.greg=MAtree.predselect.greg)
	    output[[multestvar]][["titles"]][["title.main"]] <- strsplit(GBtreePS[[1]]$titlelst$title.tot, ";")[[1]][1]
	    output[[multestvar]][["titles"]][["title.yvar"]] <- GBtreePS[[1]]$titlelst$title.yvar

      rm(GBtreeHT)
	    rm(GBtreePS)
      rm(MAtreeGREG)
      rm(MAtree.est)
      gc()
    }
  }
  
  #output$estvarlst <- multestvarlst 
  
  returnlst <- output
}


