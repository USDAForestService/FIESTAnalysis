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
                          helperbnd = ecomap, 
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
                           prednames = prednames))



  ####################################################################
  ## Get population data for GB and MA
  ####################################################################
  message("generating population data for GB and MA...\n")
  
  ## Get population data for Model-Assisted estimates
  MApopdatlst <- lapply(names(SAdatalst), function(x)
                  modMApop(MAdata = SAdatalst[[x]], 
                           popFilter = list(AOIonly = TRUE)))
  names(MApopdatlst) <- names(SAdatalst)

  ## Get population data for Green-Book estimates
  GBpopdatlst <- lapply(names(SAdatalst), function(x)
                  modGBpop(GBdata = SAdatalst[[x]], 
                           popFilter = list(AOIonly = TRUE),
						   strata = TRUE,
						   strvar = "tnt",
						   adj = "plot"))
  names(GBpopdatlst) <- names(SAdatalst)

 
  ####################################################################
  ## Get estimates
  ####################################################################
  multest <- list()
  predselect <- list()
  raw <- list()
  
  output <- list()
  
  ## Area estimates
  ##############################################
  multestvarlst <- "FORPROP"
  MAmethod = "greg"
  
  ## Small area
  SAareaALL <- modSAarea(SApopdatlst = SApopdatlst, 
                         landarea = landarea, 
                         multest = TRUE, 
                         modelselect = modelselect, 
                         table_opts = list(rawonly=TRUE),
                         largebnd.unique = SAdomdat$largebnd.unique)
  SAarea.multest <- SAareaALL$multest
  SAarea.predselect.area <- SAareaALL$raw$predselect.area
  SAarea.predselect.unit <- SAareaALL$raw$predselect.unit
  output[["FORPROP"]][["raw"]] <- SAareaALL$raw[c("SAobjlst", "estunits")]

  rm(SAareaALL)
  gc()

  if ("AOI" %in% names(SAarea.multest)) {
    SAarea.multest <- SAarea.multest[SAarea.multest$AOI == 1, ]
  }

  ## Green-book - Post-strat
  GBareaPS <- lapply(names(GBpopdatlst), function(x)
                  modGBarea(GBpopdat = GBpopdatlst[[x]], 
                            landarea = landarea, 
                            table_opts = list(rawonly=TRUE)))
  names(GBareaPS) <- names(pltdatlst)
  GBarea.est <- do.call(rbind, lapply(GBareaPS, function(x) 
									x$raw$unit_totest[,1:3]))
  data.table::setnames(GBarea.est, c("DOMAIN", "PS", "PS.se"))

  ## Model-assisted - Greg
  MAareaGREG <- lapply(names(MApopdatlst), function(x)
                  modMAarea(MApopdat = MApopdatlst[[x]], 
				            MAmethod = MAmethod,
                            landarea = landarea, 
							modelselect = modelselect, 
                            table_opts = list(rawonly=TRUE)))
  MAarea.est <- do.call(rbind, lapply(MAareaGREG, function(x) 
									x$raw$unit_totest[,1:3]))
  MAarea.est$nhat.se <- sqrt(MAarea.est$nhat.var)
  MAarea.est$nhat.var <- NULL
  data.table::setnames(MAarea.est, c("DOMAIN", "maseGREG", "maseGREG.se"))  
  MAarea.predselect.greg <- do.call(rbind, lapply(MAareaGREG, function(x) 
									x$raw$predselectlst$totest))
  MAarea.predselect.greg[is.na(MAarea.predselect.greg)] <- 0


  area.est <- merge(SAarea.multest, GBarea.est, by="DOMAIN", all.x=TRUE)
  area.est <- merge(area.est, MAarea.est, by="DOMAIN", all.x=TRUE)
  
  multest[["FORPROP"]] <- area.est
  #  predselect[["FORPROP"]] <- list(SAest.area=SAarea.predselect.area,
  #						SAest.unit=SAarea.predselect.unit,
  #						MAest.greg=MAarea.predselect.greg)
  #  raw[["FORPROP"]] <- SAareaALL$raw[c("SAobjlst", "estunits")]
  #  raw[["FORPROP"]][["MAmethod"]] <- MAareaGREG$raw$MAmethod
  
  output[["FORPROP"]][["multest"]] <- area.est
  output[["FORPROP"]][["raw"]][["MAmethod"]] <- MAareaGREG$raw$MAmethod
  output[["FORPROP"]][["raw"]][["predselect"]] <- list(SAest.area=SAarea.predselect.area,
                                              SAest.unit=SAarea.predselect.unit,
                                              MAest.greg=MAarea.predselect.greg)
  rm(GBareaPS)
  rm(GBarea.est)
  rm(MAareaGREG)
  rm(MAarea.est)
  gc()
  
  ## Tree estimates
  ##############################################
  for (j in 1:length(estvarlst)) {
    estvar <- estvarlst[j]	
	multestvar <- ifelse(estvar == "TPA_UNADJ", "COUNT", estvar)

    for (k in 1:length(tfilterlst)) {
      tfilter <- tfilterlst[k]
      message("generating estimates for ", estvar, " - ", tfilter, "...")
	  
	  estvar.filter <- ifelse(tfilter == "live", "STATUSCD == 1",
	  ifelse(tfilter == "dead", "STATUSCD == 2 & STANDING_DEAD_CD == 1", NULL))
	  
	  ## Add filter to multestvar name
	  multestvar <- paste0(multestvar, "_", tfilter)
      multestvarlst <- c(multestvarlst, multestvar)
  
      ## Small area
      SAtreeALL <- modSAtree(SApopdatlst = SApopdatlst, 
                           landarea = landarea, 
                           estvar = estvar, 
                           estvar.filter = estvar.filter, 
                           multest = TRUE, 
                           modelselect = modelselect, 
                           table_opts = list(rawonly=TRUE), 
                           largebnd.unique = SAdomdat$largebnd.unique)
      SAtree.multest <- SAtreeALL$multest
      SAtree.predselect.area <- SAtreeALL$raw$predselect.area
      SAtree.predselect.unit <- SAtreeALL$raw$predselect.unit
      SAobjlst <- SAtreeALL$raw$SAobjlst$"1"
      output[[multestvar]][["raw"]] <- SAtreeALL$raw[c("SAobjlst", "estunits")]

      rm(SAtreeALL)
      gc()


      if ("AOI" %in% names(SAtree.multest)) {
        SAtree.multest <- SAtree.multest[SAtree.multest$AOI == 1, ]
      }
	
	  ## Green-book - Post-strat
      GBtreePS <- lapply(names(GBpopdatlst), function(x)
                  modGBtree(GBpopdat = GBpopdatlst[[x]], 
                            landarea = landarea, 
							estvar = estvar, 
                            estvar.filter = estvar.filter, 
                            table_opts = list(rawonly=TRUE)))
      names(GBtreePS) <- names(pltdatlst)
      GBtree.est <- do.call(rbind, lapply(GBtreePS, function(x) 
									x$raw$unit_totest[,1:3]))
      data.table::setnames(GBtree.est, c("DOMAIN", "PS", "PS.se"))


      ## Model-assisted - Greg
      MAtreeGREG <- lapply(names(MApopdatlst), function(x)
                  modMAtree(MApopdat = MApopdatlst[[x]], 
				            MAmethod = MAmethod,
                            landarea = landarea, 
							estvar=estvar, 
                            estvar.filter = estvar.filter, 
							modelselect = modelselect, 
                            table_opts = list(rawonly=TRUE)))
      MAtree.est <- do.call(rbind, lapply(MAtreeGREG, function(x) 
									x$raw$unit_totest[,1:3]))
      MAtree.est$nhat.se <- sqrt(MAtree.est$nhat.var)
      MAtree.est$nhat.var <- NULL
      data.table::setnames(MAtree.est, c("DOMAIN", "maseGREG", "maseGREG.se"))  
      MAtree.predselect.greg <- do.call(rbind, lapply(MAtreeGREG, function(x) 
									x$raw$predselectlst$totest))
      MAtree.predselect.greg[is.na(MAtree.predselect.greg)] <- 0
   
      tree.est <- merge(SAtree.multest, GBtree.est, by="DOMAIN", all.x=TRUE)
      tree.est <- merge(tree.est, MAtree.est, by="DOMAIN", all.x=TRUE)
    
      multest[[multestvar]] <- tree.est
      #    predselect[[multestvar]] <- list(SAest.area=SAtree.predselect.area,
      #						SAest.unit=SAtree.predselect.unit,
      #						MAest.greg=MAarea.predselect.greg)
      #    raw[[multestvar]] <- SAtreeALL$raw[c("SAobjlst", "estunits")]
      #    raw[[multestvar]][["MAmethod"]] <- MAtreeGREG$raw$MAmethod
    
      output[[multestvar]][["multest"]] <- tree.est
      output[[multestvar]][["raw"]][["MAmethod"]] <- MAmethod
      output[[multestvar]][["raw"]][["predselect"]] <- list(SAest.area=SAtree.predselect.area,
                                                SAest.unit=SAtree.predselect.unit,
                                                MAest.greg=MAtree.predselect.greg)
      rm(GBtreePS)
      rm(GBtree.est)
      rm(MAtreeGREG)
      rm(MAtree.est)
      gc()
    }
  }
  
  #output$estvarlst <- multestvarlst 
  
  returnlst <- output
}


