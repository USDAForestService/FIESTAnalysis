#' get NFS
#'
#' @param region NFS region
#' @export
getnfs <- function(region) {

  URLadminfs <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_ForestSystemBoundaries_01/MapServer/0"
  ## US Forest Service Administrative Forest Boundaries

  ## R1 - Northern region (MT, ND, IDn)
  ## R2 - Rocky Mountain Region (CO, WY, KA, NE, SD)
  ## R3 - Southwestern Region (AZ, NM)
  ## R4 - Intermountain Region (UT, NV, IDs, COw)
  ## R5 - Pacific Southwest Region (CA)
  ## R6 - Pacific Northwest Region (OR, WA)
  ## R8 - Southern Region (TX, OK, AR, LA, MI, AL, TE, GE, FL, NC, SC, VA, KY)
  ## R9 - Eastern Region (MN, IA, MO, WI, IL, MI, IN, OH, NY, PA, WV, MD, DE, NJ, CT, RI, MA, ME, VT, NH)
  ## R10 - Alaska Region (AK)

  regionlst <- c(1:6,8:10)
  if (!region %in% regionlst) {
    stop("region must be in: ", toString(regionlst))
  }

  regionC <- sapply(region, formatC, digits=2, width=2, flag="0")
  region.filter <- FIESTAutils::getfilter("REGION", regionC, syntax="sql")


  ## Get Administrative forest boundaries
  nfsbnd <- arcpullr::get_spatial_layer(url = URLadminfs,
                                       where = region.filter)

  ## check validation of polygons
  if (!any(sf::st_is_valid(nfsbnd))) {
    nfsbnd <- FIESTAutils::polyfix.sf(nfsbnd)
  }

  return(nfsbnd)
}


#' get District
#'
#' @param forest Forest Name
#' @param region NFS region
#' @export
getdistrict <- function(forest = NULL, region = NULL) {

  URLdistricts <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_RangerDistricts_01/MapServer/0"
  ## US Forest Service Ranger District Boundaries


  FORESTNAMES <- c('Allegheny National Forest','Angeles National Forest','Apache-Sitgreaves National Forests',
                   'Arapaho and Roosevelt National Forests','Ashley National Forest',
                   'Beaverhead-Deerlodge National Forest','Bighorn National Forest','Bitterroot National Forest',
                   'Black Hills National Forest','Boise National Forest','Bridger-Teton National Forest',
                   'Caribou-Targhee National Forest','Carson National Forest','Chequamegon-Nicolet National Forest',
                   'Chippewa National Forest','Chugach National Forest','Cibola National Forest','Cleveland National Forest',
                   'Coconino National Forest','Columbia River Gorge National Scenic Area','Colville National Forest',
                   'Coronado National Forest','Custer Gallatin National Forest',
                   'Dakota Prairie Grasslands','Deschutes National Forest','Dixie National Forest',
                   'Eldorado National Forest','Fishlake National Forest','Flathead National Forest','Fremont-Winema National Forest',
                   'Gifford Pinchot National Forest','Gila National Forest','Grand Mesa, Uncompahgre and Gunnison National Forests',
                   'Green Mountain and Finger Lakes National Forests',
                   'Helena-Lewis and Clark National Forest','Hiawatha National Forest','Hoosier National Forest',
                   'Humboldt-Toiyabe National Forest','Huron-Manistee National Forest',
                   'Idaho Panhandle National Forests','Inyo National Forest',
                   'Kaibab National Forest','Klamath National Forest','Kootenai National Forest',
                   'Lake Tahoe Basin Management Unit','Lassen National Forest','Lincoln National Forest',
                   'Lolo National Forest','Los Padres National Forest',
                   'Malheur National Forest','Manti-La Sal National Forest','Mark Twain National Forest',
                   'Medicine Bow-Routt National Forest','Mendocino National Forest','Midewin National Tallgrass Prairie',
                   'Modoc National Forest','Monongahela National Forest','Mt. Baker-Snoqualmie National Forest','Mt. Hood National Forest',
                   'Nebraska National Forest','Nez Perce-Clearwater National Forest',
                   'Ochoco National Forest','Okanogan-Wenatchee National Forest','Olympic National Forest','Ottawa National Forest',
                   'Payette National Forest','Pike and San Isabel National Forests','Plumas National Forest','Prescott National Forest',
                   'Rio Grande National Forest','Rogue River-Siskiyou National Forests',
                   'Salmon-Challis National Forest','San Bernardino National Forest','San Juan National Forest',
                   'Santa Fe National Forest','Sawtooth National Forest','Sequoia National Forest',
                   'Shasta-Trinity National Forest','Shawnee National Forest','Shoshone National Forest',
                   'Sierra National Forest','Siuslaw National Forest','Six Rivers National Forest',
                   'Stanislaus National Forest','Superior National Forest',
                   'Tahoe National Forest','Tongass National Forest','Tonto National Forest',
                   'Uinta-Wasatch-Cache National Forest','Umatilla National Forest','Umpqua National Forest',
                   'Wallowa-Whitman National Forest','Wayne National Forest','White Mountain National Forest','White River National Forest')


  regionlst <- c(1:6,8:10)

  if (is.null(forest)) {
    if (!is.null(region)) {
      if (!region %in% regionlst) {
        stop("region must be in: ", toString(regionlst))
      }
    } else {
      ## select region from dropdown menu
      region <- select.list(regionlst, title="region", multiple=TRUE)
    }

    regionC <- sapply(region, formatC, digits=2, width=2, flag="0")
    region.filter <- FIESTAutils::getfilter("REGION", regionC, syntax="sql")


    ## Get Administrative forest district boundaries
    ALPregion <- arcpullr::get_spatial_layer(url = URLalp,
                                             where = region.filter)
    FORESTNAMES <- sort(unique(ALPregion$FORESTNAME))

    ## Select forest from dropdown menu
    forest <- select.list(FORESTNAMES, title="forest names", multiple=TRUE)

  } else {

    if (!all(forest %in% FORESTNAMES)) {
      message("invalid forest: \n", toString(FORESTNAMES))
      forest <- select.list(FORESTNAMES, title="forest names", multiple=TRUE)
    }
  }


  ## Get forest filter
  forest.filter <- paste0("FORESTNAME IN(", toString(encodeString(forest, quote="'")), ")")


  ## Get Administrative forest district boundaries
  districts <- arcpullr::get_spatial_layer(url = URLdistricts,
                                           where = forest.filter)

  ## check validation of polygons
  if (!any(sf::st_is_valid(districts))) {
    districts <- FIESTAutils::polyfix.sf(districts)
  }

  return(districts)
}


#' get NFS watershed
#'
#' @param forest Forest Name
#' @export
getNFSwatershed <- function(forest = NULL) {


  URLwatershedsNFS <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_PriorityWatersheds_01/MapServer/0"
  ## U.S. Forest Service Priority Watersheds - HUC12 (from the Watershed Boundary Dataset)

  forest_names <- c('Allegheny National Forest','Angeles National Forest','Apache-Sitgreaves National Forest',
                    'Arapaho & Roosevelt National Forests','Ashley National Forest','Beaverhead-Deerlodge National Forest',
                    'Bighorn National Forest','Bitterroot National Forest','Black Hills National Forest',
                    'Boise National Forest','Bridger-Teton National Forest',
                    'Caribou-Targhee National Forest','Carson National Forest','Chattahoochee-Oconee National Forest',
                    'Chequamegon-Nicolet National Forest','Cherokee National Forest','Chippewa National Forest',
                    'Chugach National Forest','Cibola National Forest','Cleveland National Forest',
                    'Coconino National Forest','Columbia River Gorge National Scenic Area',
                    'Colville National Forest','Coronado National Forest','Custer Gallatin National Forest',
                    'Dakota Prairie Grasslands','Daniel Boone National Forest','Deschutes National Forest',
                    'Dixie National Forest','El Yunque National Forest','Eldorado National Forest',
                    'Fishlake National Forest','Flathead National Forest','Francis Marion-Sumter National Forest',
                    'Fremont-Winema National Forests',
                    'George Washington And Jefferson National Forests','Gifford Pinchot National Forest',
                    'Gila National Forest','Grand Mesa Uncompahgre Gunnison National Forests',
                    'Green Mountain And Finger Lakes National Forests',
                    'Helena-Lewis And Clark National Forest','Hiawatha National Forest','Hoosier National Forest',
                    'Humboldt-Toiyabe National Forest','Huron Manistee National Forest',
                    'Idaho Panhandle National Forests','Inyo National Forest',
                    'Kaibab National Forest','Kisatchie National Forest','Klamath National Forest','Kootenai National Forest',
                    'Lake Tahoe Basin Mgt Unit','Land Between The Lakes Nra','Lassen National Forest','Lincoln National Forest',
                    'Lolo National Forest',
                    'Malheur National Forest','Manti-Lasal National Forest','Mark Twain National Forest',
                    'Medicine Bow-Routt National Forests','Mendocino National Forest',
                    'Midewin National Tallgrass Prairie','Modoc National Forest','Monongahela National Forest',
                    'Mt Baker-Snoqualmie National Forest','Mt Hood National Forest',
                    'National Forests In Alabama','National Forests In Florida','National Forests In Mississippi',
                    'National Forests In North Carolina','National Forests In Texas','Nebraska National Forest',
                    'Nez Perce-Clearwater National Forest',
                    'Ochoco National Forest','Okanogan-Wenatchee National Forests','Olympic National Forest',
                    'Ottawa National Forest','Ouachita National Forest','Ozark-St Francis National Forest',
                    'Payette National Forest','Pike And San Isabel National Forests','Plumas National Forest',
                    'Prescott National Forest',
                    'Rio Grande National Forest','Rogue River-Siskiyou National Forests',
                    'Salmon-Challis National Forest','San Bernardino National Forest',
                    'San Juan National Forest','Santa Fe National Forest','Sawtooth National Forest',
                    'Sequoia National Forest','Shasta Trinity National Forest','Shawnee National Forest',
                    'Shoshone National Forest','Sierra National Forest','Siuslaw National Forest',
                    'Six Rivers National Forest','Stanislaus National Forest','Superior National Forest',
                    'Tahoe National Forest','Tongass National Forest','Tonto National Forest',
                    'Uinta-Wasatch-Cache National Forest','Umatilla National Forest','Umpqua National Forest',
                    'Wallowa Whitman National Forest','Wayne National Forest','White Mountain National Forest',
                    'White River National Forest','Willamette National Forest')

  if (!is.null(forest)) {
    if (!forest %in% forest_names) {
      message("forest is not in layer: ")
      print(as.data.frame(forest_names))
      stop()
    }
  }
  forest.filter <- FIESTAutils::getfilter("FOREST_NAME", forest, syntax="sql")

  ## Define out_fields
  out_fields <- c("FOREST_NAME", "WATERSHED_CODE", "WATERSHED_NAME",
                  "WATERSHED_CONDITION_FS_AREA", "TOTAL_WATERSHED_AREA_ACRES",
                  "FS_OWNERSHIP_PERCENT")
  if (!is.null(forest)) {
    forest.filter <- FIESTAutils::getfilter("FOREST_NAME", forest, syntax="sql")

    ## Get Watersheds
    watershedNFS <- arcpullr::get_spatial_layer(url = URLwatershedsNFS,
                                                out_fields = out_fields,
                                                where = forest.filter)
  } else {

    ## Get WatershedsNFS
    watershedNFS <- arcpullr::get_spatial_layer(url = URLwatershedsNFS,
                                                out_fields = out_fields)
  }

  ## check validation of polygons
  if (!any(sf::st_is_valid(watershedNFS))) {
    watershedNFS <- FIESTAutils::polyfix.sf(watershedNFS)
  }


  return(watershedNFS)
}


#' get ALP
#'
#' @param forest Forest Name
#' @param region NFS region
#' @export
getALP <- function(forest = NULL, region = NULL) {

  options(timeout = max(1000, getOption("timeout")))
  #Sys.setenv(ALL_PROXY = "YOUR_PROXY_HOST")

  URLalp <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_BasicOwnership_01/MapServer/0"
  #URLalp_info <- arcpullr::get_layer_info(url = URLalp)
  #URLalp_info$fields$name


  FORESTNAMES <- c('Allegheny National Forest','Angeles National Forest','Apache-Sitgreaves National Forests',
                   'Arapaho and Roosevelt National Forests','Ashley National Forest',
                   'Beaverhead-Deerlodge National Forest','Bighorn National Forest','Bitterroot National Forest',
                   'Black Hills National Forest','Boise National Forest','Bridger-Teton National Forest',
                   'Caribou-Targhee National Forest','Carson National Forest','Chequamegon-Nicolet National Forest',
                   'Chippewa National Forest','Chugach National Forest','Cibola National Forest','Cleveland National Forest',
                   'Coconino National Forest','Columbia River Gorge National Scenic Area','Colville National Forest',
                   'Coronado National Forest','Custer Gallatin National Forest',
                   'Dakota Prairie Grasslands','Deschutes National Forest','Dixie National Forest',
                   'Eldorado National Forest','Fishlake National Forest','Flathead National Forest','Fremont-Winema National Forest',
                   'Gifford Pinchot National Forest','Gila National Forest','Grand Mesa, Uncompahgre and Gunnison National Forests',
                   'Green Mountain and Finger Lakes National Forests',
                   'Helena-Lewis and Clark National Forest','Hiawatha National Forest','Hoosier National Forest',
                   'Humboldt-Toiyabe National Forest','Huron-Manistee National Forest',
                   'Idaho Panhandle National Forests','Inyo National Forest',
                   'Kaibab National Forest','Klamath National Forest','Kootenai National Forest',
                   'Lake Tahoe Basin Management Unit','Lassen National Forest','Lincoln National Forest',
                   'Lolo National Forest','Los Padres National Forest',
                   'Malheur National Forest','Manti-La Sal National Forest','Mark Twain National Forest',
                   'Medicine Bow-Routt National Forest','Mendocino National Forest','Midewin National Tallgrass Prairie',
                   'Modoc National Forest','Monongahela National Forest','Mt. Baker-Snoqualmie National Forest','Mt. Hood National Forest',
                   'Nebraska National Forest','Nez Perce-Clearwater National Forest',
                   'Ochoco National Forest','Okanogan-Wenatchee National Forest','Olympic National Forest','Ottawa National Forest',
                   'Payette National Forest','Pike and San Isabel National Forests','Plumas National Forest','Prescott National Forest',
                   'Rio Grande National Forest','Rogue River-Siskiyou National Forests',
                   'Salmon-Challis National Forest','San Bernardino National Forest','San Juan National Forest',
                   'Santa Fe National Forest','Sawtooth National Forest','Sequoia National Forest',
                   'Shasta-Trinity National Forest','Shawnee National Forest','Shoshone National Forest',
                   'Sierra National Forest','Siuslaw National Forest','Six Rivers National Forest',
                   'Stanislaus National Forest','Superior National Forest',
                   'Tahoe National Forest','Tongass National Forest','Tonto National Forest',
                   'Uinta-Wasatch-Cache National Forest','Umatilla National Forest','Umpqua National Forest',
                   'Wallowa-Whitman National Forest','Wayne National Forest','White Mountain National Forest','White River National Forest')


  regionlst <- c(1:6,8:10)

  if (is.null(forest)) {
    if (!is.null(region)) {
      if (!region %in% regionlst) {
        stop("region must be in: ", toString(regionlst))
      }
    } else {
      ## select region from dropdown menu
      region <- select.list(regionlst, title="region", multiple=TRUE)
    }

    regionC <- sapply(region, formatC, digits=2, width=2, flag="0")
    region.filter <- FIESTAutils::getfilter("REGION", regionC, syntax="sql")


    ## Get Administrative forest district boundaries
    ALPregion <- arcpullr::get_spatial_layer(url = URLalp,
                                           where = region.filter)

    ## check validation of polygons
    if (!any(sf::st_is_valid(ALPregion))) {
      ALPregion <- FIESTAutils::polyfix.sf(ALPregion)
    }

    return(ALPregion)

#    FORESTNAMES <- sort(unique(ALPregion$FORESTNAME))

#    ## Select forest from dropdown menu
#    forest <- select.list(FORESTNAMES, title="forest names", multiple=TRUE)

  } else {

    if (!all(forest %in% FORESTNAMES)) {
      message("invalid forest: \n", toString(FORESTNAMES))
      forest <- select.list(FORESTNAMES, title="forest names", multiple=TRUE)
    }
  }


  ## Get forest filter
  forest.filter <- paste0("OWNERCLASSIFICATION = 'USDA FOREST SERVICE' AND FORESTNAME IN(",
                          toString(encodeString(forest, quote="'")), ")")

  ## Get forest boundaries
  forest <- arcpullr::get_spatial_layer(url = URLalp,
                                        where = forest.filter)

  ## check validation of polygons
  if (!any(sf::st_is_valid(forest))) {
    forest <- FIESTAutils::polyfix.sf(forest)
  }

  return(forest)
}


#' get fires EDW
#'
#' @param fireyear Year
#' @param minacres Minimum acres
#' @export
getfiresEDW <- function(fireyear, minacres = NULL) {

  ## Final Fire Perimeter (All Years)
  ## The FirePerimeterFinal polygon layer represents final mapped wildland fire perimeters.
  ## USDA Forest Service National Forest System Lands GIS and Fire personnel

  #URLfire <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_FireOccurrenceAndPerimeter_01/MapServer/"
  URLfile <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_FireOccurrenceAndPerimeter_01/MapServer/10"

  ## Define out_fields
  out_fields <- c("UNIQFIREID", "FIRENAME", "TOTALACRES", "FIREYEAR", "OWNERAGENCY")

  if (fireyear == 2017) {
    year <- c(2017,2018)
  } else {
    year <- fireyear
  }

  ## Build filter for fires
  fireyear.filter <- paste0("FIREYEAR IS NOT NULL AND ", FIESTAutils::getfilter("FIREYEAR", year, syntax="sql"))

  if (!is.null(minacres)) {
    if (!is.numeric(minacres)) {
      stop("invalid minacres... must be numeric")
    }
   fireyear.filter <- paste0(fireyear.filter,
                            " AND TOTALACRES >= ", minacres)
  }


  ## Get fires
  fires <- arcpullr::get_spatial_layer(url = URLfile,
                                       out_fields = out_fields,
                                       where = fireyear.filter)
  if (fireyear == 2017) {
    fires[fires$FIREYEAR == 2017,]
  }


  # URLfire <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_FireOccurrenceAndPerimeter_01/MapServer/"
  #
  # fireyears <- 2016:2023
  # if (!fireyear %in% fireyears) {
  #   stop("fire year must be between 2016 and 2023")
  # }
  # end <- ifelse (fireyear == 2016, 5,
  #           ifelse (fireyear == 2017, 4,
  #              ifelse (fireyear == 2018, 3,
  #                  ifelse (fireyear == 2019, 17,
  #                      ifelse (fireyear == 2020, 20,
  #                          ifelse (fireyear == 2021, 24,
  #                               ifelse (fireyear == 2022, 27, 30)))))))
  # URLfireYEAR <- paste0(URLfire, end)


  ## Get fires
  #fire <- arcpullr::get_spatial_layer(url = URLfireYEAR,
  #                      out_fields = out_fields)

  ## check validation of polygons
  if (!any(sf::st_is_valid(fires))) {
    fires <- FIESTAutils::polyfix.sf(fires)
  }

  return(fires)
}



#' get landfire
#'
#' @param bnd sf boundary
#' @param type landfire type ('evc', 'evh', 'evt')
#' @param getlut if TRUE, gets attribute look up table from web
#' @param dstfile filename if want to write to file
#' @export
getlandfire <- function(bnd, type = "evc", getlut=FALSE, dstfile=NULL) {
  
  URL <- "https://lfps.usgs.gov/arcgis/rest/services/Landfire_LF240"
  ## Landfire LF240 (2023)
  
  message("downloading data for Landfire 2023...")
  
  ## evc - existing vegetation cover
  ## evh - existing vegetation height
  ## evt - existing vegetation type

  if (type == "evc") {
    url <- paste0(URL, "/US_240EVC/ImageServer")
    if (getlut) {
      lut <- fread("https://landfire.gov/sites/default/files/CSV/2024/LF2024_EVC.csv")
    }
  } else if (type == "evh") {
    url <- paste0(URL, "/US_240EVH/ImageServer")
    if (getlut) {
      lut <- fread("https://landfire.gov/sites/default/files/CSV/2024/LF2024_EVH.csv")
    }
  } else if (type == "evt") {
    url <- paste0(URL, "/US_240EVT/ImageServer")
    if (getlut) {
      lut <- fread("https://landfire.gov/sites/default/files/CSV/2024/LF2024_EVT.csv")
    }
  }
  
  rast <- arcpullr::get_image_layer(url = url, 
                                    sf_object = bnd, format = "tiff")
  
  if (!is.null(dstfile)) {
    terra::writeRaster(rast, filename = dstfile, overwrite = TRUE)
  }
  
  if (getlut) {
    return(list(rast=rast, lut=lut))
  } else {
    return(rast)
  }
}


#' get dem
#'
#' @param bnd sf boundary
#' @param type dem type ('ned', 'srtm')
#' @param dstfile filename if want to write to file
#' @export
getdem <- function(bnd, type = "ned", dstfile = NULL) {
  
  URL <- "https://lfps.usgs.gov/arcgis/rest/services/Topo"
  ## Topo
  
  ## ned - topochange_ned_dem
  ## srtm - topochange_srtm_dem

  if (type == "ned") {
    url <- paste0(URL, "/Topochange_NED_DEM/ImageServer")
  } else if (type == "srtm") {
    url <- paste0(URL, "/Topochange_SRTM_DEM/ImageServer")
  }
  
  rast <- arcpullr::get_image_layer(url = url, 
                                    sf_object = bnd, format = "tiff")
  
  if (!is.null(dstfile)) {
    terra::writeRaster(rast, filename = dstfile, overwrite = TRUE)
  }
  
  return(rast)
}



#' get bigmap products
#'
#' @param bnd sf boundary
#' @param type bigmap type ('abovebio')
#' @param dstfile filename if want to write to file
#' @export
getbigmap <- function(bnd, type = "abovebio", dstfile = NULL) {
  
  URL <- "https://di-usfsdata.img.arcgis.com/arcgis/rest/services"
  ## 2018 species aboveground biomass
  
  ## abovebio - topochange_ned_dem
  
  if (type == "abovebio") {
    url <- paste0(URL, "/FIA_BIGMAP_2018_Species_Aboveground_Biomass/ImageServer")
  }
  
  rast <- arcpullr::get_image_layer(url = url, 
                                    sf_object = bnd, format = "tiff")
  
  if (!is.null(dstfile)) {
    terra::writeRaster(rast, filename = dstfile, overwrite = TRUE)
  }
  
  return(rast)
}


