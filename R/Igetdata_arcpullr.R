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
  nfsR9 <- arcpullr::get_spatial_layer(url = URLadminfs,
                                       where = region.filter)
  return(nfsR9)
}



getdistrict <- function(region) {
  
  URLdistricts <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_RangerDistricts_01/MapServer/0"
  ## US Forest Service Ranger District Boundaries
  
  regionlst <- c(1:6,8:10)
  if (!region %in% regionlst) {
    stop("region must be in: ", toString(regionlst))
  }
  
  regionC <- sapply(region, formatC, digits=2, width=2, flag="0")
  region.filter <- FIESTAutils::getfilter("REGION", regionC, syntax="sql")
  
  
  ## Get Administrative forest district boundaries
  districtsRegion <- arcpullr::get_spatial_layer(url = URLdistricts,
                                                 where = region.filter)
  return(districtsRegion)
}



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
  
  return(watershedNFS)
}



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
  forest.filter <- paste("OWNERCLASSIFICATION = 'USDA FOREST SERVICE' AND FORESTNAME =", encodeString(forest, quote="'"))
  
  ## Get forest boundaries
  forest <- arcpullr::get_spatial_layer(url = URLalp,
                                        where = forest.filter)
  
  return(forest)
}

