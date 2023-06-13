

#' EcoMap SpatialPolygonsDataFrame
#' 
#' Contains regional geographic delineations for analysis of ecological
#' relationships across ecological units. ECOMAP is the term used for a USDA
#' Forest Service initiative to map ecological units and encourage their use in
#' ecosystem-based approaches to forest land conservation and management. It is
#' coordinated at the national and regional levels by USDA Forest Service staff
#' and implemented in cooperation with State forestry agencies and others.
#' ECOMAP mapping criteria are outlined in the National Hierarchical Framework
#' of Ecological Units (https://www.ncrs.fs.fed.us/gla/reports/hierarch-
#' y.htm). The framework systematically divides the country into progressively
#' smaller areas of land and water that have similar physical and biological
#' characteristics and ecological processes.
#' 
#' The EcoMap Provinces feature class contains ecological province polygons
#' attributed with names and descriptions. The EcomapSections 2007 data set
#' describes the ecological sections within the conterminous United States. The
#' EcomapSubections 2007 data set describes the ecological subsections within
#' the conterminous United States.
#' 
#' Converted to simple feature\cr Transformed CRS from longlat(EPSG:4269) to
#' Albers (EPSG:5070)\cr Saved to R object, with compression='xz'
#' 
#' 
#' @name ecomap
#' @docType data
#' @format A SpatialPolygonsDataFrame with 1233 features and 3 attributes
#' PROVINCE - Ecomap Province SECTION - EcoMap Section SUBSECTION - Ecomap
#' Subsection
#' @references Cleland, D.T.; Freeouf, J.A.; Keys, J.E., Jr.; Nowacki, G.J.;
#' Carpenter, C; McNab, W.H. 2007. Ecological Subregions: Sections and
#' Subsections of the Conterminous United States [1:3,500,000] [CD-ROM]. Sloan,
#' A.M., cartog. Gen. Tech. Report WO-76. Washington, DC: U.S. Department of
#' Agriculture, Forest Service.
#' @source Downloaded from the FSGeodata Clearinghouse on 2019 October 30,
#' format ESRI geodatabase (https://data.fs.usda.gov/geodata/edw/datasets.php)
#' @keywords datasets
NULL


#' Reference table - for Automated Land Program National Forests.
#' 
#' National Forest region and names from the USDA S_USA.BasicOwnershipFS
#' (Surface Ownership Parcels) attribute table. An area depicted as surface
#' ownership parcels dissolved on the same ownership classification
#' 
#' 
#' @name ref_ALP
#' @docType data
#' @format A dataframe with 2 columns, R1 and FORESTNAME.
#' @note ## ALP National Forest boundary alp_dsn <-
#' "S_USA.BasicOwnershipFS.gdb" alp_layer <- "BasicOwnershipFS" alp.att <-
#' "FORESTNAME" alp_bnd <- spImportSpatial(dsn=alp_dsn, layer=alp_layer)
#' alp_names <- unique(sf::st_drop_geometry(alp_bnd[, c("REGION",
#' "FORESTNAME")])) alp_names <- alp_names[order(alp_names$REGION,
#' alp_names$FORESTNAME),] alp_names <- alp_names[grepl("National Forest",
#' alp_names$FORESTNAME)==TRUE,] alp_names$FORESTNAME <- sub(" National
#' Forest", "", alp_names$FORESTNAME)
#' 
#' Ownership classes that are not USDA Forest Service were removed from the
#' layer.\cr OWNERCLASSIFICATION = "USDA FOREST SERVICE" subset with following
#' query alpfs <- alpfs[!is.na(alpfs$OWNERCLASSIFICATION) &
#' alpfs$OWNERCLASSIFICATION != 'NON-FS',]
#' @source Downloaded from the USDA Forest Service FSGeodata Clearinghose on
#' 2020 June 3, format ESRI geodatabase
#' (https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=surface+ownership)
#' 
#' U.S. Forest Service, Automated Land Program (ALP) Publication Date:
#' 2020-05-26.
#' @keywords datasets
NULL


#' Reference table - diameter 2-inch class codes (DIA).
#' 
#' Table with min (MIN), max (MAX), and 2-inch class diameter codes (MEANING).
#' 
#' 
#' @name ref_diacl2in
#' @docType data
#' @format A dataframe with 3 columns, MIN, MAX, and MEANING.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source Imported from comma-delimited file.
#' @keywords datasets
NULL


#' Reference table - ICE - land use or land cover change.
#' 
#' Table with land use or land cover change codes and descriptions.
#' 
#' 
#' @name ref_ICE_agent
#' @docType data
#' @format A dataframe with 6 columns: chg_ag_2, chg_ag_2_nm, chg_ag_2_GRP,
#' chg_ag_2_GRP_nm, change_pnt, change_pnt_nm.
#' @source Imported from comma-delimited file.
#' @keywords datasets
NULL


#' Reference table - ICE - land use or land cover change.
#' 
#' Table with land use or land cover change codes and descriptions.
#' 
#' 
#' @name ref_ICE_change
#' @docType data
#' @format A dataframe with 4 columns: change_1_2, change_1_2_nm,
#' change_1_2_GRP, change_1_2_GRP_nm.
#' @source Imported from comma-delimited file.
#' @keywords datasets
NULL


#' Reference table - ICE - land use cover.
#' 
#' Table with land cover codes and descriptions.
#' 
#' 
#' @name ref_ICE_cover
#' @docType data
#' @format A dataframe with 7 columns: cover, cover_nm, cover_GRP,
#' cover_GRP_nm, cover_GRP2, cover_GRP2_nm, COLOR.
#' @source Imported from comma-delimited file.
#' @keywords datasets
NULL


#' Reference table - for generating tables.
#' 
#' Table with row/column domain (VARNM) and their pretty names for table output
#' (TABLENM).
#' 
#' 
#' @name ref_ICE_domain
#' @docType data
#' @format A dataframe with 3 columns, DOMCODE, DOMNAME, DOMTITLE.
#' @source FIA ICE look-up table.
#' @keywords datasets
NULL


#' Reference table - ICE - land use.
#' 
#' Table with land use codes and descriptions.
#' 
#' 
#' @name ref_ICE_use
#' @docType data
#' @format A dataframe with 4 columns: use, use_nm, use_FOR, use_FOR_nm.
#' @source Imported from comma-delimited file.
#' @keywords datasets
NULL


#' Reference table - Metadata for icepnt external dataset.
#' 
#' Data frame with variable names and descriptions.
#' 
#' 
#' @name ref_icepnt
#' @docType data
#' @format A data frame with 8 rows and 2 columns VARIABLE - Variable in plt
#' data frame DESCRIPTION - Description of variable in data frame
#' @source FIA look-up table
#' @keywords datasets
NULL


#' Reference table - 20-year stand age classes(STDAGE).
#' 
#' Table with min (MIN), and stand age class name (STDAGECLNM).
#' 
#' 
#' @name ref_stdagecl
#' @docType data
#' @format A dataframe with 2 columns, MIN, and STDAGENM.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source Imported from comma-delimited file.
#' @keywords datasets
NULL


