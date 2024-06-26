% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FIESTAnalysis-package.R
\docType{data}
\name{ref_ALP}
\alias{ref_ALP}
\title{Reference table - for Automated Land Program National Forests.}
\format{
A dataframe with 2 columns, R1 and FORESTNAME.
}
\source{
Downloaded from the USDA Forest Service FSGeodata Clearinghose on
2020 June 3, format ESRI geodatabase
(https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=surface+ownership)

U.S. Forest Service, Automated Land Program (ALP) Publication Date:
2020-05-26.
}
\description{
National Forest region and names from the USDA S_USA.BasicOwnershipFS
(Surface Ownership Parcels) attribute table. An area depicted as surface
ownership parcels dissolved on the same ownership classification
}
\note{
## ALP National Forest boundary alp_dsn <-
"S_USA.BasicOwnershipFS.gdb" alp_layer <- "BasicOwnershipFS" alp.att <-
"FORESTNAME" alp_bnd <- spImportSpatial(dsn=alp_dsn, layer=alp_layer)
alp_names <- unique(sf::st_drop_geometry(alp_bnd[, c("REGION",
"FORESTNAME")])) alp_names <- alp_names[order(alp_names$REGION,
alp_names$FORESTNAME),] alp_names <- alp_names[grepl("National Forest",
alp_names$FORESTNAME)==TRUE,] alp_names$FORESTNAME <- sub(" National
Forest", "", alp_names$FORESTNAME)

Ownership classes that are not USDA Forest Service were removed from the
layer.\cr OWNERCLASSIFICATION = "USDA FOREST SERVICE" subset with following
query alpfs <- alpfs[!is.na(alpfs$OWNERCLASSIFICATION) &
alpfs$OWNERCLASSIFICATION != 'NON-FS',]
}
\keyword{datasets}
