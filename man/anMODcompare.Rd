% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anMODcompare.R
\name{anMODcompare}
\alias{anMODcompare}
\title{Analysis - Compare estimates for custom-defined boundary.}
\usage{
anMODcompare(
  bnd = NULL,
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
  ...
)
}
\arguments{
\item{bnd}{sf R object or String. Area of interest (population
boundary or estimation unit). Can be a spatial polygon object, full pathname
to a shapefile, or name of a layer within a database.}

\item{bnd_dsn}{String. Data source name (dsn; e.g., sqlite or shapefile
pathname) of boundary. The dsn varies by driver. See gdal OGR vector formats
(https://www.gdal.org/ogr_formats.html). Optional if bnd is sf object.}

\item{bnd.att}{String. The attribute in bnd_layer that defines the
estimation unit(s). If NULL, bnd is default as one estimation unit.multestvar}

\item{bnd.filter}{String. Filter to subset bnd SpatialPolygons layer.}

\item{largebnd.unique}{String. The FIESTAnalysis::ecomap layer is used
as default for the borrowing large extent. The unique id can be
'SECTION', 'PROVINCE', or NULL.}

\item{strvar}{String. Name of strata variable. Note. Must be in rastlst.cat.}

\item{landarea}{String. The sample area filter for estimates ('ALL',
'FOREST', 'TIMBERLAND').  If landarea=FOREST, filtered to COND_STATUS_CD =
1; If landarea=TIMBERLAND, filtered to SITECLCD in(1:6) and RESERVCD = 0.}

\item{pcfilter}{String. A filter for plot or cond attributes (including
pltassgn).  Must be R logical syntax.}

\item{estvarlst}{String vector. One or more tree-level estimate variable
(e.g., 'VOLCFNET').}

\item{tfilterlst}{String vector. One or more tree filters ('live', 'dead').}

\item{intensity1}{Logical. If TRUE, uses only INTENSITY=1 (i.e., P2 plots)
for GB and MA estimates, to assure an equal probability sample.}

\item{forprop}{Logical. If TRUE, adds forest estimates of forest proportion.}

\item{totals}{Logical. If TRUE, sums per acre values to total values by 
by multiplying by total acres.}

\item{pse}{Logical. Add pse columns to table.}

\item{estround}{Integer. Number of digits to round estimates and estimates.se.}

\item{byeach}{Logical. If TRUE, creates an SAdom for each smallbnd polygon.}

\item{prednames}{String vector. One or more predictors to use for estimation. 
Names must match names in SAdatalst$prednames}

\item{modelselect}{Logical. If TRUE, model selection occurs.}

\item{showsteps}{Logical. If TRUE, intermediate steps of selection process
are displayed.}

\item{savedata}{Logical. If TRUE, save SAdoms spatial layer to outfolder.}

\item{savesteps}{Logical. If TRUE, save steps spatial intermediate layers
and JPG images. All spatial layers are output as *.shp format in a separate
folder (SAdoms_steps).}

\item{saveobj}{Logical. If TRUE, save objects.}

\item{savedata_opts}{List. See help(savedata_options()) for a list
of options. Only used when savedata = TRUE.}

\item{SAdomdat}{List object. Output from spGetSAdoms().}

\item{SAdatalst}{List object. Output from anGetData_list().}

\item{...}{Parameters for input to anGetData_list().}
}
\value{
Estimates
}
\description{
Wrapper to compare estimates from FIESTA's SAmodule, MAmodule, 
and GBmodule.
}
\author{
Tracey S. Frescino
}
\keyword{data}
