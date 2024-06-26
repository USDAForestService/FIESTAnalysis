% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anGetData_tsum.R
\name{anGetData_tsum}
\alias{anGetData_tsum}
\title{Data - Get plot data and auxiliary data for estimation.}
\usage{
anGetData_tsum(
  bnd_layer,
  bnd_dsn = NULL,
  bnd.att = NULL,
  bnd.filter = NULL,
  RS = NULL,
  estvarlst = NULL,
  estvar.filter = "STATUSCD == 1",
  bycond = TRUE,
  TPA = TRUE,
  getadjplot = TRUE,
  polyvlst = NULL,
  polyv_dsn = NULL,
  rastfolder = NULL,
  rastlst.cont = NULL,
  rastlst.cont.name = NULL,
  rastlst.cont.NODATA = NULL,
  rastlst.cat = NULL,
  rastlst.cat.name = NULL,
  rastlst.cat.NODATA = NULL,
  keepNA = TRUE,
  strata = FALSE,
  strvar = NULL,
  vars2keep = NULL,
  minplots = 0,
  showsteps = FALSE,
  returnxy = FALSE,
  savedata = FALSE,
  exportsp = FALSE,
  savesteps = FALSE,
  saveobj = FALSE,
  objnm = "tsumdat",
  savedata_opts = NULL,
  pltdat = NULL,
  auxdat = NULL,
  ncores = 1,
  ...
)
}
\arguments{
\item{bnd_layer}{sf R object or String. Boundary layer of estimation domain
units. Can be a spatial sf object, full pathname to a shapefile, or name of
a layer within a database.}

\item{bnd_dsn}{String. Name of data source name with bnd_layer, if in a
database.}

\item{bnd.att}{String. Name of attribute in boundary file to define
estimation domain units.}

\item{bnd.filter}{String. Optional filter of bnd_layer.}

\item{RS}{String. Name of FIA research station to restrict states to
('RMRS','SRS','NCRS','NERS','PNWRS'). If NULL, all research stations are
included.}

\item{estvarlst}{String (vector). Tree-level variable(s) estimation
variables (e.g., "TPA_UNADJ", "BA").}

\item{estvar.filter}{String. A filter for all variables in estvarlst.}

\item{bycond}{Logical. If TRUE, summaries by condition.}

\item{TPA}{Logical. If TRUE, tsumvarlst variable(s) are multiplied by the
respective trees-per-acre variable to get per-acre measurements.}

\item{getadjplot}{Logical. If TRUE, adjustments are calculated for
nonsampled conditions on plot.}

\item{polyvlst}{sf R object or String. Name(s) of polygon layers to extract
values. A spatial polygon object, full path to shapefile, or name of a layer
within a database.}

\item{polyv_dsn}{String. Data source name (dsn) where polyvlst layers are
found (e.g., *.sqlite, *.gdb, folder name). The dsn varies by driver.  See}

\item{rastfolder}{String. Name of the folder with raster layers. Optional.
Useful if all raster layers are in same folder.}

\item{rastlst.cont}{String vector or list. A list of raster(s) with
continuous data values (e.g., DEM). The list may include file name of
raster(s) or raster objects that are not InMemory.}

\item{rastlst.cont.name}{String vector. Output names for continuous rasters.
Optional. If NULL, name of raster is used as default or name+'_'+layer
number for multi-band layers.}

\item{rastlst.cont.NODATA}{Numeric vector. NODATA value for categorical
rasters (See notes). These values will be converted to NA and removed from
output if keepNA=FALSE. If 1 number, the same value will be used for all
categorical rasters. If more than 1 number, the number of values must be
equal to the number of rasters in rastlst.cont.}

\item{rastlst.cat}{String vector or list. A list of raster(s) with thematic
(i.e., categorical) data values. The list may include file name of raster(s)
or raster objects that are not InMemory.}

\item{rastlst.cat.name}{String vector. Output names for categorical rasters.
If NULL, name of raster is used as default or name+'_'+layer number for
multi-band layers.}

\item{rastlst.cat.NODATA}{Numeric vector. NODATA value for categorical
rasters (See notes). These values will be converted to NA and removed from
output if keepNA=FALSE. If 1 number, the same value will be used for all
categorical rasters. If more than 1 number, the number of values must be
equal to the number of rasters in rastlst.cat.}

\item{keepNA}{Logical. If TRUE, NA values in data extraction are retained 
in output.}

\item{strata}{Logical. If TRUE, include stratalut in output dataset.}

\item{strvar}{String. Name of strata variable to include in stratalut.  Name
must be in rastlst.cat.name or rastlst.cat (if rastlst.cat.name= NULL).}

\item{vars2keep}{String. Name of variable(s) in smallbnd to keep for output.}

\item{minplots}{Integer. Minimum number of plots in any on domain unit.}

\item{showsteps}{Logical. If TRUE, clipped points are displayed.  If
savedata=TRUE, the display is saved as *.jpg to outfolder.}

\item{returnxy}{Logical. If TRUE, returns XY coordinates.}

\item{savedata}{Logical. If TRUE, saves data to outfolder. Note:
includes XY data if returnxy = TRUE.}

\item{exportsp}{Logical. If TRUE, exports spatial object.}

\item{savesteps}{Logical. If TRUE, save steps spatial intermediate layers.}

\item{saveobj}{Logical. If TRUE, save SAest object to outfolder.}

\item{objnm}{String. Name of saved object name.}

\item{savedata_opts}{List. See help(savedata_options()) for a list
of options. Only used when savedata = TRUE.}

\item{pltdat}{R object. FIA plot data, output from spGetPlots().}

\item{auxdat}{R object. Auxiliary data, output from spGetAuxiliary().}

\item{ncores}{R object. Number of cores to use for data extraction.
(Note: needs parallel package).}

\item{...}{Other parameters for DBgetPlots.}
}
\value{
Data.
}
\description{
Wrapper to get plot data and auxiliary data for estimation.
}
\note{
If exportsp=TRUE:\cr If out_fmt="shp", the writeOGR (rgdal) function is
called. The ArcGIS driver truncates variable names to 10 characters or less.
Variable names are changed before export using an internal function.
If Spatial o bject has more than 1 record, it will be returned but not exported.

The spTransform (rgdal) method is used for on-the-fly map projection
conversion and datum transformation using PROJ.4 arguments. Datum
transformation only occurs if the +datum tag is present in the both the from
and to PROJ.4 strings. The +towgs84 tag is used when no datum transformation
is needed. PROJ.4 transformations assume NAD83 and WGS84 are identical
unless other transformation parameters are specified.  Be aware, providing
inaccurate or incomplete CRS information may lead to erroneous data shifts
when reprojecting. See spTransform help documentation for more details.
}
\author{
Tracey S. Frescino
}
\keyword{data}
