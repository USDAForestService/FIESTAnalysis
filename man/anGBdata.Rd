% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anGBdata.R
\name{anGBdata}
\alias{anGBdata}
\title{Analysis - Get data for Green-Book module.}
\usage{
anGBdata(
  bnd_layer,
  bnd_dsn = NULL,
  bnd.att = NULL,
  bnd.filter = NULL,
  RS = NULL,
  strata = TRUE,
  strattype = "RASTER",
  strat_layer = NULL,
  strat_dsn = NULL,
  strvar = NULL,
  rast.NODATA = 0,
  minplots = 0,
  showsteps = FALSE,
  cex.plots = 0.5,
  returnxy = FALSE,
  savexy = FALSE,
  savedata = FALSE,
  savesteps = FALSE,
  saveobj = FALSE,
  objnm = NULL,
  savedata_opts = NULL,
  pltdat = NULL,
  ...
)
}
\arguments{
\item{bnd_layer}{sf R object or String. Area of interest (population
boundary or estimation unit). Can be a spatial polygon object, full pathname
to a shapefile, or name of a layer within a database.}

\item{bnd_dsn}{String. Data source name (dsn; e.g., sqlite or shapefile
pathname) of boundary. The dsn varies by driver. See gdal OGR vector formats
(https://www.gdal.org/ogr_formats.html). Optional if bnd is sf object.}

\item{bnd.att}{String. The attribute in bnd_layer that defines the
estimation unit(s). If NULL, bnd is default as one estimation unit.}

\item{bnd.filter}{String. Filter to subset bnd SpatialPolygons layer.}

\item{RS}{String. Name of FIA research station to restrict states to.}

\item{strata}{Logical. If TRUE, get strata weights from strat_layer.}

\item{strattype}{String. Spatial layer type of stratlayer ("POLY",
"RASTER").  Note: raster objects must be written to file.}

\item{strat_layer}{String or raster object. Name of the strata spatial
raster layer, including fullpath and extension (e.g., *.img). If raster
object, must be written to file.}

\item{strat_dsn}{String. The data source name (dsn; i.e., folder or database
name, if polygon layer) of strat_layer. The dsn varies by driver. See gdal
OGR vector formats (https://www.gdal.org/ogr_formats.html). Optional.}

\item{strvar}{String. If strattype="POLY", name of strata attribute in
strat_layer.}

\item{rast.NODATA}{Numeric vector. NODATA value for strat_layer, 
if strattype="RASTER".}

\item{minplots}{Integer. Minimum number of plots in any on domain unit.}

\item{showsteps}{Logical. If TRUE, show intermediate steps.}

\item{cex.plots}{Number. Size of dots representing plots in display.}

\item{returnxy}{Logical. If TRUE, returns XY coordinates.}

\item{savexy}{Logical. If TRUE, saves XY data to outfolder.}

\item{savedata}{Logical. If TRUE, saves data to outfolder.}

\item{savesteps}{Logical. If TRUE, saves plotting steps as jpg images to
outfolder.}

\item{saveobj}{Logical. If TRUE, save SAest object to outfolder.}

\item{objnm}{String. Name of *.rds object.}

\item{savedata_opts}{List. See help(savedata_options()) for a list
of options. Only used when savedata = TRUE.}

\item{pltdat}{R object. FIA plot data, output from spGetPlots().}

\item{...}{Variables passed to spGetPlots.}
}
\value{
Data.
}
\description{
Wrapper to get plot data and stratification data within a given 
boundary for use in FIESTA's Green-Book module.
}
\examples{
  ## Get data for WY Bighorn administrative boundary using
  WYbhfn <- system.file("extdata",
   "sp_data/WYbighorn_adminbnd.shp",
    package="FIESTAnalysis")
  fornffn <- system.file("extdata",
   "sp_data/WYbighorn_forest_nonforest_250m.tif",
    package="FIESTAnalysis")

  ## Get data for calculating GB population data and generating GB estimates
  GBdata <- anGBdata(WYbhfn, 
                     datsource = "datamart", 
                     strat_layer = fornffn, 
                     eval = 'FIA',
                     eval_opts = list(Cur = TRUE),
                     returnxy = TRUE)
  names(GBdata)

  plot(sf::st_geometry(GBdata$bnd))
  plot(sf::st_geometry(GBdata$spxy), add=TRUE, pch=18, cex=.5)

  dim(GBdata$spxy)
  dim(GBdata$plt)
  dim(GBdata$pltassgn)

  ## Look at unitarea, area by estimation and stratalut, with strata pixel counts and weights
  GBdata$unitarea
  GBdata$stratalut

  ## Display first 6 rows of pltassgn table with plot assignments for strata
  head(GBdata$pltassgn)

}
\author{
Tracey S. Frescino
}
\keyword{data}
