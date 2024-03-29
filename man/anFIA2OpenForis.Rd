% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anFIA2OpenForis.R
\name{anFIA2OpenForis}
\alias{anFIA2OpenForis}
\title{ANALYSIS - Translate FIA data to Open Foris Collect/Calc data sets.}
\usage{
anFIA2OpenForis(
  pltstrat,
  cond,
  tree,
  seed,
  unitarea,
  strlut,
  level_1_code,
  level_1_label = NULL,
  level_2_code = NULL,
  level_2_label = NULL,
  calc = TRUE,
  collect = TRUE,
  savedata = FALSE,
  outfolder = NULL
)
}
\arguments{
\item{pltstrat}{DF/DT, comma-delimited file(*.csv), SpatialDataFrame, or
shapefile(*.shp) with one record per plot and strata/estimation unit
variable(s).}

\item{cond}{DF/DT or comma-delimited file (*.csv). The condition-level table
with one record per condtions and including nonsampled conditions.}

\item{tree}{DF/DT or comma-delimited file(*.csv). The tree table with tree
data to aggregate to condition-level. See details for necessary variables to
include.}

\item{seed}{DF/DT or comma-delimited file (*.csv). Seedling table.}

\item{unitarea}{Numeric or DF. Total area by estimation unit. If only 1
estimation unit, include number of total acreage for the area of interest or
a data frame with areavar. If more than one estimation unit, provide a data
frame of total area by estimation unit.}

\item{strlut}{DF/DT. If strata=TRUE, look-up table with strata proportions
('strwt') by strata (and estimation unit). To calculate 'strwt', set
getwt=TRUE and getwtvar= name of variable with information to calculate
weights from (e.g., pixel counts).}

\item{level_1_code}{String. Name of level 1 estimation unit variable with
code values (e.g., state).}

\item{level_1_label}{String. Name of level 1 estimation unit variable with
code value descriptions.}

\item{level_2_code}{String. Name of level 2 estimation unit variable with
code values (e.g., county).}

\item{level_2_label}{String. Name of level 2 estimation unit variable with
code value descriptions.}

\item{calc}{Logical. If TRUE, creates tables for use in Open Foris Calc.}

\item{collect}{Logical. If TRUE, creates tables for use in Open Foris
Collect.}

\item{savedata}{Logical. If TRUE, saves table(s) to outfolder.}

\item{outfolder}{String. The outfolder to write files to. If NULL, files are
written to working directory, or if gui, a window to browse.}
}
\description{
Creates comma-delimited files that can be input into an Open Foris Collect,
FIA survey design to use as input data into the Open Foris Calc estimation
tool.
}
\author{
Tracey S. Frescino
}
\keyword{data}
