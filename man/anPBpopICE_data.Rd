% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anPBpopICE_data.R
\name{anPBpopICE_data}
\alias{anPBpopICE_data}
\title{ANALYSIS - Photo-based estimates for Image Change Estimation (ICE).}
\usage{
anPBpopICE_data(
  ice.pntfn,
  ice.pltfn = NULL,
  plotid = "plot_id",
  pntid = "dot_cnt",
  changelut = NULL,
  coverlut = NULL,
  uselut = NULL,
  agentlut = NULL,
  appendluts = TRUE,
  domlut = NULL,
  savedata = FALSE,
  saveobj = FALSE,
  objnm = "PBpopICE",
  savedata_opts = NULL
)
}
\arguments{
\item{ice.pntfn}{Comma-delimited file (*.csv). The full path file name for
the ICE point-level data.}

\item{ice.pltfn}{Comma-delimited file (*.csv). The full path file name for
the ICE plot-level data.}

\item{plotid}{String. Name of unique identifier for plots.}

\item{pntid}{String. Name of unique identifier for points.}

\item{changelut}{Data frame or comma-delimted file. A different look up
table for land use or land cover change (see ref_ICE_change).}

\item{coverlut}{Data frame or comma-delimited file. A different look up
table for land cover (see ref_ICE_cover).}

\item{uselut}{Data frame or comma-delimited file. A different look up table
for land cover (see ref_ICE_use).}

\item{agentlut}{Data frame or comma-delimited file. A different look up
table for causal agent (see ref_ICE_agent).}

\item{appendluts}{Logical. If TRUE, appends all look up tables to ice.dots.}

\item{domlut}{Data frame or comma-delimited file. A look up table for
defining domain codes, names, and titles. If NULL, a default table is used.}

\item{savedata}{Logical. If TRUE, saves tables to outfolder.}

\item{saveobj}{Logical. If TRUE, saves returned list object to outfolder.}

\item{objnm}{String. Name of *.rds object.}

\item{savedata_opts}{List. See help(savedata_options()) for a list
of options. Only used when savedata = TRUE.}
}
\value{
\item{ice.dots}{ DF. ICE point data, including look up table data,
if appendluts=TRUE.  } \item{changelut}{ DF. The look up table for land use
or land cover change. } \item{coverlut}{ DF. The look up table for land
cover change. } \item{uselut}{ DF. The look up table for land use change. }
\item{agentlut}{ DF. The look up table for causal agent. } \item{domlut}{
DF. The look up table for domain information. }

If savedata=TRUE, the ice.dots data frame is written to outfolder, including
look up table data, if appendluts=TRUE.
}
\description{
Generates percent or acre estimates by domain (and estimation unit).
}
\details{
domlut - the domain look up table

\tabular{llll}{ \tab \bold{DOMCODE} \tab \bold{DOMNAME} \tab \bold{DOMTITLE}
\cr \tab change_1_2 \tab change_1_2_nm \tab Change \cr \tab change_1_2_GRP
\tab change_1_2_GRP_nm \tab Change \cr \tab cover_1 \tab cover_1_nm \tab
T1-Cover \cr \tab cover_2 \tab cover_2_nm \tab T2-Cover \cr \tab use_1 \tab
use_1_nm \tab T1-Use \cr \tab use_2 \tab use_2_nm \tab T2-Use \cr \tab
chg_ag_2 \tab chg_ag_2_nm \tab Change Agent \cr \tab chg_ag_2_GRP \tab
chg_ag_2_GRP_nm \tab Change Agent-GRP \cr \tab cover_1_GRP \tab
cover_1_GRP_nm \tab T1-Cover-GRP \cr \tab cover_2_GRP \tab cover_2_GRP_nm
\tab T2-Cover-GRP \cr \tab use_1_FOR \tab use_1_FOR_nm \tab T1-Use-FOR \cr
\tab use_2_FOR \tab use_2_FOR_nm \tab T2-Use-FOR \cr \tab change_pnt \tab
change_pnt_nm \tab Change \cr \tab use_1_2 \tab use_1_2_nm \tab Use_Change
\cr \tab cover_1_2 \tab cover_1_2_nm \tab Cover_Change \cr \tab use_1_2_FOR
\tab use_1_2_FOR_nm \tab Use_FOR_Change \cr }
}
\author{
Tracey S. Frescino
}
\keyword{data}
