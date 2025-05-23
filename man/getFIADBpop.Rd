% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IEVALIDator_compare.R
\name{getFIADBpop}
\alias{getFIADBpop}
\title{Get FIADB pop}
\usage{
getFIADBpop(
  state = NULL,
  evaltype = NULL,
  evalyr = NULL,
  evalid = NULL,
  datsource = "datamart",
  dsn = NULL,
  dbconn = NULL,
  schema. = "",
  dbconnopen = TRUE
)
}
\arguments{
\item{state}{State name}

\item{evaltype}{Evaluation Type  c('ALL', 'CURR', 'VOL', 'LULC', 'P2VEG', 'INV', 'DWM', 'CHNG', 'GRM', 'GROW', 'MORT', 'REMV')}

\item{evalyr}{Evaluation year (Two digits e.g. 19 for 2019)}

\item{evalid}{EVALID}

\item{datsource}{Data source ('datamart', 'sqlite')}

\item{dsn}{Data source name}

\item{dbconn}{Open Database connection}

\item{schema.}{Schema name. Optional}

\item{dbconnopen}{Bool. Leave dbconn open?}
}
\description{
Get FIADB pop
}
