% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anGBgetpop_evallst.R
\name{anGBgetpop_evallst}
\alias{anGBgetpop_evallst}
\title{Analysis - Generate Green-Book estimates for one or more FIA Evaluations.}
\usage{
anGBgetpop_evallst(
  states = NULL,
  RS = NULL,
  datsource = "datamart",
  data_dsn = NULL,
  dbTabs = dbTables(),
  eval = "FIA",
  eval_opts = eval_options(Type = "VOL"),
  byEndyr = FALSE,
  savedata = FALSE,
  outfolder = NULL,
  ...
)
}
\arguments{
\item{states}{String vector. Name(s) of states in evaluation.}

\item{RS}{String vector. Name of research station(s)
('RMRS','SRS','NCRS','NERS','PNWRS').  Do not use if states is populated.}

\item{datsource}{String. Source of data ('datamart', 'sqlite').}

\item{data_dsn}{String. If datsource='sqlite', the name of SQLite database
(*.sqlite).}

\item{dbTabs}{List of database tables the user would like returned.
See help(dbTables) for a list of options.}

\item{eval}{String. Type of evaluation time frame for data extraction 
('FIA', 'custom'). See eval_opts for more further options.}

\item{eval_opts}{List of evaluation options for 'FIA' or 'custom'
evaluations to determine the set of data returned. See help(eval_options)
for a list of options.}

\item{byEndyr}{Logical. If TRUE, organizes evalidlst by evalEndyr.  Set TRUE
if estimates include more than one state (e.g., if area of interest crosses
more than one state - Manti-LaSal).}

\item{savedata}{Logical. If TRUE, saves data to outfolder.}

\item{outfolder}{String. The output folder path. If NULL and savedata=TRUE,
outfolder is the working directory.}

\item{...}{Parameters to modGBpop().}
}
\value{
Set of population data from modGBpop() for each Evaluation
specified.
}
\description{
Wrapper to generate state-level estimates using FIESTA's Green-Book
module, for one or more FIA Evaluations.
}
\examples{
  ## Calculate population data for Utah, area evaluations 2015-2019
  GBpoplst <- anGBgetpop_evallst(states = "Utah", 
                        eval_opts = list(Endyr=c(2015:2019), Type = "CURR"))

  ## Get acres by forest type from each Evaluation in GBpoplst
  lapply(GBpoplst, function(x) modGBarea(GBpopdat = x, rowvar = "FORTYPGRPCD",
	              landarea = "FOREST", table_opts = list(row.FIAname=TRUE)))

}
\author{
Tracey S. Frescino
}
\keyword{data}
