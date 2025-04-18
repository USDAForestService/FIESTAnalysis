% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anGBpop_report.R
\name{anGBpop_report}
\alias{anGBpop_report}
\title{ANALYSIS - Generate a report for Green-Book custom population.}
\usage{
anGBpop_report(
  GBpopdat,
  AOInm,
  photofn = NULL,
  photo_author = NULL,
  EVALIDator_match = FALSE,
  pcfilter = NULL,
  spcd = 746,
  title.ref = NULL,
  outfolder = NULL,
  reportnm = NULL
)
}
\arguments{
\item{GBpopdat}{List object. Population data from modGBpop().}

\item{AOInm}{String. Name of area of interest.}

\item{photofn}{String. Path name to photo to add to report.}

\item{photo_author}{String. Name of photo author (Optional).}

\item{EVALIDator_match}{Logical. If TRUE, estimates match EVALIDator.}

\item{pcfilter}{String. Name of variable or string to filter plots or
conditions.  Must be in R syntax (e.g., ADFORCD == 408, COUNTYCD == 1).
Optional.}

\item{spcd}{Integer. A species code to use as focus species of the report.
Multiple codes are accepted if the focus species is a combined group.
Default is 746 (quaking aspen).}

\item{title.ref}{String. Reference information for title and output file
names.}

\item{outfolder}{String. The path of folder to output tables.}

\item{reportnm}{String. The name of the output report. If NULL, 
defaults to AOInm '_rport.docx'.}
}
\description{
Generates a standard report based on area of interest.
}
\details{
If variables are NULL, then it will prompt user to input variables.
}
\author{
Tracey S. Frescino
}
\keyword{data}
