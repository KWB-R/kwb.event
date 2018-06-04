\name{rainEvents}
\docType{data}
\alias{rainEvents}
\title{Example rain events}
\description{
Example lists of rain events observed at different rain gauges.
}
\usage{data(rainEvents)}
\format{List of eleven data frames each of which represents a list of rain 
events. Each data frame has the columns \emph{tBeg} (first timestamp), 
\emph{tEnd} (last timestamp), \emph{dur} (duration in minutes), \emph{pBefore} 
(duration of dry period before the event in minutes), \emph{pAfter} (duration of 
dry period after the event in minutes), \emph{event} (event number), 
\emph{max} (maximum rain intensity in mm/5min), \emph{mean} (mean rain 
intensitiy in mm/5min), \emph{min} (minimum rain intensity in mm/5min), 
\emph{sum} (rain height in mm). To each data frame attributes are assigned that
contain information about the parameters that were applied during the event
creation process.
}
\keyword{rain event}