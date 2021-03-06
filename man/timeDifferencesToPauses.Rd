% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eventMain.R
\name{timeDifferencesToPauses}
\alias{timeDifferencesToPauses}
\title{Time Differences to Columns "pBefore" and "pAfter"}
\usage{
timeDifferencesToPauses(timeDifferences, signalWidth = 0)
}
\arguments{
\item{timeDifferences}{numeric vector representing time differences}

\item{signalWidth}{difference between two consecutive timesteps in the 
original time series}
}
\value{
data frame with columns \emph{pBefore}, \emph{pAfter}, containing the
  given \emph{timeDifferences}, shifted against each other by one row, i.e.
  the first element in column \emph{pBefore} and the last element in column 
  \emph{pAfter} will be NA.
}
\description{
Time Differences to Columns "pBefore" and "pAfter"
}
