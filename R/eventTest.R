# exampleEvents ----------------------------------------------------------------

#' Example Events
#' 
#' Example events for testing purposes
#' 
#' @param signalWidth see description in \code{\link{hsEvents}}
#' @param eventSeparationTime see description of \code{evtSepTime} in
#'   \code{\link{hsEvents}}
#' @param from first day as character string in format yyyy-mm-dd
#' @param to last day as character string in format yyyy-mm-dd
#' @param signalDensity fraction of all timestamps in a full sequence of
#'   timestamps that are to be selected randomly from the sequence and that are
#'   treated as the "signals" contributing to an event. Default: 0.01, i.e. one
#'   percent of a full sequence of timestamps are randomly selected, ordered and
#'   passed on to \code{\link{hsEvents}} that groups these "signal" timestamps
#'   into events
#' @param \dots further arguments passed to \code{\link{hsEvents}}
#' @export
#' @examples 
#' events <- exampleEvents()
#'   
#' # Calculate event durations manually
#' dur <- as.integer(events$tEnd) - as.integer(events$tBeg) + hsSigWidth(events)
#'   
#' # All durations should be equal to the durations given in column "dur"
#' all(dur == events$dur) 
#'   
#' # All pauses after event i should be equal to the pauses before event i+1
#' all(events$pBefore[-1] == events$pAfter[-nrow(events)])
#'   
exampleEvents <- function(
  signalWidth = 60, eventSeparationTime = 60 * signalWidth, from = "2015-06-11",
  to = "2015-06-12", signalDensity = 0.01, ...
)
{
  timestamps <- kwb.datetime::to.GMT.plus.1(
    kwb.datetime::sequenceOfTimestamps(from, to, step.s = signalWidth)
  )
  
  N <- length(timestamps)
  
  events <- hsEvents(
    tseries = timestamps[sort(sample(N, size = signalDensity * N))], 
    evtSepTime = eventSeparationTime,
    signalWidth = signalWidth, 
    ...
  )
  
  kwb.utils::removeColumns(events, c("iBeg", "iEnd"))
}

# .test_JoinEvents -------------------------------------------------------------

.test_JoinEvents <- function()
{  
  events <- .randomEvents()
  joined <- hsJoinEvents(events, 1:nrow(events))
  n <- nrow(joined)
  
  stopifnot(n == 1 
            || joined$iBeg != events$iBeg[1] 
            || joined$iEnd != events$iEnd[n]
            || joined$tBeg != events$tBeg[1]
            || joined$tEnd != events$tEnd[n]
            || joined$dur  != sum(events$dur, na.rm = TRUE))
  
  print(joined)
}

# .randomEvents ----------------------------------------------------------------

.randomEvents <- function(timestep = 3600)
{
  timestamps <- .deleteItemsRandomly(.testSeriesOfTimestamps(20, timestep), 5)  
  print(timestamps)
  events <- hsEvents(timestamps, timestep, timestep)
  print(events)  
}

# .testSeriesOfTimestamps ------------------------------------------------------

.testSeriesOfTimestamps <- function(length = 60 * 24, timestep = 60)
{
  starttime <- "2000-01-01 00:00:00"
  seq(kwb.datetime::hsToPosix(starttime), by = timestep, length.out = length)
}

# .deleteItemsRandomly ---------------------------------------------------------

.deleteItemsRandomly <- function(x, n)
{
  sort(x[-sample(seq_along(x), n)])
}
