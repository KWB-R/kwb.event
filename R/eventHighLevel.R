# getAndFilterEventsWithStatistics ---------------------------------------------

#' Get and filter Events with Statistics
#' 
#' @param rainData passed to \code{\link{getEventsWithStatistics}}
#' @param seriesName passed to \code{\link{getEventsWithStatistics}}
#' @param eventSeparationTime passed to \code{\link{getEventsWithStatistics}}
#' @param signalThreshold passed to \code{\link{getEventsWithStatistics}}
#' @param durationThreshold passed to \code{\link{filterEventsWithStatistics}}
#' @param sumThreshold passed to \code{\link{filterEventsWithStatistics}}
#' @param signalComparisonOperator passed to \code{\link{getEventsWithStatistics}}
#' @param durationComparisonOperator \code{\link{filterEventsWithStatistics}}
#' @param sumComparisonOperator \code{\link{filterEventsWithStatistics}}
#' @param signalWidth passed to \code{\link{getEventsWithStatistics}}
#' 
getAndFilterEventsWithStatistics <- function(
  rainData, 
  seriesName, 
  eventSeparationTime,
  signalThreshold = 0, 
  durationThreshold = 1, 
  sumThreshold = 0, 
  signalComparisonOperator = "gt",
  durationComparisonOperator = "gt", 
  sumComparisonOperator = "gt",
  signalWidth = NA
)
{
  functions <- c("max", "sum", "mean")
  
  eventData <- getEventsWithStatistics(
    rainData = rainData, 
    seriesName = seriesName, 
    eventSeparationTime = eventSeparationTime,
    signalThreshold = signalThreshold, 
    signalComparisonOperator = signalComparisonOperator,
    functions = functions,
    signalWidth = signalWidth
  )
  
  filterEventsWithStatistics(
    eventData = eventData, 
    durationThreshold = durationThreshold, 
    sumThreshold = sumThreshold, 
    durationComparisonOperator = durationComparisonOperator, 
    sumComparisonOperator = sumComparisonOperator
  )
}

# getEventsWithStatisticsForMultipleSeries -------------------------------------

#' Get Events with Statistics for multiple Series
#' 
#' @param rainData data frame with time stamps in the first column and rain
#'   heights (or intensities) in the remaining columns
#' @param eventSeparationTime \dQuote{event separation time} in seconds. Maximal
#'   allowed time difference between two consecutive timestamps within the same
#'   event.
#' @param signalWidth \dQuote{signal width} in seconds. Length of time interval
#'   that one timestamp is representing, e.g. \eqn{5*60 = 300} if each timestamp
#'   respresents a time interval of five minutes (as e.g. a time series is 
#'   recorded on a five minute time scale). This parameter is needed to 
#'   calculate event durations.
#' @param signalThreshold value that needs to be exceeded
#'   (signalComparisonOperator == "gt") or reached (signalComparisonOperator ==
#'   "ge") by the rain heights (or intensities) in order to be counted as a
#'   "signal". Default: 0
#' @param signalComparisonOperator Operator to be applied when comparing rain
#'   values with signalThreshold. Must be one of "gt" (greater than) or "ge"
#'   greater than or equal. Default: "gt"
#' 
getEventsWithStatisticsForMultipleSeries <- function(
  rainData, 
  eventSeparationTime,
  signalWidth = kwb.datetime::getTimestepInSeconds(timestamps = rainData[, 1]),
  signalThreshold = 0, 
  signalComparisonOperator = "gt"
)
{
  validateEventFunctionArguments(
    rainData = rainData, 
    signalComparisonOperator = signalComparisonOperator
  )
  
  result <- list()
  
  seriesNames <- names(rainData)[-1]
  
  for (seriesName in seriesNames) {
    
    cat("*** Detecting events for:", seriesName, "...\n")
    
    result[[seriesName]] <- getEventsWithStatistics(
      rainData = rainData, 
      seriesName = seriesName, 
      eventSeparationTime = eventSeparationTime,
      signalThreshold = signalThreshold,  
      signalComparisonOperator = signalComparisonOperator, 
      functions = c("sum", "mean", "min", "max", "number.na", "length"),
      signalWidth = signalWidth
    )
  }
  
  result
}

# getEventsWithStatistics ------------------------------------------------------

#' Get Events with Statistics
#' 
#' @param rainData data frame with time stamps in the first column and rain
#'   heights (or intensities) in the remaining columns
#' @param seriesName Column name in rainData representing the time series to be
#'   analysed.
#' @param eventSeparationTime passed to \code{\link{getEvents}}
#' @param signalThreshold value that needs to be exceeded
#'   (signalComparisonOperator == "gt") or reached (signalComparisonOperator ==
#'   "ge") by the rain heights (or intensities) in order to be counted as a
#'   "signal". Default: 0
#' @param signalComparisonOperator Operator to be applied when comparing rain
#'   values with signalThreshold. Must be one of "gt" (greater than) or "ge"
#'   greater than or equal. Default: "gt"
#' @param eventSeparationOperator passed to \code{\link{getEvents}}
#' @param functions passed to \code{\link{getEventStatistics}}
#' @param signalWidth passed to \code{\link{getEvents}}
#' 
getEventsWithStatistics <- function(
  rainData,
  seriesName,
  eventSeparationTime, 
  signalThreshold = 0,
  signalComparisonOperator = "gt",
  eventSeparationOperator = "gt",
  functions = c("sum", "mean", "min", "max", "number.na", "length"),
  signalWidth = NA
)
{
  rainEvents <- getEvents(
    rainData, seriesName, signalThreshold, signalComparisonOperator,
    eventSeparationTime, eventSeparationOperator, signalWidth = signalWidth
  )
  
  rainEvents <- kwb.utils::removeColumns(rainEvents, c("iBeg", "iEnd"))
  
  # Get statistical information (based on complete data)
  statistics1 <- getEventStatistics(
    rainData, seriesName = seriesName, events = rainEvents, 
    functions = functions
  )
  
  signals <- attr(rainEvents, "signals")
  
  statistics2 <- getEventStatistics(
    signals, seriesName = seriesName, events = rainEvents, functions = "sum"
  )
  
  statistics2 <- kwb.utils::removeColumns(statistics2, "event")
  
  names(statistics2) <- paste(names(statistics2), "signal", sep = ".")
  
  statistics <- cbind(statistics1, statistics2)
  statistics <- statistics[, order(names(statistics))]
  
  kwb.utils::hsRestoreAttributes(
    cbind(rainEvents, statistics), attributes(rainEvents)
  )
}

# getEvents --------------------------------------------------------------------

#' Get Events
#' 
#' @param rainData data frame with time stamps in the first column and rain
#'   heights (or intensities) in the remaining columns
#' @param seriesName Column name in rainData representing the time series to be
#'   analysed.
#' @param signalThreshold Value that needs to be exceeded
#'   (signalComparisonOperator == "gt") or reached (signalComparisonOperator ==
#'   "ge") by the rain heights (or intensities) in order to be counted as a
#'   "signal". Default: 0
#' @param signalComparisonOperator Operator to be applied when comparing rain
#'   values with signalThreshold. Must be one of "gt" (greater than) or "ge"
#'   greater than or equal. Default: "gt".
#' @param eventSeparationTime Time difference in seconds that needs to be
#'   exceeded (eventSeparationOperator == "gt") or reached 
#'   (eventSeparationOperator == "ge") by two consecutive signals in order to
#'   let the signals belong to two distinct events. Otherwise the signals are
#'   assumed to belong to one and the same event. Default: 6*3600 = six hours.
#' @param eventSeparationOperator Operator to be applied when comparing the time
#'   differences between consecutive signals with the eventSeparationTime. Must
#'   be one of "gt" (greater than) or "ge" greater than or equal. Default: "gt".
#' @param signalWidth signal width (= length of the time interval represented by
#'   one row in \code{rainData}) in seconds
#' @param column.time name of the column containing the time. Default: Name of
#'   the first column
#'   
getEvents <- function(
  rainData, seriesName, signalThreshold = 0, signalComparisonOperator = "gt",
  eventSeparationTime = 6 * 3600, eventSeparationOperator = "gt", 
  signalWidth = NA, column.time = names(rainData)[1]
)
{
  getcol <- kwb.utils::selectColumns
  
  #validateEventFunctionArguments(
  #  rainData = rainData, 
  #  signalComparisonOperator = signalComparisonOperator
  #)
  
  # Filter for "signals"
  rowIndices <- whichAboveThreshold(
    getcol(rainData, seriesName),
    signalThreshold,
    signalComparisonOperator
  )
  
  columns <- c(column.time, seriesName)
  
  if (length(rowIndices) > 0) {
    
    signals <- getcol(rainData[rowIndices, ], columns)
    
    # Find signal width (= timestep in rainData)
    if (is.na(signalWidth)) {
      
      times <- getcol(rainData, columns[1])
      signalWidth <- kwb.datetime::getTimestepInSeconds(times)
    }

    # Get events
    events <- hsEvents(
      tseries = signals, 
      evtSepTime = eventSeparationTime, 
      signalWidth = signalWidth,
      evtSepOp = eventSeparationOperator
    )
    
    structure(
      events,
      signals = signals,
      signalThreshold = signalThreshold,
      signalComparisonOperator = signalComparisonOperator  
    )
  } else {
    
    message(
      "No values in column '", seriesName, "' are above the ", 
      "threshold (", signalThreshold, "). Returning NULL (no events)!"
    )
  }
}

# getEventStatistics -----------------------------------------------------------

#' Get Event Statistics
#' 
#' @param dataFrame data frame containing event data
#' @param seriesName name of column in \code{dataFrame}
#' @param events data frame containing event information as provided by 
#'   \code{\link{hsGetEvent}}
#' @param functions define statistical functions
#' @param eventNumbers vector of same length as \emph{events} has rows, giving
#'   the numbers that identify the events. Default: 1:nrow(\emph{events})
#'   
#' @return data frame with event number in first column \emph{event} and
#'   statistical values in further columns.
#'   
getEventStatistics <- function(
  dataFrame, seriesName, events, 
  functions = c("sum", "mean", "min", "max", "number.na"),
  eventNumbers = 1:nrow(events)
) 
{
  stopifnot(length(seriesName) == 1)
  
  indices <- kwb.utils::posixColumnAtPosition(dataFrame)
  
  columns <- c(names(dataFrame)[indices], seriesName)
  
  FUN <- function(i) {
    
    eventData <- hsGetEvent(dataFrame[, columns], events, i)
    
    kwb.utils::colStatistics(
      eventData[, -1, drop = FALSE], functions = functions
    )
  }
  
  statistics.list <- lapply(seq_len(nrow(events)), FUN)
  
  statistics <- data.frame(event = eventNumbers)
  
  for (i in seq_along(functions)) {
    
    FUN <- functions[i]
    
    statisticsPerEvent <- sapply(statistics.list, FUN = "[[", i)
    
    statistics <- cbind(statistics, statisticsPerEvent)
    
    names(statistics)[ncol(statistics)] <- FUN
  }
  
  statistics
}

# filterEventsWithStatistics ---------------------------------------------------

#' Filter Events with Statistics
#' 
#' @param eventData eventData of one time series, as returned in one list
#'   element by \code{\link{getEventsWithStatistics}}.
#' @param durationThreshold duration in seconds that needs to be exceeded
#'   (durationComparisonOperator == "gt") or reached (durationComparisonOperator
#'   == "ge") by the duration of the rain events. Default: 0
#' @param durationComparisonOperator Operator to be applied when comparing the
#'   duration of the events with \emph{durationThreshold}. Must be one of "gt"
#'   (greater than) or "ge" greater than or equal. Default: "gt"
#' @param sumThreshold value that needs to be exceeded (sumComparisonOperator ==
#'   "gt") or reached (sumComparisonOperator == "ge") by the 'sum' of values
#'   within the events. Default: 0
#' @param sumComparisonOperator Operator to be applied when comparing the 'sum'
#'   of values within the events with \emph{sumThreshold}. Must be one of "gt"
#'   (greater than) or "ge" greater than or equal. Default: "gt"
#' 
filterEventsWithStatistics <- function(
  eventData, durationThreshold = 0, durationComparisonOperator = "gt",
  sumThreshold = 0, sumComparisonOperator = "gt"
)
{
  validateEventFunctionArguments(
    durationComparisonOperator = durationComparisonOperator, 
    sumComparisonOperator = sumComparisonOperator
  )
  
  rowIndicesDur <- whichAboveThreshold(
    eventData$dur, durationThreshold, durationComparisonOperator
  )
  
  rowIndicesSum <- whichAboveThreshold(
    eventData$sum, sumThreshold, sumComparisonOperator
  )
  
  eventData[intersect(rowIndicesDur, rowIndicesSum), ]
}

# validateEventFunctionArguments -----------------------------------------------

#' Validate Event Function Arguments
#' 
#' @param \dots arguments, given as \code{key = value} pairs, to be checked for 
#'   validity
#' 
validateEventFunctionArguments <- function(...)
{
  arguments <- list(...) 
  
  argumentNames <- names(arguments)
  
  argumentName <- "rainData"
  
  if (argumentName %in% argumentNames) {
    
    rainData <- arguments[[argumentName]]
    
    stopifnot(is.data.frame(rainData))
    
    stopifnot("POSIXt" %in% class(rainData[, 1]))    
    
    stopifnot(kwb.datetime::isValidTimestampSequence(rainData[, 1]))
  }

  argumentNames <- c("durationComparisonOperator", "sumComparisonOperator")
  
  for (argumentName in argumentNames) {
    
    if (argumentName %in% argumentNames) {
      
      comparisonOperator <- arguments[[argumentName]]
      
      stopifnot (comparisonOperator %in% c("gt", "ge"))    
    }
  }
}

# whichAboveThreshold ----------------------------------------------------------

#' Which Values are above a Threshold?
#' 
#' @param values numeric vector of values to be compared with the 
#'   \code{threshold}
#' @param threshold numeric value against which to check the \code{values}
#' @param comparisonOperator if "gt" it is checked whether the \code{values} 
#'   are \emph{greater than} the \code{threshold}, otherwise it is checked
#'   whether the \code{values} are \code{greater than or equal to} the
#'   \code{threshold}
#' 
whichAboveThreshold <- function(values, threshold, comparisonOperator)
{  
  if (comparisonOperator == "gt") {
    
    which(values > threshold)
    
  } else {
    
    which(values >= threshold)
  }
}
