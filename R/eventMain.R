# eventsByState ----------------------------------------------------------------

#' Get Events by Evaluation of a State Variable
#' 
#' @param timestamps vector of timestamps (POSIXct)
#' @param states vector of state values in which each element corresponds to one
#'   timestamp in \emph{timstamps}. If the state is the value given in
#'   \emph{in.state}) the corresponding times are considered to be lying within
#'   an event. If the state is the value given in \emph{out.state}) the
#'   corresponding times are considered to be lying out of an event. For values
#'   that are neither of the values given in \emph{in.state} and
#'   \emph{out.state}, respectively, the corresponding timestamps are considered
#'   to either belong to an event or not, depending on the previous clear state
#'   ("in" or "out") in the sequence of states.
#' @param eventSeparationTime same meaning as in \code{\link{hsEvents}}
#' @param signalWidth see description in \code{\link{hsEvents}}
#' @param in.state value in \emph{states} indicating the state "in event".
#'   Default: 1
#' @param out.state value in \emph{states} indicating the state "out of event".
#'   Default: 0
#'   
#' @return event characteristics (begin, end, duration, ...) in a data frame, as
#'   returned by \code{\link{hsEvents}}
#' 
#' @examples 
#'   
#' # Generate random timestamps
#' starttime <- as.POSIXct("2015-03-12 10:51")
#'     
#' n <- 100
#' timestamps <- seq(starttime, by = 60, length.out = n)
#' values <- rnorm(n)
#'   
#' # Give values above 1 the state "in" and values below -1 the state "out"
#' states <- rep("", times = n)
#' states[values > 1] <- "in"
#' states[values < -1] <- "out"
#'   
#' # Generate the events
#' events <- eventsByState(
#'   timestamps, states, eventSeparationTime = 5 * 60, in.state = "in", 
#'   out.state = "out", signalWidth = 60
#' )
#'   
#' # Prepare a vector of colours
#' col <- rep("black", length(states))
#' col[states == "in"] <- "green"
#' col[states == "out"] <- "red"
#'   
#' # Plot the values, the threshold lines and a legend
#' graphics::plot(timestamps, values, type = "l", ylim = c(-5, 5))
#' points(timestamps, values, col = col)
#'   
#' graphics::abline(h = c(1, -1), lty = 2)
#'   
#' legend(
#'   "topright", bty = "n", legend = c("in", "out"), col = c("green", "red"), 
#'    pch = 1, bg = "white", horiz = TRUE
#' )
#'   
#' # Plot the event borders
#' ganttPlotEvents(events, add = TRUE, y1 = -5, y2 = 4)  
#'
eventsByState <- function(
  timestamps, states, eventSeparationTime, signalWidth, in.state = 1, 
  out.state = 0
)
{    
  stopifnot(length(timestamps) == length(states))
  
  clearStates <- c(in.state, out.state)
  
  interState <- unique(states[! (states %in% clearStates)])
  
  if (length(interState) > 1) {
    
    stop(
      "There is more than one intermediate state: ", 
      kwb.utils::stringList(interState)
    )
  }
  
  # "events" (= index intervals from iBeg to iEnd) of changing section numbers
  events <- eventsOnChange(states, include.value = TRUE)
  
  # indices of intermediate states (state == "")
  transition <- which(! (events$value %in% clearStates))
  
  # assign the last state in.state or out.state to the intermediate states.
  # If the very first element is in intermediate state, it keeps intermediate.
  events$value[transition] <- events$value[.replaceZeroWithOne(transition - 1)]
  
  onTimes <- hsGetEvent(
    tSeries = data.frame(myDateTime = timestamps),    
    events = events, 
    evtnums = which(events$value == in.state), 
    useIndex = TRUE
  )$myDateTime
  
  hsEvents(onTimes, evtSepTime = eventSeparationTime, signalWidth = signalWidth)
}

# .replaceZeroWithOne ----------------------------------------------------------

.replaceZeroWithOne <- function(x)
{
  x[x == 0] <- 1
  x
}

# renumberEvents ---------------------------------------------------------------

#' renumberEvents
#' 
#' add event number (= real row number) in column \emph{event}
#' 
#' @param events event information as returned by \code{\link{hsEvents}}
#' 
#' @return data frame with (additional) column \emph{event}
#' 
renumberEvents <- function(events)
{
  events$event <- seq_len(nrow(events))
  
  events
}

# invertedEvents ---------------------------------------------------------------

#' Events to Gaps between Events
#' 
#' "inverted" events: gaps between ends of events and begins of next events
#' 
#' @param events data frame with columns \emph{tBeg} (begin of event) and
#'   \emph{tEnd} (end of event), representing events
#'   
invertedEvents <- function(events)
{
  .checkEvents(events)
  
  signalWidth <- .getSignalWidth(events)
  
  toEvents(
    data.frame(
      tBeg = events$tEnd[-nrow(events)], 
      tEnd = events$tBeg[-1]
    ),
    signalWidth = ifelse(is.null(signalWidth), "NA", signalWidth)
  )
}

# indicesOfEventsContainingEvent -----------------------------------------------

#' indicesOfEventsContainingEvent
#' 
#' indicesOfEventsContainingEvent
#' 
#' @param events data frame with columns \emph{tBeg}, \emph{tEnd}
#' @param event data frame of one row with columns \emph{tBeg}, \emph{tEnd}
#' 
#' @return vector of indices representing the positions of the events in 
#'   \emph{events} in which \emph{event} is fully contained
#' 
indicesOfEventsContainingEvent <- function(events, event)
{
  .checkEvents(events)
  
  stopifnot(nrow(event) == 1) 
  
  which(event$tBeg >= events$tBeg & event$tEnd <= events$tEnd)
}

# indicesOfEventsContainedInEvent ----------------------------------------------

#' indicesOfEventsContainedInEvent
#' 
#' @param events data frame with columns \emph{tBeg}, \emph{tEnd}
#' @param event data frame of one row with columns \emph{tBeg}, \emph{tEnd}
#' 
#' @return vector of indices representing the positions of the events in 
#'   \emph{events} that are fully contained in \emph{event}
#' 
indicesOfEventsContainedInEvent <- function(events, event)
{
  .checkEvents(events)
  
  stopifnot(nrow(event) == 1) 
  
  which(event$tBeg <= events$tBeg & event$tEnd >= events$tEnd)
}

# eventLimits ------------------------------------------------------------------

#' only tBeg and tEnd of events (possibly extended)
#' 
#' return only the limits \emph{tBeg} and \emph{tEnd} of the event, possibly 
#'   extended by a "context"
#' 
#' @param events events as returned by e.g. \code{\link{toEvents}}
#' @param context Vector of two elements giving the "context" before and after
#'   the event to be plotted, in percentage of event duration. e.g. c(0.1, 0.2)
#'   means that the time interval to be plotted starts 10 percent of the event 
#'   duration before the event begin and ends 20 percent of the event duration 
#'   after the end of the event.
#' @param absolute if TRUE, the context values are interpreted as absolute
#'   values (seconds) instead of fractions of the event duration. Default: FALSE
#'   
#' @return data frame with columns \emph{tBeg} and \emph{tEnd}, taken from 
#'   \emph{events} and possibly reduced (tBeg) and/or extended (tEnd) by a 
#'   fraction of the event duration (read from column \emph{dur} in 
#'   \emph{events}).
#' 
eventLimits <- function(events, context = c(0, 0), absolute = FALSE)
{
  if (absolute) {
    
    durationInSeconds <- 1
    
  } else {
    
    timeUnit <- .getCurrentTimeUnitOrStop(events)
    secondsPerTimeUnit <- .getSecondsPerTimeUnitOrStop(timeUnit)
    durationInSeconds <- events$dur * secondsPerTimeUnit    
  }
  
  data.frame(
    tBeg = events$tBeg - context[1] * durationInSeconds,
    tEnd = events$tEnd + context[2] * durationInSeconds
  )
}

# .getCurrentTimeUnitOrStop ----------------------------------------------------

.getCurrentTimeUnitOrStop <- function(events) 
{
  currentTimeUnit <- .getTimeUnit(events)
  
  if (is.null(currentTimeUnit))
    stop("Event data frame does not provide the attribute \"tUnit\".")  
  
  currentTimeUnit
}

# .getTimeUnit -----------------------------------------------------------------

.getTimeUnit <- function(events) 
{
  attr(events, "tUnit")  
}

# .getSignalWidth --------------------------------------------------------------

.getSignalWidth <- function(events, default = 0) 
{
  signalWidth <- attr(events, "signalWidth")
  
  if (is.null(signalWidth)) {
    
    # try to determine the signal width from tBeg, tEnd and dur
    if (all(.eventColumns(duration = TRUE) %in% names(events))) {
      
      signalWidth <- hsSigWidth(events)
    } else {
      
      signalWidth <- default
      
      messageText <- paste(
        "Signal width is set to", default, "(seconds) since no value is given."
      )
      
      if (!is.na(default) & default == 0) {
        messageText <- paste (
          messageText, "This may lead to events of duration 0!"
        )
      }
    }
    
    warning(messageText)    
  }
  
  signalWidth
}

# .getSecondsPerTimeUnitOrStop -------------------------------------------------

.getSecondsPerTimeUnitOrStop <- function(timeUnit)
{
  .stopOnUnknownTimeUnit(timeUnit)
  
  .lookupTableTimeUnitToSeconds()[[timeUnit]]
}

# .stopOnUnknownTimeUnit -------------------------------------------------------

.stopOnUnknownTimeUnit <- function(timeUnit, type = "")
{
  knownUnits <- names(.lookupTableTimeUnitToSeconds())
  
  if (! (timeUnit %in% knownUnits)) {
    
    stop(sprintf(
      "%s time unit \"%s\" not in list of supported units (%s)", 
      type, timeUnit, kwb.utils::stringList(knownUnits)
    ))
  }
}

# .lookupTableTimeUnitToSeconds ------------------------------------------------

.lookupTableTimeUnitToSeconds <- function()
{
  data.frame(s = 1, min = 60, h = 3600, d = 86400)  
}

# eventToXLim ------------------------------------------------------------------

#' tBeg and tEnd of event to two-element vector
#' 
#' puts tBeg and tEnd of event into a vector of two POSIXct elements
#' 
#' @param event data frame with columns \emph{tBeg}, \emph{tEnd}
#'   
#' @return vector of two elements: \emph{tBeg} and \emph{tEnd} from
#'   \emph{event}, both converted to UTC timezone
#'   
eventToXLim <- function(event)
{
  kwb.datetime::toUTC(c(event$tBeg, event$tEnd))
}

# getXLimFromEventLists --------------------------------------------------------

#' overall first begin and last end of events
#' 
#' minimum tBeg and maximum tEnd found in event lists
#' 
#' @param eventLists list of data frames containing events (containing columns
#'   \emph{tBeg}, \emph{tBeg}, as returned by \code{\link{hsEvents}})
#'   
#' @return vector of two elements: the first begin (minimum of tBeg) and the
#'   last end (maximum of tEnd), found in any of the event data frames given in 
#'   \emph{eventLists}
#'   
getXLimFromEventLists <- function(eventLists)
{
  n <- length(eventLists)
  
  for (i in 1:n) {
    
    events <- eventLists[[i]]
    
    if (i == 1) {
      
      firstBegin <- min(events$tBeg)
      lastEnd <- max(events$tEnd)
      
    } else {
      
      firstBegin <- min(firstBegin, min(events$tBeg))
      lastEnd <- max(lastEnd, max(events$tEnd))      
    }
  }
  
  c(firstBegin, lastEnd)
}

# hsEventNumber ----------------------------------------------------------------

#' number timestamps according to event information
#' 
#' numbering timestamps according to event information
#' 
#' @param tstamps vector of timestamps
#' @param events event information as returned by \code{\link{hsEvents}}
#' @param eventNumbers optional vector of event numbers with as many elements as
#'   there are rows in \emph{tstamps}. Default: 1:nrow(\emph{events})
#' @param commaSeparated if there are timestamps taht belong to more than one
#'   event, the default behaviour (commaSeparated = FALSE) of this function is
#'   to return a list with each list element being a vector of integer numbers
#'   representing the numbers of events to which the corresponding timestamps
#'   belong. With commaSeparated = TRUE, the list of event numbers is converted
#'   into a vector of character where each element is a text string in which
#'   more than one event number are separated by a comma. E.g. c("1", "1,2",
#'   "2") would be returned if the first timestamp belongs to event 1, the
#'   second to both event 1 and 2, and the third to event 2.
#' 
hsEventNumber <- function(
  tstamps, events, eventNumbers = 1:nrow(events), commaSeparated = FALSE
)
{
  ## number of events
  nevts <- if (is.null(events)) {
    0
  } else {
    nrow(events)    
  }
  
  ## Return if there are no events
  if (nevts == 0) {
    
    return()
  }

  stopifnot(length(eventNumbers) == nevts)  
  
  ## Check if columns "tBeg" and "tEnd" exist
  .checkEvents(events)
  
  ## init result vector with NA
  #evtNumber <- rep(NA, length(tstamps))
  evtNumber <- as.list(rep(as.numeric(NA), length(tstamps)))
  
  tstamps <- as.double(tstamps)
  
  for (i in 1:nevts) {
    
    indices <- which(
      "&"(tstamps >= as.double(events$tBeg[i]), 
          tstamps <= as.double(events$tEnd[i]))
    )
    
    #evtNumber[condition] <- eventNumbers[i]
    
    if (length(indices) > 0) {
      
      evtNumber[indices] <- sapply(
        evtNumber[indices], 
        FUN = function(x) c(stats::na.omit(x), eventNumbers[i]),
        simplify = FALSE
      )
    }
  }

  if (any(sapply(evtNumber, length) > 1)) {
    
    infotext <- "There are timestamps that belong to more than one event!\n"
    
    if (commaSeparated) {
      
      evtNumber <- sapply(evtNumber, paste, collapse = ",")
      
      infotext <- paste(
        infotext, 
        "A vector of character is returned where each element is a text string",
        "in which more than one event numbers are separated by a comma.")
      
    } else {
      
      infotext <- paste(infotext, 
      "A list of numeric vectors is returned. The vector at each list item",
      "[[i]] contains the numbers of the events to which the timestamp at",
      "corresponding index [i] in tstamps belongs (or NA if the corresponding",
      "timestamp does not belong to any event). Use 'commaSeparated = TRUE'",
      "to change this behaviour.")
    }
    
    warning(infotext)
    
  } else {
    
    evtNumber <- unlist(evtNumber)
  }

  evtNumber
}

# hsSigWidth -------------------------------------------------------------------

#' Find signal width in event list
#' 
#' Calculates signal width that was applied in event list \emph{evts}
#' 
#' @param evts data frame containing events (as e.g. provided by hsEvents)
#' @param dbg if \code{TRUE}, debug messages are shown.
#' 
#' @return signal width in seconds
#' 
hsSigWidth <- function(evts, dbg = FALSE) 
{
  ## get names of required columns
  requiredColumns <- .eventColumns(duration = TRUE)
  
  ## Stop if evts is not a data frame or does not contain the required columns
  .checkEvents(evts, requiredColumns)
  
  ## Convert to seconds
  evts <- hsEventsToUnit(evts, "s")
  
  ## Calculate signal width from events in evts
  sigWidth <- evts$dur - (as.double(evts$tEnd) - as.double(evts$tBeg))
  
  if (dbg) {
    cat("sigWidth:\n")
    print(sigWidth)
  }
  
  ## Stop if there is more than one event and if the calculated signal widths 
  ## are not all equal 
  if (length(sigWidth) > 0 && ! all(sigWidth == sigWidth[1])) {
    
    evts$calcSigWidth <- sigWidth
    
    msg <- utils::capture.output(
      print(evts[, c(requiredColumns, "calcSigWidth")])
    )
    
    stop(
      "Unambiguous signal width encountered in event list:\n", 
      paste(msg, collapse = "\n")
    )
  }
  
  ## Return signal width
  sigWidth[1]  
}

# hsEventsToUnit ---------------------------------------------------------------

#' Time unit conversion for events
#' 
#' Converts event durations and pauses before and after events to the requested 
#' time unit. The time unit will be stored in the attribute \dQuote{tUnit} of
#' the returned data frame.
#' 
#' @param evts data frame representing events as provided by hsEvents
#' @param tUnit time unit to which durations and pauses shall be converted.
#'   
#' @return data frame containing events with durations (and pauses) given in the
#'   new time unit.
#'   
hsEventsToUnit <- function(evts, tUnit)
{
  # current time unit must be given
  currentTimeUnit <- .getCurrentTimeUnitOrStop(evts)

  # time units must be known
  .stopOnUnknownTimeUnit(currentTimeUnit, "Current")
  .stopOnUnknownTimeUnit(tUnit, "Requested")

  ## Return the original events if current time unit equals requested time unit
  if (currentTimeUnit == tUnit) return(evts)
  
  ## Number of seconds per time unit
  sPerUnit <- .lookupTableTimeUnitToSeconds()
  
  ## Conversion factor between current time unit and requested time unit
  conv <- as.double(sPerUnit[currentTimeUnit]) / as.double(sPerUnit[tUnit])
  
  evts$dur <- conv * evts$dur
  if ("pBefore" %in% names(evts)) evts$pBefore <- conv * evts$pBefore
  if ("pAfter"  %in% names(evts)) evts$pAfter  <- conv * evts$pAfter
  
  ## Round to full seconds if target time unit is "s"
  if (tUnit == "s") {
    
    evts$dur <- round(evts$dur)
    
    if ("pBefore" %in% names(evts)) evts$pBefore <- round(evts$pBefore)
    if ("pAfter"  %in% names(evts)) evts$pAfter  <- round(evts$pAfter)
  }
  
  ## Set "tUnit" attribute
  attr(evts, "tUnit") <- tUnit
  
  ## Return data converted event data frame
  evts
}

# hsEvents ---------------------------------------------------------------------

#' Timestamp differences to events
#' 
#' Creates events from vector \emph{tseries} of timestamps based on time 
#' differences between consecutive timestamps in \emph{tseries}.
#' 
#' @param tseries vector containing a sorted list of timestamps.
#' @param evtSepTime \dQuote{event separation time} in seconds. Maximal allowed
#'   time difference between two consecutive timestamps within the same event.
#' @param signalWidth \dQuote{signal width} in seconds. Length of time interval
#'   that one timestamp is representing, e.g. \eqn{5*60 = 300} if each timestamp
#'   respresents a time interval of five minutes (as e.g. a time series is
#'   recorded on a five minute time scale). This parameter is needed to
#'   calculate event durations.
#' @param tUnit time unit of event duration and event pauses
#' @param pause if TRUE, pauses before and after the events are calculated
#' @param evtSepOp event separation operator, either "gt" or "ge". If
#'   \emph{evtSepOp} = "gt" (default) events are separated on time differences
#'   between two consecutive timestamps that are \emph{g}reater \emph{t}han
#'   \emph{evtSepTime}. If \emph{evtSepOp} = "ge" events are separated on time
#'   differences between two consecutive timestamps that are \emph{g}reater than
#'   or \emph{e}qual to \emph{evtSepTime}.
#' @param dbg if \code{TRUE}, debug messages are shown.
#' @param check.sorting if \code{TRUE}, it is checked whether the timestamps
#'   given in \code{tseries} are sorted and the program stops if this is not the
#'   case.
#' @return data frame with columns \emph{iBeg} and \emph{iEnd} indicating first
#'   and last index of the event in the \emph{tseries} vector, \emph{tBeg} and 
#'   \emph{tEnd} indicating first and last timestamp of the event and \emph{dur}
#'   indicating the event duration in seconds.
#'   
#' @seealso \code{\link{eventsOnChange}}
#' 
hsEvents <- function(
  tseries, evtSepTime, signalWidth, tUnit = "s", pause = TRUE, evtSepOp = "gt",
  dbg = FALSE, check.sorting = FALSE
) 
{
  ## evtSepOp must be one of "gt", "ge"
  if (! (evtSepOp %in% c("gt", "ge"))) {
    
    stop(
      "evtSepOp must be one of \"gt\" (greater than) or \"ge\" (greater than ",
      "or equal to)"
    )
  }
  
  ## If tseries is a data frame, take the first column as timestamps
  if ("data.frame" %in% class(tseries)) {
    
    tseries <- kwb.utils::firstPosixColumn(tseries)
  }
  
  n <- length(tseries)
  
  kwb.utils::catIf(dbg, "n =", n, "\n")
  
  if (check.sorting) {
    
    if (! all(order(tseries) == seq_along(tseries))) {
      
      stop ("The series of timestamps is not sorted!")
    }
  }
  
  # Find indices after which a new event begins
  tPause <- diff(as.double(tseries))
  
  # Find pauses that are greater than or greater than or equal to evtSepTime
  ind <- if (evtSepOp == "gt") {
    
    # greater than 
    tPause > evtSepTime
    
  } else {
    
    # greater than or equal to
    tPause >= evtSepTime
  }
  
  # Find the indices of the event endings and begins
  iBeg <- c(1, (2:n)[ind])
  iEnd <- c((1:(n-1))[ind], n)
  
  # Find the corresponding times
  tBeg <- tseries[iBeg]
  tEnd <- tseries[iEnd]
  
  kwb.utils::printIf(dbg, ind)
  kwb.utils::printIf(dbg, iEnd)
  kwb.utils::printIf(dbg, iBeg)
  kwb.utils::printIf(dbg, tEnd)
  kwb.utils::printIf(dbg, tBeg)

  # Create a data frame with each row representing an event
  events <- data.frame(iBeg, iEnd, tBeg, tEnd)
  
  # add columns with event durations and pauses
  events <- toEvents(
    events = events, 
    signalWidth = signalWidth, 
    timeUnit = tUnit, 
    pause = pause, 
    timeDifferences = tPause[ind]
  )
  
  # Return result data frame with parameter values as attributes
  structure(
    events, 
    eventSeparationTime = evtSepTime,
    eventSeparationOperator = evtSepOp,
    signalWidth = signalWidth
  )
}

# eventDuration ----------------------------------------------------------------

#' Event Duration
#' 
#' @param tBeg timestamps representing the event begins
#' @param tEnd timestamps representing the event ends
#' @param signalWidth see description in \code{\link{hsEvents}}
#' 
eventDuration <- function(tBeg, tEnd, signalWidth)
{
  as.double(tEnd) - as.double(tBeg) + signalWidth
}

# toEvents ---------------------------------------------------------------------

#' Convert to Data Frame of Events
#' 
#' @param events data frame with columns \emph{tBeg} (event begins) and
#'   \emph{tEnd} (event ends)
#' @param signalWidth see description in \code{\link{hsEvents}}
#' @param timeUnit time unit of event duration and event pauses
#' @param pause if TRUE, pauses before and after the events are calculated
#' @param timeDifferences if time differences have been calculated beforehand,
#'   these may be given here (in seconds)
#' 
toEvents <- function(
  events, signalWidth = .getSignalWidth(events, default = NA), timeUnit = "s",
  pause = TRUE, timeDifferences = NULL
)
{
  .checkEvents(events)
  
  # if there is a column dur, remove it first and give a warning
  events <- .removeAndWarnIfColumnExists(events, "dur")
  
  events <- cbind(
    events, 
    dur = eventDuration(events$tBeg, events$tEnd, signalWidth)
  )
  
  # Append columns pBefore, pEnd indicating pauses before and after 
  # event if requested
  if (isTRUE(pause)) {
    
    events <- .removeAndWarnIfColumnExists(events, "pBefore")
    events <- .removeAndWarnIfColumnExists(events, "pAfter")
    
    events <- cbind(
      events, 
      eventPauses(events, signalWidth, timeDifferences)
    )
  } 
  
  ## Set time unit as attribute of result data frame
  attr(events, "tUnit") <- "s"
  
  ## Convert durations to requested time unit if time unit does not equal "s"
  if (timeUnit != "s") {
    events <- hsEventsToUnit(events, timeUnit)
  }
  
  events
}

# .removeAndWarnIfColumnExists -------------------------------------------------

.removeAndWarnIfColumnExists <- function(events, columnName)
{
  if (columnName %in% names(events)) {
    
    events <- kwb.utils::removeColumns(events, columnName)
    
    warning(sprintf("The existing column '%s' was recalculated!", columnName))
  }
  
  events
}

# .checkEvents -----------------------------------------------------------------

.checkEvents <- function(events, requiredColumns = .eventColumns(...), ...)
{  
  if (!is.data.frame(events)) {
    stop("events is expected to be a data frame!")
  }
  
  kwb.utils::checkForMissingColumns(events, requiredColumns)
}

# .eventColumns ----------------------------------------------------------------

.eventColumns <- function(useIndex = FALSE, duration = FALSE)
{
  requiredColumns <- if (isTRUE(useIndex)) {
    c("iBeg", "iEnd")
  }
  else {
    c("tBeg", "tEnd")
  }
  
  if (isTRUE(duration)) {
    
    requiredColumns <- c(requiredColumns, "dur")
  }

  requiredColumns
}

# eventPauses ------------------------------------------------------------------

#' Pauses between Events in Seconds
#' 
#' @param events event information as returned by \code{\link{hsEvents}}
#' @param signalWidth see description in \code{\link{hsEvents}}
#' @param timeDifferences if time differences have been calculated beforehand,
#'   these may be given here (in seconds)
#'   
#' @return data frame with columns \emph{pBefore}, \emph{pEnd} with the pauses 
#'   between the given \emph{events}, in seconds
#' 
eventPauses <- function(
  events, signalWidth = .getSignalWidth(events), timeDifferences = NULL
)
{ 
  .checkEvents(events)
  
  if (is.null(timeDifferences)) {
    
    # Remove first element from the event begins and the last element from the
    # event ends and calculate the differences
    n <- nrow(events)    
    timeDifferences <- as.double(events$tBeg[-1]) - as.double(events$tEnd[-n])
  }  
  
  timeDifferencesToPauses(timeDifferences, signalWidth)
}

# timeDifferencesToPauses ------------------------------------------------------

#' Time Differences to Columns "pBefore" and "pAfter"
#' 
#' @param timeDifferences numeric vector representing time differences
#' @param signalWidth difference between two consecutive timesteps in the 
#'   original time series
#' 
#' @return data frame with columns \emph{pBefore}, \emph{pAfter}, containing the
#'   given \emph{timeDifferences}, shifted against each other by one row, i.e.
#'   the first element in column \emph{pBefore} and the last element in column 
#'   \emph{pAfter} will be NA.
#' 
timeDifferencesToPauses <- function(timeDifferences, signalWidth = 0)
{
  data.frame(
    pBefore = c(NA, timeDifferences) - signalWidth, 
    pAfter  = c(timeDifferences, NA) - signalWidth
  )  
}

# eventsOnChange -------------------------------------------------------------

#' Changes in Value Vector to Events
#' 
#' Creates \dQuote{events} from vector \emph{x} of values based on changes in 
#' the value of consecutive elements in \emph{x}.
#' 
#' @param x vector containing elements to be grouped into \dQuote{events}
#' @param numberOnly if TRUE, only the number of \dQuote{events} is returned
#'   instead of a data frame containing first and last index of each
#'   \dQuote{event}.
#' @param include.value if TRUE and \emph{numberOnly} is FALSE, the returned
#'   data frame will contain a column \emph{value} containing the value that was
#'   found in each index section between \emph{iBeg} and \emph{iEnd}.
#'   
#' @return Per default (\emph{numberOnly} = FALSE) a data frame is returned with
#'   as many rows as \dQuote{events} were found in vector \emph{x}. As long as
#'   the value in \emph{x} does not change from one index to the next, it is 
#'   assumed to belong to the same event. If the value changes, a new event 
#'   begins. In the result data frame each event is represented by \emph{iBeg} 
#'   and \emph{iEnd} which are the indices of the first and last element, 
#'   respectively, in \emph{x} that build the event. If \emph{numberOnly} is 
#'   TRUE the number of \dQuote{events} is returned, that is one plus the number
#'   of changes in the value of \emph{x} from its first to its last element.
#' 
#' @seealso \code{\link{hsEvents}}
#' 
#' @examples 
#' eventsOnChange(c(1,2,2,3,4,4,4,5))
#'   
#' # Ouput: list of five events, i.e. there are four changes of 
#' #        the value in the given vector.
#' #
#' #   iBeg iEnd
#' # 1    1    1
#' # 2    2    3
#' # 3    4    4
#' # 4    5    7
#' # 5    8    8
#'   
#' eventsOnChange(c(1, 2, 2, 3, 4, 4, 4, 5), numberOnly = TRUE) ## 5 (events)
#'   
eventsOnChange <- function(x, numberOnly = FALSE, include.value = FALSE) 
{  
  changes <- kwb.utils::findChanges(x)
  
  # If desired return the number of events only
  if (numberOnly) {
    return(nrow(changes))
  }
  
  changes <- kwb.utils::renameColumns(changes, list(
    starts_at = "iBeg", 
    ends_at = "iEnd"
  ))
  
  if (include.value) {
    return(changes)
  } 
  
  kwb.utils::removeColumns(changes, "value")
}

# hsGetEvent -------------------------------------------------------------------

#' Get Sub-Timeseries belonging to Event(s)
#' 
#' @param tSeries data frame representing time series with first column holding
#'   the timestamp
#' @param events event information as returned by \code{\link{hsEvents}}
#' @param evtnums vector of event numbers to be selected
#' @param useIndex if TRUE, \emph{tSeries} is filtered by comparing the real row
#'   number in \emph{tSeries} with the begin and end indices given in columns 
#'   \emph{iBeg} and \emph{iEnd} of \emph{events}. If FALSE, tSeries is filtered
#'   by comparing the timestamps in tSeries with the begin and end timestamps
#'   given in columns \emph{tBeg} and \emph{tEnd} of \emph{events}. Defaults to
#'   TRUE if \emph{events} contains columns \emph{iBeg} and \emph{iEnd}
#'   
#' @return rows of \emph{tSeries} belonging to the event numbers listed in 
#'   \emph{evtnums}
#'   
hsGetEvent <- function(tSeries, events, evtnums, useIndex = FALSE)
{ 
  .checkEvents(events, requiredColumns = .eventColumns(useIndex = useIndex))
  
  ## Init result data frame
  res <- NULL
  
  ## Loop through numbers of events to be selected
  for (evtnum in evtnums) {
    
    # If row IDs are available then filter by row ID else by timestamp
    if (isTRUE(useIndex)) {
      
      rowinds <- events$iBeg[evtnum]:events$iEnd[evtnum]        
      
    } else {
      #tstamps <- tSeries[[1]]
      #rowinds <- (   (tstamps >= events$tBeg[evtnum]) 
      #             & (tstamps <= events$tEnd[evtnum]))      
      rowinds <- kwb.datetime::timestampIn(
        tSeries[[1]], events$tBeg[evtnum], events$tEnd[evtnum], TRUE, TRUE
      )
    }
    
    res <- rbind(res, tSeries[rowinds, , drop = FALSE])
  }
  
  res
}
