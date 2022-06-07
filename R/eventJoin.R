# hsJoinEvents -----------------------------------------------------------------

#' Join events in event list
#' 
#' Join consecutive events in event list \emph{evts}. The result of joining two
#' events A and B is a event with begin time of A and end time of B.
#' 
#' @param evts data frame containing events as provided by e.g. hsEvents
#' @param \dots numeric vectors containing the event numbers to be joined, e.g. 
#'   5:10, 15:20 will join events 5 to 10 and 15 to 20 to one event in each case
#' @param renumber if TRUE, rows in result data frame are renumbered from one to
#'   number of rows.
#' @param dbg if \code{TRUE}, debug messages are shown.
#' @return A data frame with fields \emph{tBeg}, \emph{tEnd}, \emph{dur}
#'   containing the times of event begin and event end and the event duration in
#'   seconds, respectively. The event duration is the difference between end and
#'   begin of the event plus the time period that one timestamp represents 
#'   (signal width).
#' @export
hsJoinEvents <- function(evts, ..., renumber = TRUE, dbg = FALSE) 
{
  num.events <- nrow(evts)
  intervals <- list(...)

  stopifnot(.intervalSequenceIsValid(intervals, num.events, dbg))  
  
  sigWidth <- NA  
  hasTimeColumns <- .hasTimeColumns(evts)
  
  if (hasTimeColumns) {    
    orig.time.unit <- .getTimeUnit(evts)
    evts <- hsEventsToUnit(evts, "s")
    sigWidth <- hsSigWidth(evts)
  }
  
  out <- data.frame()
  last.j <- 0 # last maximum
  
  for (interval in intervals) {
    i <- min(interval)
    j <- max(interval)
    out <- .bindRowsWithinIndexes(out, evts, last.j+1, i-1) 
    out <- rbind(out, .joinSubEvents(evts[i:j, ], sigWidth))
    last.j <- j
  }
  
  out <- .bindRowsWithinIndexes(out, evts, last.j+1, num.events)
  out <- .withRowsRenumberedIfTrue(out, renumber)
  
  if (hasTimeColumns) {
    out <- .setTimeUnitToSeconds(out)
    out <- hsEventsToUnit(out, orig.time.unit)    
  }  
  
  out
}

# .intervalSequenceIsValid -----------------------------------------------------

.intervalSequenceIsValid <- function(fromTo, num.events, dbg)
{
  # create vectors containing min and max values of index ranges in list fromTo
  maxs <- c(0, sapply(fromTo, max))
  mins <- c(sapply(fromTo, min), num.events+1)
  
  .printOnDebug(data.frame(mins, maxs), dbg)
  
  if (! all(mins > maxs)) #, na.rm = TRUE)) 
  {
    cat("\n*** Invalid sequence of event numbers. Sequences must be given in",
        "increasing order, must not overlap and must not contain numbers",
        "greater than the number of events.\n\n")
    
    return(FALSE)
  }
  
  return(TRUE)
}

# .printOnDebug ----------------------------------------------------------------

.printOnDebug <- function(object, debug)
{
  if (debug) {
    
    print(object)
  }  
}

# .catOnDebug ------------------------------------------------------------------

.catOnDebug <- function(object, debug)
{
  if (debug) {
    cat(object)
  }  
}

# .hasTimeColumns --------------------------------------------------------------

.hasTimeColumns <- function(events)
{
  "tBeg" %in% names(events)  
}

# .bindRowsWithinIndexes -------------------------------------------------------

.bindRowsWithinIndexes <- function(out, evts, i, j)
{
  if (i <= j) {
    out <- rbind(out, evts[i:j, ])      
  } 
  
  out
}

# .joinSubEvents ---------------------------------------------------------------

.joinSubEvents <- function(sub.events, sigWidth = NA)
{  
  n <- nrow(sub.events)
  col.names <- names(sub.events)
  
  result <- NULL
  
  for (col.name in col.names) {  
    
    col.values <- sub.events[, col.name]
    
    if (col.name %in% .eventColumnNamesWithoutDuration()) {
      value <- .getFirstOrNthValueDependentOnName(col.name, col.values, n)
    } 
    else if (col.name == "dur") { 
      value <- .getDurationIfPossible(sigWidth, col.names, sub.events, n)
    } 
    else {
      value <- paste(as.character(col.values), collapse = ",")
    }
    
    new.col <- .dataFrameWithNamedColumn(
      value, col.name, stringsAsFactors = FALSE
    )
    
    result <- kwb.utils::safeColumnBind(result, new.col)
  }
  
  result
}

# .eventColumnNamesWithoutDuration ---------------------------------------------

.eventColumnNamesWithoutDuration <- function() 
{
  c(.eventBeginColumnNames(), .eventEndColumnNames())
}

# .getFirstOrNthValueDependentOnName -------------------------------------------

.getFirstOrNthValueDependentOnName <- function(myname, col.values, n)
{
  value <- NA
  
  if (myname %in% .eventBeginColumnNames()) {
    
    value <- col.values[1]
    
  } else if (myname %in% .eventEndColumnNames()) {
    
    value <- col.values[n]
  }
  
  value
}

# .eventBeginColumnNames -------------------------------------------------------

.eventBeginColumnNames <- function() 
{
  c("iBeg", "tBeg", "pBefore")
}

# .eventEndColumnNames ---------------------------------------------------------

.eventEndColumnNames <- function() 
{
  c("iEnd", "tEnd", "pAfter")
}

# .getDurationIfPossible -------------------------------------------------------

.getDurationIfPossible <- function(sigWidth, col.names, sub.events, n)
{
  value <- NA
  if (!is.na(sigWidth) && ("tBeg" %in% col.names) && ("tEnd" %in% col.names)) {
    t1 <- as.integer(sub.events$tBeg[1])
    tn <- as.integer(sub.events$tEnd[n]) 
    value <- tn - t1 + sigWidth
  }
  value
}

# .dataFrameWithNamedColumn ----------------------------------------------------

.dataFrameWithNamedColumn <- function(data, col.name, ...)
{
  result <- data.frame(data, ...)
  names(result) <- col.name
  result
}

# .withRowsRenumberedIfTrue ----------------------------------------------------

.withRowsRenumberedIfTrue <- function(out, renumber)
{
  if (renumber) {
    
    rownames(out) = as.character(1:nrow(out))    
  }
  
  out
}

# .setTimeUnitToSeconds --------------------------------------------------------

.setTimeUnitToSeconds <- function(events) {
  attr(events, "tUnit") <- "s"
  events
}
