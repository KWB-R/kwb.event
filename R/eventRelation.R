# overlapping ------------------------------------------------------------------

#' Are there overlapping Events?
#' 
#' Check event list for overlaps (begin of one event before the end of a 
#'   previous event)
#' 
#' @param events event list as returned by \code{\link{hsEvents}}
#' @return TRUE if there are overlapping events, otherwise FALSE
#' @export
#' @examples 
#' events <- kwb.event::exampleEvents()
#'   
#' # The example events do not overlap
#' overlapping(events)
#'   
#' # The order of the events (here reverse order) does not matter
#' overlapping(events[nrow(events):1, ])
#'   
#' # Put the begin of the second event before the end of the last event
#' events$tBeg[2] <- mean(c(events$tBeg[1], events$tEnd[1]))
#'   
#' # Now there are overlapping events!
#' overlapping(events)
#'   
overlapping <- function(events)
{
  .checkEvents(events)
  
  N <- nrow(events)
  
  # Order the events by their begin time
  events <- events[order(events$tBeg), ]
  
  ! all(events$tEnd[-N] < events$tBeg[-1])
  ### TRUE if there are overlapping events, otherwise FALSE
}

# getParallelEventNotEndingAfter -----------------------------------------------

#' "Parallel" Events
#' 
#' Calculates event borders (event begin, event end) considering "parallel" 
#' events2. The returned results not ending after events1. For each event
#' \emph{E} in \emph{events1} (defined by event number, event begin and event
#' end time), this function first identifies the "partner" events E2,i from
#' \emph{events2} that lie within \emph{E} or have an intersecton with \emph{E}.
#' There may be no, one ore more than one "partner" events.
#' 
#' @param events1 data frame containing the reference events, e.g. discharge
#'   events
#' @param events2 parallel events, e.g. rain events
#' @param eventRelations event relations as returned by
#'   \code{\link{getEventRelations}}
#' @param extended if \code{TRUE}, the output contains more columns as the
#'   minimum columns that are containd else: \code{event1}, \code{tBeg.merged},
#'   \code{tEnd.merged}, \code{event2first}, \code{event2last}
#' @export
getParallelEventNotEndingAfter <- function(
  events1, events2, eventRelations, extended = FALSE
)
{
  # union rain events
  firstPartnerRain <- kwb.utils::hsRenameColumns(
    stats::aggregate(event2 ~ event1, data = eventRelations, FUN = min), 
    list(event2 = "event2first")
  )
  
  lastPartnerRain <- kwb.utils::hsRenameColumns(
    stats::aggregate(event2 ~ event1, data = eventRelations, FUN = max), 
    list(event2 = "event2last")
  )
  
  firstPartnerRain <- merge(
    firstPartnerRain, 
    events2[, c("event", "tBeg")], 
    by.x = "event2first", 
    by.y = "event",
    all.x = TRUE
  )
  
  lastPartnerRain <- merge(
    lastPartnerRain, 
    events2[, c("event", "tEnd")], 
    by.x = "event2last", 
    by.y = "event",
    all.x = TRUE    
  )
  
  mergedEventInfo <- kwb.utils::hsRenameColumns(
    events1[, c("event", "tBeg", "tEnd")], 
    list(event = "event1")
  )
  
  mergedEventInfo <- merge(
    mergedEventInfo, 
    firstPartnerRain, 
    by = "event1", 
    all.x = TRUE,
    suffixes = c(".event1", ".event2first")
  )
  
  mergedEventInfo <- merge(
    mergedEventInfo,
    lastPartnerRain, 
    by = "event1", 
    all.x = TRUE, 
    suffixes = c(".event1", ".event2last")
  )
  
  nEvents2 <- mergedEventInfo$event2last - mergedEventInfo$event2first + 1
  mergedEventInfo$nEvents2 <- nEvents2
  
  # as begin of the "merged" event we take the later of either the begin of the
  # partner event or the end of the last reference event
  
  n <- nrow(mergedEventInfo)
  
  indicesSource <- seq(from = 1, length.out = n - 1)
  indicesTarget <- seq(from = 2, length.out = n - 1)
  
  mergedEventInfo$tEnd.previous <- mergedEventInfo$tEnd.event1
  mergedEventInfo$tEnd.previous[1] <- NA
  mergedEventInfo$tEnd.previous[indicesTarget] <- mergedEventInfo$tEnd.event1[indicesSource]
  
  mergedEventInfo$tBeg.merged <- pmax(
    mergedEventInfo$tBeg.event2first,
    mergedEventInfo$tEnd.previous,
    na.rm = TRUE
  )
  
  # as end of the "merged" event we take the earlier of either the end of the
  # partner event or the end of the reference event
  mergedEventInfo$tEnd.merged <- pmin(
    mergedEventInfo$tEnd.event1, 
    mergedEventInfo$tEnd.event2last
  )
  
  if (extended) {
    
    mergedEventInfo    
    
  } else {
    
    kwb.utils::selectColumns(mergedEventInfo, c(
      "event1", "tBeg.merged", "tEnd.merged", "event2first", "event2last"
    ))
  }
}

# analyseEventRelations --------------------------------------------------------

#' Analyse Event Relations
#' 
#' @param eventRelations data frame as returned by
#'   \code{\link{getEventRelations}}
#' @export
analyseEventRelations <- function(eventRelations)
{
  # bring relations into order
  eventRelations$beginRelation <- factor(
    eventRelations$beginRelation, 
    levels = c("beginsBefore", "beginsAt", "beginsAfter")
  )
  
  eventRelations$endRelation <- factor(
    eventRelations$endRelation, 
    levels = c("endsBefore", "endsAt", "endsAfter")
  )
  
  # Frequency of cases table
  
  relationStatBegin <- table(eventRelations[, c(4, 3)])
  cat("\n*** Relation between begins of events:\n\n")
  print(relationStatBegin)
  
  cat("\n*** Relation between ends of events:\n\n")
  relationStatEnd <- table(eventRelations[, c(3, 4)])
  print(relationStatEnd)
  
  referenceName = attr(eventRelations, "referenceName")
  partnerName = attr(eventRelations, "partnerName")
  
  for (i in 1:2) {
    
    dimnames(relationStatBegin)[[i]] <- paste(
      partnerName, dimnames(relationStatBegin)[[i]], referenceName
    )
    
    dimnames(relationStatEnd)[[i]] <- paste(
      partnerName, dimnames(relationStatEnd)[[i]], referenceName
    )
  }
  
  graphicalParameters <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(graphicalParameters))
  
  graphics::par(mfrow = c(1, 2))
  
  cex <- 0.8
  
  graphics::barplot(relationStatBegin, cex.names = cex)
  
  graphics::title(main = sprintf(
    "Relations between begin of '%s'-event and begin of '%s'-event",
    partnerName, referenceName
  ))
  
  graphics::barplot(
    relationStatEnd, 
    main = sprintf(
      "Relations between end of '%s'-event and end of '%s'-event",
      partnerName, referenceName
    ),
    cex.names = cex
  )
  
  invisible(list(
    beginRelations = relationStatBegin, 
    endRelations = relationStatEnd
  ))
}

# getEventRelations ------------------------------------------------------------

#' Begin/End Relations between Events
#' 
#' relations between begin and end of events
#' 
#' @param events list of event lists (in a data frame), as e.g. returned by 
#'   \code{\link{getEventsWithStatisticsForMultipleSeries}}
#' @param referenceName name of column in \emph{parallelEventInfo} containing
#'   the numbers of the events to which the events in column \emph{partnerName}
#'   are to be compared.
#' @param partnerName name of column in \emph{parallelEventInfo} containing the
#'   numbers of the events which are compared to the events in column 
#'   \emph{partnerName}.
#' @param parallelEventInfo data frame as returned by
#'   \code{\link{getParallelEventsInfo}}. If NULL, \emph{events} must be
#'   specified.
#' @param dbg if \code{TRUE}, debug messages are shown.
#' @export
#' @examples 
#' # Load example data set containing a list of rain events at different gauges
#' data(rainEvents)
#'   
#' cat(sprintf(
#'   "Event lists available for: %s\n", 
#'    paste(names(rainEvents), collapse = ", ")
#' ))
#'   
#' # How are rain events in BlnX related to rain events in Wil?
#' eventRelations <- getEventRelations(
#'   events = rainEvents, 
#'   referenceName = "BlnX", 
#'   partnerName = "Wil"
#' )
#'   
#' # Let's have a look at the output
#' eventRelations
#'   
#' # Example 1: partner events that are fully containing the reference events
#' isContaining <- eventRelations$beginRelation == "beginsBefore" & 
#' eventRelations$endRelation == "endsAfter"
#'   
#' # The following table relates numbers of "partner" events (event2) to numbers
#' # of "reference" events (event1) for each case in which a reference event is
#' # fully contained in a partner event.
#' containing <- eventRelations[isContaining, ]
#'   
#' # Let's check this graphically:
#'   
#' # Define plot matrix of two rows and one column
#' old.par <- graphics::par(mfrow = c(2, 1))
#'   
#' ganttPlotEvents(
#'   events = rainEvents$BlnX[], 
#'   indicate = containing$event1, 
#'   ylim = c(1, 2.8), 
#'   title = "BlnX",
#'   indicationColour = "blue"  
#' )
#'   
#' ganttPlotEvents(
#'   rainEvents$Wil, 
#'   indicate = containing$event2, 
#'   add = TRUE, 
#'   y1 = 1.8, 
#'   title = "Wil"
#' )
#'   
#' graphics::title("Events at Wil (red), fully containing events at BlnX (blue)")
#'   
#' # Example 2: partner events that are starting before the reference event starts
#' # and ending before the reference event ends
#' isOverlappingLeft <- eventRelations$beginRelation == "beginsBefore" & 
#' eventRelations$endRelation == "endsBefore"
#'   
#' overlappingLeft <- eventRelations[isOverlappingLeft, ]
#'   
#' # Again, check this graphically:
#' ganttPlotEvents(
#'   events = rainEvents$BlnX[], 
#'   indicate = overlappingLeft$event1, 
#'   ylim = c(1, 2.8), 
#'   title = "BlnX",
#'   indicationColour = "blue"  
#' )
#'   
#' ganttPlotEvents(
#'   rainEvents$Wil, 
#'   indicate = overlappingLeft$event2, 
#'   add = TRUE, 
#'   y1 = 1.8, 
#'   title = "Wil"
#' )
#'   
#' graphics::title(paste(
#'   "Events at Wil (red), starting before the start and ending before",
#'   "the end\nof the events at BlnX (blue)"
#' ))
#'   
#' # Reset graphical parameters
#' graphics::par(old.par)  
#'   
getEventRelations <- function(
  events, referenceName, partnerName, parallelEventInfo = NULL, dbg = TRUE
) 
{
  if (is.null(parallelEventInfo)) {
    parallelEventInfo <- getParallelEventsInfo(events)
  } 
  
  referenceEventNumbers <- unique(stats::na.omit(
    parallelEventInfo[[referenceName]]
  ))
  
  eventRelations <- NULL
  
  for (referenceEventNumber in referenceEventNumbers) {
    
    kwb.utils::catIf(dbg, "referenceEventNumber:", referenceEventNumber)

    rows <- which(parallelEventInfo[[referenceName]] == referenceEventNumber)
    
    partnerEventNumbers <- unique(stats::na.omit(
      parallelEventInfo[rows, partnerName]
    ))
    
    n <- length(partnerEventNumbers)
    
    if (n == 0) {
      
      if (dbg) {
        message("No partner event at ", partnerName, " found for event", 
                referenceEventNumber, " at ", referenceName, "\n")
      }
    } else {
      
      kwb.utils::catIf(dbg, " ->", n, " partner events found.\n")        
      event1 <- events[[referenceName]][referenceEventNumber, ]
      events2 <- events[[partnerName]][partnerEventNumbers, ]
      
      eventRelations <- rbind(eventRelations, eventRelation(event1, events2))
    }
  }  
  
  structure(
    eventRelations, 
    referenceName = referenceName, 
    partnerName = partnerName
  )
}

# eventRelation ----------------------------------------------------------------

#' Relations of Start/End Times of Events
#' 
#' @param event1 data frame containing exactly one row representing the event to
#'   which the event(s) in \emph{events2} is/are to be compared. Columns
#'   \emph{tBeg} (begin of event), \emph{tEnd} (end of event) and \emph{event}
#'   (event number/ID) are required.
#' @param events2 data frame containing in rows the event(s) that are to be
#'   compared to the event given in \emph{event1}. Columns \emph{tBeg} (begin of
#'   event), \emph{tEnd} (end of event) and \emph{event} (event number/ID) are
#'   required.
#' @export
eventRelation <- function(event1, events2) 
{
  x <- data.frame(
    event1 = event1$event,
    event2 = events2$event,
    begin1 = event1$tBeg, 
    begin2 = events2$tBeg, 
    end1 = event1$tEnd,
    end2 = events2$tEnd
  )
  
  x$equalBegin <- x$begin1 == x$begin2
  x$equalEnd <- x$end1 == x$end2
  x$earlierBegin <- x$begin2 < x$begin1
  x$laterEnd <- x$end2 > x$end1
  
  x$beginRelation <- ifelse(
    x$equalBegin, "beginsAt", ifelse(
      x$earlierBegin, "beginsBefore", "beginsAfter"))
  
  x$endRelation <- ifelse(
    x$equalEnd, "endsAt", ifelse(
      x$laterEnd, "endsAfter", "endsBefore"))
  
  x[, c("event1", "event2", "beginRelation", "endRelation")]
}
