# plotMergedEventInfoForValidation ---------------------------------------------

#' Plot merged Event Info for Validation
#' 
plotMergedEventInfoForValidation <- function(mergedEvents)
{
  beginEndColumns <- c(
    "tBeg.event1", "tEnd.event1", "tBeg.event2first", "tEnd.event2last", 
    "tBeg.merged", "tEnd.merged"
  )
  
  for (i in seq_len(nrow(mergedEvents))) {
    
    mergedEvent <- mergedEvents[i, ]
    
    xlim <- range(
      kwb.datetime::hsToPosix(as.vector(as.matrix(
        mergedEvent[, beginEndColumns]
      ))),
      na.rm = TRUE
    )
    
    ganttPlotEvents(
      kwb.utils::hsRenameColumns(mergedEvent, list(
        tBeg.event1 = "tBeg", tEnd.event1 = "tEnd"
      )), 
      title = "reference event", xlim = xlim, ylim = c(1, 7)
    )
    
    graphics::abline(v = mergedEvent$tEnd.event1, lty = 2)
    
    graphics::title(paste(
      "Reference event", i, "/", nrow(mergedEvents), ":", 
      mergedEvent$tBeg.event1, "-", mergedEvent$tEnd.event1
    ))
    
    ganttPlotEvents(
      kwb.utils::hsRenameColumns(mergedEvent, list(
        tBeg.event2first = "tBeg", tEnd.event2last = "tEnd"
      )),
      title = "partner event(s)", add = TRUE, y1 = 3, col = "red"
    )
    
    A <- list(col = "red", lty = 2)
    
    kwb.utils::callWith(graphics::abline, A, v = mergedEvent$tBeg.event2first)
    kwb.utils::callWith(graphics::abline, A, v = mergedEvent$tEnd.event2last)
    
    ganttPlotEvents(
      kwb.utils::hsRenameColumns(mergedEvent, list(
        tBeg.merged = "tBeg", tEnd.merged = "tEnd"
      )),
      title = "merged event",
      add = TRUE,
      y1 = 5
    )    
  }
}

# plotEventInfo ----------------------------------------------------------------

#' Plot Event Info
#' 
#' @param eventInfo as returned by \emph{getParallelEventsInfo}, with first
#'   columns (timestamps) removed
#' 
plotEventInfo <- function(eventInfo)
{
  graphics::plot(
    NA, NA, ylim = c(1, ncol(eventInfo) + 1),  xlim = c(1, nrow(eventInfo) + 1), 
    ylab = "event list number", xlab = "event number"
  )
  
  for (i in seq_len(nrow(eventInfo))) { 
    
    na.columns <- which(is.na(eventInfo[i, ]))
    
    x <- seq_along(eventInfo)
    y <- rep(i, ncol(eventInfo))
    
    col <- (eventInfo[i, ] %% 2) + 1
    
    x[na.columns] <- NA
    y[na.columns] <- NA
    
    graphics::rect(y, x, y + 1, x + 1, col = col, border = NA)
  }  
}

# plotEventProperty1VersusEventProperty2 ---------------------------------------

#' Plot Event Property 1 versus Event Property 2
#' 
#' @param events data frame with at least two columns named as given in 
#'   \emph{propertyName1} and \emph{propertyName2}
#' @param eventNumbers vector of event numbers used for labelling. Default:
#'   rownames of \emph{events}
#' @param xlab default: propertyName1
#' @param ylab default: propertyName2
#' 
plotEventProperty1VersusEventProperty2 <- function(
  events, propertyName1, propertyName2, eventNumbers = events$eventNumber,
  xlab = propertyName1, ylab = propertyName2, cex = 0.7, ...
)
{
  kwb.utils::checkForMissingColumns(events, c(propertyName1, propertyName2))
  
  x <- events[[propertyName1]]
  y <- events[[propertyName2]]
  
  if (all(is.na(x))) {
    
    .propertyWarning(propertyName1)
    
  }  else if (all(is.na(y))) {
    
    .propertyWarning(propertyName2)
    
  } else {
    
    graphics::plot(x, y, pch = 16, cex = 0.5, xlab = xlab, ylab = ylab, ...)
    
    graphics::grid()
    
    plotRegionSize <- kwb.plot::getPlotRegionSizeInUserCoords()
    
    delta <- kwb.plot::cmToUserWidthAndHeight(0.25)
    
    graphics::text(
      x + delta$width, y + delta$height, labels = eventNumbers, cex = cex
    )
  }
}

# .propertyWarning -------------------------------------------------------------

.propertyWarning <- function(propertyName)
{
  warning(
    sprintf("Event property \"%s\" is NA for all events", propertyName),
    " -> I do not plot!"
  )  
}

# ganttPlotEventLists ----------------------------------------------------------

#' Gantt Plot of Event Lists
#' 
#' Plot event lists, one above the other
#' 
#' @param eventLists list of data frames containing events (containing columns
#'   \emph{tBeg}, \emph{tBeg}, as returned by \code{\link{hsEvents}})
#' @param \dots further arguments passed to ganttPlotEvents
#' 
ganttPlotEventLists <- function(
  eventLists, margin.top = 0.8, time.format = NULL, n.xticks = 10, 
  showLabels = TRUE, ...
)
{
  time.format <- kwb.utils::defaultIfNULL(time.format, "%d.%m.")

  elementNames <- names(eventLists)
  
  eventLists$merged <- kwb.utils::defaultIfNULL(
    eventLists$merged, mergeAllEvents(eventLists)
  )
  
  n <- length(eventLists)
  
  ylim <- c(0, n + (n-1) * margin.top)
  
  bandheight <- 0.5 * margin.top / diff(ylim) # for labels
  
  y1 <- 0
  yLabel <- y1 + rep(c(-0.3, -0.1), length.out = nrow(eventLists$merged))
  
  # start with plotting the merged events (since ganttPlotEvents does not have 
  # an xlim parameter)
  ganttPlotEvents(
    eventLists$merged, y1 = y1, ylim = ylim, yLabel = yLabel, 
    showLabels = FALSE, title = "merged", ...
  )
  
  if (showLabels) {
    
    x <- 0.5 * (
      as.numeric(eventLists$merged$tBeg) + 
      as.numeric(eventLists$merged$tEnd)
    )
    
    kwb.plot::addLabels(
      x = x, y0 = y1 + 1, col.line="grey", labels = rownames(eventLists$merged), 
      bandheight = bandheight
    )
  }  
  
  for (elementName in setdiff(elementNames, "merged")) {
    
    events <- eventLists[[elementName]]
    
    y1 <- y1 + 1 + margin.top
    yLabel <- y1 + rep(c(-0.3,-0.1), length.out=nrow(events))
    
    ganttPlotEvents(
      events, add = TRUE, y1 = y1, showLabels = FALSE, title = elementName, ...
    )
    
    if (showLabels) {
      
      x <- (as.numeric(events$tBeg) + as.numeric(events$tEnd)) / 2
      
      kwb.plot::addLabels(
        x = x, y0 = y1 + 1, col.line="grey", labels = rownames(events), 
        bandheight = bandheight
      )
    }
  }
  
  xlim <- getXLimFromEventLists(eventLists)
  
  timestamps <- xlim
  
  kwb.plot::addTimeAxis(timestamps, n = n.xticks, time.format = time.format)
  
  timestamps
}

# ganttPlotEvents --------------------------------------------------------------

#' Gantt-like Diagram to plot Event's Time Extension 
#' 
#' @param events event list as retrieved by \code{\link{hsEvents}}. Required
#'   columns: \emph{tBeg} (begin of event) and \emph{tEnd} (end of event), both
#'   of class POSIXt
#' @param add if TRUE, the event boxes are added to the current plot, otherwise
#'   a new plot is generated
#' @param y1 lower coordinates of the event boxes
#' @param y2 upper coordinates of the event boxes
#' @param xlim x limits. If NULL (default) the limits will be chosen so that all
#'   events fit into the plot
#' @param ylim y limits
#' @param col colour of shading lines
#' @param density density of shading lines
#' @param showLabels if TRUE, the event boxes are labelled with the row names of
#'   the events
#' @param eventLabels labels to be given to the events. Default:
#'   rownames(\emph{events})
#' @param yLabel y-position of labels, if labels are to be shown
#' @param type one of c("rectange", "vertical")
#' @param title title to be plotted left of event rectangles
#' @param leftMargin left margin (where title is printed) as fraction of the
#'   range of the total time interval spanned by the events
#' @param indicate indices of events to be indicated in a different color
#'   (indicationColuor)
#' @param indicationColour colour to be used for indication, default: "red" 
#'   extension factor for labels (event numbers)
#' @param bandheight passed to \code{addLabels}
#' @param alternating passed to \code{addLabels}
#' @param adj passed to \code{text} plotting the event labels
#' @param \dots further arguments passed to rect or segments
#' 
ganttPlotEvents <- function(
  events, add = FALSE, y1 = 1, y2 = y1 + 1, xlim = NULL, 
  ylim = c(min(y1), max(y2)), col = "black", density = 5, showLabels = TRUE,
  eventLabels = rownames(events), yLabel = (y1 + y2) / 2, type = "rectangle",
  title = "", leftMargin = 0.2, xlab = "Time", cex = 0.8, indicate = NULL,
  indicationColour = "red", bandheight = 0.1, alternating = FALSE, adj = 0.5, 
  ...
)
{
  if (! add) {
    
    x1 <- utils::head(events$tBeg, 1)
    
    if (is.null(xlim)) {
      
      x2 <- utils::tail(events$tEnd, 1)
      xrange <- diff(as.integer(range(x1, x2)))
      xlim <- c(x1 - leftMargin*xrange, x2)      
    }
    
    graphics::plot(
      x = x1, y = rep(NA, length.out=length(x1)), xlim = xlim, ylim = ylim,
      type = "n", xlab = xlab, ylab = "", xaxt = "n", yaxt = "n"
    )
  }
  
  if (! is.null(indicate)) {
    
    col <- rep(col, length.out = nrow(events))
    col[indicate] <- indicationColour
  }
  
  if (type == "rectangle") {
    
    graphics::rect(events$tBeg, y1, events$tEnd, y2, col=col, density=density, ...)
    
  } else if (type == "vertical") {
    
    graphics::segments(events$tBeg, y1, events$tBeg, y2, col = col, ...)
    graphics::segments(events$tEnd, y1, events$tEnd, y2, col = col, ...)
    
  } else {
    
    stop("Unsupported type: ", type)
  }
  
  if (showLabels) {
    
    x <- rowMeans(data.frame(as.numeric(events$tBeg), as.numeric(events$tEnd)))
    
    kwb.plot::addLabels(
      x = x, labels = eventLabels, y0 = yLabel, bandheight = bandheight, 
      col.line = NA, alternating = alternating, adj = adj
    )
  }
  
  userCoordinates <- kwb.plot::getPlotRegionSizeInUserCoords()
  
  x <- userCoordinates$left + 0.01 * userCoordinates$width
  
  graphics::text(x, (y1 + y2) / 2, labels = title, adj = 0, cex = cex)
}

# .hsShowEvents ----------------------------------------------------------------

.hsShowEvents <- function(evts, sigWidth = 60, nTicks = 25)
{
  n <- nrow(evts)
  
  tmin <- evts$tBeg[1]
  tmax <- evts$tEnd[n]
  
  tmin <- kwb.datetime::roundTime(tmin, 10 * sigWidth, 1)
  tmax <- kwb.datetime::roundTime(tmax, 10 * sigWidth, 0)
  
  timediff <- (as.integer(tmax) - as.integer(tmin))
  tstep <- kwb.datetime::roundTime(timediff / nTicks, 10 * sigWidth, 0)
  cat("tmin:", tmin, "tmax:", tmax, "tstep:", tstep, "\n")
  
  graphics::par(mar = c(10, 5, 5, 5))
  
  graphics::plot(
    NA, NA, xlim = c(tmin, tmax), ylim = c(0, 1), axes = FALSE, xlab = NA, 
    ylab = NA
  )
  
  graphics::rect(evts$tBeg - sigWidth, 0, evts$tEnd, 1)  
  
  positions <- seq(tmin, tmax, by = tstep)
  
  graphics::axis(
    1, at = positions, labels = format(positions, "%Y-%m-%d %H:%M"), las = 3
  )
}
