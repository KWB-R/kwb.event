# readEventFilesFromDirectory --------------------------------------------------

#' Read Event Files from Directory
#' 
#' Read event definitions from files "events_*.txt" in event.dir
#' 
#' @param event.dir full path to directory containing event definition files
#' @param prefix prefix of file names to be searched for
#' @param timezone timezone to which the timestamps are to be converted.
#'   Default: "UTC"
#' @param \dots arguments passed to \code{\link{readEventsFromFile}}
#' 
readEventFilesFromDirectory <- function(
  event.dir, prefix = "events", timezone = "UTC", ...
)
{
  savedEvents <- list()
  
  filePattern <- sprintf("^%s_.*\\.txt$", prefix)
  removePattern <- sprintf("^%s_|\\.txt$", prefix)

  fileNames <- dir(event.dir, filePattern)
  
  if (length(fileNames) > 0) {
    
    for (fileName in fileNames) {

      eventName <- gsub(removePattern, "", fileName)
      filePath <- file.path(event.dir, fileName)
      
      cat("*** Reading events from", filePath, "... ")
      savedEvents[[eventName]] <- readEventsFromFile(filePath, timezone, ...)
      cat("ok.\n")
    }      
  } else {
    
    warning(
      "No event definition files matching '", filePattern, "' found in '",
      event.dir, "'!"
    )
  }
  
  savedEvents
}

# readEventsFromFile -----------------------------------------------------------

#' Read Event Limits from File
#' 
#' @param file full path to file containing the event definitions
#' @param timezone timezone to which the timestamps are to be converted.
#'   Default: "UTC"
#' @param header TRUE if the file contains a header line (first
#'   non-comment-line). Default: FALSE. If the file contains a header line, it
#'   must contain the column captions "tBeg" and "tEnd" (begin and end
#'   timestamps of the event).
#'   
#' @return data frame with columns \emph{tBeg} and \emph{tEnd} (POSIXct)
#' 
readEventsFromFile <- function(file, timezone = "UTC",header = FALSE)
{
  x <- utils::read.table(
    file = file, sep = ",", comment.char = "#", header = header, 
    blank.lines.skip = TRUE, stringsAsFactors = FALSE
  )
  
  tcols <- c("tBeg", "tEnd")
  
  if (header) {
    kwb.utils::checkForMissingColumns(x, tcols)
  } else {
    names(x) <- tcols
  }
  
  x$tBeg <- kwb.datetime::hsToPosix(x$tBeg, tzone = timezone)
  x$tEnd <- kwb.datetime::hsToPosix(x$tEnd, tzone = timezone)
  
  x
}
