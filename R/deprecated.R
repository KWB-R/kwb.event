# hsEventsOnChange -------------------------------------------------------------

#' Deprecated. Use eventsOnChange() instead.
#' 
#' @param \dots passed to \code{\link{eventsOnChange}}}
#' @export
#' @importFrom kwb.utils warningDeprecated
hsEventsOnChange <- function(...) 
{
  kwb.utils::warningDeprecated("hsEventsOnChange", "eventsOnChange")
  eventsOnChange(...)
}
