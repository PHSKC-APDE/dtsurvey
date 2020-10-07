#' Get survey vars
#'
#'@details
#' This function is only designed to be called by other functions within J of a dtsurvey or dtrepsurvey.
#' It extracts the sdes attribute from the calling dtsurvey or dtrepsurvey to be used for calculating the survey metrics
#'
#' @export
#'
get_survey_vars <- function(){
  callme = sys.calls()
  framing = sys.frames()

  callme = lapply(callme, as.character)

  #find the last `[.data.table` call
  lastcall = which(vapply(callme, function(x) grepl('[.data.table', as.character(x)[1], fixed = T), TRUE))

  if(!any(lastcall)) stop('Using a dtsurvey function outside of `[.data.table` context')

  lastcall = lastcall[which.max(lastcall)]

  ret <- attr(framing[[lastcall]]$x, 'sdes')

  if(is.null(ret)) stop('No sdes found')

  return(ret)

}
