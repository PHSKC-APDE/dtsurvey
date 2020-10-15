#' Get survey type
#'
#'@details
#' This function is only designed to be called by other functions within J of a dtsurvey/dtrepsurvey
#' It extracts the stype attribute from the calling dtsurvey/dtrepsurvey to be used for calculating the survey metrics
#'
#' @export
#'
get_survey_type <- function(){
  callme = sys.calls()
  framing = sys.frames()

  callme = lapply(callme, as.character)

  #find the last `[.data.table` call
  lastcall = which(vapply(callme, function(x) grepl('[.data.table', as.character(x)[1], fixed = T), TRUE))
  lastcall2 = which(vapply(callme, function(x) grepl('[.dtsurvey', as.character(x)[1], fixed = T), TRUE))

  if(!any(lastcall)) stop('Using a dtsurvey function outside of `[.data.table` context')

  lastcall = lastcall[which.max(lastcall)]

  ret <- attr(framing[[lastcall]]$x, 'stype')

  if(is.null(ret)) stop('No stype found')

  return(ret)

}
