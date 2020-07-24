#' Get survey type
#'
#'@details
#' This function is only designed to be called by other functions within J of a data.table.
#' It extracts the stype attribute data.table from the calling data.table to be used for calculating the survey metrics
#'
#' @export
#'
get_svyrep_attributes <- function(){
  callme = sys.calls()
  framing = sys.frames()

  callme = lapply(callme, as.character)

  #find the last `[.data.table` call
  lastcall = which(vapply(callme, function(x) grepl('[.data.table', as.character(x)[1], fixed = T), TRUE))

  if(!any(lastcall)) stop('Using a dtsurvey function outside of `[.data.table` context')

  lastcall = lastcall[which.max(lastcall)]

  ret <- list(scaledata = attr(framing[[lastcall]]$x, 'scaledata'),
              combined.weights = attr(framing[[lastcall]]$x, 'combined.weights'),
              mse = attr(framing[[lastcall]]$x, 'mse'),
              selfrep = attr(framing[[lastcall]]$x, 'mse'))

  if(is.null(ret)) stop('No svyrep details found')

  return(ret)

}
