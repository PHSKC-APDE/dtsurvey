#' Convert a data.table into a dtadmin, dtsurvey object to use some additional functionality
#' @param DT data.table
#' @param copy logical. Determines whether a copy of DT is taken before setting new attributes.
#'
#' @details dtadmin by default works by reference.

dtadmin <- function(DT, copy = TRUE){
  stopifnot(data.table::is.data.table(DT))

  if(copy) DT <- data.table::copy(DT)

  data.table::setattr(DT, 'class', c('dtadmin', 'dtsurvey', class(DT)))
  return(invisible(DT))
}
