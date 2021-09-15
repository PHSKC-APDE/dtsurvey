#' Convert a data.table into a dtadmin, dtsurvey object to use some additional functionality
#' @param DT data.table
#' @param copy logical. Determines whether a copy of DT is taken before setting new attributes.
#'
#' @details dtadmin by default copies the dataset (not be reference)
#' @export
dtadmin <- function(DT, copy = TRUE){

  if(copy) DT <- data.table::copy(as.data.table(DT))

  stopifnot('DT must either be a data.table or convertible to one with copy = TRUE' = data.table::is.data.table(DT))

  #create it as a dtsurvey, but with no survey design variables.
  DT = dtsurvey(DT)
  data.table::setattr(DT, 'class', c('dtadmin', class(DT)))
  data.table::setattr(DT, 'stype', 'admin')

  return(invisible(DT))
}
