#' Custom extract function (or subset or whatever) for dtsurveys. A loose wrapper over `[.data.table`
#' @param x a dtsurvey object
#' @param i i in the data.table format
#' @param j j in the data.table format
#' @param by by in the data.table format
#' @param ... extra options passed to data table
#' @export
#' @name extract
"[.dtsurvey" <- function(x, i, j, by, ...){

  # if(!missing(j)){
  #   jsub = substitute(j)
  # }

  NextMethod(`[`, x)

}
