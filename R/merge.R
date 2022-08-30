#' Merge information on to a dtsurvey
#' @param x data.table
#' @param y data.frame or similar object
#' @param ... options passed to merge.data.table
#' @export
merge.dtsurvey <- function(x,y,...){

  atts = attributes(x)

  r = NextMethod()

  idchk = r[, `_id`]
  if(length(idchk) != length(unique(idchk))){
    stop('Merge resulted in duplication of values in `_id`. This will break/muck up survey calculations.')
  }

  ratts = attributes(r)

  attributes(r) <- append(ratts, atts[!names(atts) %in% names(ratts)])

  r

}
