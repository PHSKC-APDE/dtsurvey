#' Merge information on to a dtsurvey
#' @param x data.table
#' @param y data.frame or similar object
#' @param ... options passed to merge.data.table
#' @export
merge.dtsurvey <- function(x,y,...){

  atts = attributes(x)

  r = NextMethod()

  if('_id' %in% names(r)){
    idchk = r[, `_id`]
    if(length(idchk) != length(unique(idchk))){
      stop('Merge resulted in duplication of values in `_id`. This will break/muck up survey calculations.')
    }
  }else{

    if('_id.y' %in% names(r)){
      warning('removed/fixed `_id.x` and `_id.y` problem')
      r[, '_id.y' := NULL]
      data.table::setnames(r, '_id.x', '_id')
    }else{
      warning('resulting dtsurvey object does not have `_id` column')
    }
  }
  ratts = attributes(r)
  ratts = atts[!names(atts) %in% names(ratts)]

  for(a in seq_along(ratts)){
    data.table::setattr(r, names(ratts)[a], ratts[[a]])

  }

  r

}
