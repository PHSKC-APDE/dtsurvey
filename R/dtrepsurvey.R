#' A function to convert a `svyrep.design` into a dtsvyrep
#' @param svyrep svyrep.design
#' @export
dtrepsurvey <- function(svyrep){

  stopifnot(inherits(svyrep, 'svyrep.design'))
  stopifnot("Not currently set up to deal with compressed weights" = !inherits(svyrep$repweights,'repweights_compressed'))

  #extract the variables and convert to data.frame
  DT = data.table::as.data.table(svyrep$variables)
  DT[, `_id` := .I]


  sdes = data.table(cbind(svyrep$pweights, svyrep$repweights))
  data.table::setnames(sdes, c('pweights', paste0('rep', seq_len(ncol(sdes)-1))))

  data.table::setattr(DT, 'sdes', sdes)
  data.table::setattr(DT, 'stype', 'svyrepdt')
  data.table::setattr(DT, 'scaledata', list(scale = svyrep$scale, rscales = svyrep$rscales))
  data.table::setattr(DT, 'combined.weights', svyrep$combined.weights)
  data.table::setattr(DT, 'mse', svyrep$mse)
  setattr(DT, 'class', c('dtrepsurvey', class(DT)))

  return(DT)
}
