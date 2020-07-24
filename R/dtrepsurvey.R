#' A function to convert a `svyrep.design` into a dtsvyrep
#' @param svyrep svyrep.design
#' @export
dtrepsurvey <- function(svyrep){

  stopifnot(inherits(svyrep, 'svyrep.design'))
  #extract the variables and convert to data.frame
  DT = data.table::as.data.table(svyrep$variables)
  DT[, `_id` := .I]


  sdes = data.table(pweights = svyrep$pweights)
  sdes[, `_id`:= .I]

  if(inherits(svyrep$repweights, 'repweights_compressed')){
    idx = svy$repweights$index$weights
    wts = svyrep$repweights
  }else{
    idx = sdes[, `_id`]
    wts = svyrep$repweights
  }

  #this will uncompress the weights but whatever
  sdes[idx, paste0('rep', seq_len(ncol(wts))) := lapply(seq_len(ncol(wts)), function(x) wts[,x])]

  data.table::setattr(DT, 'sdes', sdes)
  data.table::setattr(DT, 'stype', 'svyrepdt')
  data.table::setattr(DT, 'scaledata', list(scale = svyrep$scale, rscales = svyrep$rscales))
  data.table::setattr(DT, 'combined.weights', svyrep$combined.weights)
  data.table::setattr(DT, 'mse', svyrep$mse)
  data.table::setattr(DT, 'selfrep', svyrep$selfrep)
  setattr(DT, 'class', c('dtrepsurvey', class(DT)))

  return(DT)
}
