#' A function to convert a `svyrep.design` into a dtsvyrep
#' @param svyrep svyrep.design
#' @export
#' @importFrom data.table data.table as.data.table setattr ".I" setnames
#'
dtrepsurvey <- function(svyrep){

  #bindings to get past rcheck
  sv <- ids <- scaledata <- cw <- mse <- selfrep <- NULL


  stopifnot(inherits(svyrep, 'svyrep.design'))
  #extract the variables and convert to data.frame
  DT = data.table::as.data.table(svyrep$variables)
  DT[, `_id` := .I]


  sdes = data.table::data.table(pweights = svyrep$pweights)
  sdes[, `_id`:= .I]

  if(inherits(svyrep$repweights, 'repweights_compressed')){
    #this will decompress the weights but whatever
    idx = svyrep$repweights$index
    wts = data.table(svyrep$repweights$weights)[, `_id` := .I]
    sdes = data.table(`_id` = idx)
    sdes = merge(sdes, wts, by = '_id', all.x = T)
    rm(idx); rm(wts);
    data.table::setorder(sdes, `_id`)
    sdes[, `_id` := .I]

  }else{
    sdes = data.table::data.table(svyrep$repweights)
    sdes[, `_id` := .I]
  }
  data.table::setnames(sdes, paste0('V', seq_len(ncol(sdes)-1)), paste0('rep', seq_len(ncol(sdes)-1)))

  sdes[, pweights := svyrep$pweights]

  data.table::setattr(DT, 'sdes', sdes)
  data.table::setattr(DT, 'stype', 'svyrepdt')
  data.table::setattr(DT, 'scaledata', list(scale = svyrep$scale, rscales = svyrep$rscales))
  data.table::setattr(DT, 'combined.weights', svyrep$combined.weights)
  data.table::setattr(DT, 'mse', svyrep$mse)
  data.table::setattr(DT, 'selfrep', svyrep$selfrep)
  setattr(DT, 'class', c('dtrepsurvey', class(DT)))

  return(DT)
}
